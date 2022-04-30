#load packages
library(shiny)
library(here)
library(dplyr)
library(tibble)
library(ggplot2)
library(readxl)
library(vroom)
library(rstatix)

#load functions
source(here("anova_kw_tester_app","anova_kw_tester_app_functions.R"))
    
#create list for summary tables
stat_list<-list(n=length,min=min,median=median,mean=mean,max=max,sd=sd,se=function(x) sd(x)/sqrt(length(x)))


ui<-navbarPage("ANOVA & Kruskal-Wallis Tester",id="mainTabs",
  tabPanel("Data Input & Exploration",value="data_input",
    sidebarLayout(
      sidebarPanel(width=3,
        radioButtons("dataSource_radio","Upload or simulate data?",
                     selected=character(0),
                     choices=c("Upload data"="file",
                               "Simulate data"="simulate")),
        uiOutput("data_upload"),
        uiOutput("data_sim"),
        checkboxGroupInput("initialViz_check", "Data visualization",
                           choices=c("Table"="table",
                                     "Boxplot"="boxplot",
                                     "Bar plot"="barplot"))
      ),
      mainPanel(
        tableOutput("raw_table"),
        plotOutput("raw_boxplot"),
        plotOutput("raw_barplot")
      )
    )
  ),
  tabPanel("ANOVA assumptions",value="ANOVA_assump",
    sidebarLayout(
      sidebarPanel(width=3,
        checkboxGroupInput("residNormTest_check","Are residuals normally distributed?",
                           choices=c("Quantile-quantile plot","Shapiro-Wilk normality test")),
        checkboxGroupInput("equalVarTest_check","Do within-group residuals have equal variance across treatment groups?",
                           choices=c("Scale-location plot","Levene's test"))
      ),
      mainPanel(
        plotOutput("raw_qqplot"),
        tableOutput("raw_shapiro"),
        plotOutput("raw_scale_loc_plot"),
        tableOutput("raw_levene")
      )
    ),
  ),
  tabPanel("Run ANOVA",value="run_ANOVA",
    sidebarLayout(
      sidebarPanel(width=3,
       checkboxInput("anovaTest_check","Run ANOVA"),
       #note: placeholder for dynamic UI--Tukey should only appear after ANOVA run
       checkboxGroupInput("tukeyTest_check","Tukey HSD post-hoc test",
                          choices=c("Run test","Visualize results"))
      ),
      mainPanel(
        tableOutput("raw_anova_table"),
        tableOutput("raw_tukey_table"),
        plotOutput("raw_tukey_plot")
      )
    )
  ),
  tabPanel("Run Kruskal-Wallis",value="run_KW",
    sidebarLayout(
      sidebarPanel(width=3,
        checkboxInput("reviewViz_check","Visualize results"),
        checkboxInput("kruskalTest_check","Run Kruskal-Wallis Test"),
        checkboxInput("dunnTest_check","Run Dunn post-hoc test")
      ),
      mainPanel(
        plotOutput("review_boxplot"),
        tableOutput("raw_kw_table"),
        tableOutput("raw_dunn_table")
      )
    )
  )
)




server<-function(input,output,session){
  
#### Tab 1: Data Input & Exploration-------------------------------------------------------
  ### Dynamic UI
  ## Dynamic UI to display file upload box if 'Upload data' selected
  output$data_upload<-renderUI({
    req(input$dataSource_radio=="file")
      fileInput("upload_file","Upload a data file",accept=c(".csv",".xls",".xlsx"))
  })
  
  ## Dynamic UI to display n size for simulated data if selected
  output$data_sim<-renderUI({
    req(input$dataSource_radio=="simulate")
    numericInput("n_sim","Choose sample size per group",value=10,min=5,max=20)
  })
  
  ### Create reactive data object either via uploaded or simulated data
  data<-reactive({
    #if(input$upload_file)
      #ext<-tools::file_ext(input$upload_file$name)
      #switch(ext,
      #  csv=vroom::vroom(input$upload_file$datapath,delim=","),
      #  xls=read_xls(input$upload_file$datapath),
      #  xlsx=read_xlsx(input$upload_file$datapath),
        #validate("Invalid file; Please upload a .csv, .xls, or .xlsx file")
     # )
    if(input$dataSource_radio=="simulate"){
      samp_maker(input$n_sim)
    }
  })

  ### Data visualizations
  ## Display summary table
  output$raw_table<-renderTable({
    req(input$initialViz_check=="table")
    data() %>%
      group_by(trmt) %>%
        summarize(across(value,stat_list,.names="{.fn}"))
  })
  
  ## Display boxplot
  output$raw_boxplot<-renderPlot({
    req(input$initialViz_check=="boxplot")
    boxplotter(data(),trmt,value)
  })
  
  ## Display bar plot
  output$raw_barplot<-renderPlot({
    req(input$initialViz_check=="barplot")
    barplotter(data(),trmt,value)
  })

  
#### Tab 2: ANOVA Assumptions--------------------------------------------------------------
  ### Build model
  ## build model if one of two tabs selected
  mod<-reactive({
    req(input$mainTabs %in% c("ANOVA_assump","run_ANOVA"))
    mod<-lm(value~trmt,data=data())
  })
  
  
  ### Normal distribution of residuals
  ## QQ plot
  output$raw_qqplot<-renderPlot({
    req(input$residNormTest_check=="Quantile-quantile plot")
    qqplotter(mod())
  })
    
  
  ## Shapiro-Wilk test
  output$raw_shapiro<-renderTable({
    req(input$residNormTest_check=="Shapiro-Wilk normality test")
    shapiro_test(resid(mod()))
  })
  
  
  ### Equal variance
  ## Scale-location plot
  output$raw_scale_loc_plot<-renderPlot({
    req(input$equalVarTest_check=="Scale-location plot")
    plot(mod(),which=3)
  })
  
  
  ## Levene's test
  output$raw_levene<-renderTable({
    req(input$equalVarTest_check=="Levene's test")
    levene_test(data(),value~trmt)
  })
  
#### Tab 3: Run ANOVA----------------------------------------------------------------------
  ### Perform ANOVA
  output$raw_anova_table<-renderTable({
    req(input$anovaTest_check)
    anova_tabler(mod())
  })

  ### Run Tukey HSD tests
  output$raw_tukey_table<-renderTable({
    req(input$tukeyTest_check=="Run test")
    tukey_hsd(mod()) %>%
      select(-null.value)
  })
  
  ### Graph Tukey HSD test results
  output$raw_tukey_plot<-renderPlot({
    req(input$tukeyTest_check=="Visualize results")
    tukey_plotter(mod())
  })


#### Tab 4: Run Kruskal-Wallis-------------------------------------------------------------
  ### Visualize data
  output$review_boxplot<-renderPlot({
    req(input$reviewViz_check)
    boxplotter(data(),trmt,value)
  })
  
  ### Run Kruskal-Wallis Test
  output$raw_kw_table<-renderTable({
    req(input$kruskalTest_check)
    kruskal_test(data(),value~trmt)
  })
  
  ### Run Dunn post-hoc test
  output$raw_dunn_table<-renderTable({
    req(input$dunnTest_check)
    dunn_test(data(),value~trmt)
  })
}
shinyApp(ui,server)



#NEXT STEPS
#add titles to tables/plots



#Future steps
#Flesh out server function
#Conditional output (a button/modal) for 1) format of data upload, 2) giving the user the option to interpret plots/stats
#Plot labels and legend need to be bigger



