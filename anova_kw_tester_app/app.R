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


ui<-navbarPage("ANOVA & Kruskal-Wallis Tester",
  tabPanel("Data Input & Exploration",
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
  tabPanel("ANOVA assumptions",
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
  tabPanel("Run ANOVA",
    sidebarLayout(
      sidebarPanel(width=3,
       checkboxInput("anovaTest_check","Run ANOVA"),
       #note: placeholder for dynamic UI--Tukey should only appear after ANOVA run
       checkboxGroupInput("tukeyTest_check","Tukey HSD post-hoc test",
                          choices=c("Run test","Visualize results"))
      ),
      mainPanel(),
    )
  ),
  tabPanel("Run Kruskal-Wallis",
    sidebarLayout(
      sidebarPanel(width=3,
      ),
      mainPanel(),
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
  ## build model if tab selected
  mod<-reactive({
    req([insert code])
    
  })
  
  ### Normal distribution of residuals
  ## QQ plot
  
  
  ## Shapiro-Wilk test
  
  ### Equal variance
  ## Scale-location plot
  
  ## Levene's test

  plotOutput("raw_qqplot"),
  tableOutput("raw_shapiro"),
  plotOutput("raw_scale_loc_plot"),
  tableOutput("raw_levene")
  
#### Tab 3: Run ANOVA----------------------------------------------------------------------
  



#### Tab 4: Run Kruskal-Wallis-------------------------------------------------------------
  

  
}
shinyApp(ui,server)





#Future steps
#Flesh out server function
#Conditional output (a button/modal) for 1) format of data upload, 2) giving the user the option to interpret plots/stats
#Plot labels and legend need to be bigger



