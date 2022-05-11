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

##### UI=================================================================================
ui<-navbarPage("ANOVA & Kruskal-Wallis Tester",id="mainTabs",
  
  #### UI: Tab 1-Data Input & Exploration----------------------------------------------
  tabPanel("Data Input & Exploration",value="data_input",
    sidebarLayout(
      sidebarPanel(width=3,
        radioButtons("dataSource_radio","Upload or simulate data?",
                     selected=character(0),
                     choices=c("Upload data"="file",
                               "Simulate data"="simulate")),
        splitLayout(
          uiOutput("data_upload"),
          uiOutput("data_upload_info")
        ),
        uiOutput("data_g_sim"),
        uiOutput("data_n_sim"),
        checkboxGroupInput("initialViz_check", "Data visualization",
                           choices=c("Table"="table",
                                     "Boxplot"="boxplot",
                                     "Bar plot"="barplot"))
      ),
      mainPanel(
        htmlOutput("raw_table_title"),
        tableOutput("raw_table"),
        br(),
        htmlOutput("raw_boxplot_title"),
        plotOutput("raw_boxplot"),
        br(),
        htmlOutput("raw_barplot_title"),
        plotOutput("raw_barplot")
      )
    )
  ),
  
  #### UI: Tab 2-ANOVA Assumptions--------------------------------------------------------
  tabPanel("ANOVA Assumptions",value="ANOVA_assump",
    sidebarLayout(
      sidebarPanel(width=3,
        checkboxGroupInput("residNormTest_check","Are residuals normally distributed?",
                           choices=c("Quantile-quantile plot","Shapiro-Wilk normality test")),
        checkboxGroupInput("equalVarTest_check","Do within-group residuals have equal variance across treatment groups?",
                           choices=c("Scale-location plot","Levene's test"))
      ),
      mainPanel(
        htmlOutput("raw_qqplot_title"),
        plotOutput("raw_qqplot"),
        br(),
        htmlOutput("raw_shapiro_title"),
        tableOutput("raw_shapiro"),
        br(),
        htmlOutput("raw_scale_loc_plot_title"),
        plotOutput("raw_scale_loc_plot"),
        br(),
        htmlOutput("raw_levene_title"),
        tableOutput("raw_levene")
      )
    )
  ),
  
  #### UI: Tab 3-Data Transformation---------------------------------------------------------------------------------
  tabPanel("Data Transformation",value="data_trans",
    sidebarLayout(
      sidebarPanel(width=3,
        checkboxInput("trans_check","Transform data?"),
        uiOutput("data_transform"),
        br(),
        htmlOutput("trans_assump"),
        uiOutput("trans_residNormTest"),
        uiOutput("trans_equalVarTest")
      ),
      mainPanel(
        htmlOutput("trans_qqplot_title"),
        plotOutput("trans_qqplot"),
        br(),
        htmlOutput("trans_shapiro_title"),
        tableOutput("trans_shapiro"),
        br(),
        htmlOutput("trans_scale_loc_plot_title"),
        plotOutput("trans_scale_loc_plot"),
        br(),
        htmlOutput("trans_levene_title"),
        tableOutput("trans_levene")
      )
    )
  ),
  
  #### UI: Tab 4-Run ANOVA-------------------------------------------------------------------------------
  tabPanel("Run ANOVA",value="run_ANOVA",
    sidebarLayout(
      sidebarPanel(width=3,
        uiOutput("anovaTest"),
        uiOutput("tukeyHSD_check")
      ),
      mainPanel(
        htmlOutput("raw_anova_table_title"),
        tableOutput("raw_anova_table"),
        br(),
        htmlOutput("raw_tukey_table_title"),
        tableOutput("raw_tukey_table"),
        br(),
        htmlOutput("raw_tukey_plot_title"),
        plotOutput("raw_tukey_plot")
      )
    )
  ),
  
  #### UI: Tab 5-Runk Kruskal-Wallis---------------------------------------------------------------
  tabPanel("Run Kruskal-Wallis",value="run_KW",
    sidebarLayout(
      sidebarPanel(width=3,
        checkboxInput("reviewViz_check","Visualize results"),
        checkboxInput("kruskalTest_check","Run Kruskal-Wallis Test"),
        uiOutput("dunnTest")
      ),
      mainPanel(
        htmlOutput("review_boxplot_title"),
        plotOutput("review_boxplot"),
        br(),
        htmlOutput("raw_kw_table_title"),
        tableOutput("raw_kw_table"),
        br(),
        htmlOutput("raw_dunn_table_title"),
        tableOutput("raw_dunn_table")
      )
    )
  )
)



##### Server Function=====================================================================
server<-function(input,output,session){
  
#### Server: Tab 1-Data Input & Exploration------------------------------------------------
  ### Dynamic UI
  ## Dynamic UI to display file upload box and info button if 'Upload data' selected
  output$data_upload<-renderUI({
    req(input$dataSource_radio=="file")
      fileInput("upload_file","Upload a data file",accept=c(".csv",".xls",".xlsx"))
  })
  
  output$data_upload_info<-renderUI({
    req(input$dataSource_radio=="file")
      actionButton("info_file",HTML("<em> i </em>"),style='margin-top:25px',class="btn btn-primary")
  })
  
  
  ## Dynamic UI to display g and n sizes for simulated data if selected
  output$data_g_sim<-renderUI({
    req(input$dataSource_radio=="simulate")
    numericInput("g_sim","Select the number of groups",value=3,min=3,max=5)
  })
  
  output$data_n_sim<-renderUI({
    req(input$dataSource_radio=="simulate")
    numericInput("n_sim","Choose sample size per group",value=10,min=5,max=20)
  })
  
  
  ### Display modal if info button pressed
  observeEvent(input$info_file,{
    showModal(modalDialog(
      title="Note when uploading files",
      footer=modalButton("Close"),
      HTML(
        paste0("Data must...",'<br/>',
          "1) be saved as a .csv., .xls, or .xlsx file",'<br/>',
          "2) contain 3, 4, or 5 groups",'<br/>',
          "3) be arranged in long format with group names in first row"
        )
      )
    ))  
  }
  )
  
  ### Create reactive data object either via uploaded or simulated data
  data<-reactive({
    #set up conditional...if file is selected and a file is chosen then
    if(input$dataSource_radio=="file"){
      req(input$upload_file)
      #R picks out the file extension
      ext<-tools::file_ext(input$upload_file$name)
      #switches to function based on ext 
      switch(ext,
        csv=vroom::vroom(input$upload_file$datapath,delim=","),
        xls=read_xls(input$upload_file$datapath),
        xlsx=read_xlsx(input$upload_file$datapath),
        #error thrown if incorrect file type selected
        validate("Invalid file; Please upload a .csv, .xls, or .xlsx file")
      ) %>%
        #once file type is correct, it pivots to long format
        pivot_longer(cols=everything(),names_to="trmt",values_to="value")
    }
    #but...if simulate is selected then data are simulated using function samp_maker
    else if(input$dataSource_radio=="simulate"){
      if(!between(input$g_sim,3,5)) {
        validate("select either 3, 4, or 5 groups")
      }
      if(!between(input$n_sim,5,20)) {
        validate("sample size cannot be fewer than 5 or greater than 20")
      }
      samp_maker(input$g_sim,input$n_sim)
    }
  })

  ### Data visualizations
  ## Display summary table title
  output$raw_table_title<-renderText({
    req(input$initialViz_check=="table")
    paste("<h4>Summary Statistics</h4>")
  })
  
  ## Display summary table
  output$raw_table<-renderTable({
    req(input$initialViz_check=="table")
    data() %>%
      group_by(trmt) %>%
        summarize(across(value,stat_list,.names="{.fn}"))
  })
  
  ## Display boxplot title
  output$raw_boxplot_title<-renderText({
    req(input$initialViz_check=="boxplot")
    paste("<h4>Boxplot</h4>")
  })
  
  ## Display boxplot
  output$raw_boxplot<-renderPlot({
    req(input$initialViz_check=="boxplot")
    boxplotter(data(),trmt,value)
  })
  
  ## Display bar plot title
  output$raw_barplot_title<-renderText({
    req(input$initialViz_check=="barplot")
    paste("<h4>Bar Plot</h4>")
  })
  
  ## Display bar plot
  output$raw_barplot<-renderPlot({
    req(input$initialViz_check=="barplot")
    barplotter(data(),trmt,value)
  })

  
#### Server: Tab 2-ANOVA Assumptions--------------------------------------------------------------
  ### Build model if one of two tabs selected
  mod<-reactive({
    req(input$mainTabs %in% c("ANOVA_assump","run_ANOVA"))
    lm(value~trmt,data=data())
  })
  
  
  ### Normal distribution of residuals
  ## QQ plot title
  output$raw_qqplot_title<-renderText({
    req(input$residNormTest_check=="Quantile-quantile plot")
    paste("<h4>Quantitle-quantile Plot</h4>")
  })
  
  ## QQ plot
  output$raw_qqplot<-renderPlot({
    req(input$residNormTest_check=="Quantile-quantile plot")
    qqplotter(mod())
  })
    
  ## Shapiro-Wilk test title
  output$raw_shapiro_title<-renderText({
    req(input$residNormTest_check=="Shapiro-Wilk normality test")
    paste("<h4>Shapiro-Wilk Test of Normality</h4>")
  })
  
  ## Shapiro-Wilk test
  output$raw_shapiro<-renderTable({
    req(input$residNormTest_check=="Shapiro-Wilk normality test")
    shapiro_test(resid(mod())) %>% 
      mutate(variable="residuals") %>%
      rename(p="p.value")
  })
  
  
  ### Equal variance
  ## Scale-location plot title
  output$raw_scale_loc_plot_title<-renderText({
    req(input$equalVarTest_check=="Scale-location plot")
    paste("<h4>Scale-location Plot</h4>")
  })
  
  ## Scale-location plot
  output$raw_scale_loc_plot<-renderPlot({
    req(input$equalVarTest_check=="Scale-location plot")
    plot(mod(),which=3,caption=NULL)
  })
  
  ## Levene's test title
  output$raw_levene_title<-renderText({
    req(input$equalVarTest_check=="Levene's test")
    paste("<h4>Levene's Test</h4>")
  })
  
  ## Levene's test
  output$raw_levene<-renderTable({
    req(input$equalVarTest_check=="Levene's test")
    levene_test(data(),value~trmt)
  })
  

#### Server: Tab 3-Data Transformation----------------------------------------------------------
  ### Dynamically display UI for data transformation
  output$data_transform<-renderUI({
    req(input$trans_check)
    selectInput("trans_select","How would you like to transform your data?",
      selected=NULL, choices=c("",
                               "Log"="log",
                               "Square-root"="sqrt",
                               "Reciprocal"="recip"))
  })
  
  ### Create reactive objects for testing ANOVA assumptions
  ## Transformed data object
  trans_data<-reactive({
    if(input$trans_select=="log"){
      data() %>%
        mutate(trans_value=log(value))
    }
    else if(input$trans_select=="sqrt"){
      data() %>%
        mutate(trans_value=sqrt(value))
    }
    else if(input$trans_select=="recip"){
      data() %>%
        mutate(trans_value=recip(value))
    }
  })


  ## Model using transformed data
  trans_mod<-reactive({
    req(input$trans_check,input$trans_select)
    lm(trans_value~trmt,data=trans_data())
    })
  
  
  ### Dynamically display UI for testing ANOVA assumptions of transformed data
  ## ANOVA Assumptions text
  output$trans_assump<-renderText({
    req(input$trans_check,input$trans_select)
    paste("<h4>Test ANOVA Assumptions</h4>")
  })
  
  ## Normality checkbox
  output$trans_residNormTest<-renderUI({
    req(input$trans_check,input$trans_select)
    checkboxGroupInput("trans_residNormTest_check","Are residuals normally distributed?",
                       choices=c("Quantile-quantile plot","Shapiro-Wilk normality test"))
  })
  
  ## Equal Variance checkbox
  output$trans_equalVarTest<-renderUI({
    req(input$trans_check,input$trans_select)
    checkboxGroupInput("trans_equalVarTest_check","Do within-group residuals have equal variance across treatment groups?",
                       choices=c("Scale-location plot","Levene's test"))
  })
  
  
  ### Normal distribution of residuals
  ## QQ plot title
  output$trans_qqplot_title<-renderText({
    req(input$trans_residNormTest_check=="Quantile-quantile plot")
    paste("<h4>Quantitle-quantile Plot</h4>")
  })
  
  ## QQ plot
  output$trans_qqplot<-renderPlot({
    req(input$trans_residNormTest_check=="Quantile-quantile plot")
    qqplotter(trans_mod())
  })
  
  ## Shapiro-Wilk test title
  output$trans_shapiro_title<-renderText({
    req(input$trans_residNormTest_check=="Shapiro-Wilk normality test")
    paste("<h4>Shapiro-Wilk Test of Normality</h4>")
  })
  
  ## Shapiro-Wilk test
  output$trans_shapiro<-renderTable({
    req(input$trans_residNormTest_check=="Shapiro-Wilk normality test")
    shapiro_test(resid(trans_mod())) %>% 
      mutate(variable="residuals") %>%
      rename(p="p.value")
  })
  
  
  ### Equal variance
  ## Scale-location plot title
  output$trans_scale_loc_plot_title<-renderText({
    req(input$trans_equalVarTest_check=="Scale-location plot")
    paste("<h4>Scale-location Plot</h4>")
  })
  
  ## Scale-location plot
  output$trans_scale_loc_plot<-renderPlot({
    req(input$trans_equalVarTest_check=="Scale-location plot")
    plot(trans_mod(),which=3,caption=NULL)
  })
  
  ## Levene's test title
  output$trans_levene_title<-renderText({
    req(input$trans_equalVarTest_check=="Levene's test")
    paste("<h4>Levene's Test</h4>")
  })
  
  ## Levene's test
  output$trans_levene<-renderTable({
    req(input$trans_equalVarTest_check=="Levene's test")
    levene_test(trans_data(),trans_value~trmt)
  })
  
  
#### Server: Tab 4-Run ANOVA----------------------------------------------------------------------
  ### Display UI for running ANOVA dynamically
  output$anovaTest<-renderUI({
    if(input$trans_check & input$trans_select %in% c("log","sqrt","recip")){
      radioButtons("anovaTest_radio","Run ANOVA with...",
                    selected=character(0),
                    choices=c("raw data"="raw",
                              "transformed data"="trans"))
    }
      else{checkboxInput("anovaTest_check","Run ANOVA")}
    })
    
  
  ### Display ANOVA table title
  output$raw_anova_table_title<-renderText({
    req(input$anovaTest_check)
    paste("<h4>ANOVA Table</h4>")
  })
  
  ### Perform ANOVA
  output$raw_anova_table<-renderTable({
    req(input$anovaTest_check)
    anova_tabler(mod())
  })

  ### Dynamically display UI for Tukey HSD Test
  output$tukeyHSD_check<-renderUI({
    req(input$anovaTest_check)
    checkboxGroupInput("tukeyTest_check","Tukey HSD post-hoc test",
                       choices=c("Run test","Visualize results"))
  })
  
  
  ### Display title of Tukey test summary table
  output$raw_tukey_table_title<-renderText({
    req(input$tukeyTest_check=="Run test")
    paste("<h4>Summary of Tukey HSD Tests</h4>")
  })
  
  ### Run Tukey HSD tests
  output$raw_tukey_table<-renderTable({
    req(input$tukeyTest_check=="Run test")
    tukey_hsd(mod()) %>%
      select(-c(null.value,term))
  })
  
  ### Display title of graphical Tukey HSD test results
  output$raw_tukey_plot_title<-renderText({
    req(input$tukeyTest_check=="Visualize results")
    paste("<h4>Multiple Comparisons Between All Pairs (Tukey)</h4>")
  })
  
  ### Graph Tukey HSD test results
  output$raw_tukey_plot<-renderPlot({
    req(input$tukeyTest_check=="Visualize results")
    tukey_plotter(mod())
  })


#### Server: Tab 5-Run Kruskal-Wallis-------------------------------------------------------------
  ### Display title of boxplot
  output$review_boxplot_title<-renderText({
    req(input$reviewViz_check)
    paste("<h4>Boxplot</h4>")
  })
  
  ### Visualize data
  output$review_boxplot<-renderPlot({
    req(input$reviewViz_check)
    boxplotter(data(),trmt,value)
  })
  
  ## Display Kruskal-Wallis Table Title
  output$raw_kw_table_title<-renderText({
    req(input$kruskalTest_check)
    paste("<h4>Kruskal-Wallis Test Results</h4>")
  })
  
  ### Run Kruskal-Wallis Test
  output$raw_kw_table<-renderTable({
    req(input$kruskalTest_check)
    kruskal_test(data(),value~trmt) %>%
      select(-c(`.y.`,method))
  })
  
  ### Dynamically display Dunn test input
  output$dunnTest<-renderUI({
    req(input$kruskalTest_check)
    checkboxInput("dunnTest_check","Run Dunn post-hoc test")
  })
  

  ### Display Dunn post-hoc test results title
  output$raw_dunn_table_title<-renderText({
    req(input$dunnTest_check)
    paste("<h4>Dunn Test Results</h4>")
  })
  
  ### Run Dunn post-hoc test
  output$raw_dunn_table<-renderTable({
    req(input$dunnTest_check)
    dunn_test(data(),value~trmt) %>%
      select(-`.y.`)
  })
}
shinyApp(ui,server)



#DONE
#server-side of data transformation (could develop a more sophisticated function)
#dynamic UI for which data to run ANOVA on


#WORK IN PROGRESS
#server side of ANOVA model (raw vs untransformed data)


#NEXT STEPS
#style
#make plot labels and legend larger
#fix Tukey plot so that geom_text more readable
#interactive plots--click/brushing and display
#developer info page


