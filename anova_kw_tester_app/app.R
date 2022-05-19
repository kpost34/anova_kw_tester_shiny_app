#load packages
library(shiny)
library(here)
library(dplyr)
library(tibble)
library(ggplot2)
library(readxl)
library(vroom)
library(rstatix)
library(bslib)

#load functions
source(here("anova_kw_tester_app","anova_kw_tester_app_functions.R"))
    
#create list for summary tables
stat_list<-list(n=length,min=min,median=median,mean=mean,max=max,sd=sd,se=function(x) sd(x)/sqrt(length(x)))

##### UI=================================================================================
ui<-navbarPage(strong("ANOVA & Kruskal-Wallis Tester"),id="mainTabs",
    #select bootswatch theme
    theme=bslib::bs_theme(bootswatch="darkly",font_scale=0.9),
  
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
        radioButtons("anovaTest_radio","Select data source",
                     selected=character(0),
                     choices=c("raw data"="raw",
                               "transformed data"="trans")),
        br(),
        checkboxGroupInput("tukeyTest_check","Tukey HSD post-hoc test",
                           selected=character(0),
                           choices=c("Run test"="run",
                                     "Visualize results"="visualize"))
      ),
      mainPanel(
        htmlOutput("anova_table_title"),
        tableOutput("anova_table"),
        br(),
        htmlOutput("tukey_table_title"),
        tableOutput("tukey_table"),
        br(),
        htmlOutput("tukey_plot_title"),
        plotOutput("tukey_plot")
      )
    )
  ),
  
  #### UI: Tab 5-Run Kruskal-Wallis---------------------------------------------------------------
  tabPanel("Run Kruskal-Wallis",value="run_KW",
    sidebarLayout(
      sidebarPanel(width=3,
        checkboxInput("reviewViz_check","Visualize results"),
        checkboxInput("kruskalTest_check","Run Kruskal-Wallis Test (raw data only)"),
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
  ),
  
  #### UI: Tab 6-Developer Information------------------------------------------------------------
  tabPanel(strong("DEVELOPER"),value="devel",
     p(h4(strong("Keith Post"))),
     p("If you would like to see the code for this Shiny app, please visit my",
       tags$a(href="https://github.com/kpost34/anova_kw_tester_shiny_app",
              "Github repo"),
       "for this project."
     ),
     p(tags$a(href="https://github.com/kpost34","GitHub Profile")),
     p(tags$a(href="https://www.linkedin.com/in/keith-post","LinkedIn")),            
    )
  
)


##### Server Function=====================================================================
server<-function(input,output,session){
  
#### Server: Tab 1-Data Input & Exploration------------------------------------------------
  ### Dynamic UI
  ## Display file upload box and info button if 'Upload data' selected
  output$data_upload<-renderUI({
    req(input$dataSource_radio=="file")
      fileInput("upload_file","Upload a data file",accept=c(".csv",".xls",".xlsx"))
  })
  
  output$data_upload_info<-renderUI({
    req(input$dataSource_radio=="file")
      actionButton("info_file",HTML("<em> i </em>"),style='margin-top:29px',class="btn btn-primary")
  })
  
  
  ## Display g and n size inputs for simulated data if selected
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
  
  
  ### Object code
  ## Create reactive dat object either via uploaded or simulated data
  dat<-reactive({
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
  ## Summary table
  # Summary table title 
  output$raw_table_title<-renderText({
    #display if box checked
    req(input$initialViz_check=="table")
    paste("<h4>Summary Statistics</h4>")
  })
  
  # Summary table body 
  output$raw_table<-renderTable({
    req(input$initialViz_check=="table")
    if(is.null(input$upload_file) & is.null(input$n_sim)){
      validate("Please simulate or upload data first")
    }
    dat() %>%
      group_by(trmt) %>%
        summarize(across(value,stat_list,.names="{.fn}"))
  })
  
  
  ## Boxplot
  # Boxplot title 
  output$raw_boxplot_title<-renderText({
    req(input$initialViz_check=="boxplot")
    paste("<h4>Boxplot</h4>")
  })
  
  ## Boxplot 
  output$raw_boxplot<-renderPlot({
    req(input$initialViz_check=="boxplot")
    if(is.null(input$upload_file) & is.null(input$n_sim)){
      validate("Please simulate or upload data first")
    }
    #custom boxplotter() function
    boxplotter(dat(),trmt,value)
  })
  
  
  ## Bar plot
  # Bar plot title 
  output$raw_barplot_title<-renderText({
    req(input$initialViz_check=="barplot")
    paste("<h4>Bar Plot</h4>")
  })
  
  # Bar plot 
  output$raw_barplot<-renderPlot({
    req(input$initialViz_check=="barplot")
    if(is.null(input$upload_file) & is.null(input$n_sim)){
      validate("Please simulate or upload data first")
    }
    #custom boxplotter() function
    barplotter(dat(),trmt,value)
  })

  
#### Server: Tab 2-ANOVA Assumptions--------------------------------------------------------------
  ### Build model if one of two tabs selected
  mod<-reactive({
    req(input$mainTabs %in% c("ANOVA_assump","run_ANOVA"))
    lm(value~trmt,data=dat())
  })
  
  
  ### Normal distribution of residuals
  ## QQ plot 
  # QQ plot title displayed 
  output$raw_qqplot_title<-renderText({
    req(input$residNormTest_check=="Quantile-quantile plot")
    paste("<h4>Quantitle-quantile Plot</h4>")
  })
  
  # QQ plot 
  output$raw_qqplot<-renderPlot({
    req(input$residNormTest_check=="Quantile-quantile plot")
    #custom qqplotter() function
    qqplotter(mod())
  })
    
  
  ## Shapiro-Wilk test
  # Shapiro-Wilk test title 
  output$raw_shapiro_title<-renderText({
    req(input$residNormTest_check=="Shapiro-Wilk normality test")
    paste("<h4>Shapiro-Wilk Test of Normality</h4>")
  })
  

  # Shapiro-Wilk test results displayed as table
  output$raw_shapiro<-renderTable({
    req(input$residNormTest_check=="Shapiro-Wilk normality test")
    shapiro_test(resid(mod())) %>% 
      mutate(variable="residuals") %>%
      rename(p="p.value")
  })
  
  
  ### Equal variance
  ## Scale-location plot 
  # Scale-location plot title displayed 
  output$raw_scale_loc_plot_title<-renderText({
    req(input$equalVarTest_check=="Scale-location plot")
    paste("<h4>Scale-location Plot</h4>")
  })
  
  # Scale-location plot 
  output$raw_scale_loc_plot<-renderPlot({
    req(input$equalVarTest_check=="Scale-location plot")
    par(mar=c(5,5.5,4,2))
    plot(mod(),which=3,pch=1,cex=1.3,caption=NULL,cex.sub=1.25,cex.lab=1.25,cex.axis=1.25)
  })
  
  
  ## Levene's test
  # Levene's test title 
  output$raw_levene_title<-renderText({
    req(input$equalVarTest_check=="Levene's test")
    paste("<h4>Levene's Test</h4>")
  })
  
  # Levene's test results as table 
  output$raw_levene<-renderTable({
    req(input$equalVarTest_check=="Levene's test")
    levene_test(dat(),value~trmt)
  })
  

#### Server: Tab 3-Data Transformation----------------------------------------------------------
  ### Dynamic UI for data transformation
  output$data_transform<-renderUI({ 
    #checkbox needs to be checked for selector to display
    req(input$trans_check)
    #selectizeInput() with one choice used so that selector defaults to NULL
    selectizeInput("trans_select","How would you like to transform your data?",
      multiple=TRUE, options=list(maxItems=1),
      choices=c("Log"="log","Square-root"="sqrt","Reciprocal"="recip"))
  })
  
  
  ### Create reactive objects for testing ANOVA assumptions
  ## Transformed data object
  trans_data<-reactive({
    #creates one of three types of data (called trans_data) based on selector choice
    if(input$trans_select=="log"){
      dat() %>%
        mutate(trans_value=log(value))
    }
    else if(input$trans_select=="sqrt"){
      dat() %>%
        mutate(trans_value=sqrt(value))
    }
    else if(input$trans_select=="recip"){
      dat() %>%
        mutate(trans_value=recip(value))
    }
  })


  ## Model using transformed data
  trans_mod<-reactive({
    #once checkbox checked and a selector input chosen, a lm() of transformed data is made
    req(input$trans_check,input$trans_select)
    lm(trans_value~trmt,data=trans_data())
    })
  
  
  ### Dynamic UI for testing ANOVA assumptions of transformed data
  ## ANOVA Assumptions text
  output$trans_assump<-renderText({
    #both UI components must not be NULL for text to display (and same with checkboxGroupInputs below)
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
    #three requirements needed
    req(input$trans_check,
        input$trans_select,
        input$trans_residNormTest_check=="Quantile-quantile plot")
    paste("<h4>Quantitle-quantile Plot</h4>")
  })
  
  
  ## QQ plot
  output$trans_qqplot<-renderPlot({
    req(input$trans_check,
        input$trans_select,
        input$trans_residNormTest_check=="Quantile-quantile plot")
    qqplotter(trans_mod())
  })
  
  
  ## Shapiro-Wilk test title
  output$trans_shapiro_title<-renderText({
    req(input$trans_check,
        input$trans_select,
        input$trans_residNormTest_check=="Shapiro-Wilk normality test")
    paste("<h4>Shapiro-Wilk Test of Normality</h4>")
  })
  
  
  ## Shapiro-Wilk test
  output$trans_shapiro<-renderTable({
    req(input$trans_check,
        input$trans_select,
        input$trans_residNormTest_check=="Shapiro-Wilk normality test")
    shapiro_test(resid(trans_mod())) %>% 
      mutate(variable="residuals") %>%
      rename(p="p.value")
  })
  
  
  ### Equal variance
  ## Scale-location plot title
  output$trans_scale_loc_plot_title<-renderText({
    #three requirements needed
    req(input$trans_check,
        input$trans_select,
        input$trans_equalVarTest_check=="Scale-location plot")
    paste("<h4>Scale-location Plot</h4>")
  })
  
  
  ## Scale-location plot
  output$trans_scale_loc_plot<-renderPlot({
    req(input$trans_check,
        input$trans_select,
        input$trans_equalVarTest_check=="Scale-location plot")
    par(mar=c(5,5.5,4,2))
    plot(trans_mod(),which=3,pch=1,cex=1.3,caption=NULL,cex.sub=1.25,cex.lab=1.25,cex.axis=1.25)
  })
  
  
  ## Levene's test title
  output$trans_levene_title<-renderText({
    req(input$trans_check,
        input$trans_select,
        input$trans_equalVarTest_check=="Levene's test")
    paste("<h4>Levene's Test</h4>")
  })
  
  
  ## Levene's test
  output$trans_levene<-renderTable({
    req(input$trans_check,
        input$trans_select,
        input$trans_equalVarTest_check=="Levene's test")
    levene_test(trans_data(),trans_value~trmt)
  })
  

#### Server: Tab 4-Run ANOVA-------------------------------------------------------------------------------------
  ### ANOVA output
  ## ANOVA table title
  output$anova_table_title<-renderText({
    #use of req() with input and if(input==...) prevents errors upon switching to tab and allows cleaner logic (e.g., disappears when de-selected)
    #four possibile choice patterns lead to two outcomes, one of which is a validate() message (i.e., selecting transformed data when they aren't 
    #available)
    req(input$anovaTest_radio)
    if(input$anovaTest_radio=="trans" & input$trans_check==FALSE){
      validate("Transform data first on the previous tab")
    }
    else if(input$anovaTest_radio=="trans" & is.null(input$trans_select)){
      validate("Transform data first on the previous tab")
    }
    else if(input$anovaTest_radio=="raw"){
      paste("<h4>ANOVA Table</h4>")
    }
    else if(input$anovaTest_radio=="trans" & !is.null(input$trans_select)){
      paste("<h4>ANOVA Table</h4>")
    }
  })
  
  
  ## Run ANOVA on specified data source
  output$anova_table<-renderTable({
    #again, same approach with req() followed by if/else if
    req(input$anovaTest_radio)
    if(input$anovaTest_radio=="raw"){
      anova_tabler(mod())
    }
    else if(input$anovaTest_radio=="trans"){
      anova_tabler(trans_mod())
    }
  })

  
  ### Tukey output
  ## Title of Tukey test summary table
  output$tukey_table_title<-renderText({
    #first req() for selecting the test
    req(input$tukeyTest_check=="run")
    #if ANOVA data source empty, then message
    if(is.null(input$anovaTest_radio)){
      validate("Run ANOVA before performing Tukey HSD tests")
    }
    if(input$anovaTest_radio=="trans" & ((input$trans_check==FALSE)|is.null(input$trans_select))){
      validate("Run ANOVA before performing Tukey HSD tests")
    }
    #second req() is having data source selected
    req(input$anovaTest_radio)
    paste("<h4>Summary of Tukey HSD Tests</h4>")
  })
  
  
  ## Tukey HSD test summary table
  output$tukey_table<-renderTable({
    #two reqs()--data source and correct checkbox
    req(input$anovaTest_radio,
        input$tukeyTest_check=="run")
    #if for raw data
    if(input$anovaTest_radio=="raw"){
      tukey_hsd(mod()) %>%
        select(-c(null.value,term))
    }
    #else if for transformed data
    else if(input$anovaTest_radio=="trans"){
      tukey_hsd(trans_mod()) %>%
        select(-c(null.value,term))
    }
  })
  
  
  ## Title of graphical Tukey HSD test results
  output$tukey_plot_title<-renderText({
    req(input$tukeyTest_check=="visualize")
    #validate messages accompany blank plots
    if(is.null(input$anovaTest_radio)){
      validate("Run ANOVA before performing Tukey HSD tests")
    }
    if(input$anovaTest_radio=="trans" & ((input$trans_check==FALSE)|is.null(input$trans_select))){
      validate("Run ANOVA before performing Tukey HSD tests")
    }
    req(input$anovaTest_radio)
    paste("<h4>Multiple Comparisons Between All Pairs (Tukey)</h4>")
  })
  
  
  ## Plot of Tukey HSD test results
  output$tukey_plot<-renderPlot({
    #similar to test results...two reqs
    req(input$anovaTest_radio,
        input$tukeyTest_check=="visualize")
    #if/else if for raw and transformed data, respectively
    if(input$anovaTest_radio=="raw"){
      tukey_plotter(mod())
    }
    else if(input$anovaTest_radio=="trans"){
      tukey_plotter(trans_mod())
    }
  })


#### Server: Tab 5-Run Kruskal-Wallis-------------------------------------------------------------
  ### Boxplot
  ## Boxplot title
  #simply a reminder of the boxplot from the first tab
  output$review_boxplot_title<-renderText({
    req(input$reviewViz_check)
    paste("<h4>Boxplot</h4>")
  })
  
  
  ## Boxplot
  output$review_boxplot<-renderPlot({
    req(input$reviewViz_check)
    boxplotter(dat(),trmt,value)
  })
  
  
  ### Kruksal-Wallis
  ## Kruskal-Wallis Table Title
  output$raw_kw_table_title<-renderText({
    #checkbox requirement
    req(input$kruskalTest_check)
    paste("<h4>Kruskal-Wallis Test Results</h4>")
  })
  
  
  ## Run Kruskal-Wallis Test
  output$raw_kw_table<-renderTable({
    req(input$kruskalTest_check)
    kruskal_test(dat(),value~trmt) %>%
      select(-c(`.y.`,method))
  })
  
  
  ### Dunn test
  ## Dynamically display Dunn test input
  output$dunnTest<-renderUI({
    #K-W test must be run first
    req(input$kruskalTest_check)
    checkboxInput("dunnTest_check","Run Dunn post-hoc test")
  })
  

  ## Dunn post-hoc test results title
  output$raw_dunn_table_title<-renderText({
    #both K-W and Dunn boxes need to be checked
    req(input$kruskalTest_check)
    req(input$dunnTest_check)
    paste("<h4>Dunn Test Results</h4>")
  })
  
  
  ## Run Dunn post-hoc test
  output$raw_dunn_table<-renderTable({
    req(input$kruskalTest_check)
    req(input$dunnTest_check)
    dunn_test(dat(),value~trmt) %>%
      select(-`.y.`)
  })
}
shinyApp(ui,server)



#DONE
#increased point sizes of plots and changed point shape of scale-location plot

#WORK IN PROGRESS


#NEXT STEPS
#add readme



#POSSIBLE IMPROVEMENTS
#interactive plots--click/brushing and display
#make code more flexible so that it retains trmt categories


