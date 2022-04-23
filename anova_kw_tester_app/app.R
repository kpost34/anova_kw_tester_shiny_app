library(shiny)

ui<-navbarPage("ANOVA & K-W Tester",
  tabPanel("Data Input & Exploration",
    sidebarLayout(
      sidebarPanel(
        radioButtons("data_source","Upload or simulate data?",choices=c("Upload data","Simulate data")),
        h5("Data visualization"),
        checkboxInput("initial_table","Summary Table"),
        checkboxInput("initial_boxplot","Boxplot"),
        checkboxInput("inital_barplot","Barplot")),
      mainPanel(
        textOutput("untrans_plot_tab_header"),
        tableOutput("untrans_table"),
        plotOutput("untrans_boxplot"),
        plotOutput("untrans_barplot")
      )
    )
  )
)


server<-function(input,output,session){
  sim_data<-reactive()
  
  output$untrans_table<-renderTable({
    
  })
  
  output$untrans_boxplot<-renderPlot({
    
  })
  
  output$untrans_barplot<-renderPlot({
    
  })
  
}
shinyApp(ui,server)

