library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(mlma)
library(fresh)
library(readxl)
library(stringr)
library(tidyverse)
library(xlsx)


not_sel <- "Not Selected"

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "Created with R Shiny",
  br(),
  "2022 Febuary"
)

#num_var_1: x
#num_var_2: y
#fact_var: level
main_page <- tabPanel(
  title = "Mediation Analysis Example",
  titlePanel("Mediation Analysis"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input", "Select CSV, TXT, or XLSX File to Import", accept = c(".csv", ".xlsx", ".txt")),
      selectInput("num_var_1", "Predictor: X", choices = c(not_sel)),
      selectInput("num_var_2", "Response: Y", choices = c(not_sel)),
      selectInput("num_var_3", "Mediators", choices = c(not_sel), multiple = TRUE),
      selectInput("fact_var", "Level of X", choices = c(not_sel)),
      numericInput(
        inputId = "num_bootstrap_replicates",
        label = "Number of Bootstrap Replications (Default is 500)",
        value = 500,
        min = 0,
        max = NA,
        step = NA,
        width = NULL
      ),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Graphic Outputs",
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Model Statistics I",
          verbatimTextOutput("model_summary_1")
        ),
        tabPanel(
          title = "Model Statistics II",
          verbatimTextOutput("model_summary_2")
        ),
      )
    )
  )
)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var, num_bootstrap_replicates){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    x <- data_input[,num_var_1]
    y <- data_input[,num_var_2]
    m1 <- data_input[,num_var_3[1]]
    m2 <- data_input[,num_var_3[2]]
    m3 <- data_input[,num_var_3[3]]
    lv <- data_input[,fact_var]
    mat = matrix(ncol = 0, nrow = length(m1))
    m4=data.frame(mat)
    # for(vector in num_var_3){
    #   m=cbind(m,vector)
    # }
    m=cbind(m1,m2,m3,m4)
    sim.111=list(x=x,m=m,y=y,level=lv)
    data2<-data.org(sim.111$x, m=data.frame(sim.111$m), 
                    f10y=list(1,c("x^2","sqrt(x+6)")), 
                    f20ky=list(1,c("x","x^3")), 
                    f10km=list(matrix(c(1,1),1),"log(x+2)"), level=sim.111$level)
    tmp2.boot<-boot.mlma(y=sim.111$y, data1=data2, boot=num_bootstrap_replicates,echo=F)
    plot(tmp2.boot) #Graphic Outputs
  }
}

create_num_var_table_1 <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    x <- data_input[,num_var_1]
    y <- data_input[,num_var_2]
    m1 <- data_input[,num_var_3[1]]
    m2 <- data_input[,num_var_3[2]]
    m3 <- data_input[,num_var_3[3]]
    lv <- data_input[,fact_var]
    # print(typeof(m1))
    # write(typeof(m1), stdout())
    mat = matrix(ncol = 0, nrow = length(m1))
    m4=data.frame(mat)
    # for(vector in num_var_3){
    #   m=cbind(m,vector)
    # }
    # m=m1
    m=cbind(m1,m2,m3,m4)
    sim.111=list(x=x,m=m,y=y,level=lv)
    data2<-data.org(sim.111$x, m=data.frame(sim.111$m), 
                    f10y=list(1,c("x^2","sqrt(x+6)")), 
                    f20ky=list(1,c("x","x^3")), 
                    f10km=list(matrix(c(1,1),1),"log(x+2)"), level=sim.111$level)
    temp2<-mlma(y=sim.111$y, data1=data2)
    summary(temp2) #numeric output
  }
}

create_num_var_table_2 <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var, num_bootstrap_replicates){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    x <- data_input[,num_var_1]
    y <- data_input[,num_var_2]
    m1 <- data_input[,num_var_3[1]]
    m2 <- data_input[,num_var_3[2]]
    m3 <- data_input[,num_var_3[3]]
    lv <- data_input[,fact_var]
    # print(typeof(m1))
    # write(typeof(m1), stdout())
    mat = matrix(ncol = 0, nrow = length(m1))
    m4=data.frame(mat)
    # for(vector in num_var_3){
    #   m=cbind(m,vector)
    # }
    # m=m1
    m=cbind(m1,m2,m3,m4)
    sim.111=list(x=x,m=m,y=y,level=lv)
    data2<-data.org(sim.111$x, m=data.frame(sim.111$m), 
                    f10y=list(1,c("x^2","sqrt(x+6)")), 
                    f20ky=list(1,c("x","x^3")), 
                    f10km=list(matrix(c(1,1),1),"log(x+2)"), level=sim.111$level)
    tmp2.boot<-boot.mlma(y=sim.111$y, data1=data2, boot=num_bootstrap_replicates,echo=F)
    summary(tmp2.boot) #Graphic Outputs
  }
}

ui <- navbarPage(
  title = "LSU - Mediation Analysis Project",
  
  use_theme(create_theme(theme = "flatly", 
  bs_vars_navbar(default_bg = "#dfb52a", default_border = "#301456", default_link_color = "#301456", default_link_active_color = "#301456", 
                 default_link_hover_color = "#FFFFFF", ), 
  bs_vars_global(body_bg = "#301456",text_color = "#dfb52a",link_color = "#dfb52a",link_hover_color = "#dfb52a",), 
  bs_vars_tabs(active_link_hover_color = "#dfb52a",), bs_vars_button(
    default_color = "#301456",default_bg = "#dfb52a",))),
  
  main_page,
  about_page
)

server <- function(input, output){
  
  options(shiny.maxRequestSize=10*1024^2) 

  data_input <- reactive({
    req(input$csv_input)
    if(str_ends(input$csv_input$datapath, "csv")) 
    {
      read.csv(input$csv_input$datapath)
    }
    else if (str_ends(input$csv_input$datapath, "(xlsx|xls)")) 
    {
      read.xlsx(input$csv_input$datapath, sheetIndex = 1)
    }
    else if(str_ends(input$csv_input$datapath, "txt"))
    {
      read.table(input$csv_input$datapath, header = TRUE)
    }
  })

  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "num_var_3", choices = choices)
    updateNumericInput(inputId = "num_bootstrap_replicates")
    updateSelectInput(inputId = "fact_var", choices = choices)
  })

  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  num_var_3 <- eventReactive(input$run_button,input$num_var_3)
  num_bootstrap_replicates <- eventReactive(input$run_button,input$num_bootstrap_replicates)
  fact_var <- eventReactive(input$run_button,input$fact_var)

  # Plot
  
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1(), num_var_2(), num_var_3(), fact_var(), num_bootstrap_replicates())
  })

  output$plot_1 <- renderPlot(plot_1())
  
  # Summary Table without Bootstrapping

  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table_1(data_input(),num_var_1(),num_var_2(),num_var_3(),fact_var())
  })

  output$model_summary_1 <- renderPrint(num_var_1_summary_table())
  
  # Summary Table Using Bootstrapping
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table_2(data_input(),num_var_1(),num_var_2(),num_var_3(),fact_var(),num_bootstrap_replicates())
  })
  
  output$model_summary_2 <- renderPrint(num_var_2_summary_table())

}

shinyApp(ui = ui, server = server)