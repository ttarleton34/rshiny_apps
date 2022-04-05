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
library(ggdag)
library(rmarkdown)
library(tinytex)

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
      selectInput("num_var_1", "Predictor: X", choices = c(not_sel), multiple = TRUE),
      selectInput("num_var_2", "Response: Y", choices = c(not_sel)),
      selectInput("num_var_3", "Mediators", choices = c(not_sel), multiple = TRUE),
      selectInput("fact_var", "Level of X", choices = c(not_sel)),

      selectInput("f10km", "Transformation of predictor (level 1) --> Mediator (level 1)", choices = c("Linear", "Log Transformation", "Power Transformation", "Square Root Transformation")),
      uiOutput("f10km_constant"),
      selectInput("f10y", "Transformation of predictor (level 1) --> Response", choices = c("Linear", "Log Transformation", "Power Transformation", "Square Root Transformation")),
      uiOutput("f10y_constant"),
      selectInput("f20ky", "Transformation of mediator (level 1) --> Response", choices = c("Linear", "Log Transformation", "Power Transformation", "Square Root Transformation")),
      uiOutput("f20ky_constant"),
      selectInput("f01y", "Transformation of predictor (level 2) --> Response", choices = c("Linear", "Log Transformation", "Power Transformation", "Square Root Transformation")),
      uiOutput("f01y_constant"),
      selectInput("f02ky", "Transformation of mediator (level 2) --> Response", choices = c("Linear", "Log Transformation", "Power Transformation", "Square Root Transformation")),
      uiOutput("f02ky_constant"),
      selectInput("f01km1", "Transformation of predictor (level 2) --> Mediator (level 1)", choices = c("Linear", "Log Transformation", "Power Transformation", "Square Root Transformation")),
      uiOutput("f01km1_constant"),
      selectInput("f01km2", "Transformation of predictor (level 2) --> Mediator (level 2)", choices = c("Linear", "Log Transformation", "Power Transformation", "Square Root Transformation")),
      uiOutput("f01km2_constant"),

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
      actionButton("run_button", "Run Analysis", icon = icon("play")),
      br(),
      br(),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport', "Download Report")
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
        tabPanel(
          title = "DAG Visualition",
          plotOutput("dag_visualization")
        )
      )
    )
  )
)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var, num_bootstrap_replicates, cf10km, cf10km_constant, cf10y, cf10y_constant, cf20ky, cf20ky_constant, cf01y, cf01y_constant, cf02ky, cf02ky_constant, cf01km1, cf01km1_constant, cf01km2, cf01km2_constant){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    y <- data_input[,num_var_2]
    lv <- data_input[,fact_var]

    #Putting x vars into dataframe
    x <- matrix(0,length(data_input[,num_var_1[1]]),0)
    for(val in 1:length(num_var_1)){
       x <- cbind(x,data_input[,num_var_1[val]])
    }
    x <- data.frame(x)

    names = c()
    for(val in 1:length(num_var_1)){
      names <- append(names, num_var_1[val])
    }
    names(x) = names

    #Putting m vars into dataframe
    m <- matrix(0,length(data_input[,num_var_3[1]]),0)
    for(val in 1:length(num_var_3)){
       m <- cbind(m,data_input[,num_var_3[val]])
    }
    m <- data.frame(m)

    names = c()
    for(val in 1:length(num_var_3)){
      names <- append(names, num_var_3[val])
    }
    names(m) = names

    sim.111=list(x=x,m=m,y=y,level=lv)

    transformation_format <- function(input, input_constant){
      if(input == "Linear")
      {
        return("x")
      }
      else if(input == "Log Transformation")
      {
        return(paste("log(x+", input_constant, ")", sep = ""))
      }
      else if(input == "Power Transformation")
      {
        return(paste("x^", input_constant, sep = ""))
      }
      else if(input == "Square Root Transformation")
      {
        return(paste("sqrt(x+", input_constant, ")", sep = ""))
      }
    }

    f01y <- transformation_format(cf01y, cf01y_constant)

    f10y <- transformation_format(cf10y, cf10y_constant)
    
    f02ky <- transformation_format(cf02ky, cf02ky_constant)

    f20ky <- transformation_format(cf20ky, cf20ky_constant)
    
    #The following three transformation related arguments give errors when "x" is returned instead of "x^1"
    if(cf01km1 == "Linear")
    {
      f01km1 <- "x^1"
    }
    else
    {
      f01km1 <- transformation_format(cf01km1, cf01km1_constant)
    }

    if(cf01km2 == "Linear")
    {
      f01km2 <- "x^1"
    }
    else
    {
      f01km2 <- transformation_format(cf01km2, cf01km2_constant)
    }

    if(cf10km == "Linear")
    {
      f10km <- "x^1"
    }
    else
    {
      f10km <- transformation_format(cf10km, cf10km_constant)
    } 

    data2<-data.org(x=data.frame(sim.111$x), m=data.frame(sim.111$m), 
                    f01y=list(1,f01y), 
                    f10y=list(1,f10y),
                    f02ky=list(1,f02ky),
                    f20ky=list(1,f20ky),
                    f01km1=list(matrix(c(1,1),1), f01km1),
                    f01km2=list(matrix(c(1,1),1), f01km2), 
                    f10km=list(matrix(c(1,1),1), f10km), level=sim.111$level) 
    tmp2.boot<-boot.mlma(y=sim.111$y, data1=data2, boot=num_bootstrap_replicates,echo=F)
    plot(tmp2.boot) #Graphic Outputs
  }
}

create_num_var_table_1 <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var, cf10km, cf10km_constant, cf10y, cf10y_constant, cf20ky, cf20ky_constant, cf01y, cf01y_constant, cf02ky, cf02ky_constant, cf01km1, cf01km1_constant, cf01km2, cf01km2_constant){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    y <- data_input[,num_var_2]
    lv <- data_input[,fact_var]

    #Putting x vars into dataframe
    x <- matrix(0,length(data_input[,num_var_1[1]]),0)
    for(val in 1:length(num_var_1)){
       x <- cbind(x,data_input[,num_var_1[val]])
    }
    x <- data.frame(x)

    names = c()
    for(val in 1:length(num_var_1)){
      names <- append(names, num_var_1[val])
    }
    names(x) = names

    #Putting m vars into dataframe
    m <- matrix(0,length(data_input[,num_var_3[1]]),0)
    for(val in 1:length(num_var_3)){
       m <- cbind(m,data_input[,num_var_3[val]])
    }
    m <- data.frame(m)

    names = c()
    for(val in 1:length(num_var_3)){
      names <- append(names, num_var_3[val])
    }
    names(m) = names

    sim.111=list(x=x,m=m,y=y,level=lv)

    transformation_format <- function(input, input_constant){
      if(input == "Linear")
      {
        return("x")
      }
      else if(input == "Log Transformation")
      {
        return(paste("log(x+", input_constant, ")", sep = ""))
      }
      else if(input == "Power Transformation")
      {
        return(paste("x^", input_constant, sep = ""))
      }
      else if(input == "Square Root Transformation")
      {
        return(paste("sqrt(x+", input_constant, ")", sep = ""))
      }
    }

    f01y <- transformation_format(cf01y, cf01y_constant)

    f10y <- transformation_format(cf10y, cf10y_constant)
    
    f02ky <- transformation_format(cf02ky, cf02ky_constant)

    f20ky <- transformation_format(cf20ky, cf20ky_constant)
    
    #The following three transformation related arguments give errors when "x" is returned instead of "x^1"
    if(cf01km1 == "Linear")
    {
      f01km1 <- "x^1"
    }
    else
    {
      f01km1 <- transformation_format(cf01km1, cf01km1_constant)
    }

    if(cf01km2 == "Linear")
    {
      f01km2 <- "x^1"
    }
    else
    {
      f01km2 <- transformation_format(cf01km2, cf01km2_constant)
    }

    if(cf10km == "Linear")
    {
      f10km <- "x^1"
    }
    else
    {
      f10km <- transformation_format(cf10km, cf10km_constant)
    } 

    data2<-data.org(x=data.frame(sim.111$x), m=data.frame(sim.111$m), 
                    f01y=list(1,f01y), 
                    f10y=list(1,f10y),
                    f02ky=list(1,f02ky),
                    f20ky=list(1,f20ky),
                    f01km1=list(matrix(c(1,1),1), f01km1),
                    f01km2=list(matrix(c(1,1),1), f01km2), 
                    f10km=list(matrix(c(1,1),1), f10km), level=sim.111$level) 
    temp2 <- mlma(y=sim.111$y, data1=data2)
    summary(temp2) #numeric output

    #print(data2$m1[1])
    #data2<-data.org(x=data.frame(sim.111$x), m=data.frame(sim.111$m), level=sim.111$level)
    #if(data2$m1[1] == NULL)
    #{
    #}
    #else
    #{ 
    #}
  }
}

create_num_var_table_2 <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var, num_bootstrap_replicates, cf10km, cf10km_constant, cf10y, cf10y_constant, cf20ky, cf20ky_constant, cf01y, cf01y_constant, cf02ky, cf02ky_constant, cf01km1, cf01km1_constant, cf01km2, cf01km2_constant){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    y <- data_input[,num_var_2]
    lv <- data_input[,fact_var]

    #Putting x vars into dataframe
    x <- matrix(0,length(data_input[,num_var_1[1]]),0)
    for(val in 1:length(num_var_1)){
       x <- cbind(x,data_input[,num_var_1[val]])
    }
    x <- data.frame(x)

    names = c()
    for(val in 1:length(num_var_1)){
      names <- append(names, num_var_1[val])
    }
    names(x) = names

    #Putting m vars into dataframe
    m <- matrix(0,length(data_input[,num_var_3[1]]),0)
    for(val in 1:length(num_var_3)){
       m <- cbind(m,data_input[,num_var_3[val]])
    }
    m <- data.frame(m)

    names = c()
    for(val in 1:length(num_var_3)){
      names <- append(names, num_var_3[val])
    }
    names(m) = names

    sim.111=list(x=x,m=m,y=y,level=lv)

    transformation_format <- function(input, input_constant){
      if(input == "Linear")
      {
        return("x")
      }
      else if(input == "Log Transformation")
      {
        return(paste("log(x+", input_constant, ")", sep = ""))
      }
      else if(input == "Power Transformation")
      {
        return(paste("x^", input_constant, sep = ""))
      }
      else if(input == "Square Root Transformation")
      {
        return(paste("sqrt(x+", input_constant, ")", sep = ""))
      }
    }

    f01y <- transformation_format(cf01y, cf01y_constant)

    f10y <- transformation_format(cf10y, cf10y_constant)
    
    f02ky <- transformation_format(cf02ky, cf02ky_constant)

    f20ky <- transformation_format(cf20ky, cf20ky_constant)
    
    #The following three transformation related arguments give errors when "x" is returned instead of "x^1"
    if(cf01km1 == "Linear")
    {
      f01km1 <- "x^1"
    }
    else
    {
      f01km1 <- transformation_format(cf01km1, cf01km1_constant)
    }

    if(cf01km2 == "Linear")
    {
      f01km2 <- "x^1"
    }
    else
    {
      f01km2 <- transformation_format(cf01km2, cf01km2_constant)
    }

    if(cf10km == "Linear")
    {
      f10km <- "x^1"
    }
    else
    {
      f10km <- transformation_format(cf10km, cf10km_constant)
    } 
    
    data2<-data.org(x=data.frame(sim.111$x), m=data.frame(sim.111$m), 
                    f01y=list(1,f01y), 
                    f10y=list(1,f10y),
                    f02ky=list(1,f02ky),
                    f20ky=list(1,f20ky),
                    f01km1=list(matrix(c(1,1),1), f01km1),
                    f01km2=list(matrix(c(1,1),1), f01km2), 
                    f10km=list(matrix(c(1,1),1), f10km), level=sim.111$level)  
    tmp2.boot<-boot.mlma(y=sim.111$y, data1=data2, boot=num_bootstrap_replicates,echo=F)
    summary(tmp2.boot) #Graphic Outputs
  }
}

create_dag_visualization <- function(num_var_1, num_var_2, num_var_3, fact_var){
  tidy_ggdag <- dagify(
    y ~ x + m1 + m2 + m3 + lv,
    m1 ~ x + lv,
    m2 ~ x + lv,
    m3 ~ x + lv,
    exposure = num_var_1[1],
    outcome = num_var_2,
    labels = c("x" = num_var_1[1], 
              "y" = num_var_2,
              "m1" = num_var_3[1],
              "m2" = num_var_3[2],
              "m3" = num_var_3[3],
              "lv" = fact_var)
  )

  ggdag_status(tidy_ggdag)
}

ui <- navbarPage(
  title = "LSU - Mediation Analysis Project",
  
  use_theme(create_theme(theme = "flatly", 
  bs_vars_navbar(default_bg = "#dfb52a", default_border = "#301456", default_link_color = "#301456", default_link_active_color = "#301456", 
                 default_link_hover_color = "#FFFFFF",), 
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

  output$f10km_constant <- renderUI({
    if(input$f10km == "Log Transformation")
    {
      numericInput(
        inputId = "f10km_constant",
        label = "Select shift d ie log(x+d)",
        value = 1)
    }
    else if(input$f10km == "Power Transformation")
    {
      numericInput(
        inputId = "f10km_constant",
        label = "Select order p ie x^p where p is an integer",
        value = 1)
    }
    else if(input$f10km == "Square Root Transformation")
    {
      numericInput(
        inputId = "f10km_constant",
        label = "Select root transformation shift d ie sqrt(x+d)",
        value = 1)
    }
  })

  output$f10y_constant <- renderUI({
    if(input$f10y == "Log Transformation")
    {
      numericInput(
        inputId = "f10y_constant",
        label = "Select shift d ie log(x+d)",
        value = 1)
    }
    else if(input$f10y == "Power Transformation")
    {
      numericInput(
        inputId = "f10y_constant",
        label = "Select order p ie x^p where p is an integer",
        value = 1)
    }
    else if(input$f10y == "Square Root Transformation")
    {
      numericInput(
        inputId = "f10y_constant",
        label = "Select root transformation shift d ie sqrt(x+d)",
        value = 1)
    }
  })

  output$f20ky_constant <- renderUI({
    if(input$f20ky == "Log Transformation")
    {
      numericInput(
        inputId = "f20ky_constant",
        label = "Select shift d ie log(x+d)",
        value = 1)
    }
    else if(input$f20ky == "Power Transformation")
    {
      numericInput(
        inputId = "f20ky_constant",
        label = "Select order p ie x^p where p is an integer",
        value = 1)
    }
    else if(input$f20ky == "Square Root Transformation")
    {
      numericInput(
        inputId = "f20ky_constant",
        label = "Select root transformation shift d ie sqrt(x+d)",
        value = 1)
    }
  })

  output$f01y_constant <- renderUI({
    if(input$f01y == "Log Transformation")
    {
      numericInput(
        inputId = "f01y_constant",
        label = "Select shift d ie log(x+d)",
        value = 1)
    }
    else if(input$f01y == "Power Transformation")
    {
      numericInput(
        inputId = "f01y_constant",
        label = "Select order p ie x^p where p is an integer",
        value = 1)
    }
    else if(input$f01y == "Square Root Transformation")
    {
      numericInput(
        inputId = "f01y_constant",
        label = "Select root transformation shift d ie sqrt(x+d)",
        value = 1)
    }
  })

  output$f02ky_constant <- renderUI({
    if(input$f02ky == "Log Transformation")
    {
      numericInput(
        inputId = "f02ky_constant",
        label = "Select shift d ie log(x+d)",
        value = 1)
    }
    else if(input$f02ky == "Power Transformation")
    {
      numericInput(
        inputId = "f02ky_constant",
        label = "Select order p ie x^p where p is an integer",
        value = 1)
    }
    else if(input$f02ky == "Square Root Transformation")
    {
      numericInput(
        inputId = "f02ky_constant",
        label = "Select root transformation shift d ie sqrt(x+d)",
        value = 1)
    }
  })

  output$f01km1_constant <- renderUI({
    if(input$f01km1 == "Log Transformation")
    {
      numericInput(
        inputId = "f01km1_constant",
        label = "Select shift d ie log(x+d)",
        value = 1)
    }
    else if(input$f01km1 == "Power Transformation")
    {
      numericInput(
        inputId = "f01km1_constant",
        label = "Select order p ie x^p where p is an integer",
        value = 1)
    }
    else if(input$f01km1 == "Square Root Transformation")
    {
      numericInput(
        inputId = "f01km1_constant",
        label = "Select root transformation shift d ie sqrt(x+d)",
        value = 1)
    }
  })

  output$f01km2_constant <- renderUI({
    if(input$f01km2 == "Log Transformation")
    {
      numericInput(
        inputId = "f01km2_constant",
        label = "Select shift d ie log(x+d)",
        value = 1)
    }
    else if(input$f01km2 == "Power Transformation")
    {
      numericInput(
        inputId = "f01km2_constant",
        label = "Select order p ie x^p where p is an integer",
        value = 1)
    }
    else if(input$f01km2 == "Square Root Transformation")
    {
      numericInput(
        inputId = "f01km2_constant",
        label = "Select root transformation shift d ie sqrt(x+d)",
        value = 1)
    }
  })

  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  num_var_3 <- eventReactive(input$run_button,input$num_var_3)
  num_bootstrap_replicates <- eventReactive(input$run_button,input$num_bootstrap_replicates)
  fact_var <- eventReactive(input$run_button,input$fact_var)

  f10km <- eventReactive(input$run_button,input$f10km)
  f10km_constant <- eventReactive(input$run_button,input$f10km_constant)
  f10y <- eventReactive(input$run_button,input$f10y)
  f10y_constant <- eventReactive(input$run_button,input$f10y_constant)
  f20ky <- eventReactive(input$run_button,input$f20ky)
  f20ky_constant <- eventReactive(input$run_button,input$f20ky_constant)
  f01y <- eventReactive(input$run_button,input$f01y)
  f01y_constant <- eventReactive(input$run_button,input$f01y_constant)
  f02ky <- eventReactive(input$run_button,input$f02ky)
  f02ky_constant <- eventReactive(input$run_button,input$f02ky_constant)
  f01km1 <- eventReactive(input$run_button,input$f01km1)
  f01km1_constant <- eventReactive(input$run_button,input$f01km1_constant)
  f01km2 <- eventReactive(input$run_button,input$f01km2)
  f01km2_constant <- eventReactive(input$run_button,input$f01km2_constant)

  # Plot
  
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1(), num_var_2(), num_var_3(), fact_var(), num_bootstrap_replicates(), f10km(), f10km_constant(), f10y(), f10y_constant(), f20ky(), f20ky_constant(), f01y(), f01y_constant(), f02ky(), f02ky_constant(), f01km1(), f01km1_constant(), f01km2(), f01km2_constant())
  })

  output$plot_1 <- renderPlot(plot_1())
  
  # Summary Table without Bootstrapping

  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table_1(data_input(),num_var_1(),num_var_2(),num_var_3(),fact_var(),f10km(), f10km_constant(), f10y(), f10y_constant(), f20ky(), f20ky_constant(), f01y(), f01y_constant(), f02ky(), f02ky_constant(), f01km1(), f01km1_constant(), f01km2(), f01km2_constant())
  })

  output$model_summary_1 <- renderPrint(num_var_1_summary_table())
  
  # Summary Table Using Bootstrapping
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table_2(data_input(), num_var_1(), num_var_2(), num_var_3(), fact_var(), num_bootstrap_replicates(), f10km(), f10km_constant(), f10y(), f10y_constant(), f20ky(), f20ky_constant(), f01y(), f01y_constant(), f02ky(), f02ky_constant(), f01km1(), f01km1_constant(), f01km2(), f01km2_constant())
  })
  
  output$model_summary_2 <- renderPrint(num_var_2_summary_table())

  dag_visualization <- eventReactive(input$run_button,{
    create_dag_visualization(num_var_1(),num_var_2(),num_var_3(),fact_var(),)
  })

  output$dag_visualization <- renderPlot(dag_visualization())

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('report.Rmd')
      params <- list(csv_input = input$csv_input$datapath, fileName = input$csv_input$name, num_var_1 = input$num_var_1, num_var_2 = input$num_var_2, 
      num_var_3 = input$num_var_3, fact_var = input$fact_var, num_bootstrap_replicates = input$num_bootstrap_replicates)

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ), params = params, envir = new.env(parent = globalenv()))
      file.rename(out, file)
    }
  )   
}

shinyApp(ui = ui, server = server)