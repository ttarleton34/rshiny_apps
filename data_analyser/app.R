library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(mlma)
library(fresh)

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
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
      selectInput("num_var_1", "Predictor: X", choices = c(not_sel)),
      selectInput("num_var_2", "Response: Y", choices = c(not_sel)),
      selectInput("num_var_3", "Mediators", choices = c(not_sel), multiple = TRUE),
      selectInput("fact_var", "Level of X", choices = c(not_sel)),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Model Graphic Outputs",
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Model Statistics",
          verbatimTextOutput("model_summary")
        )
      )
    )
  )
)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    x <- data_input[,get(num_var_1)]
    y <- data_input[,get(num_var_2)]
    m1 <- data_input[,get(num_var_3[1])]
    m2 <- data_input[,get(num_var_3[2])]
    m3 <- data_input[,get(num_var_3[3])]
    lv <- data_input[,get(fact_var)]
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
    temp2<-mlma(y=sim.111$y, data1=data2)
    plot(temp2, col = "red") #graphic output
  }
}

create_num_var_table <- function(data_input, num_var_1, num_var_2, num_var_3, fact_var){
  if(num_var_1 != not_sel & num_var_2 != not_sel  & num_var_3 != not_sel & fact_var != not_sel){
    x <- data_input[,get(num_var_1)]
    y <- data_input[,get(num_var_2)]
    m1 <- data_input[,get(num_var_3[1])]
    m2 <- data_input[,get(num_var_3[2])]
    m3 <- data_input[,get(num_var_3[3])]
    lv <- data_input[,get(fact_var)]
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

ui <- navbarPage(
  title = "LSU - Mediation Analysis Project",
  
  use_theme(create_theme(theme = "flatly", 
  bs_vars_navbar(default_bg = "#dfb52a", default_border = "#301456", default_link_color = "#301456", default_link_active_color = "#301456", 
                 default_link_hover_bg = "#FFFFFF", ), 
  bs_vars_global(body_bg = "#301456",text_color = "#dfb52a",link_color = "#dfb52a",link_hover_color = "#dfb52a",), 
  bs_vars_tabs(active_link_hover_color = "#dfb52a",))),
  
  main_page,
  about_page
)

server <- function(input, output){
  
  options(shiny.maxRequestSize=10*1024^2) 

  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })

  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "num_var_3", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })

  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  num_var_3 <- eventReactive(input$run_button,input$num_var_3)
  fact_var <- eventReactive(input$run_button,input$fact_var)

  # plot
  
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1(), num_var_2(), num_var_3(), fact_var())
  })

  output$plot_1 <- renderPlot(plot_1())
  
  # 1-d summary tables

  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(),num_var_1(),num_var_2(),num_var_3(),fact_var())
  })

  output$model_summary <- renderPrint(num_var_1_summary_table())

}

shinyApp(ui = ui, server = server)