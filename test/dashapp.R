library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Iris Chart"),
  dashboardSidebar(
    width = 0
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.info-box {min-height: 45px;} .info-box-icon 
                              {height: 45px; line-height: 45px;} .info-box-content {padding-top: 0px; 
                              padding-bottom: 0px;} 
                              '))),
    fluidRow(
      column(
        width = 12,
        column(
          width = 2,
          selectInput("Position", "", 
                      c("User_Analyses","User_Activity_Analyses"),selected = "Median", width = 
                        "400"),
          conditionalPanel(
            condition = "input.Position == 'User_Analyses'",
            style = "margin-top:-22px;",
            selectInput("stats", "", c("Time","Cases"),selected = "Median", width = "400"))
        ),
        column(
          style = "padding-top:20px;",
          width = 10,
          infoBox("User1", paste0(10), icon = icon("credit-card"), width = "3"),
          infoBox("User2",paste0(10), icon = icon("credit-card"), width ="3"),
          
          infoBox("User3",paste0(10), icon = icon("credit-card"), width ="3"),
          infoBox("User4",paste0(16), icon = icon("credit-card"), width ="3"))
      ),
      column(
        width = 12,
        conditionalPanel(
          condition = "input.Position == 'User_Analyses'",
          box(title = "Plot1", status = "primary",height = "537" ,solidHeader = T,
              plotOutput("case_hist",height = "466")),
          box(title = "Plot2", status = "primary",height = "537" ,solidHeader = T,
              plotOutput("trace_hist",height = "466"))
        ),
        conditionalPanel(
          condition = "input.Position == 'User_Activity_Analyses'",
          box(title = "Plot3",status = "primary",solidHeader = T,height = "537",width = "6",
              plotOutput("sankey_plot")),
          box(title = "Plot4",status = "primary",solidHeader = T,height = "537",width = "6",
              plotOutput("sankey_table"))
        )      
      )
    )
  )
)
server <- function(input, output) 
{
  output$case_hist <- renderPlot(
    plot(iris$Sepal.Length)
    
  )
  
  output$trace_hist <- renderPlot(
    plot(mtcars$mpg)
    
  )
  output$sankey_plot <- renderPlot({
    plot(diamonds$carat)
  })
  #Plot for Sankey Data table 
  output$sankey_table <- renderPlot({
    plot(iris$Petal.Length)
  })
}
shinyApp(ui, server)