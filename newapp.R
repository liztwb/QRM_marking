#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(Cairo)

# note this could be a list?
com1 <- "This is comment one placeholder"
com2 <- "This is comment two placeholder"
com3 <- "This is comment three placeholder"
com4 <- "This is comment four placeholder"
com5 <- "This is comment five placeholder"
com6 <- "This is comment six placeholder"# Define UI for application that draws a histogram

com_vec <- c(com1,com2,com3,com4,com5,com6)

ui <- fluidPage(

    # Application title
  #  titlePanel("Old Faithful Geyser Data"),
# custom CSS
  tags$head(tags$style(
    HTML("
          pre, table.table{
          font-size: smaller;
          }
          ")
  )),
  fluidRow(
    column(2,
           numericInput("mark", "Mark:", NA, min = 0, max = 100,
                        width = 80),
           br(),
           downloadButton("qrmrubric", "Generate report")
           ),
    column(10, 
           plotOutput("plot1", height = 400,
                      click = "plot_click"))
  ),
  fluidRow(
    column(3,
           checkboxInput("c1", "c1_summary"),
           checkboxInput("c2", "c2_summary")),
    column(3,
           checkboxInput("c3", "c3_summary"),
           checkboxInput("c4", "c4_summary")),
    column(3,
           checkboxInput("c5", "c5_summary"),
           checkboxInput("c6", "c6_summary"))
  ),
column(width = 12, offset = 1,
       textAreaInput("comment", "Comment", width = 800, resize = "vertical"))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
getrc <- function(x, y){
  col <- ifelse(x > 200, 3, ifelse(x > 100, 2, 1))
  row <- ifelse(y > 500, 1,
                ifelse(y > 400, 2,
                       ifelse(y > 300, 3,
                              ifelse(y > 200, 4, 
                                     ifelse(y > 100, 5, 6)
                                     ))))
  return(c(row, col))
}

getrect <- function(r, c){
  xb <- switch (c,
                c(0,100),
                c(100,200),
                c(200, 300))
  yb <- switch(r,
               c(500, 600),
               c(400, 500),
               c(300, 400),
               c(200,300),
               c(100, 200),
               c(0, 100))
  return(c(xb[1], yb[1], xb[2], yb[2]))
}

plotmat <- function(mat){
  plot(
    c(-100, 300),
    c(-100, 600),
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  text(-2, 500, "Presentation", pos = 2)
  text(-2, 450, "comprehension of statistics", pos = 2)
  text(-2, 350, "Comprehension of risk man", pos = 2)
  text(-2, 250, "Comprehension of computation", pos = 2)
  text(-2, 150, "Clarity of expression", pos = 2)
  text(-2,  50, "QUality of analysis", pos = 2)
  rect(0, seq(0, 600, 100), 100, 200, lwd = 2)
  rect(100, seq(0, 600, 100), 200, 200, lwd = 2)
  rect(200, seq(0, 600, 100), 300, 200, lwd = 2)
  if(any(mat)){
    r <- row(mat)[which(mat)]
    c <- col(mat)[which(mat)]
    tc <- cbind(r, c)
    lst <- list()
    for(i in 1:dim(tc)[1]){
      lst[[i]] <- getrect(tc[i, 1], tc[i,2])
    }
    sapply(lst, function(x)
      rect(x[1], x[2], x[3], x[4], lwd = 2, col = "lightblue"))
  }
}

  matvals  <- reactiveValues(
    rmat = matrix(FALSE, nrow = 6, ncol = 3)
  )
  
  observeEvent(input$plot_click,{
    newpt <- getrc(clk$x, clk$y)
    if(any(matvals$rmat[newpt[1],])){
      matvals$rmat[newpt[1], which(matvals$rmat[newpt[1],])] <- FALSE
    }
    matvals$rmat[newpt[1], newpt[2]] <- TRUE
  })
  
  output$plot1 <- renderPlot({
    plotmat(matvals$rmat)
  })
  
  output$qrmrubric <- downloadHandler(
    filename <- "qrm_rubric.pdf",
    content <- function(file){
      tempReport <- file.path(tempdir(), "qrm_rubric.Rmd")
      file.copy("qrm_rubric.Rmd", tempReport, overwrite = TRUE)
      tempHead <- file.path(tempdir(), "header.tex")
      file.copy("qrm_rubric.Rmd", tempReport, overwrite = TRUE)
      
    }
  )
  
} # end of server func

# Run the application 
shinyApp(ui = ui, server = server)
