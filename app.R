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



server <- function(input, output, session) {

#  cat(stderr(), "getwd()", getwd())  
#  wd <- getwd
 

  getrc <- function(x, y) {
    col <- ifelse(x > 200, 3,
                  ifelse(x > 100, 2, 1))
    row <- ifelse(y > 500, 1,
                  ifelse(y > 400, 2,
                         ifelse(y > 300, 3,
                                ifelse(
                                  y > 200, 4,
                                  ifelse(y > 100, 5, 6)
                                ))))
    return(c(row, col))
  }
  
  getrect <- function(r, c) {
    xb <- switch(c,
                 c(0, 100),
                 c(100, 200),
                 c(200, 300))
    yb <- switch(r,
                 c(500, 600),
                 c(400, 500),
                 c(300, 400),
                 c(200, 300),
                 c(100, 200),
                 c(0, 100))
    
    
    #    cat(file  = stderr(), "xb = ", xb, " yb = ", yb, "\n")
    return(c(xb[1], yb[1], xb[2], yb[2]))
  }
  
  
  
  plotmat <- function(mat) {
    plot(
      c(-100, 300),
      c(-100, 600),
      type = "n",
      axes = FALSE,
      xlab = "",
      ylab = ""
    )
    #rect(0, 0, c(100,200,300), 100, col = "blue", border = "black", lwd=2) # transparent
    text(-2,550, "Presentation", pos = 2)
    text(-2,450, "Comprehension of statistics", pos =2)
     text(-2,350, "Comprehension of risk man", pos = 2)
      text(-2,250, "Comprehension of computation", pos =2)
       text(-2,150, "Clarity of expression", pos=2)
             text(-2,50, "Quality of analysis", pos=2)
              
    rect(0, seq(0, 600, 100), 100, 200, lwd = 2)
    rect(100, seq(0, 600, 100), 200, 200, lwd = 2)
    rect(200, seq(0, 600, 100), 300, 200, lwd = 2)
    if (any(mat)) {
      r <- row(mat)[which(mat)]
      c <-  col(mat)[which(mat)]
      tc <- cbind(r, c)
      lst <- list()
      for (i in 1:dim(tc)[1]) {
        lst[[i]] <- getrect(tc[i, 1], tc[i, 2])
      }
      sapply(lst, function(x)
        rect(x[1], x[2], x[3], x[4], lwd = 2, col = "lightblue"))
    }
  }
  
  matvals <- reactiveValues(
    rmat = matrix(FALSE, nrow = 6, ncol = 3)
  )
  
  observeEvent(input$plot_click,{
               clk <- input$plot_click
               newpt <- getrc(clk$x, clk$y)
               if(any(matvals$rmat[newpt[1],])){ # something in row
                # blank <- c(newpt[1], which(matvals$rmat[newpt[1],]))
                 matvals$rmat[newpt[1], which(matvals$rmat[newpt[1],])] <- FALSE  
               }
               matvals$rmat[newpt[1], newpt[2]] <- TRUE
  })
  
  output$plot1 <- renderPlot({
plotmat(matvals$rmat)
  })
  
  output$qrmrubric <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "qrm_rubric.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "qrm_rubric.Rmd")
      file.copy("qrm_rubric.Rmd", tempReport, overwrite = TRUE)
      tempHead <- file.path(tempdir(), "header.tex")
      file.copy("header.tex", tempHead, overwrite = TRUE)
      com <- input$comment
      if(input$small_samp){ com <- paste(com, small_samp_comment, sep = "\n\n")}
      if(input$log){ com <- paste(com, log_comment, sep = "\n\n")}
      if(input$badpval){ com <- paste(com, badpval_comment, sep = "\n\n")}
      if(input$twocnomin){ com <- paste(com, twoc_nomin_comment, sep = "\n\n")}
      if(input$twocnoexplore){ com <- paste(com, twoc_noexplore_comment, sep = "\n\n")}
      if(input$aw){ com <- paste(com, aw_comment, sep = "\n\n")}
            if(input$nomult){ com <- paste(com, nomult_comment, sep = "\n\n")}
      
      
      # cat(file=stderr(), "tempReport = ",tempReport, "\n")
      # Set up parameters to pass to Rmd document
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      # cat(file = stderr(), "file = ", file, "\n")
      # cat(file = stderr(), "getwd = ", getwd(), "\n")
      # write the mark to the file
      wd <- getwd()
      pstr <- strsplit(wd, "/")[[1]][8]
      part <- paste(strsplit(pstr, "_")[[1]][1:2], collapse = " ")
      library(dplyr)
      library(readr)
      gfile <- "C:/Users/liztwb/Documents/QRM/Marking/Grades.csv"
      d <- read_csv(gfile)
      #id <- paste("Participant", part)
      #idname <- names(d)[1]
      #dplyr::filter(d, idname == id)
      row <- which(d[,1] == part)
      d[row,"Grade"] <- input$mark
      write_csv(d, gfile)
      dd <- list(comment = input$comment, log = input$log, badpval = input$badpval, 
                 small_samp = input$small_samp, nomult = input$nomult, 
                 twocnomin = input$twocnomin, twocnoexplore = input$twocnoexplore, 
                 aw = input$aw, mark = input$mark, participant = part,
                 rmat = matvals$rmat, time = Sys.time() )
      fullname <- "C:/Users/liztwb/Documents/QRM/Marking/record.rds"
      if(!file.exists(fullname)){
        saveRDS(dd, fullname)
      } else {
        d <- readRDS(fullname)
        n <- length(d)
        d[[n+1]] <- dd
        saveRDS(d, fullname)
      }
      params <- list(rmat = matvals$rmat, 
                     comment = com, 
                     mark = input$mark,
                     part = part)
      out <- rmarkdown::render(tempReport,
                        params = params,
                        envir = new.env(parent = globalenv())
                        
      )
      file.copy(out, "../feedback.pdf", overwrite = TRUE)   
      stopApp()
    }
)
}


shinyApp(ui, server)