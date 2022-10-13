library(shiny)
library(ggplot2)
library(Cairo)

# sidebar version

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
    c(-100, 630),
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
  text(50,625, "Poor", adj = 0.5)
  text(150,625, "Satisfactory", adj = 0.5)
  text(250,625, "Very good", adj = 0.5)
  
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
# note this could be a list?

# com1 etc are the actual comments to go in the feedback
# scom1 etc are the short versions for the input selectors
com1 <- "This is comment one placeholder"
com2 <- "This is comment two placeholder"
com3 <- "This is comment three placeholder"
com4 <- "This is comment four placeholder"
com5 <- "This is comment five placeholder"
com6 <- "This is comment six placeholder"# Define UI for application that draws a histogram

com_vec <- c(com1,com2,com3,com4,com5,com6)

scom1 <- "short c1"
scom2 <- "short c2"
scom3 <- "short c3"
scom4 <- "short c4"
scom5 <- "short c5"
scom6 <- "short c6"
scom_vec <- c(scom1,scom2,scom3,scom4,scom5,scom6)

coms <- data.frame(scom_vec, com_vec)

ui <- fluidPage(
# ui <- pageWithSidebar(
#  shinythemes::themeSelector(),
  titlePanel('QRM Marking'),
  sidebarLayout(
  sidebarPanel(
    numericInput("mark", "Mark:", NA, min = 0, max = 100,
                 width = 80),
    
#    checkboxInput("c1", "c1_summary"),
#    checkboxInput("c2", "c2_summary"),
#    checkboxInput("c3", "c3_summary"),
#    checkboxInput("c4", "c4_summary"),
#    checkboxInput("c5", "c5_summary"),
#    checkboxInput("c6", "c6_summary"),
checkboxGroupInput("comments", "Choose comment",
                   choiceNames = coms[,1],
                   choiceValues = coms[,2]
                   
),
    downloadButton("qrmrubric", "Generate report"),
  ),
 mainPanel(
           plotOutput("plot1", height = 400,
                      click = "plot_click"),
  column(width = 12, offset = 1,
         textAreaInput("comment", "Comment", width = 800, resize = "vertical"))
))
)




server <- function(input, output, session) {

#  cat(stderr(), "getwd()", getwd())  
#  wd <- getwd
 



  
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
      sel_coms <- coms[input$comments,2]
      com <- paste(com, sel_coms, collapse = "\n\n")
#      if(input$c1){ com <- paste(com, com1, sep = "\n\n")}
#      if(input$c2){ com <- paste(com, com2, sep = "\n\n")}
#      if(input$c3){ com <- paste(com, com3, sep = "\n\n")}
#      if(input$c4){ com <- paste(com, com4, sep = "\n\n")}
#      if(input$c5){ com <- paste(com, com5, sep = "\n\n")}
#      if(input$c6){ com <- paste(com, com6, sep = "\n\n")}
      # Set up parameters to pass to Rmd document
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
# restore after testing
#            wd <- getwd()
#      pstr <- strsplit(wd, "/")[[1]][8]
#      part <- paste(strsplit(pstr, "_")[[1]][1:2], collapse = " ")
      part <- 333333     
      library(dplyr)
      library(readr)
      gfile <- "C:/Users/liztwb/Documents/QRM/Marking/Grades.csv"
      gfile <- "test.csv"
      d <- read_csv(gfile)
      #id <- paste("Participant", part)
      #idname <- names(d)[1]
      #dplyr::filter(d, idname == id)
      row <- which(d[,1] == part)
      d[row,"Grade"] <- input$mark
      write_csv(d, gfile)
      dd <- list(comment = input$comment, 
                 input$comments,
#                 c1 = input$c1,  c2 = input$c2,
#                 c3 = input$c3, c4 = input$c4,
#                 c5 = input$c5, c6 = input$c6,
                 mark = input$mark, participant = part,
                 rmat = matvals$rmat, time = Sys.time() )
      fullname <- "C:/Users/liztwb/Documents/QRM/Marking/record.rds"
      fullname <- "record.rds"
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
#      file.copy(out, "../feedback.pdf", overwrite = TRUE)   
            file.copy(out, "feedback.pdf", overwrite = TRUE)   
      stopApp()
    }
)
}


shinyApp(ui, server)