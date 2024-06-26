library(shiny)
library(tidyverse)
library(ggplot2)
library(Cairo)
library(dplyr)
library(flexdashboard)
source("setup.R")

start <- Sys.time()

ui <- fluidPage(
  titlePanel('QRM Marking'),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        "mark",
        "Mark:",
        NA,
        min = 0,
        max = 100,
        width = 80
      ),
      checkboxGroupInput(
        "comments",
        "Choose comment",
        choiceNames = coms[, 1],
        choiceValues = coms[, 2]
      ),
      actionButton("qrmrubric", "Generate Report", class = "btn-success"),
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("Main",plotOutput("plot1", height = 350,
                 click = "plot_click"),
      column(
        width = 11,
        offset = 1,
        textAreaInput(
          "comment",
          "Comment",
          width = "100%",
          rows = 5,
          resize = "vertical"
        )
      )
      ),
      tabPanel("Summary",
               fluidRow(
                 column(3,
                        shinydashboard::box(
                          width = 12,
                          title = "Progress Rate",
                          gaugeOutput("proggauge")
                        )
                 ),
               column(3,
                      shinydashboard::box(
                        width = 12,
                        title = "Average Mark",
                      gaugeOutput("avgauge")
                      )
                ),
               column(3,
                      shinydashboard::box(
                        width = 12,
                        title = "Average time",
                        gaugeOutput("tgauge")
                      )
               )
               )

      ),
 tabPanel("Mark histogram",
            fluidRow(
              plotOutput("markhist", width = "100%")
            )
    )
      )
  )))

server <- function(input, output, session) {
  nd <- readRDS("C:/Users/liztwb/Documents/QRM/Marking/2024/record.rds")
  prop_done <- mt <- NA
  if(length(nd) >= 1){
  times <- sapply(nd, function(x) x$time - x$start)
  mt <- mean(times)
  
  mks <- sapply(nd, function(x) x$mark)
  avmark <- mean(mks, na.rm = TRUE)
  library(ggplot2)
  mdat <- data.frame(m = mks)
  p <- ggplot(mdat, aes(x = m)) +
    geom_histogram(aes(y = after_stat(density)),
                   colour = 1, fill = "white", binwidth = 5) +
    xlab("Mark") + 
    geom_density()
  
  grd <- readr::read_csv("C:/Users/liztwb/Documents/QRM/Marking/2024/Grades.csv", show_col_types = FALSE)
  tot <- dim(grd)[1]
  prop_done <- round(100*length(nd)/tot,2)}
  
  matvals <- reactiveValues(rmat = matrix(FALSE, nrow = 6, ncol = 3))
  
  observeEvent(input$plot_click, {
    clk <- input$plot_click
    newpt <- getrc(clk$x, clk$y)
    if (any(matvals$rmat[newpt[1], ])) {
      matvals$rmat[newpt[1], which(matvals$rmat[newpt[1], ])] <-
        FALSE
    }
    matvals$rmat[newpt[1], newpt[2]] <- TRUE
  })
  
  output$proggauge = renderGauge({
    gauge(prop_done, #input$value, 
          min = 0, 
          max = 100, 
          symbol = '%',
          sectors = gaugeSectors(success = c(0.5, 1), 
                                 warning = c(0.3, 0.5),
                                 danger = c(0, 0.3)))
  })
  
  output$tgauge = renderGauge({
    gauge(mt, #input$value, 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(0.5, 1), 
                                 warning = c(0.3, 0.5),
                                 danger = c(0, 0.3)))
  })
  
  output$avgauge = renderGauge({
    gauge(avmark, #input$value, 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(60, 100), 
                                 warning = c(40, 60),
                                 danger = c(0, 40)))
  })
  
  output$markhist <- renderPlot({
   p
  })
  
  output$plot1 <- renderPlot({
    plotmat(matvals$rmat)
  })
  

  observe({
    filename = "qrm_rubric.pdf"
    tempReport <- file.path(tempdir(), "qrm_rubric.Rmd")
    file.copy("qrm_rubric.Rmd", tempReport, overwrite = TRUE)
    tempHead <- file.path(tempdir(), "header.tex")
    file.copy("header.tex", tempHead, overwrite = TRUE)
    com <- input$comment
    sel_coms <- input$comments
    small_coms <- paste(sel_coms, collapse = "\n\n")
    com <- paste(com, small_coms, sep  = "\n\n")
    # Set up parameters to pass to Rmd document
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    # restore after testing
         wd <- getwd()
        pstr <- strsplit(wd, "/")[[1]][9]
      #  part <- paste(strsplit(pstr, "_")[[1]][1:2], collapse = " ")
        part <- strsplit(pstr, "_")[[1]][2] # just number
    #part <- 333333
    library(dplyr)
    library(readr)
    gfile <- "C:/Users/liztwb/Documents/QRM/Marking/2024/Grades.csv"
    #gfile <- "test.csv"
    d <- readr::read_csv(gfile)
    #id <- paste("Participant", part)
    #idname <- names(d)[1]
    #dplyr::filter(d, idname == id)
    partstr <- paste0("Participant ",part)
    row <- which(d[, 1] == partstr)
    d[row, "Grade"] <- input$mark
    readr::write_csv(d, gfile)
    dd <- list(
      comment = input$comment,
      input$comments,
      mark = input$mark,
      participant = part,
      rmat = matvals$rmat,
      start =  start,
      time = Sys.time()
    )
    fullname <- "C:/Users/liztwb/Documents/QRM/Marking/2024/record.rds"
   # fullname <- "record.rds"
    if (!file.exists(fullname)) {
      saveRDS(dd, fullname)
    } else {
      d <- readRDS(fullname)
      n <- length(d)
      d[[n + 1]] <- dd
      saveRDS(d, fullname)
    }
    params <- list(
      rmat = matvals$rmat,
      comment = com,
      mark = input$mark,
      part = part
    )
    out <- rmarkdown::render(
      tempReport,
      output_format = "pdf_document",
      params = params,
      envir = new.env(parent = globalenv())
      
    )
    #      file.copy(out, "../feedback.pdf", overwrite = TRUE)
    # check if we can write - if not creat the next in sequence
    
    copy_result <- try(file.copy(out, "feedback.pdf", overwrite = TRUE))
    if(!copy_result){
      feeds <- list.files(pattern = glob2rx("feedback*.pdf"))
      dim_feeds <- length(feeds)
      new_name <- paste0("feedback", dim_feeds,".pdf")
      file.copy(out, new_name)
    }
    stopApp()
    
  }) %>%
    bindEvent(input$qrmrubric)
}

shinyApp(ui, server)