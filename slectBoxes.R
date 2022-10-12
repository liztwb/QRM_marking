library(shiny)
examplesubset<-read.table(text="
                          elements locations
                          element_One A,M,P,A,R,T
                          element_Two A,B,C,M,P,E,I,N,S
                          element_Three G,M,T,F,S,V,P" ,  header=TRUE, stringsAsFactors=FALSE)
examplesubset$elements<-as.factor(examplesubset$elements)

ui<-fluidPage(    
  tags$head(tags$style(HTML("
                            .multicol .shiny-options-group{
                            -webkit-column-count: 3; /* Chrome, Safari, Opera */
                            -moz-column-count: 3;    /* Firefox */
                            column-count: 3;
                            -moz-column-fill: balanced;
                            -column-fill: balanced;
                            }
                            .checkbox{
                            margin-top: 0px !important;
                            -webkit-margin-after: 0px !important; 
                            }
                            "))),
  titlePanel("Panel"),
  sidebarLayout(      
    sidebarPanel(
      selectInput("elements", "Select elements:", 
                  choices=examplesubset$elements)
    ) ,
    mainPanel(

fluidRow(
  column(3,
         uiOutput("checkboxesui")
  ))))
)

server<-function(input, output,session) {
  elementsselected<-reactive({
    sp<-examplesubset[examplesubset$elements==input$elements,]
    sp<-droplevels(sp)
  })
  locationsreactive<- reactive({
    j<-as.factor(unique(unlist(strsplit(elementsselected()$locations, ",", fixed = TRUE) ) ) )
    j<-droplevels(j)
  })
  output$checkboxesui<-renderUI({
    tags$div(align = 'left',
             class = 'multicol',
             checkboxGroupInput("locationscheckboxes", "locations",
                                choices=levels(locationsreactive()) 
                                , selected=c() )
    ) 
  })
}
shinyApp(ui = ui, server = server)
