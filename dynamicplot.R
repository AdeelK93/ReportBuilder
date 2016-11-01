library(tidyverse) #use dev version to prevent reactive dplyr grouping crashes

#create the inputs for the section modules
boxUI <- function(ID) {
  ns <- NS(ID) #generate namespace
  wellPanel(
    #section title is abbreviated to namespace-t
    textInput(ns("t"),"Section Title",placeholder = "Section Title"),
    #sction comment is namespace-c
    textAreaInput(ns("c"),"Comments",placeholder="Add commments here (markdown accepted)"),
    actionLink(ns("modalBtn"),"Plot Builder",icon("wrench")),
    #section code is namespace-e
    tags$div(id=ns("e"),name=ns("e"),class="editor form-group shiny-input-container"),
    plotOutput(ns("plot"))
  )
}

#create the outputs for the section modules
dynPlot <- function(input,output,session,ID) {
  #plot builder modal, see plotbuilder.js for more details
  observeEvent(input$modalBtn, {
    showModal(modalDialog(
      title = paste(input$t,"Plot Builder"),
      fluidRow(
        column(4,
               sliderInput("filter.voltage","Select a voltage range:",filter.min()[1],filter.max()[1],
                           c(filter.min()[1],filter.max()[1]),0.1),
               sliderInput("filter.totaltime","Select a total time range:",filter.min()[4],filter.max()[4],
                           c(filter.min()[4],filter.max()[4]),1)
        ),
        column(4,
               sliderInput("filter.current","Select a current range:",filter.min()[2],filter.max()[2],
                           c(filter.min()[2],filter.max()[2]),0.1),
               sliderInput("filter.steptime","Select a step time range:",filter.min()[5],filter.max()[5],
                           c(filter.min()[5],filter.max()[5]),1)
        ),
        column(4,
               sliderInput("filter.amphours","Select a capacity range:",filter.min()[3],filter.max()[3],
                           c(filter.min()[3],filter.max()[3]),0.1),
               sliderInput("filter.step","Is there a step to isolate? (0 for no)",0,
                           tryCatch(max(db()$Step[db()$Step<=ceiling(quantile(
                             db()$Step,.99))]),error=function(e) 1),0,1)
        )
      ),
      tabsetPanel(
        tabPanel(
          "Quick",
          br(),
          fluidRow(
            column(4,radioButtons("modalq","Capacity Checks",
                                  c("Discharge Capacity","Reserve Capacity","Ah In During Charge","%Ah In During Charge","Time to 100% Recharge Factor"))),
            column(4,radioButtons("modalq","Cold Cranking/Charge Acceptance",
                                  c("CCA Duration","Voltage After 30s","Time to 6V","Current at 10m","Ah In at 10m"))),
            column(4,radioButtons("modalq","Cycling",
                                  c("Minimum Discharge Voltage","BOC Voltage","EOC Voltage","BOC Current","EOC Current")))
          )
        ),
        tabPanel(
          "Continuous",
          br(),
          fluidRow(
            column(6,selectInput("modalcx","X-axis",colnames(db()))),
            column(6,selectInput("modalcy","Y-axis",colnames(db())))
          )
        ),
        tabPanel(
          "Discrete",
          br(),
          fluidRow(
            column(4,selectInput("modaldx","Known variable",colnames(db()))),
            column(4,textInput("modaldv","Value of known variable:",placeholder="Numeric, or min/max")),
            column(4,selectInput("modaldy","Unknown variable",colnames(db())))
          )
        )
      ),
      footer=fluidRow(
        column(2,actionButton("modalbar","Bar Graph",icon("bar-chart"),class="btn-primary")),
        column(2,actionButton("modalbox","Box Plot",icon("archive"),class="btn-primary")),
        column(2,actionButton("modalline","Line (All)",icon("line-chart"),class="btn-primary")),
        column(2,actionButton("modalline2","Line (Avg)",icon("area-chart"),class="btn-primary")),
        column(4,modalButton("Dismiss",icon("remove")))
      ),
      #this variable will be used to identify the code editor to modify. also initializing buttons.
      tags$script(paste0("var modalID='",ID,"-e';document.getElementById('modalbar').onclick=modalBar;document.getElementById('modalbox').onclick=modalBox;document.getElementById('modalline').onclick=modalLine;document.getElementById('modalline2').onclick=modalLine2;radioSet('modalq','Discharge Capacity');")),
      size="l",
      easyClose = T
    ))
  })
  
  #the output plot
  output$plot <- renderPlot(if(!is.null(input$e)){
    x <- db()
    eval(parse(text=input$e))
  } else(ggplot())
  )
}

#merge all outputs in a markdown format
dynReport <- function(input,output,session) {
  if(nchar(input$t)) chunkTitle <- gsub("[[:punct:]]","",make.names(input$t))
  else chunkTitle <- ""
  code <- {
    if(!length(input$e)) "x %>% ggplot()"
    else input$e
  }
  sprintf(
    "
    ##%s  
    %s
    
    ```{r %s}
    %s
    ```
    ",input$t,input$c,chunkTitle,input$e
  )
}

aceBuilder <- function(ID,cont) {
  runjs(paste0("aceBuilder('",ID,"-e',",jsQuote(cont),");"))
}
runjs <- function(js) session$sendCustomMessage("run",js)
#correctly escape string before sending to javascript
jsQuote <- function(x) gsub("\n","\\\\n",shQuote(x))

colorline <- function(dataset) {
  dataset <- dataset[!duplicated(dataset$Battery.ID),c("Type","Battery.ID")] %>%
    arrange(Battery.ID)
  spl <- split(dataset$Battery.ID,dataset$Type)
  dataset$Line <- unsplit(lapply(spl,seq_along),dataset$Type)
  dataset
}