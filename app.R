# TODO
# smart multicycle handling
# table and js plot outputs
# colorline dashed line support
# restore progress
library(shiny)
library(rmarkdown)
library(DBI)
options(shiny.sanitize.errors=F)
defaultsql <- "SELECT * FROM db\nWHERE \"Test\" = 'EN Cold Crank'\nAND \"Date\" < '2015-12-01';"

ui <- function(request) {
  fluidPage(
    theme = shinythemes::shinytheme("paper"),
    tags$head(
      tags$style(".editor {position: relative;width: 100%;height: 200px;}"),
      tags$style(".sqleditor {position: relative;width: 100%;height: 150px;}"),
      tags$script(src="ace/ace.js"),
      tags$script(src="ace/ext-language_tools.js"),
      tags$script(src="acebuilder.min.js"),
      tags$script(src="plotbuilder.min.js")
    ),
    titlePanel("SQL Report Builder"),
    
    sidebarPanel(
      tags$div(id="sql",name="sql",class="sqleditor form-group shiny-input-container"),
      helpText("Use SQL query to pull up the data you want to study. Dataset can later be referenced as the variable 'x'. 
               Remember to double quote column names and single quote strings."),
      actionButton('qBtn','Run Query',icon("search"),class="btn"),
      br(),
      tags$b("Summary of query:"),
      verbatimTextOutput("summary"),
      tags$hr(),
      bookmarkButton("Bookmark State",class="btn btn-primary"),
      downloadButton('download','Download Report',class="btn btn-primary")
    ),
    
    mainPanel(
      fillRow(
        textInput("Title","Report Title","Report Title"),
        textInput("Author","Report Author")
      ),
      br(),br(),br(),br(),
      tags$div(id = 'report'),
      actionButton('iBtn','Insert Section',class="btn"), 
      actionButton('rBtn','Remove Last',class="btn")
    ) 
  )
}

server <- function(input, output, session) {
  source("dynamicplot.R", local=T)
  inserted <- c() #this is the list that holds the names of elements
  sqlInvalidate <- FALSE #has the sql been invalidated by a restore?
  #initialize the sql query ace editor
  runjs(paste0("aceSql(",jsQuote(defaultsql),");$('#qBtn').click()"))
  
  #query the database and store in memory, should add safeties for overflow
  db <- reactive({
    input$qBtn
    if(is.null(isolate(input$sql))) return(0)
    if(sqlInvalidate) { #requery and invalidate when restore is complete
      if(input$sql != defaultsql) sqlInvalidate <<- FALSE
      runjs("setTimeout(function(){$('#qBtn').click();},100);")
    }
    conn <- dbConnect(
      RPostgreSQL::PostgreSQL(),host="localhost",
      user="postgres",password="pg"
    )
    query <- sqlInterpolate(conn,isolate(input$sql))
    x <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    #update autocomplete with database column names
    runjs(paste0("coln=[",paste0(sapply(colnames(x),function(x){
      sprintf("{value:'%s',score:1000,meta:'Database'}",x)
      }),collapse = ","),"];"))
    return(x)
  })
  output$summary <- renderPrint(
    db() %>% unclass() %>% as.data.frame() %>% str()
  )
  #calculations for the plot builder filters
  filter.cols <- c("Voltage","Current","Amp.Hours","Step.time","Total.Time")
  filter.min <- reactive({if(is.null(db())) return(rep(NaN,6))
    sapply(db()[filter.cols],min)
  })
  filter.max <- reactive({if(is.null(db())) return(rep(NaN,6))
    sapply(db()[filter.cols],max)
  })

  #insert a new section
  observeEvent(input$iBtn, {
    btn <- input$iBtn
    ID <- paste0('b', btn)
    if(ID %in% inserted) return(0)
    insertUI(
      selector = '#report',
      ui = tags$div(boxUI(ID),id = ID)
    )
    inserted <<- c(inserted, ID)
    callModule(dynPlot,ID,ID=ID)
    #create the js bindings for ace editor
    runjs(aceBuilder(ID,"x %>% ggplot()"))
  })
  
  #remove the last section
  observeEvent(input$rBtn, {
    removeUI(selector = paste0('#',inserted[length(inserted)]))
    inserted <<- inserted[-length(inserted)]
  })
  
  #create that big bundle of download
  output$download <- downloadHandler(
    filename = function() {
      gsub("[[:punct:]]"," ",make.names(input$Title)) %>%
        paste0('.zip')
    },
    function(file) {
      od <- getwd()
      setwd(tempdir())
      filen <- make.names(input$Title)
      sql <- gsub("\n","\n# ",sqlInterpolate(ANSI(),input$sql))
      header <- sprintf(
"---
title: %s
author: %s
output: 
  html_notebook: 
    code_folding: hide
theme: paper
---

```{r setup,include=F}
knitr::opts_chunk$set(echo=TRUE,warning=FALSE)
library(tidyverse)
x <- readRDS('%s.rds')
# Alternately, you can use a sql connection to load the data rather than the rds
# library(DBI)
# conn <- dbConnect(
#   RPostgreSQL::PostgreSQL(),host='localhost',
#   user='postgres',password='pg'
# )
# x <- dbGetQuery(conn, \"%s\")
# dbDisconnect(conn)
```
",input$Title,input$Author,filen,sql
      )
      body <- sapply(unique(inserted),function(x) callModule(dynReport,x)) %>%
        paste(collapse="\\pagebreak\n")
      writeLines(paste0(header,body),paste0(filen,".Rmd"))
      saveRDS(db(),paste0(filen,".rds"))
      render(paste0(filen,".Rmd"), "pdf_document")
      render(paste0(filen,".Rmd"), "html_notebook")
      zip(zipfile=file, files=paste0(filen,c(".Rmd",".rds",".pdf",".nb.html")))
      setwd(od) #go back to the original directory
    },
    contentType = "application/zip"
  )

  #bookmarking/restore
  setBookmarkExclude(c(
    "modalq","modalcx","modalcy","modaldx","modaldv","modaldy",
    "modalbar","modalbox","modalline","modalline2",
    "filter.voltage","filter.current","filter.amphours",
    "filter.totaltime","filter.steptime","filter.step",
    "qBtn","rBtn"
  ))
  onBookmark(function(state) {
    i <- paste(unique(inserted),collapse="") #compact everything up
    state$values$i <- unique(i)
  })
  onBookmarked(function(url) {
    p <- parseQueryString(url)
    #list of box contents for boxes that do not occur in string 'inserted'
    removed <- names(p)[grepl("-",names(p))&!grepl(paste(inserted,collapse="|"),names(p))] %>%
      paste(collapse="|")
    if(nchar(removed)) {
      #regex out the removed strings and correct button counters
      url <- gsub(paste0("(",removed,").+?&"),"",url)
      maxinserted <- inserted %>% parse_number() %>% max()
      url <- sub("&iBtn=[[:digit:]]+",paste0("&iBtn=",maxinserted),url)
    }
    #no point in keeping modal data
    url <- gsub("&b[[:digit:]]+-modalBtn=[[:digit:]]+","",url)
    updateQueryString(url)
    showModal(urlModal(
      url,"Bookmarked report link",
      "This link stores the current state of this application."
    ))
  })
  onRestore(function(state) {
    #correct sql values, if necessary
    if(state$input$sql!=defaultsql) {
      sql <- jsQuote(state$input$sql)
      runjs(paste0("ace.edit('sql').setValue(",sql,",1);"))
      sqlInvalidate <<- TRUE
    }
    #decompress inserted list
    i <- strsplit(state$values$i,"b") %>% unlist() %>% paste0("b",.)
    inserted <<- unique(i[i!="b"]) #remove empties    
    #reinsert the divs
    lapply(inserted,function(ID) {
      insertUI(
        selector = '#report',
        ui = tags$div(boxUI(ID),id = ID)
      )
      callModule(dynPlot,ID,ID=ID)
      runjs(aceBuilder(ID,state$input[[paste0(ID,"-e")]]))
    })
  })
}

shinyApp(ui, server, enableBookmarking = "url")