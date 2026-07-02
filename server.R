library(shiny)
library(abind)
library(DT)
#library(readr)
#library(httr)
#library(jsonlite)
#library(diffobj)
#library(diffr)
#library(xml2)
#library(dplyr)
#library(shinycssloaders)
#library(shinyjs)
#library(DT)
#library(tools)
is.system<-Sys.getenv("SYS")
target<-is.system
countries<-read.csv("country_data.csv")
q<-"https://dataverse.no/api/access/datafile/:persistentId?persistentId=doi:10.18710/VM2K4O/GEVNMF"
#countries<-read.csv("country_data.csv")
src.zip<-paste0(Sys.getenv("HKW_TOP"),"/AVL/2025/textur/dataverse_files/gpt-stories.zip")
src.doi<-"https://dataverse.no/api/access/datafile/:persistentId?persistentId=doi:10.18710/VM2K4O/GEVNMF"
sbctempdir<-tempdir()
local<-F
staging<-T
# if(dracorframetarget=="")
#   dracorframetarget<-"https://dracor.dh-index.org"
# Load defaults when app starts
# observe({
#   # Load default speaker names from database
#   #sp_default <- load_default_speakers()
#   defaults<-load_defaults(1)
#   # Update the text input with the loaded default
#   updateTextInput(session, "speaker", value = defaults$speaker)
#   updateTextInput(session, "h1", value = defaults$h1)
#   updateTextInput(session, "h2", value = defaults$h2)
#   updateTextInput(session, "cast", value = defaults$cast)
#   
# })
#pdiv<-get.pdfs()
source("functions.R")
# Define server logic
function(input, output, session) {
  # Reactive values to store intermediate states
  rv <- reactiveValues(
    cc = "DE",
    country = "Germany",
    selected_id = 1,
    heads = c("head 1","head 2"),
    story = "keine deutsche story...",
    df = NULL,
    zip = src.zip,
    doi = src.doi,
    countries = c("Germany","Andorra"),
    stories = NULL,
    local = F,
    cp = NULL,
    titles = NULL,
    mdns = NULL
  )
  # cc<-"DE"
  # country<-"Germany"
  # selected_id <- 1
  # Update dropdown choices when dataframe changes
  observe({
    s<-ifelse(rv$local,"- locally -","- online -")
    showNotification(paste0("fetching corpus...: ",s), type = "message")
    
    output$apidoc <- renderUI({ div(tags$pre("Processing...")) })  # Show a loading message
   # output$proutput <- renderText("processing...\n")
    
    # Fetch the transcript
    s<-rv$doi
    if(!is.null(s)){
#    rv$local <- F
 #   rv$doi <- transcript
    div<-c("country and ID processing...")
        output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(div, collapse = "\n"))
      )
    })

    tlist <- get.cdirs(s,rv$local)  # Store the transcript in reactiveValues
    cat("--- cdf --- \n")
    print(dim(tlist$cdf))
    cdf<-tlist$cdf
    rv$df <- cdf
    
  
    
    # Update the UI with the fetched transcript
    # output$apidoc <- renderUI({
    #   div(
    #     style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
    #     tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
    #              paste(rv$t1, collapse = "\n"))
    #   )
    # })
    showNotification("corpus fetched...", type = "message")
     div<-c("finished processing corpus with",length(unique(rv$df$Story_ID)),"stories\n")
        output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(div, collapse = "\n"))
      )
    })
  }
    #    output$proutput <- renderText("transcript fetched...\n")
    
  })
  
  observe({
       output$pdfdiv<-renderUI({
      pdiv<-get.pdfs()
         pdif<-pdiv$div
      print(pdiv)
      pdiv<-paste0("<h4>query downloads yet available...</h4>",pdiv)
      HTML(pdiv)
       })
      })
  observeEvent(input$refresh, {
     #if (pdiv!="") {
        output$pdfdiv<-renderUI({
      pdiv1<-get.pdfs()
          pdiv<-pdiv1$div
      print(pdiv)
      pdiv<-paste0("<h4>query downloads yet available...</h4>",pdiv)
      md<- knitr::kable(pdiv1$td)
      html <- markdown::markdownToHTML(md, fragment.only = TRUE)
    HTML(html)
#      HTML(pdiv)
     
    #}
        })
  })
  observeEvent(input$tabset, {
    if (input$tabset == "downloads") {
        output$pdfdiv<-renderUI({
      pdiv<-get.pdfs()
          pdiv<-pdiv$div
      print(pdiv)
      pdiv<-paste0("<h4>query downloads yet available...</h4>",pdiv)
      HTML(pdiv)
        })
  }
    })
    observeEvent(input$tabset, {
    if (input$tabset == "downloads-table") {
       pdiv<-get.pdfs()
          pdiv<-pdiv$td
     
        output$pdfs<-renderDT({
    datatable(
          pdiv,
      escape = FALSE   # render HTML
    )
  })
}
    })
  
  # Update name input when selection changes
  observeEvent(input$id_select, {
    if (!is.null(input$id_select)) {
      id <- input$id_select
#      selected_df <- countries[countries$alpha.2==id,]
      updateTextInput(session, "cast", value = rv$country)
      rv$selected_id <- id
     # rv$story <- rv$df[id]
    } else {
      updateTextInput(session, "cast", value = "empty")
      #rv$selected_id <- NULL
    }
  })
    observeEvent(input$load_id, {
    req(input$id_select)
    req(input$co_select)
    id<-input$id_select
    co<-input$co_select
div<-c("processing title, open <processed> when finished")
        output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(div, collapse = "\n"))
      )
    })
    ###########################
    # defaults<-load_defaults(id)
    ###########################
    # print(co)
    
    id<-which(id==rv$heads)
    id<-as.double(id)
    # print(id)
    cat("observe load story...\n")
    cdf<-rv$df
    print(dim(cdf))
    subset<-data.frame(cdf[cdf$Country_Name==co,])
    story<-as.character(subset$Story[id])
    print(dim(subset))
    # print(colnames(subset))
    # print(head(subset,1))
    cat("--- story:",co,id,"\n\n",substr(story,1,100),"\n")

    mdh<-readLines("mdy.md")
    
    title<-gsub("[*]","",story[1])
    # storyhead<-c("---",
    # paste0('title: "',title,'"'),
    #   "---")
    # story.out<-c(storyhead,story)
    story.out<-story
    repl<-function(mdh){
    mdh<-gsub("#mdns#",mdns,mdh)
    mdh<-gsub("#countrycode#",paste0(co," - ",id),mdh)
    }
    ######################
   mdt<-tempfile("x.md")
    # mdt<-tempfile("x.qmd")
    writeLines(story.out,mdt)
    writeLines(story,mdt)
    mds<-readLines(mdt)
    #qmd<-".qmd"
    mdns<-paste0("gptstories_",co,"-",id)
    # print(mdns)
    ### qmd

    #mdh<-mdq
    ###########################
    mdh<-repl(mdh)
    mdw<-paste0("www/",mdns,".md")
    md<-c(mdh,story)
    
 if(!staging){
      
    mdh<-readLines("visite/_story-template.qmd")
    writeLines(story.out,"www/_story.md")
    mdh<-repl(mdh)
      mdw<-paste0("www/",mdns,".qmd")
    md<-mdh
    writeLines(md,mdw)
    quarto::quarto_render(mdw)
     htmns<-paste0("www/",mdns,".html")
    htmns<-"www/gtpstories_Andorra-1.html"
 html<-readLines(htmns)
 #  html <- markdown::markdownToHTML(md, fragment.only = TRUE)
    
 }
   # md<-mdh
   if(staging){ 
    writeLines(md,mdw)
    ######################
    rmarkdown::render(mdw)
    mdp<-paste0(md,collapse="\n")
   html <- markdown::markdownToHTML(mdp, fragment.only = TRUE)
    }
    ###
    #library(quarto)
   # ??quarto::render  
      
    rv$mdns<-mdns
    div<-c("processed title, open <processed> to view text")
        output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(div, collapse = "\n"))
      )
    })
    output$processed <- renderUI({
     
    #HTML(sprintf('<div><a href="%s" target="_blank">download text as pdf</a></div>',mdns))
    HTML(html)
    #  div(
    #     style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
    #     p(style = "font-family: monospace;",story))
    })
    
    })
    observeEvent(input$load_co, {
    #req(input$id_select)
    req(input$co_select)
    #id<-input$id_select
    co<-input$co_select
    ###########################
    # defaults<-load_defaults(id)
    ###########################
    # print(co)
    # id<-as.double(id)
    # print(id)
    cat("observe load country...\n")
    cdf<-rv$df
    print(dim(cdf))
    subset<-data.frame(cdf[cdf$Country_Name==co,])
    #story<-as.character(subset$Story[id])
    stories<-subset$Story
     heads<-lapply(stories,function(x){
      h1<-unlist(strsplit(x,"\n"))[1]
    })
    heads<-unlist(heads)

    heads<-gsub("^.*Title: ","",heads)
    heads<-paste(1:length(heads),heads,sep=" - ")
    rv$heads <- heads
        updatePickerInput(
      session,
      "id_select",
      choices = heads
    )

 
  })
  
  
  
  output$md_html <- renderUI({
    md_file <- "about-md.md"
    html <- markdown::markdownToHTML(md_file, fragment.only = TRUE)
    HTML(html)
  })

  observeEvent(input$submit.doc, {
    s<-ifelse(rv$local,"- locally -","- online -")
    showNotification(paste0("fetching corpus...: ",s), type = "message")
    
    output$apidoc <- renderUI({ div(tags$pre("Processing...")) })  # Show a loading message
    s<-rv$doi
    if(!is.null(s)){
    div<-c("country and ID processing...")
        output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(div, collapse = "\n"))
      )
    })

    tlist <- get.cdirs(s,rv$local)  # Store the transcript in reactiveValues
    cat("--- cdf --- \n")
    print(dim(tlist$cdf))
    cdf<-tlist$cdf
    rv$df <- cdf
    
    showNotification("corpus fetched...", type = "message")
     div<-c("finished processing corpus with",length(unique(rv$df$Story_ID)),"stories\n")
        output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(div, collapse = "\n"))
      )
    })
  }
    #    output$proutput <- renderText("transcript fetched...\n")
    
  })
  
 
}