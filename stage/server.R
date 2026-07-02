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
# takes 9.40min to install packages on silver
#source("ezd2tei.R")
#source("functions.R")
#load("default-values.RData")
# SNC:
# 15383.1.lapsi
#sp.default<-"Iwanette,Golowin,Wolsey,Stormond,Bender"
# transcript<-"iwanette"
# output_file_s_www<-"r-tempxmlout.xml"
# output_file<-paste0("www/",output_file_s_www)
# output_file<-tempfile("tempxmlout.xml")
# output_file_ezd<-"www/ezdmarkup.txt"
# output_file_ezd<-tempfile("ezdmarkup.txt")
# output_file_pb<-"www/r-tempxmlout_pb.xml"
# output_dracor<-paste0(Sys.getenv("GIT_TOP"),"/ulysses/work/dracor")
is.system<-Sys.getenv("SYS")
target<-is.system
# dracorapitarget<-Sys.getenv("dracorapitarget")
# dracorframetarget<-Sys.getenv("dracorframetarget")
countries<-read.csv("country_data.csv")
q<-"https://dataverse.no/api/access/datafile/:persistentId?persistentId=doi:10.18710/VM2K4O/GEVNMF"
#countries<-read.csv("country_data.csv")
src.zip<-paste0(Sys.getenv("HKW_TOP"),"/AVL/2025/textur/dataverse_files/gpt-stories.zip")
src.doi<-"https://dataverse.no/api/access/datafile/:persistentId?persistentId=doi:10.18710/VM2K4O/GEVNMF"
sbctempdir<-tempdir()
local<-F

get.pdfs<-function(){
  fs<-list.files("www",pattern=".pdf",full.names=T)
  cat("--- new files fetch ---\n")
  li<-lapply(fs,function(x){
    print(x)
    n<-basename(x)
    d<-paste0('<li><a href="',n,'" target="_blank">',n,"</a></li>")
  })
ul<-paste0("<ul>",paste0(unlist(li),collapse="\n"),"</ul>")
cat(ul)
div<-paste0("<div>",ul,"</div>")
td<-lapply(fs,function(x){
    print(x)
    n<-basename(x)
  d<-data.frame(link=paste0('<a href="',n,'" target="_blank">',n,'</a>'))
  # d<-paste0('<li><a href="',n,'" target="_blank">',n,"</a></li>")
    # d<-data.frame(link=paste0('[',n,'](',n,')'))

})
tdf<-data.frame(abind(td,along=1))
  return(list(div=div,td=tdf))
}

get.cdirs<-function(cp,local){
  print("--- get.cdirs() ---")
### tempfile to store zip
sbctemp<-tempfile("SBCtemp.zip")
sbctempdir<-tempdir()
f<-list.files(".")
m<-grepl("^SBCtemp.zip$",f)
dload<-ifelse(sum(m)>0,F,T)
m2<-grepl("cdf.RData",f)
m2<-sum(m2>0)
if(m2){
  cat("--- loading from saved cdf ---\n")
  load("cdf.RData")
  print(dim(cdf))
  print(colnames(cdf))
  return(list(cdf=cdf))
}
dload<-ifelse(sum(m)>0,F,T)
fc<-f[m]
ifelse(!local&dload,download.file(q,sbctemp),file.copy(fc,sbctemp))
file.copy(sbctemp,"SBCtemp.zip")
unzip(sbctemp,exdir = sbctempdir)
cat("--- top zip ---\n")
print(list.files(sbctempdir))
sbctrn<-paste0(sbctempdir,"/")
filestrn<-list.files(sbctrn)
cat("--- filestrn ---\n")

print(filestrn)
f<-list.dirs(paste0(sbctempdir,"/",filestrn[2]),full.names=T)
cat("--- country dirs ---\n")
f<-f[2:length(f)]
print(f)
cdf<-lapply(f,function(x){
  #read.csv()
    cat("--- lapply files --- :",x,":\n")
    csf<-list.files(paste0(x),full.names=T)
    print(csf)

    m<-grep("stories",basename(csf))
    print(csf[m])
    cdf<-read.csv(csf[m])
    #stories<-cdf$Story
})
library(abind)
cat("dim cdf:",dim(cdf),"\n")
cdf<-data.frame(abind(cdf,along=1))

save(cdf,file="cdf.RData")
return(list(cdflist=f,cdf=cdf))
}
get.stories<-function(country,cdf){
  print(country)
cc<-countries[countries$country_name==country]
cc<-cc[cc!=""]
cc<-cc[!is.na(cc)]
cat("--- extrcating... ---\n")
sbctemp<-tempfile("SBCtemp.zip")
# sbctempdir<-tempdir()
#download.file(q,sbctemp)
#ifelse(!local,download.file(q,sbctemp),file.copy(cp,sbctemp))
#unzip(sbctemp,exdir = sbctempdir)
#print(list.files(sbctempdir))
#sbctrn<-paste0(sbctempdir,"/")
#filestrn<-list.files(sbctrn)
#print(filestrn)
#f<-list.files(paste0(sbctempdir,"/",filestrn[2]))
f
cl<-f

print("---- codes ---")
print(cc)
sbctrn<-paste0(sbctempdir,"/")
filestrn<-list.files(sbctrn)
cat("--- folders---\n")
print(filestrn)

    csf<-list.files(paste0(sbctempdir))#,"/",filestrn[2],"/",cc),full.names=T)
    print(csf)

    m<-grep("stories",basename(csf))
   # print(csf[m])
    cdf<-read.csv(csf[m])
   # stories<-cdf$Story
}
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
     #      output$showsamples <- renderTable({
      # n<-input$bins
      # df<-get.sample(n,k6)
      # print(df)
      # cns<-c("paradigm_he","paradigm_lat","kwic","art","adj","noun")
      # df<-df[,colnames(df)%in%cns]
    
    
    
        #   country <- input$co_select
  #   cat("--- selected country: ",country,"\n")
  #   rv$country <- country
  #   #choices <- c("Create new..." = "new", choices)
  #   stories<-get.stories(country,rv$cp)
  #   # cc<-countries$country_name[countries$alpha.2==country]
  #   # csf<-list.files(paste0(sbctempdir,"/",filestrn[2],"/",cc),full.names=T)
  #   # csf
  #   # m<-grep("stories",basename(csf))

  #   # cdf<-read.csv(csf[m])
  #   # stories<-cdf$Story
  #   #rv$df <- stories
  #   #stories[1]
  #   # heads<-lapply(stories,function(x){
  #   #   h1<-unlist(strsplit(x,"\n"))[1]
  #   # })
  #   # heads<-unlist(heads)
  #   # rv$heads <- heads
  #   # cat("--- hedas ---\n")
  #   # print(heads)
  
  #   # updatePickerInput(
  #   #   session,
  #   #   "id_select",
  #   #   choices = heads
  #   # )}
  # })

  
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
    print(co)
    
    id<-which(id==rv$heads)
    id<-as.double(id)
    print(id)
    cat("observe load story...\n")
    cdf<-rv$df
    print(dim(cdf))
    subset<-data.frame(cdf[cdf$Country_Name==co,])
    story<-as.character(subset$Story[id])
    # stories<-subset$Story
    #  heads<-lapply(stories,function(x){
    #   h1<-unlist(strsplit(x,"\n"))[1]
    # })
    # heads<-unlist(heads)
    # rv$heads <- heads
    #     updatePickerInput(
    #   session,
    #   "id_select",
    #   choices = heads
    # )

    # cat("--- hedas ---\n")
    # print(heads)    
    print(dim(subset))
    print(colnames(subset))
    print(head(subset,1))
    cat("--- story:",co,id,"\n\n",substr(story,1,100),"\n")
    #print(substr(story,1,100))
    #  output$processed <- renderUI({
    #   div(
    #     style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
    #     p(style = "font-family: monospace;",story))
    # })

    mdh<-readLines("mdy.md")
    title<-gsub("[*]","",story[1])
    storyhead<-c("---",
    paste0('title: "',title,'"'),
      "---")
    story.out<-c(storyhead,story)
    story.out<-story
    mdq<-readLines("visite/_story-template.qmd")
    ######################
   mdt<-tempfile("x.md")
    # mdt<-tempfile("x.qmd")
    writeLines(story.out,mdt)
    writeLines(story.out,"www/_story.md")
    writeLines(story,mdt)
    mds<-readLines(mdt)
    qmd<-".qmd"
    mdns<-paste0("gtpstories_",co,"-",id)
    print(mdns)
    ### qmd
    #mdh<-mdq
    ###########################
    mdh<-gsub("#mdns#",mdns,mdh)
    mdh<-gsub("#countrycode#",paste0(co," - ",id),mdh)
 
    md<-c(mdh,story)
   # md<-mdh
    mdw<-paste0("www/",mdns,".qmd")
    mdw<-paste0("www/",mdns,".md")
    
    writeLines(md,mdw)
    ######################
    rmarkdown::render(mdw)
    ###
    #library(quarto)
   # ??quarto::render  
      
    #quarto::quarto_render(mdw)
    htmns<-paste0(mdns,".html")
    rv$mdns<-mdns
    mdp<-paste0(md,collapse="\n")
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
    html <- markdown::markdownToHTML(mdp, fragment.only = TRUE)
    #html<-readLines(htmns)
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
    print(co)
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


    # cat("--- hedas ---\n")
    # print(heads)    
    # print(dim(subset))
    # print(colnames(subset))
    # print(head(subset,1))
    # cat("--- story:",co,id,"\n\n",substr(story,1,100),"\n")
    # print(substr(story,1,100))
    # #  output$processed <- renderUI({
    # #   div(
    # #     style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
    # #     p(style = "font-family: monospace;",story))
    # # })
    # output$processed <- renderUI({
    # html <- markdown::markdownToHTML(story, fragment.only = TRUE)
    # HTML(html)
    # #  div(
    # #     style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
    # #     p(style = "font-family: monospace;",story))
    # })
    #  #story<-cdf[]
    ################################################################
   
  })
  
  # output$debug_output <- renderPrint({
  #   cat("Selected ID:", input$id_select, "\n")
  #   cat("Current country input:", input$co, "\n")
  #   # cat("Dataframe dimensions:", dim(rv$df), "\n")
  # })
  
  
  
  output$md_html <- renderUI({
    md_file <- "about-md.md"
    html <- markdown::markdownToHTML(md_file, fragment.only = TRUE)
    HTML(html)
  })
  # observeEvent(input$upload_tr,{
  #   showNotification("processing zip...", type = "message")
    
  #   file<-input$upload_tr
  #   # ext<-tools::file_ext(file$datapath)
  #   # req(file)
  #   # validate(need(ext=="txt","please upload a plain text file"))
  #   t4<-file$datapath
  #   rv$zip<-t4
  #   rv$local<-T
  #   src<-paste0(from,ifelse(local,"from local source","from DOI source"))
  #   showNotification(paste0("zip loaded... ",src), type = "message")
    
  #  # output$proutput <- renderText("transcript fetched...\n")
  # })

  observeEvent(input$submit.doc, {
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
  
  
    # writeLines(rv$t2,"www/ezdmarkup.txt")
    # Update the UI with the processed act headers
    # output$proutput <- renderText(paste("level 1/2 headers found:\n",paste(rv$heads, collapse = "\n"),collapse = "\n"))
    #output$proutput<-renderDT(rv$heads)
  
  
  #################################
  
    
  #   # writeLines(xml.t,paste0(output_dracor,"/dracortei.xml"))
  #   xml.test<-c("<p>testxmlrender</p>","<h1>head1</h1><p><stage>stages</stage>paragraph</p>")
  #   # xml.test<-list.files(".")
  #   # xml.str<-paste0("<div>",paste0(xml.test),"</div>")
  #   xml.str<-paste0(xml.t,collapse = "")
  #   #print("----- xmlstr ------")
  #   # print(xml.str)
  #   b64 <- jsonlite::base64_enc(charToRaw(xml.str))
  #   rv$b64<-b64
  #   rv$xmlout<-xml.t
    
  #   #  valid<-validate_tei(output_file,"dracor-scheme.rng") # not on M7, cant install jing
  #   #t2<-xml.t
  #   # print(valid$ok)
  #   #    output$proutput <- renderText("ezd > TEI processed...\n")
  #   message<-xml.f$message
  #   showNotification(message, type = "message")
  #   rv$xmlprocessed<-T
  #   output$processed <- renderUI({
  #     div(
  #       style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
  #       tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
  #                paste(xml.t, collapse = "\n"))
  #     )
  #   })
  #   output$xmlrendered <- renderUI({
  #     # div(id="xml",
  #     # style="width:100%; height:100%;",
  #     tags$iframe(
  #       src = paste0("data:application/xml;base64,", rv$b64),
  #       # src = paste0("data:text/html,", rv$xmlout),
        
  #       # src = "r-tempxmlout.xml",
  #       # src = output_file_s_www, # this wks.
  #       # src = output_file,
  #       style="width:100%; height:100vH; border:none;"
  #     )
  #   })
  # })
  
  
  # output$acts <- renderText(paste(rv$heads, collapse = "\n"))
  # output$apidoc <- renderUI({
  #   div(
  #     style = "height: 70vh; background: #f8f8f8; padding: 10px; color: #888;",
  #     "Output will appear here after processing."
  #   )
  # })
  # output$framePreview <- renderUI({
  #   tags$iframe(
  #     src = "http://localhost:8088/files/preview",
  #     style = "width: 100%; height: 300px; border: 1px solid #ddd;"
  #   )
  # })
  # observeEvent(input$tabset, {
  #   if (input$tabset == "dracor_preview") {
  #     push.dracor(rv$dracorapitarget,rv$xml.t,"teipreview","preview")
      
  #     shinyjs::addClass("iframe-navbar","showing")
  #     #shinyjs::toggle("iframe-navbar")
  #     # Insert the fullscreen iframe
  #     insertUI(
  #       selector = "body",
  #       where = "beforeEnd",
  #       ui = tags$iframe(
  #         id = "fullscreen-iframe",
  #         class = "fullscreen-iframe",
  #         src = paste0(dracorframetarget,"/teipreview/preview")
  #       )
  #     )
      
  #   }
  # })
  # observeEvent(input$`view-external`, {
  #   # Show the iframe navbar
  #  # push.dracor(rv$dracorapitarget,rv$xml.t,"test","preview")
    
  #   shinyjs::addClass("iframe-navbar","showing")
  #   #shinyjs::toggle("iframe-navbar")
  #   # Insert the fullscreen iframe
  #   insertUI(
  #     selector = "body",
  #     where = "beforeEnd",
  #     ui = tags$iframe(
  #       id = "fullscreen-iframe",
  #       class = "fullscreen-iframe",
  #       src = paste0(dracorframetarget,"/teipreview/preview")
  #     )
  #   )
  #   # shinyjs::addClass("iframe-navbar","showing")
  #   shinyjs::show("iframe-navbar")
    
    
  # })
  # observeEvent(input$flushdb, {
  #   # Show the iframe navbar
  #   xml.t<-"samplepreview.xml"
  #   push.dracor(rv$dracorapitarget,xml.t,"teipreview","preview")
  #   showNotification("processed file flushed from DB", type = "warning")
    
    
    
  # })
  
  # # Handle return to app
  # observeEvent(input$`back-to-app`, {
  #   # Remove the iframe
  #   removeUI("#fullscreen-iframe")
    
  #   # Hide the navbar
  #   shinyjs::hide("iframe-navbar")
    
    
  #   # Switch back to the first tab
  #   # updateTabsetPanel(session, "mainTabs", selected = "progress")
  # })
  # ### init dracor preview.xml to clean db from previous play
  # observe ({
  #   xml.t<-"samplepreview.xml"
  #   dracorapitarget<-Sys.getenv("dracorapitarget")
  #   # xml.f<-transform.ezd(rv$t3,output_file,meta,h1.first)
  #   # xml.t<-xml.f$xml
  #   # tryCatch({
  #   #   push.dracor(dracorapitarget,xml.t,"teipreview","preview")
  #   # },error = function(e){
  #   #   #    showNotification("sample pushdracor failed...", type = "message")
      
  #   #   return("pushdracor failed...")
  #   # })
  # })
  
}