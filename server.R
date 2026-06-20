library(shiny)
#library(readr)
#library(httr)
library(jsonlite)
#library(diffobj)
#library(diffr)
#library(xml2)
#library(dplyr)
library(shinycssloaders)
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
src.zip<-paste0(Sys.getenv("HKW_TOP"),"/Users/guhl/boxHKW/UNIhkw/21S/DH/local/AVL/2025/textur/dataverse_files/gpt-stories.zip")
src.doi<-"https://dataverse.no/api/access/datafile/:persistentId?persistentId=doi:10.18710/VM2K4O/GEVNMF"
sbctempdir<-tempdir()
local<-F
get.cdirs<-function(cp,local){
  print("--- get.cdirs() ---")
### tempfile to store zip
sbctemp<-tempfile("SBCtemp.zip")
sbctempdir<-tempdir()
f<-list.files(".")
m<-grepl("SBCtemp.zip",f)
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
    print(csf[m])
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
  # observe({
  #   if (!is.null(rv$cp)) {
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
    print(substr(story,1,100))
    #  output$processed <- renderUI({
    #   div(
    #     style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
    #     p(style = "font-family: monospace;",story))
    # })
    mdh<-readLines("mdy.md")
    mdt<-tempfile("x.md")
    writeLines(story,mdt)
    mds<-readLines(mdt)
    mdns<-paste0("gtpstories_",co,"-",id)
    print(mdns)
    mdh<-gsub("#pdf#",mdns,mdh)

    md<-c(mdh,story)
    mdw<-paste0("www/",mdns)
    writeLines(md,mdw)
    rmarkdown::render(mdw)
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
    
    HTML(sprintf('<div><a href="%s">download text as pdf</a>',mdns))
    html <- markdown::markdownToHTML(mdp, fragment.only = TRUE)
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
                 paste(div,rv$df$country_, collapse = "\n"))
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
  
  # Observe the submit button for processing act headers
  observeEvent(input$z.local, {
    rv$zip <- input$z.local
    rv$local <- T
  })
  observeEvent(input$submit.h, {
    showNotification("processing headers...", type = "message")
    #    output$proutput <- renderText("processing headers...\n")
    
    vario.1 <- input$h1
    vario.2<-input$h2
    print("getting H1")
    print(vario.1)
    print(vario.2)
    # t <- get.heads.s(rv$t1, vario.1,vario.2)  # Use the transcript stored in reactiveValues
    t <- get.heads.4(rv$t1, vario.1,vario.2)  # Use the transcript stored in reactiveValues
    rv$t2 <- t$text  # Store the updated text in reactiveValues
    rv$heads <- t$vario  # Store the act headers in reactiveValues
    # heads<-data.frame(found=1:length(rv$heads),head=rv$heads)
    rv$h1.sf<-vario.1
    rv$h2.sf<-vario.2
    rv$h1.first<-t$h1.first
    # heads$h1<-rv$h1.sf
    # heads$h2=rv$h2.sf
    print(rv$heads)
    # if(length(rv$heads)>0){
    if(!is.null(t$h1.first)){
      rv$h1.set<-TRUE
      rv$ezd<-rv$t2
    }
    # writeLines(rv$t2,"www/ezdmarkup.txt")
    # Update the UI with the processed act headers
    # output$proutput <- renderText(paste("level 1/2 headers found:\n",paste(rv$heads, collapse = "\n"),collapse = "\n"))
    #output$proutput<-renderDT(rv$heads)
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t2, collapse = "\n"))
      )
    })
    output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$heads, collapse = "\n"))
      )
    })
    showNotification("headers processed...", type = "message")
    
    # output$pr_progress<-renderDT(
    #   iris
    #   )
  })
  observeEvent(input$guess.sp, {
    print("guess speakers")
    showNotification("guessing speakers...", type = "message")
    
    sp.guess<-guess_speaker(rv$t2,input$cast)
    rv$sp.guess<-sp.guess
    output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste("SPEAKERS guessed:\n",paste(rv$sp.guess,collapse = "\n")))
      )
    })
   # output$proutput<- renderText(paste("SPEAKERS guessed:\n",paste(sp.guess,collapse = "\n")))
    updateTextInput(session, "speaker", value = paste0(sp.guess,collapse = ","))
  })
  #################################
  
  observeEvent(input$submit.sp, {
    vario <- input$speaker
    rswitch<-input$rswitch
    req(rv$h1.set)
    req(rv$h1.first,!is.null(rv$h1.first))
    h1.first<-rv$h1.first
    showNotification("processing speakers...", type = "message")
    copyrighted<-rv$copyrighted
    print("getting speaker")
    #t <- get.speakers(t3, vario)  # Use the transcript stored in reactiveValues
    print(rv$cast)
    # t <- get.speakers(rv$t2, vario)  # Use the transcript stored in reactiveValues
    t4 <- get.castlist(rv$t2,rv$cast)
    if(sum(unlist(grepl("^ERR:", t4$cast)))>0){
      showNotification("ERROR: no castlist provided, no speaker processed...", type = "message")
      return()
    }
    t4<-t4$lines
    
    print("got cast...")
    t5<-get.front(t4)
    print("got front...")
    t6 <- get.speakers(t5, vario,rswitch,copyrighted)# Use the transcript stored in reactiveValues
    t2 <- t6  # Store the updated text in reactiveValues
    #t2<-t4
    print("got speakers...")
    rv$speaker <- append(rv$speaker,t2$vario,after = length(rv$vario)) # Store the act headers in reactiveValues
    rv$speaker<-unique(rv$speaker)
    rv$speaker<-rv$speaker[!is.na(rv$speaker)]
    rv$speaker.crit<-append(rv$speaker.crit,t2$eval,after=length(rv$speaker.crit))
    rv$speaker.crit<-rv$speaker.crit[!is.na(rv$speaker.crit)]
    rv$sp.sf<-input$speaker
    # Update the UI with the processed act headers
    # output$acts <- renderText(paste(rv$heads, collapse = "\n"))
    ### remove linebreaks
    sp6<-gsub("%front%","",t2$text)
    # sp6<-gsub("%hnl%","",t2$text)
    # sp6<-t2$text
    t3<-clean.t(sp6,F,rv$repl,h1.first)
    t3<-gsub("%hnl%","",t3)
    t3<-gsub("^\\((.+)\\)$","$\\1",t3)
    print("clean.t F in submit.sp..")
    # t3<-get.front(t3)
    rv$t3<-t3
    rv$ezd<-t3
    # writeLines(rv$t3,"www/ezdmarkup.txt")
    
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(rv$t3, collapse = "\n"))
      )
    })
    output$pr_progress<-renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste("SPEAKERS processed. You can now create the XML.\n")
                       # paste(rv$speaker,collapse = "\n"),
                       # "\ncritical lines:\n",paste(rv$speaker.crit,collapse = "\n")))
      ))
    })
    showNotification("speaker processed...", type = "message")
    
    # output$proutput<- renderText(paste("SPEAKERS found:\n",paste(rv$speaker,collapse = "\n"),
    #                                    "\ncritical lines:\n",paste(rv$speaker.crit,collapse = "\n")))
  })
  observeEvent(input$submit.xml, {
    author<-rv$author
    title<-rv$title
    subtitle<-rv$subtitle
    h1.first<-rv$h1.first
    dracorapitarget<-Sys.getenv("dracorapitarget")
    meta<-list(author=author,title=title,subtitle=subtitle)
    #    output$proutput <- renderText("processing ezd > TEI...\n")
    showNotification("transform ezd > TEI...", type = "message")
    #   output_text <- capture.output({
    #     print("Starting process...")
    #     Sys.sleep(1)
    #     print("Step 1 complete")
    #     Sys.sleep(1)
    #     print("Step 2 complete")
    #     xml.t<-transform.ezd(rv$t3,output_file,meta)
    #     print("finished...")
    #   })
    #   
    #   # Update the reactive value
    #   console_text(paste(output_text, collapse = "\n"))
    # 
    # 
    # output$pr_progress <- renderText({
    #   console_text()
    # })
    xml.f<-transform.ezd(rv$t3,output_file,meta,h1.first)
    xml.t<-xml.f$xml
    rv$xml.t<-xml.t
    rv$dracorapitarget<-dracorapitarget
    # push.dracor(dracorapitarget,xml.t,"files","preview")
    
    # writeLines(xml.t,paste0(output_dracor,"/dracortei.xml"))
    xml.test<-c("<p>testxmlrender</p>","<h1>head1</h1><p><stage>stages</stage>paragraph</p>")
    # xml.test<-list.files(".")
    # xml.str<-paste0("<div>",paste0(xml.test),"</div>")
    xml.str<-paste0(xml.t,collapse = "")
    #print("----- xmlstr ------")
    # print(xml.str)
    b64 <- jsonlite::base64_enc(charToRaw(xml.str))
    rv$b64<-b64
    rv$xmlout<-xml.t
    
    #  valid<-validate_tei(output_file,"dracor-scheme.rng") # not on M7, cant install jing
    #t2<-xml.t
    # print(valid$ok)
    #    output$proutput <- renderText("ezd > TEI processed...\n")
    message<-xml.f$message
    showNotification(message, type = "message")
    rv$xmlprocessed<-T
    output$processed <- renderUI({
      div(
        style = "height: 70vh; overflow-y: auto; background: #f8f8f8; padding: 10px;",
        tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-family: monospace;",
                 paste(xml.t, collapse = "\n"))
      )
    })
    output$xmlrendered <- renderUI({
      # div(id="xml",
      # style="width:100%; height:100%;",
      tags$iframe(
        src = paste0("data:application/xml;base64,", rv$b64),
        # src = paste0("data:text/html,", rv$xmlout),
        
        # src = "r-tempxmlout.xml",
        # src = output_file_s_www, # this wks.
        # src = output_file,
        style="width:100%; height:100vH; border:none;"
      )
    })
  })
  
  observeEvent(input$compare, {
    req(rv$xmlprocessed,rv$xmlprocessed==TRUE)
    ifelse(rv$t1=="%ezd%",text1<-rv$t3,
           text1 <- rv$t1)
    #print(text1)
    text1<-gsub("^[ ]{1,}","",text1)
    text1<-text1[text1!=""]
    #text2 <- paste0(rv$t3,collapse = "<nl>")
    doc<-read_xml(output_file)
    texts <- xml_text(xml_find_all(doc, "//text()"))
    
    text2 <- texts
    tempapi<-tempfile("api.txt")
    writeLines(text1,tempapi)
    tempproc<-tempfile("proc.txt")
    writeLines(text2,tempproc)
    # Split into lines for better diff display
    # lines1 <- unlist(strsplit(text1, "\n"))
    # lines2 <- unlist(strsplit(text2, "\n"))
    # 
    #?    renderDiffr
    tryCatch(({
      output$diff_output <- renderDiffr({
        # input$compare
        #text1 <- rv$t1
        #text2 <- rv$t3
        # tempapi<-tempfile("api.txt")
        # writeLines(rv$t1,tempapi)
        # tempproc<-tempfile("proc.txt")
        # writeLines(rv$t3,tempproc)
        # div(id="diff",style="width:100%;height:100vH;",
        isolate({
          diffr(
            file1 = tempapi,
            file2 = tempproc,
            before = "uploaded",
            after = "processed",
            contextSize = 3,
            wordWrap = TRUE
          )
        })
        # )
      })
    }))
  })
  output$acts <- renderText(paste(rv$heads, collapse = "\n"))
  output$apidoc <- renderUI({
    div(
      style = "height: 70vh; background: #f8f8f8; padding: 10px; color: #888;",
      "Output will appear here after processing."
    )
  })
  # output$framePreview <- renderUI({
  #   tags$iframe(
  #     src = "http://localhost:8088/files/preview",
  #     style = "width: 100%; height: 300px; border: 1px solid #ddd;"
  #   )
  # })
  observeEvent(input$tabset, {
    if (input$tabset == "dracor_preview") {
      push.dracor(rv$dracorapitarget,rv$xml.t,"teipreview","preview")
      
      shinyjs::addClass("iframe-navbar","showing")
      #shinyjs::toggle("iframe-navbar")
      # Insert the fullscreen iframe
      insertUI(
        selector = "body",
        where = "beforeEnd",
        ui = tags$iframe(
          id = "fullscreen-iframe",
          class = "fullscreen-iframe",
          src = paste0(dracorframetarget,"/teipreview/preview")
        )
      )
      
    }
  })
  observeEvent(input$`view-external`, {
    # Show the iframe navbar
   # push.dracor(rv$dracorapitarget,rv$xml.t,"test","preview")
    
    shinyjs::addClass("iframe-navbar","showing")
    #shinyjs::toggle("iframe-navbar")
    # Insert the fullscreen iframe
    insertUI(
      selector = "body",
      where = "beforeEnd",
      ui = tags$iframe(
        id = "fullscreen-iframe",
        class = "fullscreen-iframe",
        src = paste0(dracorframetarget,"/teipreview/preview")
      )
    )
    # shinyjs::addClass("iframe-navbar","showing")
    shinyjs::show("iframe-navbar")
    
    
  })
  observeEvent(input$flushdb, {
    # Show the iframe navbar
    xml.t<-"samplepreview.xml"
    push.dracor(rv$dracorapitarget,xml.t,"teipreview","preview")
    showNotification("processed file flushed from DB", type = "warning")
    
    
    
  })
  
  # Handle return to app
  observeEvent(input$`back-to-app`, {
    # Remove the iframe
    removeUI("#fullscreen-iframe")
    
    # Hide the navbar
    shinyjs::hide("iframe-navbar")
    
    
    # Switch back to the first tab
    # updateTabsetPanel(session, "mainTabs", selected = "progress")
  })
  ### init dracor preview.xml to clean db from previous play
  observe ({
    xml.t<-"samplepreview.xml"
    dracorapitarget<-Sys.getenv("dracorapitarget")
    # xml.f<-transform.ezd(rv$t3,output_file,meta,h1.first)
    # xml.t<-xml.f$xml
    # tryCatch({
    #   push.dracor(dracorapitarget,xml.t,"teipreview","preview")
    # },error = function(e){
    #   #    showNotification("sample pushdracor failed...", type = "message")
      
    #   return("pushdracor failed...")
    # })
  })
  
}