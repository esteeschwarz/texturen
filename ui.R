library(shiny)
#library(diffr)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(DT)
### this script is templated from another (https://github.com/esteeschwarz/dracorTEI) i.e. still messy with deprecated stuff.
version<-"SNC:16255.v0.0.5"
status<-"wks."
#error<-"cannot mtfrm"
countries<-read.csv("country_data.csv")
src.zip<-paste0(Sys.getenv("HKW_TOP"),"/Users/guhl/boxHKW/UNIhkw/21S/DH/local/AVL/2025/textur/dataverse_files/gpt-stories.zip")
src.doi<-"https://dataverse.no/api/access/datafile/:persistentId?persistentId=doi:10.18710/VM2K4O/GEVNMF"



c.all<-countries$country_name
print(c.all)
#c.all<-c.all[1:20]
#css<-readtext("render.css")$text
# Define UI for application
fluidPage(
  useShinyjs(),  # Enable shinyjs for UI manipulation
  
  tags$style(HTML("
    .scrollable-sidebar {
      height: 80vh; /* Set the height of the sidebar */
      overflow-y: auto; /* Enable vertical scrolling */
      overflow-x: hidden; /* Disable horizontal scrolling */
      padding-right: 10px; /* Add some padding for better appearance */
    }
  ")),
  tags$head(
    #tags$head(
      # Dynamically insert dependencies
      uiOutput("dynamic_head"),
    tags$style(HTML('
    .fullscreen-iframe {
        position: fixed;
        display: inline;
        top: 50px;
        left: 0;
        width: 100vw;
        height: 95vh;
        z-index: 10000;
        background: white;
        border: none;
      }
      .iframe-navbar {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 50px;
        background: #f8f9fa;
        padding: 10px 20px;
        z-index: 10000;
        border-bottom: 1px solid #dee2e6;
        display: none;
        justify-content: space-between;
        align-items: center;
      }
      .hidden {
        display: none;
      }
      .showing {
        display: flex;
      }
      #main-content {
        transition: all 0.3s ease;
      }
      .blurred {
        filter: blur(5px);
        pointer-events: none;
      }
      .diff-container { 
        border: 1px solid #ddd; 
        border-radius: 5px; 
        padding: 10px; 
        margin: 10px 0; 
        background: #f8f9fa;
        overflow-x: auto;
        width:100%;
        height:100vH;
      }
      .diff-header {
        background: #e9ecef;
        padding: 10px;
        border-radius: 3px;
        margin-bottom: 10px;
      }
      .ace_editor {
        border: 1px solid #ddd;
        border-radius: 4px;
      }
      #processed{
      margin: 10%;}
    ')),
  # tags$style(HTML(css)),
  div(
    id = "iframe-navbar",
    #   class = "iframe-navbar hidden",
    class = "iframe-navbar",
    h4("External Content Viewer", style = "margin: 0;"),
    actionButton("back-to-app", "Return to App", class = "btn-primary btn-sm")
  ),
  # Application title
  titlePanel("gpt stories"),
  
  # Main layout
  sidebarLayout(
    sidebarPanel(
      class = "scrollable-sidebar",  # Apply the custom CSS class
      helpText(version),
      h4("CONFIGURATION"),
      helpText("get corpus..."),
      textInput("z.doi","corpus from DOI source",src.doi),
      #fileInput("z.local","upload local corpus zip",accept = ".zip",buttonLabel = "browse..."),
      actionButton("submit.doc","load corpus"),
      # pickerInput("copy",label = "copyrighted source?",selected = TRUE,
      #             choices = c(TRUE,FALSE)),
      # fileInput("upload_ezd","upload ezd marked-up transcript",accept = ".txt",buttonLabel = "browse..."),
      # fileInput("upload_repl","upload replacements",accept = ".csv",buttonLabel = "browse..."),
      # helpText("set title and author"),
      # textInput("title","Title",""),
      # textInput("subtitle","SubTitle, if vorhanden",""),
      # textInput("author","Author",""),
      # textInput("cast","castlist declaration:","Personen."),
      # helpText("set body begin and act definitions"),
      # textInput("h1","act header declarations:","Act|Akt|Handlung|.ufzug"),
      # #actionButton("submit.h1","apply act definitions"),
      # textInput("h2","scene header declarations:","Scene|Szene|.uftritt"),
      # actionButton("submit.h","apply act|scene definitions"),
      
      helpText('the corpus is fetched once from the DOI source preconfigured...'),
     # helpText('we have found the following acts declarations:'),
      #verbatimTextOutput("acts"),
     # actionButton("sumbit.keep.act","use act definitions"),
    #   helpText("declare speaker"),
    #  actionButton("guess.sp","guess speakers"),
    #   textInput(
    #     "speaker",
    #     "speaker names:",
    #     ""
    #   ),
     
    #  helpText("Enter speaker names separated by commas, then click the button to process them."),
    #  switchInput("rswitch","regex",value = FALSE,"ON","OFF"),
    #   actionButton("submit.sp", "Process Names"),
    #  actionButton("compare", "compare processed", class = "btn-primary", icon = icon("code-compare")),
     # downloadButton("downloadEZD","downdload ezd-markup text"),
     
      hr(),
     pickerInput(
       "co_select",
       "Select Country:",
       choices = c.all,
       options = list(
         `live-search` = TRUE,
         `actions-box` = TRUE
       )
     ),
     pickerInput(
       "id_select",
       "Select Story:",
       choices = 1:50,
       options = list(
         `live-search` = TRUE,
         `actions-box` = TRUE
       )
     ),
  #  textInput("id.defaults.save","ID to save settings"),
    actionButton("load_co", "Load selected country", class = "btn-primary"),

    actionButton("load_id", "Load selected title", class = "btn-primary"),
    # actionButton("save_btn", "Save to Selected ID", class = "btn-primary"),
    # actionButton("new_btn", "Create New ID", class = "btn-success"),
   # actionButton("defaults.save","save settings"),
  #  textInput("id.defaults.load","load settings from ID"),
   # actionButton("defaults.load","load settings"),
    #actionButton("submit.xml", "create XML"),
    #downloadButton("downloadXML","downdload .xml"),
    helpText("select a country and a text by title from list and switch to <processed> tab.")
  ),
  mainPanel(
   # verbatimTextOutput("proutput"),
    tabsetPanel(id="tabset",
      
      tabPanel("progress",
      h4("processing"),
     # verbatimTextOutput("pr_progress")
     #withSpinner(verbatimTextOutput("spinner")),
     withSpinner(uiOutput("pr_progress")
      )),
      # tabPanel("raw",
      #          h4("raw text"),
               
              #  uiOutput("apidoc")),
      tabPanel("processed",
              h4("output"),
              
      withSpinner(uiOutput("processed"))
      ),
      # tabPanel("downloads",
      #         h4("available pdf downloads"),
      #         actionButton("refresh", "refresh pdfs", class = "btn-primary"),
      #         htmlOutput("pdfdiv")


              
    
      # ),
      tabPanel("downloads-table",
        # tableOutput("pdfs")
          DTOutput("pdfs")

),
    #   tabPanel("render",h4("rendered xml view"),
    #            uiOutput("xmlrendered")
    #   ),
    #   tabPanel("diff",
    #   div(class = "diff-container",
    #       h4("diff compare"),
    #       withSpinner(diffrOutput("diff_output"))
    #   )),
    #  tabPanel(
    #    "dracor_preview",
    #   # h2("External Content Viewer"),
    #    #p("Click the button below to view external content in full-screen mode."),
    #    actionButton("view-external", "view dracor preview", class = "btn-success"),
    #   br(),br(),
    #    p("if your processed play is copyrighted and should not be available to download for others, click the flush DB button."),
    #    actionButton("flushdb", "flush preview file in DB", class = "btn-success"),
    #    verbatimTextOutput("flushnotice")
      
      # br(), br(),
       # div(
       #   id = "preview-container",
       #   style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
       #   
       #   h4("Content Preview"),
       #   htmlOutput("framePreview")
       # )
     
      tabPanel("about",
               htmlOutput("md_html")
      ),
      # tabPanel("helper",
      #          htmlOutput("nb")
      # )
      
      # tabPanel("render",h4("rendered view"),
      #          uiOutput("xmlrendered")
      #          )
    ))
  
  )
)
)
