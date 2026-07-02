#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

### functions
#setwd("texturen")
getwd()
q<-"https://dataverse.no/file.xhtml?persistentId=doi:10.18710/VM2K4O/GEVNMF&version=1.0"
q<-"https://dataverse.no/api/access/datafile/:persistentId?persistentId=doi:10.18710/VM2K4O/GEVNMF"
### tempfile to store zip
sbctemp<-tempfile("SBCtemp.zip")
sbctempdir<-tempdir()
download.file(q,sbctemp)
unzip(sbctemp,exdir = sbctempdir)
list.files(sbctempdir)
sbctrn<-paste0(sbctempdir,"/")
filestrn<-list.files(sbctrn)
filestrn
f<-list.files(paste0(sbctempdir,"/",filestrn[2]))
f
co<-f
getwd()
countries<-read.csv("country_data.csv")
#download.file(q,"tdata.zip")





# Run the application 
shinyApp(ui = ui, server = server)
