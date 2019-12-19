#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mice)
library(DT)
library(naniar)

# define user interface
ui <-
    navbarPage(
        "Inspect missing data",
        #sets basic virtual structure (layout function)
        tabPanel("Explore dataset",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             #is the input of the app that the user interacts with
                             "dataset",
                             label = h3("Choose dataset"),
                             choices = data(package = "mice")$results[, "Item"]
                         ),
                         fileInput("upload", label = h3("Upload dataset")),
                         
                         hr(),
                         fluidRow(column(4, verbatimTextOutput("value")))
                     ),
                     mainPanel(DTOutput(#where to place the output table
                         "table"))
                 )),
        tabPanel(
            "Missingness pattern",
            plotOutput("md_pattern", height = "auto")
        ),
        navbarMenu(
            "More",
            tabPanel("Summary", verbatimTextOutput(#where to place the output code
                "summary")),
            tabPanel("About",
                     fluidRow(
                         column(6,
                                includeMarkdown("about.md")),
                         column(
                             3,
                             img(
                                 class = "img-polaroid",
                                 src = paste0(
                                     "https://github.com/gerkovink/shinyMice/blob/master/ThesisProposal/Figures/logo.png?raw=true"
                                 ),
                                 style = paste0("width:110%;")
                             ),
                             tags$small(
                                 "Impression of the hex sticker ",
                                 "for the interactive evaulation ",
                                 "suite for multiple imputation 'ShinyMICE'",
                                 a(href = "https://github.com/gerkovink/shinyMice")
                             )
                         )
                     ))
        )
    )


# define reactive server behavior (whenever the user changes their selection in the UI, the output(s) will recalculate and update in the browser.)
server <- function(input, output, session) {
    dataset <-
        reactive({
            #to make updating the dataset twice redundant (see `dataset <--` below)
            get(input$dataset, "package:mice")
        })
    
    output$summary <-
        renderPrint({
            #display a statistical summary of the data with fixed-width (verbatim) text
            # dataset <- get(input$dataset, "package:datasets") #redundant, after adding reactive and () after dataset
            summary(dataset())
        })
    
    output$table <-
        renderDT(dataset())
    #display the actual data frame in a table
    # dataset <- get(input$dataset, "package:datasets") #redundant
    
    output$md_pattern <-
        renderPlot({
            md.pattern(dataset())
        }, height = function() {
            1.5 * session$clientData$output_md_pattern_width
        })
}
# construct and initiate
shinyApp(ui, server)
# use link in console to see in browser, when app is not closed yet

# proceed here: https://mastering-shiny.org/basic-reactivity.html
