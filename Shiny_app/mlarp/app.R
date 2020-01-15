# Briefly Inspect Missingness Using R Shiny (BIMURS)
# Shiny web application by Hanne Oberman. 
# Developed as part of the course 'Markup Languages and Reproducible Programming.
# Solely for the purpose of showcasing my abilities to program an app.

library("shiny")
library("mice")
library("DT")
library("naniar")
library("rmarkdown")

# define user interface
ui <-
    navbarPage(
        "Briefly Inspect Missingness Using R Shiny (BIMURS)",
        #sets basic virtual structure (layout function)
        tabPanel("Explore dataset",
                 sidebarLayout(
                     sidebarPanel(
                         shinyjs::useShinyjs(),
                         id = "sidebar",
                         fileInput("upload", label = h3("Upload CSV file..."), 
                                   accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                         ),
                         checkboxInput("header", h6("Un-check this box if the CSV file does not have headers"), TRUE),
                         selectInput(
                             #is the input of the app that the user interacts with
                             "choice",
                             label = h3("...or use MICE data"),
                             choices = data(package = "mice")$results[, "Item"]
                         ),
                         actionButton("reset", "Reset"),
                     ),
                     mainPanel(
                         h2("Tabulated dataset"),
                         h5("NB. Empty cells denote missing data points."),
                         DTOutput("table"))
                 )),
        tabPanel(
            "Missingness pattern",
            h2("Observed missingness pattern per variable"),
            h5("NB. Observed data is blue, missing data is red."),
            plotOutput("md_pattern", height = "auto")
        ),
        navbarMenu(
            "More",
            tabPanel(
                "Summary", 
                h2("Descriptive statistics per variable"),
                h5("NB. Particularly the number of NA values is of interest."),
                verbatimTextOutput(#where to place the output code
                "summary")),
            tabPanel("About",
                     h2("About the BIMURS app"),
                     fluidRow(
                         column(6,
                                includeMarkdown("about.rmd")),
                         column(
                             3,
                             img(
                                 class = "img-polaroid",
                                 src = paste0(
                                     "https://raw.githubusercontent.com/gerkovink/shinyMice/master/1.ThesisProposal/Figures/logo.png"
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
    
    rv <- reactiveValues(data = NULL)
    
    observe({
    if (is.null(input$upload)) {
        rv$data <- get(input$choice, "package:mice")
    } else {rv$data <- read.csv(input$upload$datapath, header = input$header)}
    })
    
    observeEvent(input$reset, {
        shinyjs::reset("sidebar")
        rv$data <- get(input$choice, "package:mice")
    })
    
    output$summary <-
        renderPrint({
            #display a statistical summary of the data with fixed-width (verbatim) text
            # dataset <- get(input$dataset, "package:datasets") #redundant, after adding reactive and () after dataset
            summary(rv$data)
        })
    
    output$table <-
        renderDT(rv$data)
    #display the actual data frame in a table
    # dataset <- get(input$dataset, "package:datasets") #redundant
    
    output$md_pattern <-
        renderPlot({
            md.pattern(rv$data)
        }, height = function() {
            1.5 * session$clientData$output_md_pattern_width
        })
}
# construct and initiate
shinyApp(ui, server)