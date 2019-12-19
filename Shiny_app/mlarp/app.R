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

# define user interface
ui <- navbarPage( #sets basic virtual structure (layout function)
    titlePanel("Inspect missing data"),
    
    sidebarLayout(
        
        sidebarPanel(
            selectInput( #is the input of the app that the user interacts with
            "dataset", label = "Dataset", choices = data(package = "mice")$results[,"Item"])
    ),# change to selectize?
    
    mainPanel(
        tabsetPanel(
    tabPanel("Table", DTOutput( #where to place the output table
        "table")),
    tabPanel("Summary", verbatimTextOutput( #where to place the output code
        "summary")),
    tabPanel("Plot", plotOutput("md_pattern", height = "auto"))
)
)
)
)
# define reactive server behavior (whenever the user changes their selection in the UI, the output(s) will recalculate and update in the browser.)
server <- function(input, output, session) { 
    dataset <- reactive({ #to make updating the dataset twice redundant (see `dataset <--` below)
        get(input$dataset, "package:mice")
    })
    
    output$summary <- renderPrint({ #display a statistical summary of the data with fixed-width (verbatim) text
        # dataset <- get(input$dataset, "package:datasets") #redundant, after adding reactive and () after dataset
        summary(dataset())
    })
    
    output$table <- renderDT(dataset(), #display the actual data frame in a table
        # dataset <- get(input$dataset, "package:datasets") #redundant
        options = list(
            pageLength = 5#,
            #initComplete = JS("function(settings, json) {alert('Done.');}")
        )
    )
    # # basic layout:
    # output$ID <- renderTYPE({
    #   # Expression that generates whatever kind of output
    #   # renderTYPE expects
    # })
    output$md_pattern <- renderPlot({
        md.pattern(dataset())}, height = function() {
            1.5 * session$clientData$output_md_pattern_width
        })
}
# construct and initiate
shinyApp(ui, server)
# use link in console to see in browser, when app is not closed yet

# proceed here: https://mastering-shiny.org/basic-reactivity.html

# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
