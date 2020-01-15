library(shiny)

shinyServer(function(input, output, session) {
  
  observe({
    
    if (input$browse == 0) return()
    
    updateTextInput(session, "path",  value = file.choose())
  })
  
  contentInput <- reactive({ 
    
    if(input$upload == 0) return()
    
    isolate({
      writeLines(paste(readLines(input$path), collapse = "\n"))
    })
  })
  
  output$content <- renderPrint({
    contentInput()
  })
  
})