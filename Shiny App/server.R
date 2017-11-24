#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("suggest_words.R")

updateTextHis <- function(texthis,input){
  if (nchar(input) == 0) return(texthis)
  else return(tail(c(texthis,input),10))
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session){
  
  suggestion <- reactive(Suggest(as.character(input$inputText)))
  Mylist <- reactiveValues()
  isolate({
    Mylist$textHistory <- vector(mode = "character", length = 10)
    for (i in 1:10) Mylist$textHistory[i] <- "."
    })

  output$textHisOutput <- renderTable(Mylist$textHistory,
                                      width = "100%", align = "r",
                                      rownames = FALSE, colnames = FALSE)
  
  #textList <- reactiveValues()
  #for (i in 1:10) textList[i] <- div(class="card text-white bg-success", width = "100%", margin = "5px",
  #                                     div(class="card-body",
  #                                         p(align = "r",Mylist$textHistory[i])
  #                                     )
  #                                   )
  
  #output$textHisOutput <- renderUI({
  #  fixedPanel(top=0,width="100%",height="85%",tagList(textList))
  #  })
  
    
  observe({
    #if (nchar(as.character(input$inputText)) == 0) return()
    t <- input$Sendbtn
    if (length(t) == 0) return()
    if (t == 0) return()
    isolate({
      #Mylist$textHistory <- tail(c(Mylist$textHistory,as.character(input$inputText)),10)
      Mylist$textHistory <- updateTextHis(Mylist$textHistory,as.character(input$inputText))
      updateTextInput(session, "inputText", value = "")
    })
  })
    
  observe({
    t <- input$Sugg1*10 + input$Sugg2*100 + input$Sugg3*1000
    #if (nchar(as.character(input$inputText)) == 0) return()
    if (length(t) == 0) return()
    if (t == 0) return()
    isolate ({
      suggWords <- suggestion()
      updateTextInput(session, "inputText",
                      value = replLastWord(input$inputText, suggWords[log10(t)]))
    })
  })
  
  
  
  output$Sugg <- renderUI({

    Sugg <- suggestion()
    #if(nchar(word) == 0) {
    #  word = "..."
    #}
    fluidRow(
    column(3,
           actionButton("Sugg1", Sugg[1], width = "100%", class="btn btn-outline-secondary")
           ),
    column(3, 
           actionButton("Sugg2", Sugg[2], width = "100%", class="btn btn-outline-secondary")
           ),
    column(3,
           actionButton("Sugg3", Sugg[3], width = "100%", class="btn btn-outline-secondary")
           ))
  })
})  

