#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  titlePanel("Text Messenger Simulator"),

  
  #tableOutput("textHisOutput"),
  uiOutput("textHisOutput"),
  
  #hr(),
  #fixedPanel(bottom=0,width="100%",height="15%",
  fluidRow(
    column(10,
           textInput("inputText", label= NULL, value = "", width = "100%")
           ),
    column(1, 
           actionButton("Sendbtn", "Sned", class="btn btn-info")
           )
  ),
  
  uiOutput("Sugg")
  #)
))
