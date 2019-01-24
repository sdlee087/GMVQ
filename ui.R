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
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Estimated GMVQ-mu"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("target",
                   "loglambda:",
                   min = -10,
                   max = 0,
                   value = -10,
                   step = 0.2,
                   animate = animationOptions(interval = 800, loop = FALSE)
                   ),
       selectInput(inputId = "n",
                   label = "Choose n:",
                   choices = c("10", "50", "200"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("muPlot"),
       plotOutput("KPlot")
    )
  )
))
