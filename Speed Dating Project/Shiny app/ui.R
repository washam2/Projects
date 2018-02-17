
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny) 
ui = fluidPage(
  titlePanel("Speed Dating Analysis"),
  
  column(3,
         selectInput("select", h3("Select Analysis"), 
                     choices = list("Gender by Wave" = 1, "Age by Wave" = 2,
                                    "Age Difference" = 3, "Fields" = 4, "Career" = 5, 
                                    "Race" = 6, "Match by Gender" = 7, "Match by Wave" = 8, 
                                    "Number of Match (Men)" = 9, "Number of Match (Women)" = 10 ,
                                    "Important feature (Men)" = 11, "Important feature (Women)" = 12,"Location" = 13), selected = 1)),
  mainPanel(
    plotOutput("active_plot"),
    textOutput("text")
  )
)