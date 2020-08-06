#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rio)
df <- import("../data/asset_development.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("colored line plot"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("line_plot", 
                label = "Choose a color",
                choices = c("coral", "turquoise"),
                selected = "coral")
            ),
        
        mainPanel(
            plotOutput("line_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({ 
        df %>% ggplot(aes(x = year, y = assets)) +
            geom_line(color = input$line_plot)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
