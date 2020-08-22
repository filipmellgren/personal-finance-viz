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
library(patchwork)
source("../code/aux_functions.R")
#source("../code/savings.R")

#df <- import("../data/asset_development.csv")
colorpal <- scale_color_brewer(type = 'qual', palette = 6)


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("colored line plot"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("line_plot", 
                label = "Choose a color",
                choices = c("coral", "turquoise"),
                selected = "coral"),
            
            sliderInput("start_wage", h3("Starting net wage (per month)"),
                        min = 0, max = 100000, value = 20000),
            
            sliderInput("wage_growth", h3("Annual wage growth"),
                        min = 0, max = 0.1, value = 0.02),
            
            sliderInput("start_wealth", h3("Today's wealth"),
                        min = 0, max = 10^7, value = 7*10^5),
            
            sliderInput("consumption", h3("Consumption & housing"),
                        min = 0, max = 10^4, value = 7*10^3),
            
            sliderInput("cons_growth", h3("Annual consumption growth"),
                        min = 0, max = 0.1, value = 0.05)
            ),
        
        mainPanel(
            plotOutput("line_plot"),
            plotOutput("start_wage"),
            plotOutput("start_wealth"),
            plotOutput("consumption"),
            plotOutput("cons_growth")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    time_hz <- 40
    df <- tibble(.rows = time_hz)
    output$line_plot <- renderPlot({ 
        # Year
        df <- df %>% mutate(year = 1:time_hz)
        
        # Wage
        df <- df %>% mutate(wage = growth.seq(input$wage_growth, time_hz, input$start_wage))
        
        # Consumption & housing
        df <- df %>% mutate(consumption = growth.seq(input$cons_growth, time_hz, input$consumption))

        # CSN
        csn_cf <- import("../data/csn.csv") %>% 
            select(csn_cf) %>% filter(row_number()<=time_hz)
        
        while (nrow(csn_cf) < time_hz) {
            # adds zeros to the csn payments after it is done
            csn_cf <- rbind(csn_cf, 0)
        }
        
        df <- cbind(df, csn_cf)
        
        # Saving
        df <- df %>% mutate(savings = wage - consumption - csn_cf)
        
        # Assets
        asset_g <- rep(0.07, time_hz)
        df <- df %>% mutate(assets = assets_seq(savings, asset_g, time_hz, input$start_wealth))
        
        # Randomness
        asset_sg <- rnorm(time_hz, mean = 0.07, sd = 0.15)
        df <- df %>% mutate(assets = assets_seq(savings, asset_sg, time_hz, input$start_wealth))
        
        # Financial independence
        df <- df %>% mutate(findependence = 25*consumption)
        
        # Plot
        plot.findep <- df %>% select(year, assets, findependence) %>% 
            gather(key = "Series", value = "SEK", -year) %>%
            ggplot(aes(x = year, y = SEK)) +
            geom_line(aes(color = Series), size = 1) +
            theme_minimal() + 
            colorpal
        
        plot.savings <- df %>% select(year, savings, csn_cf, consumption) %>% 
            gather(key = "Series", value = "SEK", -year) %>% 
            ggplot(aes(x = year, y = SEK/12)) +
            geom_line(aes(color = Series)) + theme_minimal() +
            labs(y = "Monthly expenditure & saving") +
            colorpal
            
        plot.findep / plot.savings# TODO: add other information to the dashboard
        # Total debt (csn + mortgage)
        # Total debt to equity ratio
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
