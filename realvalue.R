#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(fable)
library(tidyr)
library(lubridate)
library(tsibble)
library(stringr)
library(feasts)
library(imputeTS)
library(leaps)

historical <- read.csv("historical_initial.csv", header = T)
info <- read.csv("info_initial.csv", header = T)
county_zips <- read.csv('county-zips.csv', header = T) %>% rename(zip = Zip) %>% mutate(zip = as.character(zip))
county_region = read.csv("county_regions.csv")

h <- historical %>% separate(address, sep = ', ', into = c("street","city","statezip")) %>%
  separate(statezip, sep = ' ', into = c('state','zip')) %>%
  separate(Price, sep = " ", into = c("price", NA)) %>%
  mutate(Date = yearmonth(mdy(Date)), price = as.numeric(str_replace_all(price, "[[$,]]",""))) %>%
  select(street:price) %>% relocate(Date, .before = street) %>% relocate(zip, .before = street)

i <- info %>% separate(address, sep = ', ', into = c("street","city","statezip")) %>%
  separate(statezip, sep = ' ', into = c('state','zip')) %>% select(link:sqft) %>%
  filter(beds > 0 & baths > 0)

zr <- county_zips %>% left_join(county_region, by = "County") %>% select(zip, Region) %>% filter(!duplicated(.))
hi <- h %>% left_join(i, by = c("zip","street","city","state")) %>% left_join(zr, by = "zip") %>% filter(complete.cases(.) & !duplicated(.))

si <- hi %>% filter(Event == "Sold")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tabsetPanel(
    tabPanel("Historical Data",
             # Application title
             titlePanel("RealValue! Home Price Historical Data"),
             # Do we want to do Zip codes or county names? Also, how do we want to encode these?
             sidebarLayout(
               sidebarPanel(
                 selectInput("region_hist", "Region:", 
                         choices = levels(as.factor(si$Region)), selected = "C"),
                 selectInput("beds_hist", "Number of Bedrooms:",
                         choices = levels(as.factor(si$beds)), selected = "2"),
                 selectInput("baths_hist", "Number of Bathrooms:",
                             choices = levels(as.factor(si$baths)), selected = '2'),
                 # sliderInput("sqft_hist", "Square Footage", value = c(1000, 2000), min = 0, max = max(si$sqft)),
                 # dateRangeInput("daterng", "Select Date Range for Historical Data", start = "2000-01-01", end = "2022-04-26"),
                 # actionButton("click_hist", "Generate Historical Data"),
               ),
            # Show a plot of the generated distribution
              mainPanel(
                plotOutput("tsplot")
                
              )
            )
        ),
    tabPanel("Forecasting Tool",
             # Application title
             titlePanel("RealValue! Home Price Forecasting Tool"),
             # Do we want to do Zip codes or county names? Also, how do we want to encode these?
             sidebarLayout(
              sidebarPanel(
                selectInput("county_fc", "County:", 
                            choices = levels(as.factor(si$County)),
                            multiple = TRUE),
                selectInput("beds_fc", "Number of Bedrooms:",
                            choices = levels(as.factor(si$beds))),
                selectInput("baths_fc", "Number of Bathrooms:",
                            choices = levels(as.factor(si$baths))),
                sliderInput("sqft_fc", "Square Footage", value = c(1000, 2000), min = 0, max = max(si$sqft)),
                selectInput("model", "Forecasting Model:",
                              c("TBD" = "1",
                                "TBD" = "2",
                                "TBD" = "3")),
                selectInput("periods", "Forecast Horizion (Months):",
                            c("1" = "1",
                              "3" = "3",
                              "6" = "6")),
                # actionButton("click_frc", "Generate Forecast")
              ),
               # Show a plot of the generated distribution
              mainPanel(
                
            
              )
            )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$tsplot <- renderPlot({
    
    # ts_mean <- si %>% filter(beds == input$beds_hist &&
    #                            baths == input$baths_hist &&
    #                            sqft == input$sqft_hist) %>% group_by(Date) %>% summarise(mean_sale = mean(price)) %>% as_tsibble(index = Date)
    
    ts_mean <- si %>% filter(beds == input$beds_hist &
                               baths == input$baths_hist &
                               Region == input$region_hist) %>% group_by(Date) %>% summarise(mean_sale = mean(price)) %>% as_tsibble(index = Date)
    
    ts_mean %>% autoplot() + xlab("Month of Sale") + ylab("Mean Sale Price") + ggtitle("Monthly Mean Home Sale Price")
    
  })
  
}

#Run
shinyApp(ui, server)

