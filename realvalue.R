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
library(DT)

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

zr <- county_zips %>% left_join(county_region, by = "County") %>% select(zip, Region) %>% filter(!duplicated(.)) %>% left_join(county_region, by = 'Region')
hi <- h %>% left_join(i, by = c("zip","street","city","state")) %>% left_join(zr, by = "zip") %>% filter(complete.cases(.) & !duplicated(.))

si <- hi %>% filter(Event == "Sold")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tabsetPanel(
    tabPanel("Historical Data",
             # Application title
             titlePanel("RealValue! Home Price Historical Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("county_hist", "County:", choices = levels(as.factor(si$County))),
                 # uiOutput("zipControls"),
                 # selectInput("zip_hist", "Zip Code:", 
                         # choices = levels(as.factor(si$zip)), selected = "46777"),
                 uiOutput("bedControls"),
                 # selectInput("beds_hist", "Number of Bedrooms:",
                         # choices = levels(as.factor(si$beds)), selected = "2"),
                 selectInput("baths_hist", "Number of Bathrooms:",
                             choices = levels(as.factor(si$baths)), selected = '2'),
                 sliderInput("sqft_hist", "Square Footage", value = c(1000, 2000), min = 0, max = max(si$sqft)),
                 # dateRangeInput("daterng", "Select Date Range for Historical Data", start = "2000-01-01", end = "2022-04-26"),
                 # actionButton("click_hist", "Generate Historical Data"),
               ),
            # Show a plot of the generated distribution
              mainPanel(
                plotOutput("tsplot"),
                div(style = 'overflow-y: scroll',dataTableOutput("trace_table"))
                
              )
            )
        ),
    tabPanel("Forecasting Tool",
             # Application title
             titlePanel("RealValue! Home Price Forecasting Tool"),
             sidebarLayout(
                sidebarPanel(
                  selectInput("county_fc", "County:", choices = levels(as.factor(si$County)), selected = 'Allen'),
                  # uiOutput("zipControls"),
                  # selectInput("zip_hist", "Zip Code:", 
                  # choices = levels(as.factor(si$zip)), selected = "46777"),
                  uiOutput("bedControlsfc"),
                  # selectInput("beds_hist", "Number of Bedrooms:",
                  # choices = levels(as.factor(si$beds)), selected = "2"),
                  selectInput("baths_fc", "Number of Bathrooms:",
                              choices = levels(as.factor(si$baths))),
                  sliderInput("sqft_fc", "Square Footage", value = c(1000, 2000), min = 0, max = max(si$sqft)),
                  # dateRangeInput("daterng", "Select Date Range for Historical Data", start = "2000-01-01", end = "2022-04-26"),
                  # actionButton("click_hist", "Generate Historical Data"),
                selectInput("periods", "Forecast Horizon:",
                            c("1 year",
                              "3 years",
                              "6 years")),
                # actionButton("click_frc", "Generate Forecast")
              ),
               # Show a plot of the generated distribution
              mainPanel(
                plotOutput("fcplot"),
                div(style = 'overflow-x: scroll',dataTableOutput("fctable"))
            
              )
            )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  output$bedControls <- renderUI({
    beds <- si %>%
      filter(County == input$county_hist) %>%
      mutate(beds = as.factor(beds)) %>%
      group_by(Date, beds) %>%
      summarise(mean_sale = mean(price)) %>%
      group_by(beds) %>%
      summarise(n = n()) %>%
      filter(n >= 2) %>% 
      .$beds %>% levels()
    selectInput("beds_hist", "Beds", beds, selected = '2')
  })
  
  output$bedControlsfc <- renderUI({
    beds <- si %>%
      filter(County == input$county_fc) %>%
      mutate(beds = as.factor(beds)) %>%
      group_by(Date, beds) %>%
      summarise(mean_sale = mean(price)) %>%
      group_by(beds) %>%
      summarise(n = n()) %>%
      filter(n >= 2) %>% 
      .$beds %>% levels()
    selectInput("beds_fc", "Beds", beds, selected = '2')
  })

  
  output$tsplot <- renderPlot({
    
    ts_mean <- si %>% filter(County == input$county_hist & beds == input$beds_hist &
                               baths == input$baths_hist &
                              (sqft >= input$sqft_hist[1] && sqft <= input$sqft_hist[2])) %>% group_by(Date) %>% summarise(mean_sale = mean(price)) %>% as_tsibble(index = Date)
    
    ts_mean %>% autoplot() + xlab("Month of Sale") + ylab("Mean Sale Price") + ggtitle("Monthly Mean Home Sale Price")
    
  })
  
  output$trace_table <- renderDataTable({
    ts_meantb <- si %>% filter(County == input$county_hist & beds == input$beds_hist &
                               baths == input$baths_hist &
                               (sqft >= input$sqft_hist[1] && sqft <= input$sqft_hist[2])) %>% group_by(Date) %>% summarise(mean_sale = mean(price)) %>% mutate(Date = as.character(Date))
    datatable(ts_meantb, options = list(paging = T), colnames = c("Date of Sale", "Mean Home Sale Price"))
    
  })
  
  output$decomp <- renderPlot({
    
    ts_meanfc <- si %>%
      filter(beds == input$beds_fc &
                               baths == input$baths_fc &
                               County == input$county_fc &
                                 (sqft >= input$sqft_fc[1] && sqft <= input$sqft_fc[2])) %>%
      group_by(Date) %>%
      summarise(mean_sale = mean(price)) %>%
      as_tsibble(index = Date)
    
    fit <- ts_meanfc %>% model(TSLM(mean_sale ~ trend()))
    
    # fit %>% forecast(h = input$periods) %>% autoplot(ts_meanfc) + xlab("Month of Sale") + ylab("Mean Sale Price") + ggtitle("Monthly Mean Home Sale Price")
    ts_meanfc %>%
      fill_gaps(mean_sale = 0) %>%
      model(stl = STL(mean_sale)) %>%
      components() %>%
      autoplot()
    
  })
  
  
  output$fcplot <- renderPlot({
    
    ts_meanfc <- si %>%
      filter(beds == input$beds_fc &
               baths == input$baths_fc &
               County == input$county_fc &
               (sqft >= input$sqft_fc[1] && sqft <= input$sqft_fc[2])) %>%
      group_by(Date) %>%
      summarise(mean_sale = mean(price)) %>%
      as_tsibble(index = Date)
    
    fit <- ts_meanfc %>% model(TSLM(mean_sale ~ trend() + season()))
    
    fit %>% forecast(h = input$periods) %>% autoplot(ts_meanfc) + xlab("Month of Sale") + ylab("Mean Sale Price") + ggtitle("Monthly Mean Home Sale Price")

    
  })
  
  output$fctable <- renderDataTable({
    
    ts_meanfc <- si %>%
      filter(beds == input$beds_fc &
               baths == input$baths_fc &
               County == input$county_fc &
               (sqft >= input$sqft_fc[1] && sqft <= input$sqft_fc[2])) %>%
      group_by(Date) %>%
      summarise(mean_sale = mean(price)) %>%
      as_tsibble(index = Date)
    
    fit <- ts_meanfc %>% model(TSLM(mean_sale ~ trend() + season()))
    
    meanfctb <- fit %>%
      forecast(h = input$periods) %>%
      mutate('expected ROI' = paste0("$",round((.mean - (ts_meanfc %>% filter(Date == max(Date)) %>%
                                          .$mean_sale)),2),sep = "")) %>% as.data.frame() %>%
      mutate(Date = as.character(Date)) %>% select(Date, `expected ROI`)
    datatable(meanfctb, options = list(paging = T))
  })
  
}

#Run
shinyApp(ui, server)

