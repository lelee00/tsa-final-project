#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(fable)
library(tidyr)
library(forecast)
library(lubridate)
library(tsibble)
library(stringr)
library(feasts)
library(imputeTS)
library(leaps)
library(DT)
library(plotly)
library(scales)
library(dplyr)


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
  filter(beds > 0 & baths > 0 & sqft > 0)

zr <- county_zips %>% left_join(county_region, by = "County") %>% filter(!duplicated(.)) %>% group_by(County) %>% filter(row_number()==1)
hi <- h %>% left_join(i, by = c("zip","street","city","state")) %>% left_join(zr, by = "zip") %>% filter(complete.cases(.) & !duplicated(.))

si <- hi %>% filter(Event == "Sold")


ts <- si %>%
  filter(complete.cases(si)) %>%
  group_by(Date) %>%
  summarise(mean_sale = mean(price), mean_baths = mean(baths), mean_beds = mean(beds)) %>%
  as_tsibble() %>% fill_gaps(mean_sale = 0, mean_beds = 0, mean_baths = 0)

# fit <- ts %>% model(TSLM(mean_sale ~ trend() + season()))

# fit <- ts %>% model(TSLM(mean_price ~ mean_beds + mean_baths + mean_beds:mean_baths))

# fit <- ts %>% model(ARIMA(mean_sale))

fit <- ts %>% model(ARIMA(mean_sale))

options <- si %>%
  group_by(yearmonth(Date), beds, baths, County) %>%
  summarise(mean_sale = mean(price)) %>%
  group_by(beds, baths, County) %>%
  summarise(n = n()) %>% filter(n >= 3) %>% select(beds, baths, County) %>%
  mutate(beds = as.factor(beds), baths = as.factor(baths), County = as.factor(County)) %>%
  mutate(bdba = as.factor(paste0(beds, " beds, ",baths, " baths")))

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tabsetPanel(
    tabPanel("Historical Data",
             # Application title
             titlePanel("RealValue! Home Price Historical Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("county_hist", "County:", choices = levels(as.factor(options$County))),
                 uiOutput("bdbaControls"),
                 uiOutput("sqftControls")
                 # sliderInput("sqft_hist", "Square Footage", value = c(1000, 3000), min = 0, max = max(si$sqft))
               ),
            # Show a plot of the generated distribution
              mainPanel(
                plotlyOutput("tsplot"),
                div(style = 'overflow-y: scroll',dataTableOutput("trace_table"))
              )
            )
        ),
    tabPanel("Forecasting Tool",
             # Application title
             titlePanel("RealValue! Home Price Forecasting Tool"),
             sidebarLayout(
                sidebarPanel(
                  selectInput("county_fc", "County:", choices = levels(as.factor(options$County))),
                  uiOutput("bdbaControlsfc"),
                  uiOutput("sqftControlsfc"),
                  # sliderInput("sqft_fc", "Square Footage", value = c(1000, 3000), min = 0, max = max(si$sqft)),
                selectInput("periods", "Forecast Horizon:",
                            c("1 year",
                              "3 years",
                              "6 years"))
              ),
               # Show a plot of the generated distribution
              mainPanel(
                plotOutput("fcplot"),
                dataTableOutput("fctable")
                # tableOutput("test")
            
              )
            )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$bdbaControls = renderUI({

    combined = reactive ({
      options  %>%
        filter(County == input$county_hist) %>%
        pull(bdba) %>% as.character()
    })

    selectizeInput("bdba_hist","Beds & Baths", choices = combined())

  })
  
  
  beds = reactive({

    options %>%
      filter(County == input$county_hist) %>%
      filter(bdba == input$bdba_hist) %>%
      pull(beds) %>% as.character()

  })
  
  baths = reactive({
  
    options %>%
      filter(County == input$county_hist) %>%
      filter(bdba == input$bdba_hist) %>%
      pull(baths) %>% as.character()

  })
  
  output$sqftControls <- renderUI({
    sqft <- si %>%
      filter(County==input$county_hist) %>%
      filter(beds == beds()) %>%
      filter(baths==baths()) %>%
      summarise(mini = min(sqft), maxi = max(sqft))
    mini = sqft$mini
    maxi = sqft$maxi
    sliderInput("sqft_hist", "Square Footage", value = c(mini, maxi), min = 0, max = maxi)
  })
  
  ts_mean <- reactive({ 

    si %>%
      filter(County == input$county_hist) %>%
      filter(beds == beds()) %>%
      filter(baths == baths()) %>%
      filter(sqft >= input$sqft_hist[1] & sqft <= input$sqft_hist[2]) %>%
      group_by(Date) %>%
      summarise(mean_sale = mean(price)) %>%
      as_tsibble(index = Date)
    
    
  })
  
  tab <- reactive({ 
    
    si %>%
      filter(County == input$county_hist) %>%
      filter(beds == beds()) %>%
      filter(baths == baths()) %>%
      filter(sqft >= input$sqft_hist[1] & sqft <= input$sqft_hist[2]) %>%
      group_by(Date) %>%
      summarise(mean_sale = dollar(mean(price))) %>%
      mutate(Date = as.character(Date)) %>% as.data.frame()
    
    
  })
  
  # output$test <- renderTable({ 
  #   fit %>% forecast(h = input$periods)
  #   
  #   })
  
  output$bdbaControlsfc <- renderUI({
    
    combined <- reactive ({
      options  %>%
        filter(County == input$county_fc) %>%
        pull(bdba)
    })
    
    selectizeInput("bdba_fc","Beds & Baths", combined())
    
  })
  
  
  bedsfc <- reactive({
    
    options %>%
      filter(County == input$county_fc) %>%
      filter(bdba == input$bdba_fc) %>% pull(beds) %>% as.character()
    
  })
  
  bathsfc <- reactive({
    
    options %>%
      filter(County == input$county_fc) %>%
      filter(bdba == input$bdba_fc) %>% pull(baths) %>% as.character()
    
  })
  
  output$sqftControlsfc <- renderUI({
    sqft <- si %>%
      filter(County==input$county_fc) %>%
      filter(beds == bedsfc()) %>%
      filter(baths==bathsfc()) %>%
    summarise(mini = min(sqft), maxi = max(sqft))
    mini = sqft$mini
    maxi = sqft$maxi
    sliderInput("sqft_fc", "Square Footage", value = c(mini, maxi), min = 0, max = maxi)
  })
  
  ts_meanfc <- reactive({ 
    
    si %>%
      filter(County == input$county_fc) %>%
      filter(beds == bedsfc()) %>%
      filter(baths == bathsfc()) %>%
      filter(sqft >= input$sqft_fc[1] & sqft <= input$sqft_fc[2]) %>%
      group_by(Date) %>%
      summarise(mean_sale = mean(price)) %>%
      as_tsibble(index = Date)
    
    
  })
  
  tabfc <- reactive({ 
    
    si %>%
      filter(County == input$county_fc) %>%
      filter(beds == bedsfc()) %>%
      filter(baths == bathsfc()) %>%
      filter(sqft >= input$sqft_fc[1] & sqft <= input$sqft_fc[2]) %>%
      group_by(Date) %>%
      summarise(mean_sale = dollar(mean(price))) %>%
      mutate(Date = as.character(Date)) %>% as.data.frame()
    
    
  })

  

  
  output$tsplot <- renderPlotly({
    
    req(ts_mean())
    
    plot <- ggplot(ts_mean(), aes(x = Date, y = mean_sale)) +
      geom_line() +
      xlab("Month of Sale") +
      ylab("Mean Sale Price") +
      ggtitle("Monthly Mean Home Sale Price") + 
      scale_y_continuous(labels = scales::dollar_format())
    
    ggplotly(plot)
    
  })
  
  output$trace_table <- renderDataTable({
    req(tab())
    
    datatable(tab(), options = list(paging = T), colnames = c("Date of Sale", "Mean Home Sale Price"))
    
  })
  
  
  output$fcplot <- renderPlot({
    
    req(ts_meanfc())
    
    fc <- fit %>% forecast(h = input$periods)
    
    fcplot <- fit %>%
      forecast(h = input$periods) %>%
      autoplot(ts_meanfc()) + xlab("Month of Sale") + ylab("Mean Sale Price") + ggtitle("Monthly Mean Home Sale Price")
    
    fcplot
    
    # ggplot(ts_meanfc(), aes(x = Date, y = mean_sale)) +
    #   geom_line() +
    #   geom_line(data = fc, aes(x = Date, y = .mean), colour = "blue") +
    #   geom_ribbon(data = fc, aes(x = Date, ymin = lo95, ymax = hi95), fill = "blue", alpha = 0.25) +
    #   geom_ribbon(data = fc, aes(x = Date, ymin = lo80, ymax = hi80), fill = "blue", alpha = 0.25)

    
  })
  
  output$fctable <- renderDataTable({
    
    req(tabfc())
    req(ts_meanfc())
    
    last_sale = ts_meanfc() %>% filter(Date == max(Date)) %>% .$mean_sale
    date_last = ts_meanfc() %>% filter(Date == max(Date)) %>% pull(Date) %>% yearmonth()
    meanfctb <- fit %>%
      forecast(h = input$periods) %>%
      mutate(price_diff = dollar(.mean - last_sale), total_sale = dollar(.mean)) %>%
      as.data.frame() %>%
      select(Date, price_diff, total_sale) %>% mutate(Date = as.character(Date))
    datatable(meanfctb, options = list(paging = T), colnames = c("Projected Date of Sale", paste0("Price Difference from ", date_last, sep = " "), "Total Sale Price at Projected Date"))
  })

}

#Run
shinyApp(ui, server)

