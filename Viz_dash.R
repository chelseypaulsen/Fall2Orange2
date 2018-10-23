rm(list=ls())

#install.packages(c('maps','mapproj','caschrono'))
#install.packages('rlang')
#install.packages(c('stringi'))

library(shiny)
library(ggplot2)
library(maps)
library(mapproj)
library(tidyverse)
library(readxl)
library(zoo)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(seasonal)
library(lubridate)
library(tseries)
library(foreign)
library(caschrono)
library(TSA)
library(quantmod)
library(imputeTS)
library(dplyr)
library(rlang)

#load("C:/Users/Steven/Desktop/Well_Viz.RData")
load('C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Well Data/Well_Viz2.RData')

ui <- fluidPage(
  # The UI code
  titlePanel('South Florida Well Visualization Dashboard'),
  
  # Set up the conditional panels that are dependent on the user's first selection
  
  sidebarLayout(
    sidebarPanel('Options',
                 selectInput('choice','What Do You Want to Do?', c('Explore','Predict'), selected='Explore'),
                 # 'Explor' sidebar panels
                 conditionalPanel(
                   condition = 'input.choice == "Explore"',
                   checkboxGroupInput('well_check','Well',
                                      choices=welllist,selected='G852'),
                   selectInput('year_Input','Year',unique(year(full_df$datetime)),selected='2009'),
                   selectInput('month_Input','Month',''),
                   selectInput('day_Input','Day','')),
                 # 'Predict' sidebar panels
                 conditionalPanel(
                   condition = 'input.choice == "Predict"',
                   selectInput('well_Input','Well',welllist,selected='G852'),
                   sliderInput('range_Input','Hours Predicted',0,168,c(1))
                 )
    ),
    mainPanel(
      # 'Explore' panels
      conditionalPanel(
        condition = 'input.choice == "Explore"',
        h4('Timeseries Plot of Selected Well'),
        plotOutput('timeOutput'),
        br(),
        h4('Well Heights on Selected Date'),
        plotOutput('dateOutput')),
      br(),
      # 'Predict' panels
      conditionalPanel(
        condition = 'input.choice == "Predict"',
        h4('Well Prediction for Selected Well and Hours'),
        plotOutput('predictOutput'),
        br(),
        h4('Rain Measurements'),
        plotOutput('rainOutput')),
      br())
  )
)

# Below is the server code for shiny

server <- function(input,output,session){
  
  reactive_data_well <- reactive({
    full_df %>% select(datetime,input$well_check) %>% gather(well, depth, -datetime)
  })
  
  observe({
    print(input$well_check)
    print(names(reactive_data_well()))
  })
  
  reactive_data_year <- reactive({
    full_df %>% filter(year(datetime) == input$year_Input) 
  })
  # Need observe function to make the dropdown menu option reactive
  observe({
    updateSelectInput(session,'month_Input',
                      choices=unique(month((reactive_data_year())$datetime)))
  })
  
  # First allow for month input to not have a value to prevent error
  # If it has a value, use it
  observe({
    if(input$month_Input == ''){
      return()
    }
    else{
      reactive_data_month <- reactive({(full_df %>%
                                          filter(year(datetime) == input$year_Input) %>%
                                          filter(month(datetime) == input$month_Input))})
      updateSelectInput(session,'day_Input',
                        choices=unique(day((reactive_data_month())$datetime)))
    }
  })
  # Again use observe to allow the ggplot to have a variable number of lines in it
  observe({
    if(is.null(input$well_check)){
      output$timeOutput <- renderPlot({
        ggplot(reactive_data_well(), aes(x=datetime))
      })
    }
    else{
      # Below the plot iterates over however many wells are selected and adds them to the graph
      output$timeOutput <- renderPlot({
        p <- ggplot(reactive_data_well(), aes(x=datetime, y=depth, color=well)) + geom_line(alpha=0.5)  
          #geom_vline(xintercept = reactive_date) #TODO attempts at this failed
        
        # Need better colors
        cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
        
        p <- p + theme(legend.position='right') +
          labs(y='Well Elevation (ft)', x='Year') + scale_color_manual(values=cbbPalette)
        p
      })}
  })
  # The bar chart is below, need observe because the inputs are reactive to other inputs
  observe({
    if(input$month_Input == '' | input$day_Input == ''){
      return()
    }
    else{
      reactive_prelim <- reactive({(full_df %>% select(datetime, one_of(welllist)) %>% filter(year(datetime) == input$year_Input,month(datetime) == input$month_Input,
                                                                                              day(datetime) == input$day_Input) %>% summarise_all(funs(mean)) %>% select(-datetime) %>%
                                      gather(well, depth))})
      
      reactive_data_date <- reactive({new <- reactive_prelim()
      new$sign <- as.factor(reactive_prelim()$depth > 0)
      
      new})
      
      output$dateOutput <- renderPlot({
        ggplot(reactive_data_date(), aes(x=well,y=depth,fill=sign)) +
          geom_col() +
          labs(x='Well',y='Well Elevation (ft)') +
          guides(fill=F) + geom_text(aes(label=round(depth, digits=2)), vjust=-0.25, size=4) + 
          scale_color_manual(values=c('blue','red'))}) 
    }
  })
  
  observe({
    
    reactive_rain <- reactive({full_df %>% select(datetime,input$well_Input,paste(input$well_Input,'_RAIN',sep=''))
    })
    print(head(reactive_rain()))
    output$rainOutput <- renderPlot({ggplot(reactive_rain(), aes_string(x='datetime',y=paste(input$well_Input,'_RAIN',sep=''))) +
        geom_line() + labs(x='Year',y='Rain (ft)')
    })
  })
  
}
# Call the app
shinyApp(ui=ui, server=server)
