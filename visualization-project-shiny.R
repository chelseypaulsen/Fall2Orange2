library(shiny)
library(ggplot2)
#install.packages(c('maps','mapproj','caschrono'))
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
#install.packages(c('stringi'))
#install.packages(c('shiny','ggplot2','dplyr','tidyverse','readxl','forecast','haven','fma','expsmooth','lubridate','caschrono','imputeTS'))
# Set up the working directory and initialize the well list

data.dir <- 'C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Well Data/'

wells <- c('G-852','F-45','F-179','F-319','G-561_T','G-580A','G-860','G-1220_T',
           'G-1260_T','G-2147_T','G-2866_T','G-3549','PB-1680_T')

# Create a sequence of dates and convert to dataframe to find missing dates in well data

start <- ymd_h("2007/10/01 00", tz='UTC')
end <- ymd_h("2018/06/12 23", tz='UTC')
hourseq <- seq(start,end, by='hours')

full_df <- data.frame(datetime=hourseq)
hourseqdf <- as.data.frame(hourseq)
names(hourseqdf) <- c('datetime')

# Read the excel files in and clean them

for (well in wells){
  if (well == 'G-852'){
    df <- read_excel(paste(data.dir,well,'.xlsx',sep=''),sheet='Well',
                     col_names=c('date','time','tz_code','well_ft',
                                 'Code','Corrected'),skip=32)
  }
  else{
  df <- read_excel(paste(data.dir,well,'.xlsx',sep=''),sheet='Well')
  }
  well_df <- data.frame(df)
  print(well)
  well_df_clean <- mutate(well_df, time=hour(time))  # adds date to datetime
  well_df_clean$datetime <- as.POSIXct(paste(well_df_clean$date,well_df_clean$time), format='%Y-%m-%d %H',tz='UTC')
  well_df_clean <- well_df_clean %>%    # summarizes to hourly data and
    group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
    summarise(well_ft=mean(Corrected)) %>%
    filter(datetime >= ymd_hm('2007-10-01 01:00') &    # filters to dates defined in Simmons instructions
             datetime <= ymd_hm('2018-06-12 23:00'))
  well_df_clean <- well_df_clean %>% select(datetime,well_ft)
  # Join the data onto the date sequence to find missing values
  full_well_df <- left_join(hourseqdf,well_df_clean,by='datetime')
  # Create the timeseries object and then impute missing values using imputeTS package
  startday <- as.numeric(strftime(full_well_df$datetime[1], format='%j'))
  timeseries <- ts(full_well_df$well_ft, start=c(2007,startday*24), frequency=(365.25*24))
  imputed <- na.seadec(timeseries, algorithm='locf')
  full_well_df$filled <- imputed
  full_well_df <- full_well_df %>% select(datetime, filled)
  # Rename the column to the well name
  names(full_well_df)[names(full_well_df) == 'filled'] <- gsub('-','',well)
  # Join all the well columns together into one master dataframe
  full_df <- full_df %>% left_join(full_well_df, by='datetime')

  }

full_df %>% filter(year(datetime) == '2009',month(datetime) == '10',
                   day(datetime) == '25') %>%
  summarise_all(funs(mean)) %>% select(-datetime) %>%
  gather(well, depth)
day(full_df$datetime)

###############################
# Below is the shiny app code #
###############################

ui <- fluidPage(
  # The UI code
  titlePanel('South Florida Well Visualization Dashboard'),
  
  # Set up the conditional panels that are dependent on the user's first selection
  
  sidebarLayout(
    sidebarPanel('Options',
                 selectInput('choice','What Do You Want to Do?', c('Explore','Predict'), selected='Explore'),
                 conditionalPanel(
                   condition = 'input.choice == "Explore"',
                    checkboxGroupInput('well_check','Well',
                                       choices=colnames(full_df[-1]),selected='G852'),
                    selectInput('year_Input','Year',unique(year(full_df$datetime)),selected='2009'),
                    selectInput('month_Input','Month',''),
                    selectInput('day_Input','Day','')),
                 conditionalPanel(
                   condition = 'input.choice == "Predict"',
                   selectInput('well_Input','Well',colnames(full_df[,-1]),selected='G852'),
                   sliderInput('range_Input','Hours Predicted',0,168,c(1))
                 )
                ),
    mainPanel(
      conditionalPanel(
        condition = 'input.choice == "Explore"',
          h4('Timeseries Plot of Selected Well'),
          plotOutput('timeOutput'),
          br(),
          h4('Well Heights on Selected Date'),
          plotOutput('dateOutput')),
      br(),
      conditionalPanel(
        condition = 'input.choice == "Predict"',
        h4('Well Prediction for Selected Well and Hours'),
        plotOutput('predictOutput')),
      br())
    )
)

# Below is the server code for shiny

server <- function(input,output,session){
  reactive_data_well <- reactive({
    full_df %>% select(datetime,input$well_check)
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
   # Below the plot iterates over however many wells are selected and adds them to the graph
    output$timeOutput <- renderPlot({
    p <- ggplot(reactive_data_well(), aes(x=datetime))
    # Need better colors
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    i <- 1
    for (selection in input$well_check){
      
      p <- p + geom_line(aes_string(y=selection), color=cbbPalette[i])
      i <- i + 1
    }
    p <- p + scale_fill_discrete(name='Well', labels=input$well_check) + theme(legend.position='right')
    p
  })
  })
  # The bar chart is below, need observe because the inputs are reactive to other inputs
  observe({
    if(input$month_Input == '' | input$day_Input == ''){
      return()
    }
    else{
      reactive_data_date <- reactive({(full_df %>% filter(year(datetime) == input$year_Input,
                                               month(datetime) == input$month_Input,
                                               day(datetime) == input$day_Input) %>%
        summarise_all(funs(mean)) %>% select(-datetime) %>%
        gather(well, depth))})
      
      output$dateOutput <- renderPlot({
        ggplot(reactive_data_date(), aes_string(x='well',y='depth')) +
          geom_col()
      })
    }
  })
  
}
# Call the app
shinyApp(ui=ui, server=server)