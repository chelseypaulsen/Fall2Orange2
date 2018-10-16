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

data.dir <- 'C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Well Data/'

wells <- c('G-852','F-45','F-179','F-319','G-561_T','G-580A','G-860','G-1220_T',
           'G-1260_T','G-2147_T','G-2866_T','G-3549','PB-1680_T')

start <- ymd_h("2007/10/01 00", tz='UTC')
end <- ymd_h("2018/06/12 23", tz='UTC')
hourseq <- seq(start,end, by='hours')

full_df <- data.frame(datetime=hourseq)
hourseqdf <- as.data.frame(hourseq)
names(hourseqdf) <- c('datetime')

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
  full_well_df <- left_join(hourseqdf,well_df_clean,by='datetime')
  print(sum(is.na(full_well_df$well_ft)))
  print(well)
  startday <- as.numeric(strftime(full_well_df$datetime[1], format='%j'))
  timeseries <- ts(full_well_df$well_ft, start=c(2007,startday*24), frequency=(365.25*24))
  imputed <- na.seadec(timeseries, algorithm='locf')
  full_well_df$filled <- imputed
  print(sum(is.na(full_well_df$filled)))
  full_well_df <- full_well_df %>% select(datetime, filled)
  names(full_well_df)[names(full_well_df) == 'filled'] <- gsub('-','',well)
  print(sum(is.na(full_well_df[gsub('-','',well)])))
  full_df <- full_df %>% left_join(full_well_df, by='datetime')
  print(sum(is.na(full_df[gsub('-','',well)])))
  
  }

full_df %>% filter(year(datetime) == '2009',month(datetime) == '10',
                   day(datetime) == '25') %>%
  summarise_all(funs(mean)) %>% select(-datetime) %>%
  gather(well, depth)
day(full_df$datetime)
ui <- fluidPage(
  
  titlePanel('South Florida Well Visualization Dashboard'),
  
  sidebarLayout(
    sidebarPanel('Inputs',
                 selectInput('well_Input','Well',colnames(full_df[,-1]),selected='G852'),
                 selectInput('year_Input','Year',unique(year(full_df$datetime)),selected='2009'),
                 selectInput('month_Input','Month',''),
                 selectInput('day_Input','Day','')
                ),
    mainPanel(
      h4('Timeseries Plot of Selected Well'),
      plotOutput('timeOutput'),
      br(),
      h4('Well Heights on Selected Date'),
      plotOutput('dateOutput'),
      br())
  )
)

server <- function(input,output,session){
  reactive_data_well <- reactive({
    full_df %>% select(datetime,input$well_Input)
  })
  
  reactive_data_year <- reactive({
    full_df %>% filter(year(datetime) == input$year_Input) 
  })
  
  observe({
    updateSelectInput(session,'month_Input',
                      choices=unique(month((reactive_data_year())$datetime)))
  })
  
    
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

  output$timeOutput <- renderPlot({
    
    ycol <- input$well_Input
    
    ggplot(reactive_data_well(), aes_string(x='datetime',y=ycol)) +
      geom_line()
  })
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

shinyApp(ui=ui, server=server)