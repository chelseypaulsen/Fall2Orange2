rm(list=ls())
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
#install.packages('rlang')
#install.packages(c('stringi'))
library(rlang)
#install.packages(c('shiny','ggplot2','dplyr','tidyverse','readxl','forecast','haven','fma','expsmooth','lubridate','caschrono','imputeTS'))
# Set up the working directory and initialize the well list

data.dir <- 'C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Well Data/'
data.dir <- 'C:/Users/Steven/Documents/MSA/Analytics Foundations/Forecasting/data/Well Data/'

wells <- c('G-852','F-45','F-179','F-319','G-561_T','G-580A','G-860','G-1220_T',
           'G-1260_T','G-2147_T','G-2866_T','G-3549','PB-1680_T')

rainlist <- c('G852_RAIN','F45_RAIN','F179_RAIN','F319_RAIN','G561_T_RAIN','G580A_RAIN','G860_RAIN','G1220_T_RAIN',
           'G1260_T_RAIN','G2147_T_RAIN','G2866_T_RAIN','G3549_RAIN','PB1680_T_RAIN')

welllist <- c('G852','F45','F179','F319','G561_T','G580A','G860','G1220_T',
              'G1260_T','G2147_T','G2866_T','G3549','PB1680_T')
# Create a sequence of dates and convert to dataframe to find missing dates in well data

start <- ymd_h("2007/10/01 00", tz='UTC')
end <- ymd_h("2018/06/12 23", tz='UTC')
hourseq <- seq(start,end, by='hours')

full_df <- data.frame(datetime=hourseq)
hourseqdf <- as.data.frame(hourseq)
names(hourseqdf) <- c('datetime')
end_df = data.frame(names = wells, 
                    ends = c('','3/26/2018 10','6/4/2018 10','4/9/2018 12','6/12/2018 23','4/9/2018 11','6/4/2018 12','','6/8/2018 11','6/8/2018 09',
                             '6/8/2018 09','6/12/2018 23','2/8/2018 09'),
                    starts = c('10/1/2007 00','10/1/2007 01','10/1/2007 01','10/1/2007 01','10/5/2007 00','10/1/2007 00','10/1/2007 01',
                               '10/1/2007 00','10/1/2007 01','10/10/2007 00','10/1/2007 01','10/1/2007 01','10/1/2007 01'),
                    stringsAsFactors = F)

# Read the excel files in and clean them

for (well in wells){
  if (well == 'G-852'){
    df <- read_excel(paste(data.dir,well,'.xlsx',sep=''),sheet='Well',
                     col_names=c('date','time','tz_code','well_ft',
                                 'Code','Corrected'),skip=1)
  }
  else{
  df <- read_excel(paste(data.dir,well,'.xlsx',sep=''),sheet='Well')
  }
  well_df <- data.frame(df)
  print(well)
  
  startwell <- (end_df %>% filter(names==well) %>% select(starts))
  endwell <- (end_df %>% filter(names==well) %>% select(ends))
  well_df_clean <- mutate(well_df, time=hour(time))  # adds date to datetime
  well_df_clean$datetime <- as.POSIXct(paste(well_df_clean$date,well_df_clean$time), format='%Y-%m-%d %H',tz='UTC')
  if ((endwell) != ''){
    well_df_clean <- well_df_clean %>%    # summarizes to hourly data and
    group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
    summarise(well_ft=mean(Corrected)) %>%
    filter(datetime >= mdy_h(startwell) &    # filters to dates defined in Simmons instructions
             datetime <= mdy_h(endwell))}
  else{
    well_df_clean <- well_df_clean %>%
      group_by(datetime) %>%
      summarise(well_ft=mean(Corrected)) %>%
      filter(datetime >= mdy_h(startwell))
  }
  well_df_clean <- well_df_clean %>% select(datetime,well_ft)
  if(endwell != ''){
  hourseq2 <- seq(mdy_h(startwell),mdy_h(endwell), by='hours')
  }
  else{
    hourseq2 <- seq(mdy_h(startwell),max(well_df_clean$datetime),by='hours')
  }
  hourseq2df <- as.data.frame(hourseq2)
  names(hourseq2df) <- c('datetime')
  # Join the data onto the date sequence to find missing values
  full_well_df <- left_join(hourseq2df,well_df_clean,by='datetime')
  # Create the timeseries object and then impute missing values using imputeTS package
  startday <- as.numeric(strftime(mdy_h(startwell), format='%j'))
  timeseries <- ts(full_well_df$well_ft, start=c(2007,startday*24), frequency=(365.25*24))
  imputed <- na.seadec(timeseries, algorithm='locf')
  full_well_df$filled <- imputed
  final_well <- left_join(hourseqdf,full_well_df, by='datetime')
  final_well <- final_well %>% select(datetime, filled)
  
  #####################
  ##### Rain Data #####
  #####################
  
  rain <- read_xlsx(paste(data.dir,well,'.xlsx',sep=""),sheet='Rain')
  rain$date<- as.Date(rain$Date)
  rain$time<- format(as.POSIXct(rain$Date, "%H:%M:%S"))
  rain_df <-data.frame(rain)
  
  rain_df_clean <- mutate(rain_df, datetime=date(date))  # adds date to datetime
  hour(rain_df_clean$datetime) <- hour(rain_df_clean$time) # Adds hour to datetime. Removes minutes from all hours
  rain_df_clean$datetime <- as.POSIXct(rain_df_clean$datetime) # change time type of newly created Datetime
  if(endwell != ''){
  rain_df_clean <- rain_df_clean %>%    # summarizes to hourly data and  
    group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
    summarise(RAIN_FT=mean(RAIN_FT)) %>%
    filter(datetime >= mdy_h(startwell) &    # filters to dates defined in Simmons instructions
             datetime <= mdy_h(endwell))}
  else{
    rain_df_clean <- rain_df_clean %>%
      group_by(datetime) %>%
      summarise(RAIN_FT=mean(RAIN_FT)) %>%
      filter(datetime >= mdy_h(startwell))
  }
  names(rain_df_clean)[names(rain_df_clean) == 'RAIN_FT'] <- paste(gsub('-','',well),'_RAIN',sep='')
           
  # Rename the column to the well name
  names(final_well)[names(final_well) == 'filled'] <- gsub('-','',well)
  final_well <- left_join(final_well, rain_df_clean, by='datetime')
  # Join all the well columns together into one master dataframe
  full_df <- full_df %>% left_join(final_well, by='datetime')

  }

head(full_df)

###############################
# The modelling code is below #
###############################

well_ts <- read.zoo(full_df %>% select(datetime,G3549))
rain_ts <- read.zoo(full_df %>% select(datetime,G3549_RAIN))

train_well = well_ft_impute[67325:93623,] #just using last 3 years to speed processing
test_well = well_ft_impute[93624:93791,]

#need to replace df3 with more better df 
train_rain = df3$RAIN_FT[67325:93623]
test_rain = df3$RAIN_FT[93624:93791]

yearly = 24*365.25

seasons <- msts(train_well, start=1, seasonal.periods = c(yearly))

#fitting sine/cosine with fourier
x.reg=train_rain
model5<-Arima(seasons,order=c(2,0,2), xreg=cbind(fourier(seasons,K=1),x.reg))
summary(model5)

# Impute missing values (fill in well_ft NAs)
rain_ft_impute <- na.approx(rain_ft)
model.rain=auto.arima(rain_ft_impute)
rain.future=forecast(model.rain,h=168)
r.f=rain.future$mean

seasons2 <- msts(test_well, start=1, seasonal.periods = c(yearly))
newx=cbind(r.f)

final.pred=forecast(model5,xreg=cbind(fourier(seasons2,K=1),newx),h=168) 

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
      #TODO attempts at this failed: +geom_vline(xintercept = ) 
    
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
