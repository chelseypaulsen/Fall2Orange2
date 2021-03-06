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
#install.packages('shinydashboard')
library(shinydashboard)
#install.packages(c('shiny','ggplot2','dplyr','tidyverse','readxl','forecast','haven','fma','expsmooth','lubridate','caschrono','imputeTS'))
# Set up the working directory and initialize the well list

data.dir <- 'C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Well Data/'
#data.dir <- 'C:/Users/Steven/Documents/MSA/Analytics Foundations/Forecasting/data/Well Data/'

wells <- c('G-852','F-45','F-179','F-319','G-561_T','G-580A','G-860','G-1220_T',
           'G-1260_T','G-2147_T','G-2866_T','G-3549','PB-1680_T')

rainlist <- c('G852_RAIN','F45_RAIN','F179_RAIN','F319_RAIN','G561_T_RAIN','G580A_RAIN','G860_RAIN','G1220_T_RAIN',
           'G1260_T_RAIN','G2147_T_RAIN','G2866_T_RAIN','G3549_RAIN','PB1680_T_RAIN')

welllist <- c('G852','F45','F179','F319','G561_T','G580A','G860','G1220_T',
              'G1260_T','G2147_T','G2866_T','G3549','PB1680_T')
# Create a sequence of dates and convert to dataframe to find missing dates in well data

start <- ymd_h("2007/10/01 00", tz='UTC')
end <- ymd_h("2018/06/12 23", tz='UTC')
hourseq <- seq(start,end+(168*3600), by='hours')

full_df <- data.frame(datetime=hourseq)
hourseqdf <- as.data.frame(hourseq)
names(hourseqdf) <- c('datetime')
end_df = data.frame(names = wells, 
                    ends = c('','3/26/2018 10','6/4/2018 10','4/9/2018 12','6/12/2018 23','4/9/2018 11','6/4/2018 12','','6/8/2018 11','6/8/2018 09',
                             '6/8/2018 09','6/12/2018 23','2/8/2018 09'),
                    starts = c('10/1/2007 00','10/1/2007 01','10/1/2007 01','10/1/2007 01','10/5/2007 00','10/1/2007 00','10/1/2007 01',
                               '10/1/2007 00','10/1/2007 01','10/10/2007 00','10/1/2007 01','10/1/2007 01','10/1/2007 01'),
                    stringsAsFactors = F)


# pred_model5 <- function(final_well, well.2){
#   # TODO, incoporate and check that this function works in the for loop
#   
#   # this function takes in a dataframe of well datetimes,well data, and rain data
#   # it returns the original dataframe w/ new cols for forecast and confidence intervals
#   # it also depends on startwell and endwell vectors
#   # returned df also has new names
#   
#   #splitting newly assembled well data for clean model generation
#   if (well.2 == 'G-852' | well.2 == 'G-1220_T'){end_dt <- max(well_df_clean$datetime)
#   }
#   
#   else{end_dt <- mdy_h(endwell)}
#   train <- final_well %>%
#     filter(datetime >= mdy_h(startwell) & datetime <= end_dt)
#   # Generating model on training data
#   yearly <- 24*365.25
#   x.reg <- train$RAIN_FT
#   seasons <- msts(train$filled, start=1, seasonal.periods = c(yearly))
#   model5 <- Arima(seasons,order=c(2,0,2), xreg=cbind(fourier(seasons,K=1),x.reg))
#   
#   # forecasting across last 168 days
#   rain.ts <- read.zoo(train$RAIN_FT)
#   rain.impute <- na.approx(rain.ts)
#   rain.model <- auto.arima(rain.impute)
#   rain.preds <- forecast(rain.model, h=168)
#   newx <- rain.preds$mean # using actual rainfall data, because our rain model is really bad
#   final.pred=forecast(model5,xreg=cbind(fourier(seasons,K=1),newx),h=168)
#   
#   # building df from results
#   df_results <- as.data.frame(cbind(
#     final.pred$mean,
#     final.pred$upper[,1],
#     final.pred$upper[,2],
#     final.pred$lower[,1],
#     final.pred$lower[,2]))
#   colnames(df_results) <- c('Forecast', 'Upper80', 'Upper95', 'Low80', 'Low95') #probably an unnecessary line
#   df_results$datetime <- test$datetime
#   final_well <- final_well %>%
#     left_join(df_results, by="datetime")
#   
#   # Rename the column to the appropriate names
#   well2 <- gsub('-','',well)
#   names <- c('','_RAIN', '_Forecast', '_Up80', '_Up95', '_Lo80', '_Lo95')
#   uniq_names <- paste(well2,names,sep="")
#   colnames(final_well) <- c("datetime", uniq_names)
#   
#   
#   return(final_well)
# }

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
      filter(datetime >= mdy_h(startwell) & datetime <= mdy_h('6/12/2018 23'))
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
  
  final_well <- left_join(final_well, rain_df_clean, by='datetime')
  
  #####################
  ##### Forecasting ####
  #####################
  
  # Run function to get forecast
  # final_well <- pred_model5(rain_n_well) # DISFUNCTIONAL FUNCTION
  
  # splitting newly assembled well data for clean model generation
  if (well == 'G-852' | well == 'G-1220_T'){end_dt <- max(well_df_clean$datetime)
  }
  
  else{end_dt <- mdy_h(endwell)}
  train <- final_well %>% select(datetime,filled,RAIN_FT) %>%
    filter(datetime >= mdy_h(startwell) & datetime <= end_dt)
  # Generating model on training data
  yearly <- 24*365.25
  x.reg <- train$RAIN_FT
  seasons <- msts(train$filled, start=1, seasonal.periods = c(yearly))
  model5 <- Arima(seasons,order=c(2,0,2), xreg=cbind(fourier(seasons,K=1),x.reg))
  
  # forecasting across last 168 days
  rain.ts <- read.zoo(train %>% select(datetime,RAIN_FT))
  rain.impute <- na.approx(rain.ts)
  rain.model <- auto.arima(rain.impute)
  autoplot(rain.impute)
  rain.preds <- forecast(rain.model, h=168)
  r.f <- rain.preds$mean
  r.f.df <- as.data.frame(r.f)
  num <- length(seasons)
  
  index <- num-(365.25*24)
  
  sine <- fourier(seasons,K=1)[,1]
  cose <- fourier(seasons,K=1)[,2]
  
  final.pred <- forecast(model5,xreg=cbind(sine[index:(index+167)],cose[index:(index+167)],r.f.df$x),h=168)

  # building df from results
  df_results <- as.data.frame(cbind(
    final.pred$mean,
    final.pred$upper[,1],
    final.pred$upper[,2],
    final.pred$lower[,1],
    final.pred$lower[,2]))
  colnames(df_results) <- c('Forecast', 'Upper80', 'Upper95', 'Low80', 'Low95') #probably an unnecessary line
  df_results$datetime <- seq(end_dt+hours(1), end_dt+hours(168), by='1 hour')
  final_well <- final_well %>%
    left_join(df_results, by="datetime")

  # Rename the column to the appropriate names
  well2 <- gsub('-','',well)
  names <- c('','_RAIN', '_Forecast', '_Up80', '_Up95', '_Lo80', '_Lo95')
  uniq_names <- paste(well2,names,sep="")
  colnames(final_well) <- c("datetime", uniq_names)
  
  # Join all the well columns together into one master dataframe
  full_df <- full_df %>% left_join(final_well, by='datetime')
    
  }

head(full_df)

full_df %>% filter(datetime <= mdy_h('6/13/2018 03') & datetime >= mdy_h('6/12/2018 22'))


# to save time on the above steps. Be careful to not save it to the Git repository. That'll eventually take up a lot of space.
save(full_df, welllist, file="Well_Viz_Full.RData") # need to add model to this save effort
load("C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Well Data/Well_Viz_Full.RData")
load("C:/Users/johnb/OneDrive/Documents/MSA/Fall 2/Well Data/Well_Viz_10_24.RData")


full_df %>% select(datetime,G852,G852_Forecast) %>% filter(is.na(!!as.symbol('G852')))
###############################
# Below is the shiny app code #
###############################

ui <- dashboardPage(
  # The UI code
  dashboardHeader(
    title='Options'),
  
  # Set up the conditional panels that are dependent on the user's first selection
  dashboardSidebar(
    sidebarMenu(id='menu1',
                menuItem('Explore', tabName='explore', icon=icon('compass')),
                menuItem('Predict', tabName='predict', icon=icon('bullseye')
                ),
                 # 'Explor' sidebar panels
                 conditionalPanel(
                   #condition = 'input.choice == "Explore"',
                   condition = 'input.menu1 == "explore"',
                    checkboxGroupInput('well_check','Well',
                                       choices=welllist,selected='G852'),
                    dateRangeInput('dateRange_Input', 'Date Range', 
                                  start='2016-01-01',
                                  end='2018-01-01', 
                                  min='2007-10-01', 
                                  max='2018-06-12'),
                    selectInput('year_Input','Year',unique(year(full_df$datetime)),selected='2009'),
                    selectInput('month_Input','Month',''),
                    selectInput('day_Input','Day','')),
                 # 'Predict' sidebar panels
                 conditionalPanel(
                   #condition = 'input.choice == "Predict"',
                   condition = 'input.menu1 == "predict"',
                   selectInput('well_Input','Well',welllist,selected='G852'),
                   numericInput('range_Input','Hours Predicted (max. 168)',68,0,168,1),
                   radioButtons('decomp_Input','Effects',choices=c('Rain','Seasonal'),selected='Rain'),
                   dateInput('start_date','Initial Plot Date',value='2018-06-01',min='2007-10-01',max='2018-07-01')
                 )
                )),

  dashboardBody(
    mainPanel(
      tabItems(
        tabItem(tabName='explore',
                fluidRow(
                  box(#title='Timeseries Plot of Selected Well(s)',
                      plotOutput('timeOutput'), width=12),
                  box(#title='Well Elevation on Selected Date',
                      plotOutput('dateOutput'), width=12)
                )
        ),
        tabItem(tabName='predict',
                fluidRow(
                  box(#title='Forecast for Selected Well',
                      plotOutput('predictOutput'), width=12),
                  conditionalPanel(
                    condition = 'input.decomp_Input == "Rain"',
                    box(#title='Rain Influence on Predictions',
                        plotOutput('rainefctOutput'),width=12)
                  ),
                  conditionalPanel(
                    condition = 'input.decomp_Input == "Seasonal"',
                    box(#title='Seasonal Influence on Predictions',
                        plotOutput('seasefctOutput'),width=12)
                  )

                ))
      ))
    # 'Explore' panels
    # conditionalPanel(
    #   condition = 'input.choice == "Explore"',
    #     h4('Timeseries Plot of Selected Well'),
    #     plotOutput('timeOutput'),
    #     br(),
    #     h4('Well Heights on Selected Date'),
    #     plotOutput('dateOutput')),
    # br(),
    # # 'Predict' panels
    # conditionalPanel(
    #   condition = 'input.choice == "Predict"',
    #   h4('Well Prediction for Selected Well and Hours'),
    #   plotOutput('predictOutput'),
    #   br(),
    #   h4('Rain Measurements'),
    #   plotOutput('rainOutput')),
    # br()))
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
  
  observe({
    print(input$well_Input)
    print(input$range_Input)
  })
  
  reactive_data_year <- reactive({
    full_df %>% filter(year(datetime) == input$year_Input) 
  })
  
  reactive_TS_date <- reactive({
    as.POSIXct(input$dateRange_Input)
  })
  

  # Need observe function to make the dropdown menu option reactive
  # observe({
  #   updateDateRangeInput(session,'dateRange_Input',
  #                        start=min(reactive_data_well()$datetime),
  #                        end=max(reactive_data_well()$datetime))
  # })
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
      alphas <- c(1,0.7,0.5,0.3)
      if(length(input$well_check) == 1){
        a2 <- alphas[1]
      }
      else if(length(input$well_check) < 5){
        a2 <- alphas[2]
      }
      else if(length(input$well_check) < 9){
        a2 <- alphas[3]
      }
      else{
        a2 <- alphas[4]
      }
      
      cbbPalette <- c('G852'='#000000','F45'='#a6cee3','F179'='#1f78b4','F319'='#b2df8a','G561_T'='#33a02c',
                      'G580A'='#fb9a99','G860'='#e31a1c','G1220_T'='#fdbf6f','G1260_T'='#ff7f00',
                      'G2147_T'='#cab2d6','G2866_T'='#6a3d9a','G3549'='#ffff99','PB1680_T'='#b15928')            
      output$timeOutput <- renderPlot({
        p <- ggplot(reactive_data_well(), aes(x=datetime, y=depth, color=well)) + geom_line(alpha=a2) +
          xlim(reactive_TS_date())
        #TODO attempts at this failed: +geom_vline(xintercept = ) 
        
        # Need better colors
        #cbbPalette <- c('#000000','#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
        #                '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
        
        
        p <- p + theme(legend.position='right') +
          labs(y='Well Elevation (ft)', x='Year') + scale_color_manual(values=cbbPalette) + 
          guides(color=guide_legend(title='Well'))+theme_minimal()+ ggtitle("Timeseries Plot of Selected Well(s)")+
          theme(axis.title=element_text(size=20),plot.title=element_text(size=28, hjust=0.5),
                axis.text = element_text(size=12), 
                legend.text=element_text(size=12),
                legend.title=element_text(size=12))
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
      cols = c('TRUE'='#00BFC4','FALSE'='#F8766D')
      output$dateOutput <- renderPlot({
        ggplot(reactive_data_date(), aes(x=well,y=depth,fill=sign)) +
          geom_col() +
          labs(x='Well',y='Well Elevation (ft)') +
          guides(fill=F) + geom_text(aes(label=round(depth, digits=2), vjust = ifelse(depth >= 0, -0.25, 1.25)), size=5) +
          scale_fill_manual(values=cols)+theme_minimal()+ggtitle("Well Elevation on Selected Date")+
          theme(axis.title=element_text(size=20),plot.title=element_text(size=28, hjust=0.5),
                axis.text = element_text(size=12))
        #cale_fill_manual(values=c('red','blue'))
      }) 
    }
  })
  
  vars <- c('_Forecast','_Up80','_Up95','_Lo80','_Lo95')
  
  reactive_predict <- reactive({full_df %>% select(datetime,input$well_Input,paste(input$well_Input,vars,sep='')) %>%
      filter(!is.na(!!as.symbol(input$well_Input)) | !is.na(!!as.symbol(paste(input$well_Input,'_Forecast',sep=''))))})
  
  reactive_rain_pred <- reactive({full_df %>% select(datetime,paste(input$well_Input,'_RAIN',sep=''),
                                                     paste(input$well_Input,'.rain.efct',sep=''),
                                                     paste(input$well_Input,'.seas.efct',sep=''))})
  
  observe({
  
  # need to use the as.symbol function to make the string into a symbol so the filter function works
  # reactive_predict <- reactive({full_df %>% select(datetime,input$well_Input,paste(input$well_Input,vars,sep='')) %>%
  #     filter(!is.na(!!as.symbol(input$well_Input)) | !is.na(!!as.symbol(paste(input$well_Input,'_Forecast',sep=''))))})
  # 
  # reactive_rain_pred <- reactive({full_df %>% select(datetime,paste(input$well_Input,'_RAIN',sep=''),
  #                                                    paste(input$well_Input,'.rain.efct',sep=''),
  #                                                    paste(input$well_Input,'.seas.efct',sep=''))})
  
    updateDateInput(session,'start_date',
                    min=(max(reactive_predict()$datetime)-years(1)),
                    max=(max(reactive_predict()$datetime)-days(14)))
  
    
    # if(input$start_date == ''){
    #   return()
    # }
    # else{
    
    
  # https://stackoverflow.com/questions/17148679/construct-a-manual-legend-for-a-complicated-plot
  output$predictOutput <- renderPlot({ggplot(reactive_predict(), aes_string(x='datetime',y=paste(input$well_Input,'_Forecast',sep=''))) +
        geom_line(color='#F8766D') +
        geom_vline(xintercept=max((reactive_predict() %>% filter(!is.na(!!as.symbol(input$well_Input))))$datetime), linetype=2, alpha=0.7) +
        geom_line(aes_string(y=input$well_Input)) +
        geom_line(aes_string(y=paste(input$well_Input,'_Up95',sep='')),color='#00BFC4',alpha=0.7) +
        geom_line(aes_string(y=paste(input$well_Input,'_Lo95',sep='')),color='#00BFC4',alpha=0.7) +
        scale_x_datetime(limits=c(as.POSIXct(ymd(input$start_date)),(max(reactive_predict()$datetime) - hours(168-input$range_Input)))) +
        #scale_y_continuous(limits=c(min(reactive_predict() %>% select(input$well_Input)) - 1, max(reactive_predict() %>% select(input$well_Input)) + 1)) +
        geom_line(aes_string(y=paste(input$well_Input,'_Up80',sep='')),color='#00BFC4',linetype=2) +
        geom_line(aes_string(y=paste(input$well_Input,'_Lo80',sep='')),color='#00BFC4',linetype=2) +
        labs(x='Time',y='Well Elevation (ft)')+ggtitle("Forecast for Selected Well")+theme_minimal()+
        guides(color = guide_legend(order = 1), lines = guide_legend(order = 2)) +
        theme(axis.title=element_text(size=20),
              plot.title=element_text(size=28, hjust=0.5),
              axis.text = element_text(size=12),
              plot.margin = unit(c(5,40,5,5),'points'))
    })
    
    # if(input$start_date == ''){
    #   return()
    # }
    # else{
    output$rainefctOutput <- renderPlot({
      ggplot(reactive_rain_pred(), aes(x=datetime)) +
        geom_line(aes_string(y=paste(input$well_Input,'.rain.efct',sep='')), linetype=5) +
        geom_vline(xintercept=max((reactive_predict() %>% filter(!is.na(!!as.symbol(input$well_Input))))$datetime), linetype=2, alpha=0.7) +
        geom_histogram(stat='identity',aes_string(y=paste(input$well_Input,'_RAIN*12',sep='')),fill='#00BFC4') +
        scale_x_datetime(limits=c(as.POSIXct(ymd(input$start_date)),(max(reactive_predict()$datetime) - hours(168-input$range_Input)))) +
        scale_y_continuous(sec.axis = sec_axis(~.*12, name = "Rainfall (in)")) +
        labs(x='Time',y='Rain Effect (ft)')+ theme_minimal()+ggtitle("Rain Influence")+
        theme(axis.title=element_text(size=20),
              plot.title=element_text(size=28, hjust=0.5),
              axis.text = element_text(size=12), 
              axis.title.y.right = element_text(color = '#00BFC4'),
              axis.text.y.right = element_text(color ='#00BFC4', margin = margin(t = 0, r = 0, b = 0, l = 10)),
              plot.margin = unit(c(5,5,5,5),'points')
              )
    })
    
    output$seasefctOutput <- renderPlot({
      ggplot(reactive_rain_pred(), aes(x=datetime)) +
        geom_line(aes_string(y=paste(input$well_Input,'.seas.efct',sep=''))) +
        geom_vline(xintercept=max((reactive_predict() %>% filter(!is.na(!!as.symbol(input$well_Input))))$datetime), linetype=2, alpha=0.7) +
        scale_x_datetime(limits=c(as.POSIXct(ymd(input$start_date)),(max(reactive_predict()$datetime) - hours(168-input$range_Input)))) + 
        labs(x='Time',y='Seasonal Effect (ft)')+theme_minimal()+ggtitle('Seasonal Influence on Predictions')+
        theme(axis.title=element_text(size=20),
              plot.title=element_text(size=28, hjust=0.5),
              axis.text = element_text(size=12),
              plot.margin = unit(c(5,40,5,5),'points')) 

    })
  })
}

# Call the app
shinyApp(ui=ui, server=server, options=list(height=1080))
