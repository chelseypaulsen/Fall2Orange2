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

wells <- c('F-45','F-179','F-319','G-561_T','G-580A','G-860','G-1220_T',
           'G-1260_T','G-2147_T','G-2866_T','G-3549','PB-1680_T')

start <- ymd_h("2007/10/01 00", tz='UTC')
end <- ymd_h("2018/06/12 23", tz='UTC')
startday <- as.numeric(strftime(start, format='%j'))
hourseq <- seq(start,end, by='hours')

full_df <- data.frame(datetime=hourseq)

for (well in wells){
  if (well == 'G-852'){
    df <- read_excel(paste(data.dir,well,'.xlsx',sep=''),sheet='Well',
                    col_names=c('date','time','tz_cd','well_ft','Code',
                               'Corrected'),skip=32)
    well_df <- data.frame(df)
    head(well_df)
    well_df_clean <- mutate(well_df, time=hour(time))  # adds date to datetime
    well_df_clean$datetime <- as.POSIXct(paste(well_df_clean$date,well_df_clean$time), format='%Y-%m-%d %H',tz='UTC')
    well_df_clean <- well_df_clean %>%    # summarizes to hourly data and
      group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
      summarise(well_ft=mean(Corrected)) %>%
      filter(datetime >= ymd_hm('2007-10-01 01:00') &    # filters to dates defined in Simmons instructions
               datetime <= ymd_hm('2018-06-12 23:00'))
    well_df_clean <- well_df_clean %>% select(datetime,well_ft)
    timeseries <- ts(well_df_clean$well_ft, start=c(2007,startday*24), frequency=(365.25*24))
    imputed <- na.seadec(timeseries, algorithm='locf')
    well_df_clean$filled <- imputed
    names(well_df_clean)[names(well_df_clean) == 'filled'] <- paste('filled',well,sep='_')
    well_df_clean <- well_df_clean %>% select(filled, datetime)
    full_df <- full_df %>% left_join(well_df_clean, by='datetime')
  }
  else{
  df <- read_xlsx(paste(data.dir,well,'.xlsx',sep=''),sheet='Well')
  well_df <- data.frame(df)
  head(well_df)
  well_df_clean <- mutate(well_df, time=hour(time))  # adds date to datetime
  well_df_clean$datetime <- as.POSIXct(paste(well_df_clean$date,well_df_clean$time), format='%Y-%m-%d %H',tz='UTC')
  well_df_clean <- well_df_clean %>%    # summarizes to hourly data and
    group_by(datetime) %>%              # averages Corrected values when multiple rows have same datetime
    summarise(well_ft=mean(Corrected)) %>%
    filter(datetime >= ymd_hm('2007-10-01 01:00') &    # filters to dates defined in Simmons instructions
             datetime <= ymd_hm('2018-06-12 23:00'))
  well_df_clean <- well_df_clean %>% select(datetime,well_ft)
  timeseries <- ts(well_df_clean$well_ft, start=c(2007,startday*24), frequency=(365.25*24))
  imputed <- na.seadec(timeseries, algorithm='locf')
  well_df_clean$filled <- imputed
  well_df_clean <- well_df_clean %>% select(datetime, filled)
  names(well_df_clean)[names(well_df_clean) == 'filled'] <- paste('filled',gsub('-','',well),sep='_')
  full_df <- full_df %>% left_join(well_df_clean, by='datetime')
  }}
head(full_df)

ggplot(full_df, aes(x=datetime,y=filled_G580A)) +
  geom_line()

names(full_df)
