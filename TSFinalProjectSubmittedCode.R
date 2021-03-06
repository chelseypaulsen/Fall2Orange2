rm(list=ls())

library(readxl)
library(zoo)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(seasonal)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)
library(foreign)
library(caschrono)
library(TSA)
library(quantmod)
library(tidyr)

#file.dir <- "C:\\Users\\Allison\\Documents\\Time Series\\"
# file.dir <- "C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\Well Data\\"
file.dir <- "C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\Forecasting\\data\\Well Data\\"

#well data read in and clean
#well<- read_xlsx("C:\\Users\\Allison\\Documents\\Time Series\\G-3549.xlsx",3)
#well<- read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\Well Data\\G-3549.xlsx",3)
well <- read_xlsx(paste(file.dir, "G-3549.xlsx",sep=""),3)
well_df <- data.frame(well)

well_df_clean <- mutate(well_df, Datetime=date(date))  # adds date to datetime
hour(well_df_clean$Datetime) <- hour(well_df_clean$time) # Adds hour to datetime. Removes minutes from all hours
well_df_clean$Datetime <- as.POSIXct(well_df_clean$Datetime)  # change time type of newly created Datetime
well_df_clean <- well_df_clean %>%    # summarizes to hourly data and  
  group_by(Datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected)) %>%
  filter(Datetime >= ymd_hm('2007-10-01 01:00') &    # filters to dates defined in Simmons instructions
           Datetime <= ymd_hm('2018-06-12 23:00'))

#tide data read in and clean
#tide<-read_xlsx("C:\\Users\\Allison\\Documents\\Time Series\\G-3549.xlsx",2)
#tide<-read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\Well Data\\G-3549.xlsx",2)
tide <- read_xlsx(paste(file.dir, "G-3549.xlsx",sep=""),2)
tide_df <- data.frame(tide)

tide_df_clean <- mutate(tide_df, Datetime=date(Date))  # adds date to datetime
hour(tide_df_clean$Datetime) <- hour(tide_df_clean$Time) # Adds hour to datetime. Removes minutes from all hours
tide_df_clean$Datetime <- as.POSIXct(tide_df_clean$Datetime)  # change time type of newly created Datetime
tide_df_clean <- tide_df_clean %>%    # summarizes to hourly data and 
  group_by(Datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(Tide_ft=mean(Tide_ft)) %>%
  filter(Datetime >= ymd_hm('2007-10-01 01:00') &    # filters to dates defined in Simmons instructions
           Datetime <= ymd_hm('2018-06-12 23:00'))

#rain data read in and clean
#rain<-read_xlsx("C:\\Users\\Allison\\Documents\\Time Series\\G-3549.xlsx",1)
#rain<-read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\Well Data\\G-3549.xlsx",1)
rain <- read_xlsx(paste(file.dir, "G-3549.xlsx",sep=""),1)
rain$date<- as.Date(rain$Date)
rain$time<- format(as.POSIXct(rain$Date, "%H:%M:%S"))
rain_df <-data.frame(rain)

rain_df_clean <- mutate(rain_df, Datetime=date(date))  # adds date to datetime
hour(rain_df_clean$Datetime) <- hour(rain_df_clean$time) # Adds hour to datetime. Removes minutes from all hours
rain_df_clean$Datetime <- as.POSIXct(rain_df_clean$Datetime)  # change time type of newly created Datetime
rain_df_clean <- rain_df_clean %>%    # summarizes to hourly data and  
  group_by(Datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(RAIN_FT=mean(RAIN_FT)) %>%
  filter(Datetime >= ymd_hm('2007-10-01 01:00') &    # filters to dates defined in Simmons instructions
           Datetime <= ymd_hm('2018-06-12 23:00'))


# create df of hourly dates in daterange, then join well data
# df has all hours, but some hours will have NA for well_ft
datetimes <- as.data.frame(seq(ymd_hm('2007-10-01 01:00'), ymd_hm('2018-06-12 23:00'), by='1 hour'))
names(datetimes) <- c('Datetime')
df1 <- datetimes %>%
  left_join(well_df_clean, by='Datetime')

df2 <- datetimes %>%
  left_join(tide_df_clean, by='Datetime')

df3<-datetimes %>%
  left_join(rain_df_clean, by='Datetime') 

# change to zoo object
well_ft <- read.zoo(df1)
tide_ft <- read.zoo(df2)
rain_ft <- read.zoo(df3)

#sum(is.na(rain_df_clean))

# Impute missing values (fill in well_ft NAs)
well_ft_impute <- na.approx(well_ft)
well_ft_df <- data.frame(well_ft_impute)

df_well <- read.csv('C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\well_ft_impute.csv')

df4<-cbind(datetimes,df_well$x,df2$Tide_ft,df3$RAIN_FT)
write.csv(df4, "C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\df4.csv")


write.csv(well_ft_impute, file="C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\well_ft_impute.csv")
write.csv(tide_ft, file="C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\tide_ft.csv")
write.csv(rain_ft, file="C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\rain_ft.csv")

#Split into training and test
train_well = well_ft_impute[67325:93623,] #just using last 3 years to speed processing
test_well = well_ft_impute[93624:93791,]

train_tide = df2$Tide_ft[67325:93623]
test_tide = df2$Tide_ft[93624:93791]

train_rain = df3$RAIN_FT[67325:93623]
test_rain = df3$RAIN_FT[93624:93791]

#impact of tide

plot(train_rain)
plot(train_tide)
plot(train_well)
plot(test_rain, type='h')

#Select model creation
yearly = 24*365.25

seasons <- msts(train_well, start=1, seasonal.periods = c(yearly))

#fitting sine/cosine with fourier
x.reg=train_rain
model5<-Arima(seasons,order=c(2,0,2), xreg=cbind(fourier(seasons,K=1),x.reg))
summary(model5)

#acf and pacf plots
acf(model5$residuals)
pacf(model5$residuals)


#White noise plots
White.LB <- rep(NA, 48)
for(i in 24:48){
  White.LB[i] <- Box.test(model5$residuals, lag = i, type = "Ljung", fitdf = 24)$p.value
}
#fitdf has to be the sum of ar and ma terms

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")
dim()

#plotting forecasted and test values for last week

# change to zoo object
rain_ft <- read.zoo(df3)

# Impute missing values (fill in well_ft NAs)
rain_ft_impute <- na.approx(rain_ft)
model.rain=auto.arima(rain_ft_impute)
rain.future=forecast(model.rain,h=168)
r.f=rain.future$mean

seasons2 <- msts(test_well, start=1, seasonal.periods = c(yearly))
newx=cbind(r.f)

final.pred=forecast(model5,xreg=cbind(fourier(seasons2,K=1),newx),h=168) 

############ FINAL PLOTS ###############

#Building dataframe of results cause my head was going to explode with plotting TimeSeries objects...
df_results <- as.data.frame(cbind( 
  final.pred$mean, 
  final.pred$upper[,1],
  final.pred$upper[,2], 
  final.pred$lower[,1], 
  final.pred$lower[,2]))
colnames(df_results) <- c('Forecast', 'Upper80', 'Upper95', 'Low80', 'Low95')
df_results$Date.H <- index(test_well) #can't mutate or bind, as it'll lose POSIX format
pred_start = df_results$Date.H[1] # use first point of predictions as basis for actual data extents
pred_start 

#apply filter to imputed well data, then join df_results also add join here
df_plot <- as.data.frame(well_ft_impute) #inclusion of all well data allows for display of training data if desired
df_plot$Date.H <- index(well_ft_impute)  # again can't mutate or bind w/o trouble
df_plot <- df_plot %>%
  filter(Date.H >= (as.POSIXct(pred_start)-1*86400)) %>% # subtracting days * seconds per day, for glimpse at training data
  left_join(df_results, by="Date.H") %>%
  rename(Actual= well_ft_impute)
str(df_plot) #all well data and predictions, trimmed for graph

#Actually plotting
plt1 <- ggplot(df_plot, aes(x = Date.H)) +   
  ggtitle("Well G-3549 Forecast") + xlab("Date") + ylab("Water Table Elevation (FT-MSL)") +
  #labs(subtitle = "2018 Results")+
  geom_ribbon(aes(ymin = Low95, ymax = Upper95, fill = "95%"), alpha=.5) +
  geom_ribbon(aes(ymin = Low80, ymax = Upper80, fill = "80%"), alpha=.7) +
  geom_line(aes(y = Actual, group = 1, colour = "Actual"), size = 1, alpha=.7) +
  geom_line(aes(y = Forecast, group = 2, colour = "Forecast"), size = 1, lty=2) +
  scale_colour_brewer(name = "Legend", type = "qual", palette = "Dark2") +
  scale_fill_brewer(name = "Intervals") +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  theme_grey()
plt1

#Getting mape and rmse for the test set
rmse= sqrt(sum(as.numeric(final.pred$mean)-as.numeric(test_well))**2/168)
MAPE= sum(abs(as.numeric(test_well)-as.numeric(final.pred$mean))/as.numeric(test_well))/168
rmse_rain= sqrt(sum(as.numeric(test_rain)-as.numeric(newx))**2/168)
rmse_rain
rmse
