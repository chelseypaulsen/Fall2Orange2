
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

##################################################  HW1  #################################################################

#well<- read_xlsx("C:\\Users\\chels\\Desktop\\MSA\\Fall 2\\Time Series 2\\Well Data\\G-3549.xlsx",3)
well<- read_xlsx("C:\\Users\\Steven\\Documents\\MSA\\Analytics Foundations\\lab and hw\\Time Series\\G-3549.xlsx",3)
well_df <- data.frame(well)

well_df_clean <- mutate(well_df, Datetime=date(date))  # adds date to datetime
hour(well_df_clean$Datetime) <- hour(well_df_clean$time) # Adds hour to datetime. Removes minutes from all hours
well_df_clean$Datetime <- as.POSIXct(well_df_clean$Datetime)  # change time type of newly created Datetime
well_df_clean <- well_df_clean %>%    # summarizes to hourly data and  
  group_by(Datetime) %>%              # averages Corrected values when multiple rows have same datetime
  summarise(well_ft=mean(Corrected)) %>%
  filter(Datetime >= ymd_hm('2007-10-01 01:00') &    # filters to dates defined in Simmons instructions
           Datetime <= ymd_hm('2018-06-12 23:00'))

dim(well_df_clean)
#92934

# create df of hourly dates in daterange, then join well data
# df has all hours, but some hours will have NA for well_ft
datetimes <- as.data.frame(seq(ymd_hm('2007-10-01 01:00'), ymd_hm('2018-06-12 23:00'), by='1 hour'))
names(datetimes) <- c('Datetime')
df <- datetimes %>%
  left_join(well_df_clean, by='Datetime')

dim(datetimes)
#93791 --- so #857

# change to zoo object
well_ft <- read.zoo(df)

# Impute missing values (fill in well_ft NAs)
well_ft_impute <- na.approx(well_ft)

#Split into training and test
train_well = well_ft_impute[1:93623,]
test_well = well_ft_impute[93624:93791,]

#making time series plots
plot(well_ft_impute,xlab = "Time (Years)", ylab = "Corrected Well Height (feet)", main="Time Series Plot of Well 3549 Height")

#time series decomposition 
decomp <- ts(train_well, freq=8760)
new <- stl(decomp, s.window = 7)
plot(new)

#checking various seasonal components
#checking yearly seasons  (check)
decomp <- ts(train_well, freq=8766)
new <- stl(decomp, s.window = 7)
plot(new)
#yearly seasons present

#6months
decomp2 <- ts(train_well, freq=4383)
new <- stl(decomp2, s.window = 7)
plot(new)
#6 month seasons present

#checking for monthly seasonal component (29.5 days-based on lunar calendar)
well_mth <- ts(train_well[1:4248,]) #truncating to view more easily
decomp_mth <- ts(well_mth, freq=708)
new_mth <- stl(decomp_mth, s.window=7)
plot(new_mth)
#monthly season present

#checking for weekly seasonal component
well_wk <- ts(train_well[1:1000,])
decomp_wk <- ts(well_wk, freq=168)
new_wk <- stl(decomp_wk, s.window=7)
plot(new_wk)
#no weekly season

#checking for daily seasonal component
well_day <- ts(train_well[1:480,])
decomp_day <- ts(well_day, freq=24)
new_day <- stl(decomp_day, s.window=7)
plot(new_day)
#no daily season 

#checking lunar season
well_lun <- ts(train_well[1:480,])
decomp_lun <- ts(well_lun, freq=27.3)
new_lun <- stl(decomp_lun, s.window=7)
plot(new_lun)
#no lunar season

#checking tidal season
well_tide  <- ts(train_well[1:480,])
decomp_tide <- ts(well_tide, freq=12.5)
new_tide <- stl(decomp_tide, s.window=7)
plot(new_tide)

#ADF test- deterministic/stochastic seasons? 
adf.test(decomp, alternative = "stationary", k = 0) #p=0.01, so reject null, so deterministic seasonal

#export to do arima analysis in sas
# write.foreign(well_ft_impute, "C:\\Users\\Allison\\Documents\\Time Series\\well_clean.txt", 
#               "C:\\Users\\Allison\\Documents\\Time Series\\well_clean.sas", package = "SAS")
yearly = 24*365.25
monthly = 24*(365.25/12)
tidaly = 24*(365.25/24.83)
weekly = 24*(365.25/52)
daily = 24
biannual = 24*(365.25/2)

#trying with two seasons
seasons <- msts(train_well, start=1, seasonal.periods = c(yearly, monthly, biannual))

#fitting sine/cosine with fourier
arima.3<-Arima(seasons,order=c(4,1,6), xreg=fourier(seasons,K=c(2,2,2)))
summary(arima.3)

#autoplot of focasted data
#arima.3 %>%
 # forecast(xreg=fourier(seasons, K=5, h=8760)) %>%
  #autoplot() 

#acf and pacf plots
acf(arima.3$residuals)
pacf(arima.3$residuals)

#White noise plots
White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(arima.3$residuals, lag = i, type = "Ljung", fitdf = 4)$p.value
}
#fitdf has to equal the highest ar or ma term

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")


#plotting forecasted and test values for last week
arima.4<-Arima(train_well,order=c(4,1,6))#, xreg=fourier(seasons,K=c(2,2,2)))
summary(arima.4)
f1=forecast(arima.4, h=168)#,xreg=fourier(test_seasons,K=c(2,2,2)))

plot(test_well, col="red", ylim=c(.1,.6))
lines(f1$mean)
lines(f1$upper[,2])
lines(f1$lower[,2])

