#cci30 data wrangling and time series analysis
#OHLCV daily values of the index
#data source: https://cci30.com/#:~:text=Crypto%20Currency%20Index%2030%20%7C%20CCi30&text=The%20CCi30%20is%20a%20rules,by%20market%20capitalization%2C%20excluding%20stablecoins.
install.load::install_load("tidyquant"
                           ,"timetk"
                           , "tibbletime"
                           , "sweep"
                           , "anomalize"
                           , "caret"
                           , "forecast"
                           , "funModeling"
                           , "xts"
                           , "fpp"
                           , "lubridate"
                           , "tidyverse"
                           , "urca"
                           , "prophet"
)
#loading in the dataset

cci<-read_csv("https://cci30.com/ajax/getIndexHistory.php")
glimpse(cci)

#contains the high,low,open,close,date and volume data
#can look at the corr among all the variables - i.e. a feature plot
library(GGally)
GGally::ggpairs(cci)
#seems to be some postivie corr among pretty much all the variables
#focusing specifically on the closing price
cls<-cci_ts[,5]
View(cls)

#fixing the date####
library(lubridate)
cci$Date<-ymd(cci$Date) #using lubricate package
cci.tibble<-as_tbl_time(cci,index=Date) #important for time subsetting
View(cci.tibble)
min.date<-min(cci.tibble$Date)
min.year<-year(min.date)
min.month<-month(min.date)
max.date<-max(cci.tibble$Date)
max.year<-year(max.date)
max.month<-month(max.date)

#making cci as a tibble
cci<-as_tibble(cci)

#can look at the daily return and log return for the given data
cci.ts.daily <-cci %>%
  tq_transmute(       #transmute is to add new data to our existing tibble 
    select = Close
    , periodReturn
    , period = "daily"
    , type = "log"
    , col_rename = "daily.Log.Returns"
  )
head(cci.ts.daily,n=2)
#and then get the get the data by month and get monthly log return
cci.ts.monthly <-cci %>%
  tq_transmute(       #transmute is to add new data to our existing tibble 
    select = Close
    , periodReturn
    , period = "monthly"
    , type = "log"
    , col_rename = "Monthly.Log.Returns"
  )

head(cci.ts.monthly,n = 5)

#to look for anomalies:
#we use anomalize package
cci_monthly_anon <- cci.ts.monthly %>%
  time_decompose(Monthly.Log.Returns, method = "twitter") %>% #we use twitter method as trend not a large factor in this case(otherwise use stl)
  anomalize(remainder, method = "gesd") %>% #gesd is better method at detecting outliers, although more time consuming
  time_recompose()

#plotting the anomalies
cci_monthly_anon%>%
  plot_anomaly_decomposition()+ #part of anomalize package
  xlab("Monthly log return")+
  ylab("value")+
  labs(title="Anomalies for CCI30 monthly log rtrn",subtitle = "method=GESD")
#can see from the graph that majority of anomalies occur during the end of 2017 crypto craze  
max.daily.log.return<-max(cci.ts.monthly$Monthly.Log.Returns)
min.daily.log.return<-min(cci.ts.monthly$Monthly.Log.Returns)
start.date<-min(cci.ts.monthly$Date)
end.date<-max(cci.ts.monthly$Date)
training.region<-round(nrow(cci.ts.monthly)*0.7,0)
test.region<-nrow(cci.ts.monthly)-training.region
training.stop.date<-as.Date(max(cci.ts.monthly$Date))%m-% months(as.numeric(test.region),abbreviate = F)
#basic plot
plot.ts(cci.ts.monthly$Monthly.Log.Returns)

#forecasting log monthly returns
#first need to split data into training and test set

train<-cci.ts.monthly%>%
  filter(Date<=training.stop.date)

test<-cci.ts.monthly%>%
  filter(Date>training.stop.date)
#adding ts to training set
train_aug<-train %>%
  tk_augment_timeseries_signature()

#make an extensible time series object - for xts package
#for forecsting with fpp, we will need to convert data into an xts/ts object
monthly.log.ret.ts<-ts(cci.ts.monthly$Monthly.Log.Returns,frequency = 12,start=c(min.year,min.month))
plot.ts(monthly.log.ret.ts)
monthly.xts<-as.xts(monthly.log.ret.ts)

#using arima
#first check if the data is stationary
ndiffs(monthly.log.ret.ts)
nsdiffs(monthly.log.ret.ts)
monthly.log.ret.ts.diff<-diff(monthly.log.ret.ts)
plot.ts(monthly.log.ret.ts.diff)
ggtsdisplay(monthly.log.ret.ts.diff)
checkresiduals(monthly.log.ret.ts.diff)
#no need for further differenciation so yes, data is stationary
arima_1<-auto.arima(ts(monthly.log.ret.ts))
#arima(0,0,0)
forecast(arima_1,h=1)
# Point Forecast      Lo 80     Hi 80      Lo 95     Hi 95
#66     0.05488917 -0.3109085 0.4206868 -0.5045501 0.6143284
autoplot(forecast(arima_1,h=12),PI=F)


