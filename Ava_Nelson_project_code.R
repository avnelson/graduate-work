weather <- read.csv("/Users/avanelson/Desktop/IST 707 Data Analytics/Project/austin_weather.csv")
str(weather)
hist(weather$TempHighF)
hist(weather$TempLowF)

#install.packages("metR")
#install.packages("weathermetrics")
#install.packages("ThermIndex") #install package to calc windchill
library(ggplot2)
library(stringr)
library(plyr)
library(metR)
library(weathermetrics)
library(ThermIndex)

#CLEAN UP DATA---------------------------------
str(weather) 

weather$Date <- as.Date(weather$Date)
weather$TempHighF <- as.numeric(weather$TempHighF)
weather$TempAvgF <- as.numeric(weather$TempAvgF)
weather$TempLowF <- as.numeric(weather$TempLowF)
weather$DewPointHighF <- as.numeric(weather$DewPointHighF)
weather$DewPointAvgF <- as.numeric(weather$DewPointAvgF)
weather$DewPointLowF <- as.numeric(weather$DewPointLowF)
weather$HumidityHighPercent <- as.numeric(weather$HumidityHighPercent)
weather$HumidityAvgPercent <- as.numeric(weather$HumidityAvgPercent)
weather$HumidityLowPercent <- as.numeric(weather$HumidityLowPercent)
weather$SeaLevelPressureHighInches <- as.numeric(weather$SeaLevelPressureHighInches)
weather$SeaLevelPressureAvgInches <- as.numeric(weather$SeaLevelPressureAvgInches)
weather$SeaLevelPressureLowInches <- as.numeric(weather$SeaLevelPressureLowInches)
weather$VisibilityHighMiles <- as.numeric(weather$VisibilityHighMiles)
weather$VisibilityAvgMiles <- as.numeric(weather$VisibilityAvgMiles)
weather$VisibilityLowMiles <- as.numeric(weather$VisibilityLowMiles)
weather$WindHighMPH <- as.numeric(weather$WindHighMPH)
weather$WindAvgMPH <- as.numeric(weather$WindAvgMPH)
weather$WindGustMPH <- as.numeric(weather$WindGustMPH)
weather$PrecipitationSumInches <- as.numeric(as.character(weather$PrecipitationSumInches))
weather$Events <- gsub("[[:space:]]", "", weather$Events) #remove white space
weather$Events <- gsub("", "Clear", weather$Events) #replace blank with descriptor
weather$Events <- as.factor(weather$Events) #convert events to a factor
levels(weather$Events)
levels(weather$Events) <- c("Clear", "Fog", "FogRain", "FogRainTStorm", "FogTStorm", "Rain", "RainSnow", "RainTStorm", "TStorm")
weather$Season <- season(weather$Date, lang="en") #make date discrete season
levels(weather$Season) <-c("Winter", "Spring", "Summer", "Fall")
weather$PrecipitationSumInches[is.na(weather$PrecipitationSumInches)] <-0.01 #replace trace NA with small value

#HEAT INDEX---------------------------------
weather$HeatIndexAvg <- heat.index(t=weather$TempAvgF, rh=weather$HumidityAvgPercent, temperature.metric = "fahrenheit")
weather$HeatIndexHigh <- heat.index(t=weather$TempHighF, rh=weather$HumidityAvgPercent, temperature.metric = "fahrenheit")
max(weather$TempAvgF)
max(weather$HeatIndexAvg)
max(weather$TempHighF)
max(weather$HeatIndexHigh)


#WIND CHILL----------------------------------
weather$TempAvgC <- tempftoc(weather$TempAvgF) #mean temp to celcius
weather$WindAvgMS <- mphtoms(weather$WindAvgMPH) #wind velocity to m/s
weather$WindGustMS <- mphtoms(weather$WindGustMPH) #wind gust to m/s

weather$WindChillAvgC <- wc(weather$TempAvgC, weather$WindAvgMS) #average daily windchill
weather$WindChillGustC <- wc(weather$TempAvgC, weather$WindGustMS) #windchill with gust

weather$WindChillAvgF <- (weather$WindChillAvgC*1.8)+32 #convert back to F
weather$WindChillGustF <- (weather$WindChillGustC*1.8)+32

str(weather)

#INITIAL VISUALIZATION---------------
colorsIV <- c("Daily High"="#E52F2E", "Avg Heat Index"="#D14333", "Max Heat Index"="#F37321", 
              "Daily Low"="#5B72B7", "Avg Wind Chill"="#6255A4", "Max Wind Chill"="#7F4B9E",
              "Average Temperature"="#70C281")
ggplot(data=weather) +
  geom_line(aes( x=Date,
                 y=TempHighF,
                 color="Daily High")) +
  geom_line(aes(x=Date,
                y=HeatIndexAvg,
                color="Avg Heat Index"),
            linetype="dotdash") +
  geom_line(aes(x=Date,
                y=HeatIndexHigh,
                color="Max Heat Index"),
            linetype="longdash") +
  geom_line(aes(x=Date,
                y=TempLowF, 
                color="Daily Low")) +
  geom_line(aes(x=Date,
                y=WindChillAvgF, 
                color="Avg Wind Chill"),
            linetype="dotdash") +
  geom_line(aes(x=Date,
                y=WindChillGustF,
                color="Max Wind Chill"),
            linetype="longdash") +
  geom_line(aes(x=Date,
                y=TempAvgF, 
                color="Average Temperature")) +
  scale_color_manual(values=colorsIV, name="Temp Values") +
  labs(title="Austin Temperature in Fahrenheit 2014-17", 
       y="Temperature (F)")


#SIMPLIFY DATAFRAME TO LOOK FOR OUTLIERS
weatherSimp <- weather[,c("Date", "TempHighF", "TempAvgF", "TempLowF", "WindGustMPH", 
                          "HeatIndexAvg", "HeatIndexHigh", "WindChillAvgF", "WindChillGustF", 
                          "PrecipitationSumInches", "Events", "Season")]
str(weatherSimp) #isolate columns I want


weatherMatrix <- as.matrix(weather[,c("TempHighF", "TempAvgF", "TempLowF", "WindGustMPH", 
                                      "HeatIndexAvg", "HeatIndexHigh", "WindChillAvgF", "WindChillGustF", 
                                      "PrecipitationSumInches")])
heatmap(weatherMatrix, scale="column") #heatmap to observe trends


#SCATTERPLOTS-----------------------------
#COLOR BY EVENT
ggplot(data=weatherSimp, aes(x=TempHighF)) +
  geom_point(aes(y=TempLowF, size=PrecipitationSumInches, color=Events, shape=Season)) +
  labs(title="Scatter Plot by Event")
#as Heat Index
ggplot(data=weatherSimp, aes(x=TempHighF)) +
  geom_point(aes(y=HeatIndexAvg, size=PrecipitationSumInches, color=Events, shape=Season)) +
  labs(title="Avg Heat Index by Event")
ggplot(data=weatherSimp, aes(x=TempHighF)) +
  geom_point(aes(y=HeatIndexHigh, size=PrecipitationSumInches, color=Events, shape=Season)) +
  labs(title="Top Heat Index by Event")
#as Wind Chill
ggplot(data=weatherSimp, aes(x=TempLowF)) +
  geom_point(aes(y=WindChillAvgF, size=PrecipitationSumInches, color=Events, shape=Season)) +
  labs(title="Avg Wind Chill by Event")
ggplot(data=weatherSimp, aes(x=TempLowF)) +
  geom_point(aes(y=WindChillGustF, size=PrecipitationSumInches, color=Events, shape=Season)) +
  labs(title="Max Wind Chill by Event")

#COLOR BY SEASON
ggplot(data=weatherSimp, aes(x=TempHighF)) +
  geom_point(aes(y=TempLowF, size=PrecipitationSumInches, shape=Events, color=Season)) +
  scale_shape_manual(values=1:nlevels(weather$Events))+
  labs(title="Scatter Plot by Season")
#as Heat Index
ggplot(data=weatherSimp, aes(x=TempHighF)) +
  geom_point(aes(y=HeatIndexAvg, size=PrecipitationSumInches, shape=Events, color=Season)) +
  scale_shape_manual(values=1:nlevels(weather$Events))+
  labs(title="Avg Heat Index by Season")
ggplot(data=weatherSimp, aes(x=TempHighF)) +
  geom_point(aes(y=HeatIndexHigh, size=PrecipitationSumInches, shape=Events, color=Season)) +
  scale_shape_manual(values=1:nlevels(weather$Events))+
  labs(title="Top Heat Index by Season")
#as Wind Chill
ggplot(data=weatherSimp, aes(x=TempLowF)) +
  geom_point(aes(y=WindChillAvgF, size=PrecipitationSumInches, shape=Events, color=Season)) +
  scale_shape_manual(values=1:nlevels(weather$Events))+
  labs(title="Avg Wind Chill by Season")
ggplot(data=weatherSimp, aes(x=TempLowF)) +
  geom_point(aes(y=WindChillGustF, size=PrecipitationSumInches, shape=Events, color=Season)) +
  scale_shape_manual(values=1:nlevels(weather$Events))+
  labs(title="Max Wind Chill by Season")

#WINDCHILL AND HEAT INDEX
ggplot(data=weatherSimp, aes(x=HeatIndexAvg)) +
  geom_point(aes(y=WindChillAvgF, size=PrecipitationSumInches, shape=Events, color=Season)) +
  scale_shape_manual(values=1:nlevels(weather$Events))+
  labs(title="HI/SC by Season")
ggplot(data=weatherSimp, aes(x=HeatIndexHigh)) +
  geom_point(aes(y=WindChillGustF, size=PrecipitationSumInches, shape=Events, color=Season)) +
  scale_shape_manual(values=1:nlevels(weather$Events))+
  labs(title="Extreme HI/SC by Season")

#--------------------------------------------
#ISOLATE OUTLIERS-----------------------------
OutliersTempHigh <- boxplot(weatherSimp[,c("TempHighF")])$out
OutliersTempLow <- boxplot(weatherSimp[,c("TempLowF")])$out #no cold outliers
OutliersHeatIndex <- boxplot(weatherSimp[,c("HeatIndexHigh")])$out
OutliersHeatIndexAvg <- boxplot(weatherSimp[,c("HeatIndexAvg")])$out #no outliers
OutliersWindChill <- boxplot(weatherSimp[,c("WindChillGustF")])$out
OutliersWindChillAvg <- boxplot(weatherSimp[,c("WindChillAvgF")])$out
OutliersWind <- boxplot(weatherSimp[,c("WindGustMPH")])$out
OutliersPrecipitation <- boxplot(weatherSimp[,c("PrecipitationSumInches")])$out

HighTempOut <- weatherSimp[OutliersTempHigh,] #18 outliers
HeatOut <- weatherSimp[OutliersHeatIndex,] #2 outliers
ChillOut <- weatherSimp[OutliersWindChill,] #5 outliers
ChillOutAvg <- weatherSimp[OutliersWindChillAvg,] #3 outliers
WindOut <- weatherSimp[OutliersWind,] #15 outliers
RainOut <- weatherSimp[OutliersPrecipitation,] #49 outliers

Histogram_It <- function(DF){
  hist(DF$TempHighF)
  hist(DF$HeatIndexHigh)
  hist(DF$TempLowF)
  hist(DF$WindChillGustF)
  hist(DF$WindGustMPH)
  hist(DF$PrecipitationSumInches)
  plot(DF$Season)
}

Histogram_It(HighTempOut)
Histogram_It(HeatOut)
Histogram_It(ChillOut)
Histogram_It(WindOut)
Histogram_It(RainOut)

plot(HighTempOut$Events)
plot(HeatOut$Events)
plot(ChillOut$Events)
plot(WindOut$Events)
plot(RainOut$Events)

#------------------------------------------
#VISUALIZE THE OUTLIERS--------------------
#make into single dataframe
OutlierDF <- rbind(HighTempOut, HeatOut)
OutlierDF <- rbind(OutlierDF, ChillOut)
OutlierDF <- rbind(OutlierDF, ChillOutAvg)
OutlierDF <- rbind(OutlierDF, WindOut)
OutlierDF <- rbind(OutlierDF, RainOut) #92 observations
OutDFu <- unique(OutlierDF) #22 observations, 70 days were shared outliers

#Scatter Plots by Event (more interesting visual)
ggplot(data=WeatherFinal, aes(x=TempHighF)) +
  geom_point(aes(y=TempLowF, size=PrecipitationSumInches, color=dScores, shape=Events))+
  labs(title="Outliers by Event")
#as Heat Index
ggplot(data=DoI, aes(x=TempHighF)) +
  geom_point(aes(y=HeatIndexHigh, size=PrecipitationSumInches, color=Events, shape=Season)) +
  labs(title="Outlier Heat Index by Event")
#as Wind Chill
ggplot(data=DoI, aes(x=TempLowF)) +
  geom_point(aes(y=WindChillGustF, size=PrecipitationSumInches, color=Events, shape=Season)) +
  labs(title="Outlier Max Wind Chill by Event")

#Line Plot for Outliers
ggplot(data=DoI) +
  geom_line(aes( x=Date,
                 y=TempHighF,
                 color="Daily High")) +
  geom_line(aes(x=Date,
                y=HeatIndexAvg,
                color="Avg Heat Index"),
            linetype="dotdash") +
  geom_line(aes(x=Date,
                y=HeatIndexHigh,
                color="Max Heat Index"),
            linetype="longdash") +
  geom_line(aes(x=Date,
                y=TempLowF, 
                color="Daily Low")) +
  geom_line(aes(x=Date,
                y=WindChillAvgF, 
                color="Avg Wind Chill"),
            linetype="dotdash") +
  geom_line(aes(x=Date,
                y=WindChillGustF,
                color="Max Wind Chill"),
            linetype="longdash") +
  geom_line(aes(x=Date,
                y=TempAvgF, 
                color="Average Temperature")) +
  scale_color_manual(values=colorsIV, name="Temp Values") +
  labs(title="Outliers Austin Temperature in Fahrenheit 2014-17", 
       y="Temperature (F)")

#--------------------------------------------
#now discretize for ARM----------------------

str(weatherSimp)
min(weatherSimp$TempHighF) #32
max(weatherSimp$TempHighF) #107

min(weatherSimp$HeatIndexHigh) #32
max(weatherSimp$HeatIndexHigh) #117

min(weatherSimp$WindChillGustF) #16
max(weatherSimp$WindChillGustF) #99

min(weatherSimp$WindGustMPH) #1mph
max(weatherSimp$WindGustMPH) #37mph

min(weatherSimp$PrecipitationSumInches) #0in
max(weatherSimp$PrecipitationSumInches) #5.2in

weatherSimp$dTempHighF <- cut(weatherSimp$TempHighF, breaks= c(0, 32, 40, 50, 60, 70, 80, 90, 100, 110, Inf), 
                              labels= c("Freezing", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s", "Hell"))
weatherSimp$dHeatIndex <- cut(weatherSimp$HeatIndexHigh, breaks= c(0, 32, 40, 50, 60, 70, 80, 90, 100, 110, Inf), 
                              labels= c("Freezing", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s", "Hell"))
weatherSimp$dWindChill <- cut(weatherSimp$WindChillGustF, breaks= c(0, 32, 40, 50, 60, 70, 80, 90, 100, 110, Inf), 
                              labels= c("Freezing", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s", "Hell"))
weatherSimp$dWindMPH <- cut(weatherSimp$WindGustMPH, breaks= c(0, 10, 20, 30, 40, Inf), 
                            labels= c("0-10mpg", "11-20mph", "21-30mph", "31-40mph", "41mph+"))
weatherSimp$dPrecip <- cut(weatherSimp$PrecipitationSumInches, breaks= c(-0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, Inf), 
                           labels= c("0-0.5in", "0.5-1in", "1-1.5in", "1.5-2in", "2-2.5in", "2.5-3in", "3-3.5in", "3.5-4in", "4-4.5in", "4.5in-5in", ">5in"))
head(weatherSimp)
str(weatherSimp)

weatherDiscrete <- weatherSimp
weatherDiscrete[,2:10] <- NULL #remove non discrete variables
weatherDiscrete <- rename(weatherDiscrete, c("dTempHighF"="TempHighF", "dHeatIndex"="HeatIndex", 
                                             "dWindChill"="WindChill", "dWindMPH"="WindMPH", "dPrecip"="Precipitation"))

str(weatherDiscrete)

#TURN INTO TRANSACTION DATA---------------
library(arules)
library(arulesViz)

weatherDnoDate <- weatherDiscrete[,2:8]
weather_basket <- as(weatherDnoDate, "transactions") #convert into basketdata

itemFrequencyPlot(weather_basket, topN=20, lift=FALSE)

rules1 <- apriori(weather_basket, parameter=list(supp=0.8, conf=0.9)) #1 rule
rules2 <- apriori(weather_basket, parameter=list(supp=0.6, conf=0.6)) #4 rules
rules3 <- apriori(weather_basket, parameter=list(supp=0.4, conf=0.6)) #5 rules
rules4 <- apriori(weather_basket, parameter=list(supp=0.4, conf=0.5)) #7 rules
rules5 <- apriori(weather_basket, parameter=list(supp=0.5, conf=0.3))
rules6 <- apriori(weather_basket, parameter=list(supp=0.35, conf=0.4)) #12 rules
rules7 <- apriori(weather_basket, parameter=list(supp=0.3, conf=0.4)) #13 rules

inspect(rules7)
summary(rules7)
plot(rules7, method="graph", engine= "interactive", main="Rules Graph Plot")

# # Discretize Variables for decision tree
weather$discTemp <- cut(weather$TempAvgF, breaks = c(0,40,50,60,70,80,Inf),labels=c("40 or fewer","40-50","50-60","60-70","70-80", "greater than 80"))
weather$discDewPoint <- cut(weather$DewPointAvgF, breaks = c(0,30,40,50,60,70,Inf),labels=c("30 or fewer","30-40","40-50","50-60","60-70","greater than 70"))
weather$discHumid <- cut(weather$HumidityAvgPercent, breaks = c(0,40,50,60,70,80,90,Inf),labels=c("40 or fewer","40-50","50-60","60-70","70-80","80-90","greater than 90"))
weather$discPressure <- cut(weather$SeaLevelPressureAvgInches, breaks = c(0,29.8,30,30.2,Inf),labels=c("29.8 or fewer","29.8-30","30-30.2","greater than 30.2"))
weather$discVisibility<- cut(weather$VisibilityAvgMiles, breaks = c(0,9,Inf),labels=c("less than 10","10 and higher"))
weather$discWind <- cut(weather$WindAvgMPH, breaks = c(0,2,4,6,8,Inf),labels=c("2 or fewer","2-4","4-6","6-8","more than 8"))
# Make train and test datasets
random <- sample(1:dim(weather)[1])
cutpoint <- floor(2 * dim(weather)[1]/3)
trainweather <- weather[random[1:cutpoint],]
testweather <- weather[random[(cutpoint + 1):dim(weather)[1]],]
# J48 Decision Tree (Unused in Final Report)
library(RWeka)
model <- J48(discTemp ~ discDewPoint + discHumid + discPressure + discVisibility + discWind, data = trainweather)
summary(model)
evaluation <- evaluate_Weka_classifier(model, numFolds = 10, seed = 4, class = TRUE)
prediction <- predict (model, newdata = testweather, type = c("class"))
# Plot decision tree
library(partykit)
plot(model, gp = gpar(fontsize = 7), cex.axis=0.01)

# RPart Decision Tree
library(rpart)
library(rattle)

rpartweather <- rpart(discTemp ~ discDewPoint + discHumid + discPressure + discVisibility + discWind, data = trainweather, method = "class")
summary(rpartweather)
predictrpart <- predict(rpartweather,testweather, type = "class")
fancyRpartPlot(rpartweather)
rpartmatrix <- table(predictrpart, testweather$discTemp)
rpartmatrix

# Isolate days of interest from decision tree
weathersub <- weather[weather$discDewPoint == '30 or fewer',]
weathersub1 <- weathersub[weathersub$discHumid %in% c('50-60','60-70'),]
write.csv(weathersub1, "/Users/tejasanaka/Documents/Syracuse/IST707/TejasDays.csv")

##ISOLATE HOTTEST DAYS
HottestDays <- head(weather[order(weather$TempHighF, decreasing=TRUE),])
HottestDays
HotHIDays <- head(weather[order(weather$HeatIndexHigh, decreasing=TRUE),])
HotHIDays
HotDays <- rbind(HottestDays, HotHIDays)
HotDays <- unique(HotDays)
HotDays

##ISOLATE COLDEST DAYS
ColdDays1 <- head(weather[order(weather$TempLowF, decreasing=FALSE),])
ColdDays2 <- head(weather[order(weather$WindChillGustF, decreasing=FALSE),])
ColdDays <- rbind(ColdDays1, ColdDays2)
ColdDays <- unique(ColdDays)

##ISOLATE MOST PRECIP
RainDays <- head(weather[order(weather$PrecipitationSumInches, decreasing=TRUE),])

##ISOLATE MOST WIND
WindDays <- head(weather[order(weather$WindGustMPH, decreasing=TRUE),])

##COMPILE MOST INTERESTING DAYS
DaysOfInterest <- rbind(HotDays, ColdDays)
DaysOfInterest <- rbind(DaysOfInterest, RainDays)
DaysOfInterest <- rbind(DaysOfInterest, WindDays)
DaysOfInterest <- DaysOfInterest[,c("Date", "TempHighF", "TempAvgF", "TempLowF", "WindGustMPH", 
                                    "HeatIndexAvg", "HeatIndexHigh", "WindChillAvgF", "WindChillGustF", 
                                    "PrecipitationSumInches", "Events", "Season")] #simplify

DaysOfInterest <- rbind(DaysOfInterest, OutDFu)
DoI <- unique(DaysOfInterest)
write.csv(DoI, "/Users/avanelson/Desktop/IST 707 Data Analytics/Project/ANelson_DaysOfInterest.csv", row.names=TRUE)
DoI.Dates <- DoI$Date

#Days of Interest from Decision Tree Analysis
# Read in 
TejasDays <- read.csv("/Users/avanelson/Desktop/IST 707 Data Analytics/Project/Copy of dates from the decision tree.csv")
str(TejasDays)
TejasDays$X12.23.13 <- as.Date(TejasDays$X12.23.13, 
                               format="%m/%d/%y")
TejasDates <- TejasDays$X12.23.13

library(dplyr)
library(plyr)

TejasDatesDF <- weatherSimp[weatherSimp$Date[TejasDates],] #get columns
str(TejasDatesDF)

DoI.Dates <- c(DoI.Dates, TejasDates)
DoI.Dates <- unique(DoI.Dates)

#-------------------------------------
#GET TWITTER DATA---------------------

#install.packages("rtweet")
#install.packages("tidytext")
library(rtweet)
library(tidytext)

appname <- "Austin Weather Sentiment"
key <- "xkzBxuFR0VIzBcQF6DsVNChsq"
secretkey <- "vwiSkmDVgg33xwaywLhhbtYheurOMci6Ilv5Oa4u1LZUJ5FbYr"
accesstoken <- "720693854144344064-jIUxDSV8cbSvhL0e4LHOUkbNUSqxzIA"
accesssecret <- "bQJajgnZwQJ7hVeQeONK0QqStqJkdyLRil8R6UBm0C39s"

twitter.token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secretkey,
  access_token = accesstoken,
  access_secret = accesssecret)

rtweet:::citycoords

d2017.07.29 <- search_fullarchive(
  q= "lang:en
    place:Austin",
  n = 100,
  fromDate = "201707290000",
  toDate = "201707292359",
  env_name = "mood",
  safedir = NULL,
  parse = TRUE,
  token = twitter.token )

gsub("[[:space:]]", "", paste("x", "0000"))
as.character.dates <- as.numeric(gsub("-", "", as.character(DoI.Dates)))
as.character.dates <- gsub("[[:space:]]", "", paste(as.character.dates, "0000"))
df <- gsub("[[:space:]]", "", paste("d", as.character(DoI.Dates)))

# Attempt at creating function to capture all tweets.
# Reached API cap and decided to manually collect the rest of tweets
twitter.date <- function(dates){
  for (date in dates){
    
    x <- as.numeric(gsub("-", "", as.character(date))) #format dates as character
    
    a <- gsub("[[:space:]]", "", paste(x, "0000")) #format for fromDate
    b <- gsub("[[:space:]]", "", paste(x, "2359")) #format for toDate
    
    df <- gsub("[[:space:]]", "", paste("d", as.character(date))) #format naming strat for dataframes
    
    
    df <- search_fullarchive(
      q= "lang:en
    place:Austin",
      n = 100,
      fromDate = a,
      toDate = b,
      env_name = "dev",
      safedir = NULL,
      parse = TRUE,
      token = twitter.token )
  }}

# Use Teja's API keys now
appname <- "Austin Mood Analysis"
key <- "U5UaLGa30EDOYwEw2BqqBCUZh"
secretkey <- "LBoAklvaH6ZBR1CWdgFpI0y1V9C9caU6Kg4e1CloTyvkOxd0LN"
accesstoken <- "1254551415655215104-CgJASFLSEcF6vGd71DU0hqfVaWRSFY"
accesssecret <- "HNM0twzrCiNlUlYwsIkmf3TkNScg8F2GCquSH21vU25dG"
twitter.token <- create_token(app = appname,
                              consumer_key = key,
                              consumer_secret = secretkey,
                              access_token = accesstoken,
                              access_secret = accesssecret)


d2017.07.30 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201707300000",
                                   toDate = "201707302359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2017.07.30,"/Users/tejasanaka/Documents/Syracuse/IST707/20170730.csv")

d2017.01.04 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201701040000",
                                   toDate = "201701042359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2017.01.04,"/Users/tejasanaka/Documents/Syracuse/IST707/20170104.csv")

d2017.01.04 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201701040000",
                                   toDate = "201701042359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2017.01.04,"/Users/tejasanaka/Documents/Syracuse/IST707/20170104.csv")

d2016.12.29 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201612290000",
                                   toDate = "201612292359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2016.12.29,"/Users/tejasanaka/Documents/Syracuse/IST707/20161229.csv")

d2016.01.23 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201601230000",
                                   toDate = "201601232359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2016.01.23,"/Users/tejasanaka/Documents/Syracuse/IST707/20160123.csv")

d2016.01.22 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201601220000",
                                   toDate = "201601222359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2016.01.22,"/Users/tejasanaka/Documents/Syracuse/IST707/20160122.csv")

d2016.01.10 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201601100000",
                                   toDate = "201601102359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2016.01.10,"/Users/tejasanaka/Documents/Syracuse/IST707/20160110.csv")

d2015.11.22 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201511220000",
                                   toDate = "201511222359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.11.22,"/Users/tejasanaka/Documents/Syracuse/IST707/20151122.csv")

d2015.03.07 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201503070000",
                                   toDate = "201503072359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.03.07,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150307.csv")

d2015.03.06 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201503060000",
                                   toDate = "201503062359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.03.06,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150306.csv")

d2015.03.05 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201503050000",
                                   toDate = "201503052359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.03.05,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150305.csv")

d2015.02.18 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201502180000",
                                   toDate = "201502182359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.02.18,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150218.csv")

d2015.02.17 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201502170000",
                                   toDate = "201502172359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.02.17,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150217.csv")

d2015.01.09 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201501090000",
                                   toDate = "201501092359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.01.09,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150109.csv")

d2015.01.07 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201501070000",
                                   toDate = "201501072359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.01.07,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150107.csv")

d2015.01.05 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201501050000",
                                   toDate = "201501052359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.01.05,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150105.csv")

d2015.01.04 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201501040000",
                                   toDate = "201501042359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2015.01.04,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20150104.csv")

d2014.12.31 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201412310000",
                                   toDate = "201412312359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.12.31,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20141231.csv")

d2014.11.18 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201411180000",
                                   toDate = "201411182359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.11.18,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20141118.csv")

d2014.11.17 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201411170000",
                                   toDate = "201411172359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.11.17,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20141117.csv")

d2014.03.03 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201403030000",
                                   toDate = "201403032359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.03.03,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140303.csv")

d2014.02.27 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201402270000",
                                   toDate = "201402272359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.02.27,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140227.csv")

d2014.02.12 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201402120000",
                                   toDate = "201402122359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.02.12,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140212.csv")

d2014.02.07 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201402070000",
                                   toDate = "201402072359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.02.07,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140207.csv")

d2014.02.05 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201402050000",
                                   toDate = "201402052359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.02.05,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140205.csv")

d2014.01.02 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201401020000",
                                   toDate = "201401022359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.01.02,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140102.csv")

d2014.02.07 <- search_fullarchive( q= "lang:en
                                   place:Austin",
                                   n = 100,
                                   fromDate = "201402070000",
                                   toDate = "201402072359",
                                   env_name = "dev",
                                   safedir = NULL,
                                   parse = TRUE,
                                   token = twitter.token )

write_as_csv(d2014.02.07,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140207.csv")

d2013.12.31<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201312310000",
                                  toDate = "201312312359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2013.12.31,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20131231.csv")

d2013.12.30<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201312300000",
                                  toDate = "201312302359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2013.12.30,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20131230.csv")

d2013.12.24<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201312240000",
                                  toDate = "201312242359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2013.12.24,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20131224.csv")

d2013.12.25<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201312250000",
                                  toDate = "201312252359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2013.12.25,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20131225.csv")

d2013.12.23<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201312230000",
                                  toDate = "201312232359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2013.12.23,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20131223.csv")

d2013.12.22<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201312220000",
                                  toDate = "201312222359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2013.12.22,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20131222.csv")

d2013.12.21<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201312210000",
                                  toDate = "201312212359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2013.12.21,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20131221.csv")

d2014.01.22<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401220000",
                                  toDate = "201401222359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.22,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140122.csv")

d2014.01.24<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401240000",
                                  toDate = "201401242359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.24,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140124.csv")

d2014.01.20<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401200000",
                                  toDate = "201401202359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.20,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140120.csv")

d2014.01.19<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401190000",
                                  toDate = "201401192359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.19,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140119.csv")

d2014.01.10<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401100000",
                                  toDate = "201401102359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.10,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140110.csv")

d2014.01.08<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401080000",
                                  toDate = "201401082359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.08,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140108.csv")

d2014.01.05<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401050000",
                                  toDate = "201401052359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.05,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140105.csv")

d2014.01.30<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401300000",
                                  toDate = "201401302359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.30,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140130.csv")

d2014.01.27<- search_fullarchive( q= "lang:en
                                   place:Austin",
                                  n = 100,
                                  fromDate = "201401270000",
                                  toDate = "201401272359",
                                  env_name = "dev",
                                  safedir = NULL,
                                  parse = TRUE,
                                  token = twitter.token )

write_as_csv(d2014.01.27,"/Users/tejasanaka/Documents/Syracuse/IST707/TejasTweets/20140127.csv")

# Complete Sentiment Analysis
# Load required packages
library(tm)
# Load Ava's tweets
d2017.07.29 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20170729.csv")
d2016.08.12 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20160812.csv")
d2015.08.08 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20150808.csv")
d2015.08.09 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20150809.csv")
d2015.08.10 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20150810.csv")
d2015.08.11 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20150811.csv")
d2015.08.12 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20150812.csv")
d2017.06.23 <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/20170623.csv")

# Load AFINN lexicon
AFINN <- read.delim("/Users/tejasanaka/Documents/Syracuse/IST707/AFINNlexicon.txt", header=FALSE)

# Initially completed sentiment analysis without removing curse words
# Later realized that curse word removal was necessary and added line of code to every date to remove cursewords
cursewords <- c("fuck", "fucking")

# Clean text 
text2013.12.21 <- data.frame(d2013.12.21$text)
words.vec <- VectorSource(text2013.12.21$d2013.12.21.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2013.12.21 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2013.12.21

# Clean text 
text2013.12.22 <- data.frame(d2013.12.22$text)
words.vec <- VectorSource(text2013.12.22$d2013.12.22.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2013.12.22 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2013.12.22

# Clean text 
text2013.12.23 <- data.frame(d2013.12.23$text)
words.vec <- VectorSource(text2013.12.23$d2013.12.23.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2013.12.23 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2013.12.23

# Clean text 
text2013.12.24 <- data.frame(d2013.12.24$text)
words.vec <- VectorSource(text2013.12.24$d2013.12.24.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2013.12.24 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2013.12.24

# Clean text 
text2013.12.25 <- data.frame(d2013.12.25$text)
words.vec <- VectorSource(text2013.12.25$d2013.12.25.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2013.12.25 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2013.12.25

# Clean text 
text2013.12.30 <- data.frame(d2013.12.30$text)
words.vec <- VectorSource(text2013.12.30$d2013.12.30.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2013.12.30 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2013.12.30

# Clean text 
text2013.12.31 <- data.frame(d2013.12.31$text)
words.vec <- VectorSource(text2013.12.31$d2013.12.31.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2013.12.31 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2013.12.31

# Clean text 
text2014.01.02 <- data.frame(d2014.01.02$text)
words.vec <- VectorSource(text2014.01.02$d2014.01.02.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.02 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.02

# Clean text 
text2014.01.05 <- data.frame(d2014.01.05$text)
words.vec <- VectorSource(text2014.01.05$d2014.01.05.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.05 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.05

# Clean text 
text2014.01.08 <- data.frame(d2014.01.08$text)
words.vec <- VectorSource(text2014.01.08$d2014.01.08.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.08 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.08

# Clean text 
text2014.01.10 <- data.frame(d2014.01.10$text)
words.vec <- VectorSource(text2014.01.10$d2014.01.10.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.10 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.10

# Clean text 
text2014.01.19 <- data.frame(d2014.01.19$text)
words.vec <- VectorSource(text2014.01.19$d2014.01.19.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.19 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.19

# Clean text 
text2014.01.20 <- data.frame(d2014.01.20$text)
words.vec <- VectorSource(text2014.01.20$d2014.01.20.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.20 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.20

# Clean text 
text2014.01.22 <- data.frame(d2014.01.22$text)
words.vec <- VectorSource(text2014.01.22$d2014.01.22.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.22 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.22

# Clean text 
text2014.01.23 <- data.frame(d2014.01.23$text)
words.vec <- VectorSource(text2014.01.23$d2014.01.23.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.23 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.23

# Clean text 
text2014.01.24 <- data.frame(d2014.01.24$text)
words.vec <- VectorSource(text2014.01.24$d2014.01.24.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.24 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.24

# Clean text 
text2014.01.26 <- data.frame(d2014.01.26$text)
words.vec <- VectorSource(text2014.01.26$d2014.01.26.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.26 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.26

# Clean text 
text2014.01.27 <- data.frame(d2014.01.27$text)
words.vec <- VectorSource(text2014.01.27$d2014.01.27.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.27 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.27

# Clean text 
text2014.01.30 <- data.frame(d2014.01.30$text)
words.vec <- VectorSource(text2014.01.30$d2014.01.30.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.01.30 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.01.30

# Clean text 
text2014.02.05 <- data.frame(d2014.02.05$text)
words.vec <- VectorSource(text2014.02.05$d2014.02.05.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.02.05 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.02.05

# Clean text 
text2014.02.07 <- data.frame(d2014.02.07$text)
words.vec <- VectorSource(text2014.02.07$d2014.02.07.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.02.07 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.02.07

# Clean text 
text2014.02.12 <- data.frame(d2014.02.12$text)
words.vec <- VectorSource(text2014.02.12$d2014.02.12.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.02.12 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.02.12

# Clean text 
text2014.02.27 <- data.frame(d2014.02.27$text)
words.vec <- VectorSource(text2014.02.27$d2014.02.27.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.02.27 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.02.27

# Clean text 
text2014.03.03 <- data.frame(d2014.03.03$text)
words.vec <- VectorSource(text2014.03.03$d2014.03.03.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.03.03 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.03.03

# Clean text 
text2014.11.17 <- data.frame(d2014.11.17$text)
words.vec <- VectorSource(text2014.11.17$d2014.11.17.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.11.17 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.11.17

# Clean text 
text2014.11.18 <- data.frame(d2014.11.18$text)
words.vec <- VectorSource(text2014.11.18$d2014.11.18.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.11.18 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.11.18

# Clean text 
text2014.12.31 <- data.frame(d2014.12.31$text)
words.vec <- VectorSource(text2014.12.31$d2014.12.31.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2014.12.31 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2014.12.31

# Clean text 
text2015.01.04 <- data.frame(d2015.01.04$text)
words.vec <- VectorSource(text2015.01.04$d2015.01.04.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.01.04 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.01.04

# Clean text 
text2015.01.05 <- data.frame(d2015.01.05$text)
words.vec <- VectorSource(text2015.01.05$d2015.01.05.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.01.05 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.01.05

# Clean text 
text2015.01.07 <- data.frame(d2015.01.07$text)
words.vec <- VectorSource(text2015.01.07$d2015.01.07.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.01.07 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.01.07

# Clean text 
text2015.01.09 <- data.frame(d2015.01.09$text)
words.vec <- VectorSource(text2015.01.09$d2015.01.09.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.01.09 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.01.09

# Clean text 
text2015.02.17 <- data.frame(d2015.02.17$text)
words.vec <- VectorSource(text2015.02.17$d2015.02.17.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.02.17 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.02.17

# Clean text 
text2015.02.18 <- data.frame(d2015.02.18$text)
words.vec <- VectorSource(text2015.02.18$d2015.02.18.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.02.18 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.02.18

# Clean text 
text2015.03.05 <- data.frame(d2015.03.05$text)
words.vec <- VectorSource(text2015.03.05$d2015.03.05.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.03.05 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.03.05

# Clean text 
text2015.03.06 <- data.frame(d2015.03.06$text)
words.vec <- VectorSource(text2015.03.06$d2015.03.06.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.03.06 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.03.06

# Clean text 
text2015.03.07 <- data.frame(d2015.03.07$text)
words.vec <- VectorSource(text2015.03.07$d2015.03.07.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.03.07 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.03.07

# Clean text 
text2015.08.08 <- data.frame(d2015.08.08$text)
words.vec <- VectorSource(text2015.08.08$d2015.08.08.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.08.08 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.08.08

# Clean text 
text2015.08.09 <- data.frame(d2015.08.09$text)
words.vec <- VectorSource(text2015.08.09$d2015.08.09.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.08.09 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.08.09

# Clean text 
text2015.08.10 <- data.frame(d2015.08.10$text)
words.vec <- VectorSource(text2015.08.10$d2015.08.10.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.08.10 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.08.10

# Clean text 
text2015.08.11 <- data.frame(d2015.08.11$text)
words.vec <- VectorSource(text2015.08.11$d2015.08.11.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.08.11 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.08.11

# Clean text 
text2015.08.12 <- data.frame(d2015.08.12$text)
words.vec <- VectorSource(text2015.08.12$d2015.08.12.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.08.12 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.08.12

# Clean text 
text2015.11.22 <- data.frame(d2015.11.22$text)
words.vec <- VectorSource(text2015.11.22$d2015.11.22.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2015.11.22 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2015.11.22

# Clean text 
text2016.01.10 <- data.frame(d2016.01.10$text)
words.vec <- VectorSource(text2016.01.10$d2016.01.10.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2016.01.10 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2016.01.10

# Clean text 
text2016.01.22 <- data.frame(d2016.01.22$text)
words.vec <- VectorSource(text2016.01.22$d2016.01.22.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2016.01.22 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2016.01.22

# Clean text 
text2016.01.23 <- data.frame(d2016.01.23$text)
words.vec <- VectorSource(text2016.01.23$d2016.01.23.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2016.01.23 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2016.01.23

# Clean text 
text2016.08.12 <- data.frame(d2016.08.12$text)
words.vec <- VectorSource(text2016.08.12$d2016.08.12.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2016.08.12 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2016.08.12

# Clean text 
text2016.12.29 <- data.frame(d2016.12.29$text)
words.vec <- VectorSource(text2016.12.29$d2016.12.29.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2016.12.29 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2016.12.29

# Clean text 
text2017.01.04 <- data.frame(d2017.01.04$text)
words.vec <- VectorSource(text2017.01.04$d2017.01.04.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2017.01.04 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2017.01.04

# Clean text 
text2017.06.23 <- data.frame(d2017.06.23$text)
words.vec <- VectorSource(text2017.06.23$d2017.06.23.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2017.06.23 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2017.06.23

# Clean text 
text2017.07.29 <- data.frame(d2017.07.29$text)
words.vec <- VectorSource(text2017.07.29$d2017.07.29.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2017.07.29 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2017.07.29

# Clean text 
text2017.07.30 <- data.frame(d2017.07.30$text)
words.vec <- VectorSource(text2017.07.30$d2017.07.30.text)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, removeWords, cursewords)
tdm <- TermDocumentMatrix(words.corpus)
tdm
matrix <- as.matrix(tdm)
wordCounts <- rowSums(matrix)
words <- names(wordCounts)
wordCounts <- sort(wordCounts, decreasing=TRUE)
worddf <- data.frame(word=names(wordCounts),freq=wordCounts)
# test with AFINN lexicon
colnames(AFINN) <- c("Word", "Score")
mergedTable <- merge(worddf, AFINN, by.x="word", by.y="Word")
# Calculate score
overallScore2017.07.30 <-sum(mergedTable$freq*mergedTable$Score)
overallScore2017.07.30

# Create sentiments dataframe
scores <- c(overallScore2013.12.21,
            overallScore2013.12.22,
            overallScore2013.12.23,
            overallScore2013.12.24, 
            overallScore2013.12.25, 
            overallScore2013.12.30,
            overallScore2013.12.31,
            overallScore2014.01.02,
            overallScore2014.01.05,
            overallScore2014.01.08,
            overallScore2014.01.10,
            overallScore2014.01.19,
            overallScore2014.01.20,
            overallScore2014.01.22,
            overallScore2014.01.23,
            overallScore2014.01.24,
            overallScore2014.01.26,
            overallScore2014.01.27,
            overallScore2014.01.30,
            overallScore2014.02.05,
            overallScore2014.02.07,
            overallScore2014.02.12,
            overallScore2014.02.27,
            overallScore2014.03.03,
            overallScore2014.11.17,
            overallScore2014.11.18,
            overallScore2014.12.31,
            overallScore2015.01.04,
            overallScore2015.01.05,
            overallScore2015.01.07,
            overallScore2015.01.09,
            overallScore2015.02.17,
            overallScore2015.02.18,
            overallScore2015.03.05,
            overallScore2015.03.06,
            overallScore2015.03.07,
            overallScore2015.08.08,
            overallScore2015.08.09,
            overallScore2015.08.10,
            overallScore2015.08.11,
            overallScore2015.08.12,
            overallScore2015.11.22,
            overallScore2016.01.10,
            overallScore2016.01.22,
            overallScore2016.01.23,
            overallScore2016.08.12,
            overallScore2016.12.29,
            overallScore2017.01.04,
            overallScore2017.06.23,
            overallScore2017.07.29,
            overallScore2017.07.30)

dates <- c('2013-12-21',
           '2013-12-22',
           '2013-12-23',
           '2013-12-24',
           '2013-12-25',
           '2013-12-30',
           '2013-12-31',
           '2014-01-02',
           '2014-01-05',
           '2014-01-08',
           '2014-01-10',
           '2014-01-19',
           '2014-01-20',
           '2014-01-22',
           '2014-01-23',
           '2014-01-24',
           '2014-01-26',
           '2014-01-27',
           '2014-01-30',
           '2014-02-05',
           '2014-02-07',
           '2014-02-12',
           '2014-02-27',
           '2014-03-03',
           '2014-11-17',
           '2014-11-18',
           '2014-12-31',
           '2015-01-04',
           '2015-01-05',
           '2015-01-07',
           '2015-01-09',
           '2015-02-17',
           '2015-02-18',
           '2015-03-05',
           '2015-03-06',
           '2015-03-07',
           '2015-08-08',
           '2015-08-09',
           '2015-08-10',
           '2015-08-11',
           '2015-08-12',
           '2015-11-22',
           '2016-01-10',
           '2016-01-22',
           '2016-01-23',
           '2016-08-12',
           '2016-12-29',
           '2017-01-04',
           '2017-06-23',
           '2017-07-29',
           '2017-07-30')

Sentiments <- data.frame(scores, dates)
sentiments$dates <- as.Date(sentiments$dates)
str(sentiments)
write.csv(Sentiments,"/Users/tejasanaka/Documents/Syracuse/IST707/Sentiments.csv")


# Merge SENTIMENT ANALYSIS
Sentiments <- read.csv("/Users/avanelson/Desktop/IST 707 Data Analytics/Project/sentiments.csv")
Sentiments$dates <- as.Date(Sentiments$dates)
Sentiments <- Sentiments[,2:3]
Sentiments$Date <- Sentiments$dates
Sentiments <- Sentiments[,c("Date", "scores")]

WeatherFinal <- merge(weatherSimp, Sentiments, by="Date", all.y = TRUE)
str(WeatherFinal)

#normalize sentiment analysis
WeatherFinal$scoreNorm <- scale(WeatherFinal$scores)
WeatherFinal$dScores <- cut(WeatherFinal$scoreNorm, breaks= c(-Inf, -2, -1, 0, 1, 2, Inf), 
                            labels= c("Very Negative", "Negative", "Somewhat Negative", "Somewhat Positive", "Positive", "Very Positive"))

write.csv(WeatherFinal, "/Users/avanelson/Desktop/IST 707 Data Analytics/Project/WeatherSentiment.csv")

boxplot(WeatherFinal$scoreNorm)
WeatherFinal[3,]

# Ran word clouds for most negative, neutral and positive days by re-running corresponding TM code
# Repeated process before and after curse word usage

# Make word clouds
library(wordcloud)
# 2013-12-23 (Most Negative)
wordcloud(words = worddf$word, freq = worddf$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
worddf <- worddf[-c(16),]

# 2017-01-04 (Most Neutral)
wordcloud(words = worddf$word, freq = worddf$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# 2015-01-09 (Most Positive)
wordcloud(words = worddf$word, freq = worddf$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(6, "Dark2"))




#ARM ON SENTIMENT DATA------------------------
#---------------------------------------------
#discretize variables
WeatherFinal$dTempHighF <- cut(WeatherFinal$TempHighF, breaks= c(0, 32, 40, 50, 60, 70, 80, 90, 100, 110, Inf), 
                               labels= c("Freezing", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s", "Hell"))
WeatherFinal$dHeatIndex <- cut(WeatherFinal$HeatIndexHigh, breaks= c(0, 32, 40, 50, 60, 70, 80, 90, 100, 110, Inf), 
                               labels= c("Freezing", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s", "Hell"))
WeatherFinal$dWindChill <- cut(WeatherFinal$WindChillGustF, breaks= c(0, 32, 40, 50, 60, 70, 80, 90, 100, 110, Inf), 
                               labels= c("Freezing", "30s", "40s", "50s", "60s", "70s", "80s", "90s", "100s", "Hell"))
WeatherFinal$dWindMPH <- cut(WeatherFinal$WindGustMPH, breaks= c(0, 10, 20, 30, 40, Inf), 
                             labels= c("0-10mpg", "11-20mph", "21-30mph", "31-40mph", "41mph+"))
WeatherFinal$dPrecip <- cut(WeatherFinal$PrecipitationSumInches, breaks= c(-0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, Inf), 
                            labels= c("0-0.5in", "0.5-1in", "1-1.5in", "1.5-2in", "2-2.5in", "2.5-3in", "3-3.5in", "3.5-4in", "4-4.5in", "4.5in-5in", ">5in"))

str(WeatherFinalDisc)

WeatherFinalDisc <- WeatherFinal
WeatherFinalDisc[,1:10] <- NULL #remove non discrete variables pt1
WeatherFinalDisc[,3:4] <- NULL #remove non discrete variables pt2
WeatherFinalDisc <- plyr::rename(WeatherFinalDisc, c("Events"="Events", "Season"="Season",
                                                     "dScores"="Sentiment", "dTempHighF"="TempHighF", "dHeatIndex"="HeatIndex", 
                                                     "dWindChill"="WindChill", "dWindMPH"="WindMPH", "dPrecip"="Precipitation"))

#CONVERT TO TRANSACTIONS
library(arules)
library(arulesViz)

WeatherFinalDiscMinimal <- WeatherFinalDisc[,c("Events", "Sentiment", "HeatIndex", "WindChill", 
                                               "WindMPH")]

final_weather_basket <- as(WeatherFinalDisc, "transactions") #convert into basketdata

itemFrequencyPlot(final_weather_basket, topN=20, lift=FALSE)

rules1 <- apriori(final_weather_basket, parameter=list(supp=0.8, conf=0.9)) #2 rules
rules2 <- apriori(final_weather_basket, parameter=list(supp=0.6, conf=0.6)) #7 rules use this one
rules3 <- apriori(final_weather_basket, parameter=list(supp=0.4, conf=0.6)) #5 rules
rules4 <- apriori(final_weather_basket, parameter=list(supp=0.4, conf=0.5)) #14 rules
rules5 <- apriori(final_weather_basket, parameter=list(supp=0.5, conf=0.3)) #12 rules
rules6 <- apriori(final_weather_basket, parameter=list(supp=0.35, conf=0.4)) #34 rules
rules7 <- apriori(final_weather_basket, parameter=list(supp=0.3, conf=0.4)) #47 rules

rules5b <- apriori(final_weather_basket, parameter=list(supp=0.3, conf=0.4),
                   appearance = list(lhs="Sentiment=Somewhat Positive"))
inspect(sort(rules5b, by="lift", decreasing=TRUE))

inspect(sort(rules2, by="lift", decreasing=TRUE))
summary(rules2)
plot(rules5, method="graph", engine= "interactive", main="Rules Graph Plot with Sentiment")

# Complete decision trees, now to predict sentiment
# Read in Final Weather csv
weatherSentiment <- read.csv("/Users/tejasanaka/Documents/Syracuse/IST707/WeatherSentiment.csv")
str(weatherFinal)

# Discretize final weather
weatherFinal$discTemp <- cut(weatherFinal$TempAvgF, breaks = c(0,40,50,60,70,80,Inf),labels=c("40 or fewer","40-50","50-60","60-70","70-80", "greater than 80"))
weatherFinal$dChillAvg <- cut(weatherFinal$WindChillAvgF, breaks = c(0,30,40,50,60,Inf),labels=c("30 or fewer","30-40","40-50","50-60", "greater than 60"))
weatherFinal$dWindGust <- cut(weatherFinal$WindGustMPH, breaks = c(0,5,10,15,20,Inf),labels=c("5 or fewer","5-10","10-15","15-20","greater than 20"))
weatherFinal$dHIndexAvg <- cut(weatherFinal$HeatIndexAvg, breaks = c(0,40,50,70,Inf),labels=c("40 or fewer","40-50","50-70", "greater than 70"))

# Create final test and train sets
random <- sample(1:dim(weatherFinal)[1])
cutpoint <- floor(3*dim(weatherFinal)[1]/5)
trainweatherFinal <- weatherFinal[random[1:cutpoint],]
testweatherFinal <- weatherFinal[random[(cutpoint + 1):dim(weatherFinal)[1]],]

# New decisions trees
rpartweather <- rpart(dScores ~ discTemp + dChillAvg + dWindGust + dHIndexAvg + Events + Season, data = trainweatherFinal, method = "class")
summary(rpartweather)
predictrpart <- predict(rpartweather, testweatherFinal, type = "class")
fancyRpartPlot(rpartweather)
rpartmatrix <- table(predictrpart, testweatherFinal$dScores)
rpartmatrix

rpartweather <- rpart(dScores ~ discTemp + dChillAvg + dWindGust + dHIndexAvg + Events + Season, data = trainweatherFinal, method = "class", control = rpart.control(minbucket = 4))
summary(rpartweather)
predictrpart <- predict(rpartweather, testweatherFinal, type = "class")
fancyRpartPlot(rpartweather)
rpartmatrix <- table(predictrpart, testweatherFinal$discTemp)
rpartmatrix


#NAIVE BAYES--------------------------
#-------------------------------------
library(e1071)
library(caret)
library(klaR) #to plot NB
install.packages("klaR")


# With Curse Words
TrainSize <- floor(nrow(WeatherFinalDisc)*.75)


TrainRows <- sample(rownames(WeatherFinalDisc), size=TrainSize, replace=FALSE)
RowsToKeep <- which(TrainRows %in% rownames(WeatherFinalDisc))
TrainData <- WeatherFinalDisc[TrainRows,] #get training data
TestData <- WeatherFinalDisc[-RowsToKeep,] #get test data
TestDataNoSentiment <- TestData[,-3] #remove Sentiment

Model <- naiveBayes(Sentiment~., data=TrainData, laplace = 1)
NBayePredict <- predict(Model, TestDataNoSentiment)
confusionMatrix(NBayePredict, TestData$Sentiment, dnn=c("Prediction", "Reference"))
summary(Model)

#Simplify Score Factor
WeatherFinal$dScores2 <- cut(WeatherFinal$scoreNorm, breaks= c(-Inf, 0, Inf), 
                             labels= c("Negative", "Positive"))
WeatherFinalDisc2 <- WeatherFinal
WeatherFinalDisc2[,1:10] <- NULL #remove non discrete variables pt1
WeatherFinalDisc2[,3:4] <- NULL #remove non discrete variables pt2
WeatherFinalDisc2[,3] <- NULL #remove old score
WeatherFinalDisc2 <- plyr::rename(WeatherFinalDisc2, c("Events"="Events", "Season"="Season",
                                                       "dScores2"="Sentiment", "dTempHighF"="TempHighF", "dHeatIndex"="HeatIndex", 
                                                       "dWindChill"="WindChill", "dWindMPH"="WindMPH", "dPrecip"="Precipitation"))

TrainRows2 <- sample(rownames(WeatherFinalDisc2), size=TrainSize, replace=FALSE)
RowsToKeep2 <- which(TrainRows2 %in% rownames(WeatherFinalDisc2))
TrainData2 <- WeatherFinalDisc2[TrainRows2,] #get training data
TestData2 <- WeatherFinalDisc2[-RowsToKeep2,] #get test data
TestDataNoSentiment2 <- TestData2[,-3] #remove Sentiment

Model2 <- naiveBayes(Sentiment~., data=TrainData2, laplace = 1)
NBayePredict2 <- predict(Model2, TestDataNoSentiment2)
confusionMatrix(NBayePredict2, TestData2$Sentiment, dnn=c("Prediction", "Reference"))
summary(Model)
plot(Model)

PlotTest2 <- TestData2
PlotTest2$Prediction <- NBayePredict2
PlotTest2$Correct <- ifelse(PlotTest2$Sentiment==PlotTest2$Prediction, TRUE, FALSE)

ggplot(PlotTest2, aes(x=WindChill, y=HeatIndex)) +
  geom_point(aes(shape=Sentiment, color = Correct, size=Prediction)) +
  ggtitle("Score Prediction Naive Bayes")

ggplot(PlotTest2, aes(x=Sentiment)) +
  geom_bar(aes(fill = Correct)) +
  ggtitle("Score Prediction Naive Bayes")

WeatherFinalNum <- WeatherFinal
WeatherFinalNum[,c(1,11:13, 15:22)] <- NULL
WeatherFinalNum$scoreNorm <- as.numeric(WeatherFinalNum$scoreNorm)
WeatherFinalMatrix <- matrix(WeatherFinalNum)

ggplot(data=WeatherFinalNum) +
  geom_tile(aes(x=TempHighF, y=TempLowF, fill=scoreNorm)) +
  scale_fill_gradient2(
    low = "red",
    mid = "yellow",
    high = "green",
    midpoint = 0
  ) +
  ggtitle("Sentiment Map")


WeatherFinalNew <- read.csv("/Users/avanelson/Desktop/IST 707 Data Analytics/Project/WeatherFinalNew.csv")

WeatherFinalNum2 <- WeatherFinalNew
WeatherFinalNum2[,c(1,11:13, 15:22)] <- NULL
WeatherFinalNum2$scores <- as.numeric(WeatherFinalNum2$scores)
WeatherFinalNum2$Difference <- WeatherFinalNum2$scores - WeatherFinal$scores
WeatherFinalNum2$DiffNorm <- scale(WeatherFinalNum2$Difference)

ggplot(data=WeatherFinalNum2) +
  geom_tile(aes(x=TempHighF, y=TempLowF, fill=DiffNorm)) +
  scale_fill_gradient2(
    low = "red",
    mid = "yellow",
    high = "green",
    midpoint = 0
  ) +
  ggtitle("Change in Sentiment Map")

WeatherFinal[31,]

# RERUN MODEL WITHOUT CURSE WORDS
Model2 <- naiveBayes(Sentiment~., data=TrainData2, laplace = 1)

NBayePredict2 <- predict(Model2, TestDataNoSentiment2)

confusionMatrix(NBayePredict2, TestData2$Sentiment, dnn=c("Prediction", "Reference"))

PlotTest2 <- TestData2
PlotTest2$Prediction <- NBayePredict2
PlotTest2$Correct <- ifelse(PlotTest2$Sentiment==PlotTest2$Prediction, TRUE, FALSE)

ggplot(PlotTest2, aes(x=Sentiment)) +
  
  geom_bar(aes(fill = Correct)) +
  
  ggtitle("Score Prediction Naive Bayes")

write.csv(WeatherFinal, "/Users/tejasanaka/Documents/Syracuse/IST707/WeatherFinal.csv")

WeatherFinalNum <- WeatherFinal

WeatherFinalNum[,c(1,11:13, 15:22)] <- NULL

WeatherFinalNum$scoreNorm <- as.numeric(WeatherFinalNum$scoreNorm)

WeatherFinalMatrix <- matrix(WeatherFinalNum)



ggplot(data=WeatherFinalNum) +
  
  geom_tile(aes(x=TempHighF, y=TempLowF, fill=scoreNorm)) +
  
  scale_fill_gradient2(
    low = "red",
    mid = "yellow",
    high = "green",
    midpoint = 0
  ) +
  
  ggtitle("Sentiment Map without Curse Words")

boxplot(WeatherFinal$scoreNorm)

