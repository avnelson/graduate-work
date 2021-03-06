tx_birds <- read.csv('/Users/avanelson/Desktop/IST719 Data Viz/2020Release_Nor/States/Texas.csv')

str(tx_birds)
tx_birds$AOU <- as.factor(tx_birds$AOU)

years <- c(1966:2019)

#----------------
#GET ROUTE INFO

routes = read.csv('/Users/avanelson/Desktop/IST719 Data Viz/2020Release_Nor/routes.csv')
routes_tx = routes[routes$StateNum == 83,]

#add lat/long to big dataset
tx_birds$latitude = routes_tx$Latitude[match(tx_birds$Route, routes_tx$Route)]
tx_birds$longitude = routes_tx$Longitude[match(tx_birds$Route, routes_tx$Route)]


#-------------
#SPLIT BIRDS DOWN

#Brown Thrasher
brown.thrasher <- tx_birds[which(tx_birds$AOU==07050),]
thrasher.counts <- c() 
for (i in 1:length(years)){
  thrasher.counts<- c(thrasher.counts, sum(brown.thrasher$SpeciesTotal[brown.thrasher$Year == years[i]]))
}

#Red Headed Woodpecker
red.headed.woodpecker <- tx_birds[which(tx_birds$AOU==04060),]
woodpecker.counts <- c() 
for (i in 1:length(years)){
  woodpecker.counts<- c(woodpecker.counts, sum(red.headed.woodpecker$SpeciesTotal[red.headed.woodpecker$Year == years[i]]))
}

#Fish Crow
fish.crow <- tx_birds[which(tx_birds$AOU==04900),]
crow.counts <- c() 
for (i in 1:length(years)){
  crow.counts<- c(crow.counts, sum(fish.crow$SpeciesTotal[fish.crow$Year == years[i]]))
}

#Eastern Kingbird
eastern.kingbird <- tx_birds[which(tx_birds$AOU==04440),]
kingbird.counts <- c() 
for (i in 1:length(years)){
  kingbird.counts<- c(kingbird.counts, sum(eastern.kingbird$SpeciesTotal[eastern.kingbird$Year == years[i]]))
}

#Mississippi Kite
miss.kite <- tx_birds[which(tx_birds$AOU==03290),]
kite.counts <- c() 
for (i in 1:length(years)){
  kite.counts<- c(kite.counts, sum(miss.kite$SpeciesTotal[miss.kite$Year == years[i]]))
}

#Wood Duck
wood.duck <- tx_birds[which(tx_birds$AOU==01440),]
duck.counts <- c() 
for (i in 1:length(years)){
  duck.counts<- c(duck.counts, sum(wood.duck$SpeciesTotal[wood.duck$Year == years[i]]))
}

bird_sums <- data.frame(years, crow.counts, duck.counts, kingbird.counts, 
                        kite.counts, thrasher.counts, woodpecker.counts)

par(mfrow = c(2:3))

plot(bird_sums$years, 
        bird_sums$thrasher.counts, 
     main= "Brown Thrasher",
     xlab= "Year", ylab= "Count", type='l')

plot(bird_sums$years, 
     bird_sums$woodpecker.counts, 
     main= "Red Headed Woodpecker",
     xlab= "Year", ylab= "Count", type='l')

plot(bird_sums$years, 
     bird_sums$kite.counts, 
     main= "Mississippi Kite",
     xlab= "Year", ylab= "Count", type='l')

plot(bird_sums$years, 
     bird_sums$kingbird.counts, 
     main= "Eastern Kingbird",
     xlab= "Year", ylab= "Count", type='l')

plot(bird_sums$years, 
     bird_sums$duck.counts, 
     main= "Wood Duck",
     xlab= "Year", ylab= "Count", type='l')

plot(bird_sums$years, 
     bird_sums$crow.counts, 
     main= "Fish Crow",
     xlab= "Year", ylab= "Count", type='l')

#Multivariable Plot
my.alpha = 100
col.vec <- rep(rgb(255,255,255, maxColorValue = 255, alpha=0), 
               nrow(tx_birds))

col.vec[tx_birds$AOU == 07050] <-rgb(34, 87, 122, maxColorValue = 255, alpha=my.alpha)
col.vec[tx_birds$AOU == 04060] <-rgb(56, 163, 165, maxColorValue = 255, alpha=my.alpha)
col.vec[tx_birds$AOU == 04900] <-rgb(87, 204, 153, maxColorValue = 255, alpha=my.alpha)
col.vec[tx_birds$AOU == 04440] <-rgb(128, 237, 153, maxColorValue = 255, alpha=my.alpha)
col.vec[tx_birds$AOU == 03290] <-rgb(243, 211, 74, maxColorValue = 255, alpha=my.alpha)
col.vec[tx_birds$AOU == 01440] <-rgb(176, 124, 158, maxColorValue = 255, alpha=my.alpha)

par(mfrow = c(1,1))
plot(tx_birds$Year, tx_birds$SpeciesTotal, pch=16, cex=1, col=col.vec, ylim=c(0,60),
     main="Count Rates for Selected Species", xlab="Year", ylab="Count")

plot(birds$Year[birds$Species == 'Mississippi Kite'], birds$SpeciesTotal[birds$Species == 'Mississippi Kite'], pch=16, cex=1, ylim=c(0,60),
     main="Count Rates for Selected Species", xlab="Year", ylab="Count")

#Geographic Plot

library(ggplot2)
install.packages('ggmap')
library(ggmap)

'''states <- map_data("state")
texas <- subset(states, region == "texas")
counties <- map_data("county")
tx_county <- subset(counties, region == "texas")

duck_map <- ggplot(data = texas, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.2) + 
  geom_polygon(color = "black", fill = "gray") + theme_nothing()
duck_map <- duck_map + geom_polygon(data = tx_county, fill = NA, color = "white", lwd = 0.2) 
  #geom_polygon(data = tx_county, color = "black", fill = NA)  # get the state border back on top
duck_map + geom_point(data = wood.duck, mapping = aes(x=longitude, y=latitude))
'''

#compile all birds into one df
eastern.kingbird['Species'] <- 'Eastern Kingbird' 
brown.thrasher['Species'] <- 'Brown Thrasher'
fish.crow['Species'] <- 'Fish Crow'
miss.kite['Species'] <- 'Mississippi Kite'
red.headed.woodpecker['Species'] <- 'Red Headed Woodpecker'
wood.duck['Species'] <- 'Wood Duck'

birds <- rbind(eastern.kingbird, brown.thrasher)
birds <- rbind(birds, fish.crow)
birds <- rbind(birds, miss.kite)
birds <- rbind(birds, red.headed.woodpecker)
birds <- rbind(birds, wood.duck)

# compute the bounding box
tx_bbox <- make_bbox(lat = latitude, lon = longitude, data = birds)
tx_bbox

tx_big <- NULL
# grab the maps from google
tx_big <- get_map(location = tx_bbox, source = 'stamen', maptype = 'toner')
#> Warning: bounding box given to google - spatial extent only approximate.
#> converting bounding box to center/zoom specification. (experimental)
#> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=51.86665,-127.98475&zoom=6&size=640x640&scale=2&maptype=terrain&language=en-EN&sensor=false


#------
#AGGREGATED PLOTS
# plot the points and color them by sector <1980
ggmap(tx_big) + 
  geom_point(data = birds[birds$Year < 1980,], mapping = aes(x = longitude, y = latitude, size=SpeciesTotal, color=Species)) +
  ggtitle(label = "Distribution of Selected Birds: < 1980")


# plot the points and color them by sector <1990-2000
ggmap(tx_big) + 
  geom_point(data = birds[birds$Year > 1990 & birds$Year < 2000,], mapping = aes(x = longitude, y = latitude, size=SpeciesTotal, color=Species)) +
  ggtitle(label = "Distribution of Selected Birds 1990-2000")

# plot the points and color them by sector >2010
ggmap(tx_big) + 
  geom_point(data = birds[birds$Year > 2010,], mapping = aes(x = longitude, y = latitude, size=SpeciesTotal, color=Species)) +
  ggtitle(label = "Distribution of Selected Birds: > 2010")

# 2019
ggmap(tx_big) + 
  geom_point(data = birds[birds$Year == 2019,], mapping = aes(x = longitude, y = latitude, size=SpeciesTotal, color=Species)) +
  ggtitle(label = "Distribution of Selected Birds 2019")


#------
#PLOT BY BIRD
library(RColorBrewer)

#Plot All Year Groups
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Species == 'Eastern Kingbird',], aes(x = longitude, y = latitude, fill=Year),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: < 1980") +
  scale_fill_gradientn(colours=rev(brewer.pal(7,"Spectral")))


#EASTERN KINGBIRD
# plot the points and color them by sector <1980
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year < 1980 & birds$Species == 'Eastern Kingbird',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: < 1980") 
# plot the points and color them by sector 1980-1990
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 1980 & birds$Year < 1990 & birds$Species == 'Eastern Kingbird',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 1980-1990") 
# plot the points and color them by sector 1990-2000
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 1990 & birds$Year < 2000 & birds$Species == 'Eastern Kingbird',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 1990-2000") 
# plot the points and color them by sector 2000-2010
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 2000 & birds$Year < 2010 & birds$Species == 'Eastern Kingbird',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 2000-2010") 
#2010+
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 2010 & birds$Species == 'Eastern Kingbird',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: >2010") 
# 2019
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year == 2019 & birds$Species == 'Eastern Kingbird',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 2019") 


#MISSISSIPPI KITE
# plot the points and color them by sector <1980
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year < 1980 & birds$Species == 'Mississippi Kite',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: < 1980") 
# plot the points and color them by sector 1980-1990
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 1980 & birds$Year < 1990 & birds$Species == 'Mississippi Kite',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 1980-1990") 
# plot the points and color them by sector 1990-2000
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 1990 & birds$Year < 2000 & birds$Species == 'Mississippi Kite',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 1990-2000") 
# plot the points and color them by sector 2000-2010
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 2000 & birds$Year < 2010 & birds$Species == 'Mississippi Kite',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 2000-2010") 
#2010+
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year > 2010 & birds$Species == 'Mississippi Kite',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: >2010") 
# 2019
ggmap(tx_big) + 
  stat_density2d(data = birds[birds$Year == 2019 & birds$Species == 'Mississippi Kite',], aes(x = longitude, y = latitude, fill=Species),alpha=0.3, geom='polygon') +
  ggtitle(label = "Distribution of Selected Birds: 2019") 



#CLIMATE DATA
weather <- read.csv('/Users/avanelson/Desktop/IST719 Data Viz/Poster Graphics/Final/historysummary_data.csv')
weather$Year <- c(1980:2019)

plot(weather$Year, 
     weather$Maximum.Temperature, 
     main= "TX Maximum Temperature",
     xlab= "Year", ylab= "Temp F", ylim= c(0,110), type='l')

plot(weather$Year, 
     weather$Minimum.Temperature, 
     main= "TX Min Temperature",
     xlab= "Year", ylab= "Temp F", type='l')

plot(weather$Year, 
     weather$Temperature, 
     main= "TX Avg Temperature",
     xlab= "Year", ylab= "Temp F", type='l')


sum(birds$SpeciesTotal[birds$Species == 'Mississippi Kite' & birds$Year == 2019])
sum(birds$SpeciesTotal[birds$Species == 'Mississippi Kite' & birds$Year == 1967])
