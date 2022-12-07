install.packages('stringr')
install.packages('reshape')
install.packages("maps")
install.packages("tidyverse")
install.packages( "rgeos","lubridate")                 

library("ggplot2")  
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
world <- ne_countries(scale = "medium", returnclass = "sf")

library(ggplot2)
library(stringr)
library(reshape)
library(maps)
library(tidyverse)
library(lubridate)
map(database = "world")

earthquakes = read.csv("Earthquakes.csv")


### Histogram ###
# 12 najczęstszych miejsc wybrane #
miejsce <- vapply(strsplit(earthquakes$place,","), `[`, 2, FUN.VALUE=character(1))

most_fr_eq <- data.frame(number=rev(sort(table(miejsce)))[1:12])
colnames(most_fr_eq)[1] <- 'miejsce'
colnames(most_fr_eq)[2] <- 'częstość'

ggplot(most_fr_eq, aes(x = miejsce, y = częstość)) + geom_col(fill = 'hotpink') + 
  theme(axis.text.x = element_text(angle = 90)) + geom_text(aes(label = częstość), nudge_y = -20, col='white')

### Mapka ###
eq_points_map_lg <- earthquakes$longitude
eq_points_map_lat <- earthquakes$latitude
dfmap <- data.frame(eq_points_map_lat,eq_points_map_lg)

world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = dfmap,
    aes(eq_points_map_lg, eq_points_map_lat, color = 'red'),
    alpha = 0.7
  ) 

### Correlations ###

## Liczba stacji w stosunku do lat
years <- vapply(strsplit(earthquakes$Date,"-"), `[`, 1, FUN.VALUE=character(1))
df_nst_years <- data.frame(years,earthquakes$nst)
colnames(df_nst_years)[2] <- 'nst'

ggplot(df_nst_years,aes(x=years,y=nst)) + geom_point(color='hotpink') + scale_x_discrete(breaks=seq(1900, 2013, 10))

# Z czasem powstało więcej stacji badawczych trzęsienia ziemi :) 

# Liczba trzęsień ziemi w stosunku do lat
eq_to_years <- data.frame(number=sort(table(years)))
colnames(eq_to_years)[1] <- 'lata'
colnames(eq_to_years)[2] <- 'częstość'

ggplot(eq_to_years,aes(x=lata,y=częstość)) + geom_col(color='hotpink') + scale_x_discrete(breaks=seq(1900, 2013, 15)) + theme(axis.text.x = element_text(angle = 90))

# Liczba co do miesiąca
miesiace <- vapply(strsplit(earthquakes$Date,"-"), `[`, 2, FUN.VALUE=character(1))

eq_to_months <- data.frame(number=table(miesiace))
colnames(eq_to_months)[1] <- 'miesiace'
colnames(eq_to_months)[2] <- 'częstość'

ggplot(eq_to_months,aes(x=miesiace,y=częstość)) + geom_col(fill='hotpink') + theme(axis.text.x = element_text(angle = 90))

#Okej czyli pora roku nie wpływa na ilość, a szkoda, bo myślałam że może jednak lato coś :( 

# Time to eq
time_rounded <- rep(NA,length(earthquakes$Time))
for (i in 1:length(earthquakes$Time)){
  date_time <- paste(earthquakes$Date[i],earthquakes$Time[i])
 time_rounded[i] <- format(round(round_date(ymd_hms(date_time),'hour'), units="hours"), format="%H:%M") 
 
}
eq_to_time <- data.frame(number=table(time_rounded))
colnames(eq_to_time)[1] <- 'czas'
colnames(eq_to_time)[2] <- 'częstość'

ggplot(eq_to_time,aes(x=czas,y=częstość)) + geom_col(fill='hotpink') + theme(axis.text.x = element_text(angle = 90))

#Czyli pora dnia też nie, sad :/

#Głębokość(?) a siła trzęsienia
dfmd <- data.frame(earthquakes$mag,earthquakes$depth)
colnames(dfmd)[1] <- 'siła'
colnames(dfmd)[2] <- 'głębokość'

ggplot(dfmd,aes(x=głębokość,y=siła)) + geom_point(color='hotpink')
# nie ma korelacji między siłą a głębokością

# Siła a lata
df_mag_years <- data.frame(earthquakes$mag,years)
colnames(df_mag_years)[1] <- 'lata'
colnames(df_mag_years)[2] <- 'siła'
ggplot(df_mag_years,aes(y=lata,x=siła)) + geom_point(color='hotpink') + scale_x_discrete(breaks=seq(1900, 2013, 15))

### Boxploty ###
#Głębokość
ggplot(earthquakes,aes(x=depth)) + geom_boxplot(color='hotpink')

#Siła
ggplot(earthquakes,aes(x=mag)) + geom_boxplot(color='hotpink')


### For later ERRORS by Nans 
# https://jezykrwedlugtw.wordpress.com/o-jezyku-r-2/obrobka-danych-w-r/brakujace-dane/





