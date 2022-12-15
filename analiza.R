install.packages('stringr')
install.packages('reshape')
install.packages("maps")
install.packages("tidyverse")
install.packages( "rgeos","lubridate")
install.packages('corrplot')
install.packages('bookdown')

library("ggplot2")
library('corrplot')
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
library(dplyr)
map(database = "world")

earthquakes = read.csv("Earthquakes.csv")


### Histogram ###
# 12 najczęstszych miejsc wybrane #
miejsce <- vapply(strsplit(earthquakes$place,","), `[`, 2, FUN.VALUE=character(1))

most_fr_eq <- data.frame(number=rev(sort(table(miejsce)))[1:12])
colnames(most_fr_eq)[1] <- 'miejsce'
colnames(most_fr_eq)[2] <- 'częstość'
most_fr_eq <- most_fr_eq %>% drop_na()

ggplot(most_fr_eq, aes(x = miejsce, y = częstość)) + geom_col(fill = 'hotpink') + 
  theme(axis.text.x = element_text(angle = 90)) + geom_text(aes(label = częstość), nudge_y = -20, col='white') +
  labs(x='kraj', y='liczba wystąpień')


### Mapka ###
eq_points_map_lg <- earthquakes$longitude
eq_points_map_lat <- earthquakes$latitude
dfmap <- data.frame(eq_points_map_lat,eq_points_map_lg)
dfmap <- dfmap %>% drop_na()

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
  ) + labs(x='', y='')

### Correlations ###

#### Pojedyncze bo tak o ####

## Liczba stacji w stosunku do lat
years <- vapply(strsplit(earthquakes$Date,"-"), `[`, 1, FUN.VALUE=character(1))
df_nst_years <- data.frame(years,earthquakes$nst)
colnames(df_nst_years)[2] <- 'nst'
df_nst_years <- df_nst_years %>% drop_na()

ggplot(df_nst_years,aes(x=years,y=nst)) + geom_point(color='hotpink') + scale_x_discrete(breaks=seq(1900, 2013, 10))

# Z czasem powstało więcej stacji badawczych trzęsienia ziemi :) 

# Liczba trzęsień ziemi w stosunku do lat
eq_to_years <- data.frame(number=sort(table(years)))
colnames(eq_to_years)[1] <- 'lata'
colnames(eq_to_years)[2] <- 'częstość'
eq_to_years <- eq_to_years %>% drop_na()

ggplot(eq_to_years,aes(x=lata,y=częstość)) + geom_col(color='hotpink') + scale_x_discrete(breaks=seq(1900, 2013, 15)) + theme(axis.text.x = element_text(angle = 90))

# Liczba co do miesiąca
miesiace <- vapply(strsplit(earthquakes$Date,"-"), `[`, 2, FUN.VALUE=character(1))

eq_to_months <- data.frame(number=table(miesiace))
colnames(eq_to_months)[1] <- 'miesiace'
colnames(eq_to_months)[2] <- 'częstość'
eq_to_months <- eq_to_months %>% drop_na()

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
eq_to_time <- eq_to_time %>% drop_na()

ggplot(eq_to_time,aes(x=czas,y=częstość)) + geom_col(fill='hotpink') + theme(axis.text.x = element_text(angle = 90)) +
  labs(y='liczba wystąpień')

#Czyli pora dnia też nie, sad :/


#Głębokość(?) a siła trzęsienia
dfmd <- data.frame(earthquakes$mag,earthquakes$depth)
colnames(dfmd)[1] <- 'siła'
colnames(dfmd)[2] <- 'głębokość'
dfmd <- dfmd %>% drop_na()

ggplot(dfmd,aes(x=głębokość,y=siła)) + geom_point(color='hotpink')
# nie ma korelacji między siłą a głębokością, ale to widać na macierzy korelacji więc naura


# Siła a lata
df_mag_years <- data.frame(earthquakes$mag,years)
colnames(df_mag_years)[1] <- 'siła'
colnames(df_mag_years)[2] <- 'lata'
df_mag_years <- df_mag_years %>% drop_na()

ggplot(df_mag_years,aes(y=siła,x=lata)) + geom_point(color='hotpink') + scale_x_discrete(breaks=seq(1900, 2013, 15))


### OGÓLNY ZESTAW KORELACJI BO JEST FUNKCJA :D #####
df_allnum <- earthquakes
df_allnum$years <- years
keeps <- c("depth","mag",'years',"nst",'longitude','latitude')

df_allnum$years <- as.numeric(as.character(df_allnum$years))

df_allnum <- df_allnum[keeps]
df_allnum <- transform(df_allnum,as.numeric(years))
df_allnum <- df_allnum %>% drop_na()


############ Jak ogarniemy braki - usunąć paramter use ########## usunęłam i działa, SUKCES!!!

corrplot(cor(df_allnum),method = 'number',col = COL2('PuOr'))


### Boxploty ###
df_mag_depth <- data.frame('mag'=earthquakes$mag, 'depth'=earthquakes$depth) %>% drop_na()

#Głębokość
ggplot(df_mag_depth,aes(x=depth)) + geom_boxplot(color='hotpink')

#Siła
ggplot(df_mag_depth,aes(x=mag)) + geom_boxplot(color='hotpink')
