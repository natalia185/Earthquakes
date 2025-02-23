install.packages('stringr')
install.packages('reshape')
install.packages("maps")
install.packages("tidyverse")
install.packages( "rgeos","lubridate")
install.packages('corrplot')
install.packages('bookdown')

#library("ggplot2")
library('corrplot')
#theme_set(theme_bw())
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
#map(database = "world")
library(knitr)

earthquakes = read.csv("Earthquakes.csv")

#typy danych w tabelce
df_type <- sapply(earthquakes, class)
kable(df_type, col.names=c('typ'))

# E(mag) dla miejsc
place <- vapply(strsplit(earthquakes$place,","), `[`, 2, FUN.VALUE=character(1))

df_Emag <- data.frame(miejsce=miejsce, mag=earthquakes$mag) %>% drop_na()
df_Emag <- subset(df_Emag, miejsce != " ") 
df_Emag <- df_Emag[order(df_Emag$miejsce),]
num <- df_Emag %>% count(miejsce) 

df <- data.frame()
for( i in 1:length(num$miejsce)){
  col <- c()
  for( j in 1:length(df_Emag$miejsce)){
    if(num$miejsce[i] == df_Emag$miejsce[j]){
      col <- append(col, df_Emag$mag[j])
    }
  }
  meancol <- mean(col)
  df <- rbind(df, meancol)
}
df <- cbind(df, num$miejsce)
colnames(df) <- c('Emag', 'miejsce')

### Histogram ###
# 12 najczęstszych miejsc wybrane #
miejsce <- vapply(strsplit(earthquakes$place,","), `[`, 2, FUN.VALUE=character(1))

most_fr_eq <- data.frame(number=rev(sort(table(miejsce)))[1:12])
colnames(most_fr_eq)[1] <- 'miejsce'
colnames(most_fr_eq)[2] <- 'częstość'

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


### Mapka 2.0 ###
dfmap <- data.frame(longitude=earthquakes$longitude, 
                    latitude=earthquakes$latitude,
                    mag=earthquakes$mag)
dfmap <- dfmap %>% drop_na(mag)

world <- map_data("world")

world_map <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = dfmap,
    aes(longitude, latitude, color = mag),
    alpha = 0.7
  ) + labs(x='', y='') + scale_colour_gradient(low = "hotpink", high = "blue")

world_map


### Correlations ###

#### Pojedyncze bo tak o ####

## korelacja pomiędzy liczbą trzęsień a ich sklalą <- chciałam zrobić, ale średnio mi idzie, może jutro
transform_data <- data.frame(place=earthquakes$place, mag=earthquakes$mag)
transform_data$place <- vapply(strsplit(earthquakes$place,","), `[`, 2, FUN.VALUE=character(1))
transform_data <- transform_data %>% drop_na()
transform_data <- subset(transform_data, place != " ")
counter <- transform_data %>% count(place)

ggplot(transform_data, aes(x=place, y=mag)) + geom_boxplot() +coord_flip()


## Liczba stacji w stosunku do lat
years <- vapply(strsplit(earthquakes$Date,"-"), `[`, 1, FUN.VALUE=character(1))
df_nst_years <- data.frame(years,earthquakes$nst)
colnames(df_nst_years)[2] <- 'nst'
df_nst_years <- df_nst_years %>% drop_na()

ggplot(df_nst_years,aes(x=years,y=nst)) + geom_point(color='hotpink') + scale_x_discrete(breaks=seq(1900, 2013, 10))

cor(as.numeric(df_nst_years$years), as.numeric(df_nst_years$nst))

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

cor(as.numeric(eq_to_time$częstość), as.numeric(eq_to_time$czas), method='spearman')

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
