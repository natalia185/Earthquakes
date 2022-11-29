install.packages('stringr')
install.packages('reshape')

library(ggplot2)
library(stringr)
library(reshape)

earthquakes = read.csv("Earthquakes.csv")

#place_separated <-str_split(earthquakes$place, " , ", 2)
#place_separated

test <- vapply(strsplit(earthquakes$place,","), `[`, 2, FUN.VALUE=character(1))

most_fr_eq <- data.frame(number=rev(sort(table(test)))[1:12])
colnames(most_fr_eq)[1] <- 'place'
colnames(most_fr_eq)[2] <- 'frequency'
most_fr_eq

#par(mar=c(100,300,2,10))
ggplot(most_fr_eq, aes(x = place, y = frequency)) + geom_col(fill = 'hotpink') + 
  theme(axis.text.x = element_text(angle = 90)) + geom_text(aes(label = frequency), nudge_y = -20, col='white')
