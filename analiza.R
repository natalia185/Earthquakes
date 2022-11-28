install.packages('stringr')

library(ggplot2)
library(stringr)

earthquakes = read.csv("Earthquakes.csv")

#place_separated <-str_split(earthquakes$place, " , ", 2)
#place_separated

test <- vapply(strsplit(earthquakes$place,","), `[`, 2, FUN.VALUE=character(1))
test
#ggplot(test) + geom_histogram()
table(test)
## dużo danych, przesiejemy je na te z najczęstszymi wystąpieniami obviously 

