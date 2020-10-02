
happy<-read.csv("HappyData.csv")
str(happy)

library(DataExplorer)
introduce(happy)
plot_missing(happy)