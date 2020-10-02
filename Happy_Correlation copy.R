library(pacman)

pacman::p_load(dplyr, DataExplorer, ROCR, pROC, ggplot2)

#loading in data:
happydraft<-read.csv("HappyData.csv")

#introducing the data
library(DataExplorer)
introduce(happydraft)

#Missing Data
plot_missing(happydraft)
str(happydraft)

### Cleaning the Data
library(dplyr)
happydraft<-select(happydraft, -Most.people.can.be.trusted..WVS.round.2010.2014, -GINI.index..World.Bank.estimate., -Most.people.can.be.trusted..WVS.round.2005.2009, -Most.people.can.be.trusted..WVS.round.1994.1998, -Most.people.can.be.trusted..WVS.round.1999.2004, -Most.people.can.be.trusted..WVS.round.1989.1993, -Most.people.can.be.trusted..Gallup, -Most.people.can.be.trusted..WVS.round.1981.1984)
str(happydraft)
plot_missing(happydraft) #confirms that bad/remove were removed from the data

#reintroduce data to see what is missing now
library(DataExplorer)
introduce(happydraft)

happydraft$year <- as.factor(happydraft$year)

#correlations:
happy_nums<-unlist(lapply(happydraft, is.numeric))
happy_nums
happy_nums2<-happydraft[, happy_nums]
happy_nums2
str(happy_nums2)

#correlation plot
M<-cor(happy_nums2, use = "complete.obs")
library("corrplot")
corrplot(M, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = TRUE)
#this is an easy way to visualize associations between variables. strong postivie associations are indicated by dark blue dots, and strong negative are indicated by dark red.
#There are strong assocaitions between the standard.deviation.of.ladder.by.country.year and the standard.deviation.mean.of.ladder.by.coutnry.year, standard.deviation.mean.of.ladder.by.coutnry.year and Life.ladder.
#i will not include the second in the regression analysis because that is with the target variable and will be skewed..


#imputing the values for the rest of the missing data:
#gini.of.household.income.reported.in.Gallup..by.wp5.year
summary(happydraft$gini.of.household.income.reported.in.Gallup..by.wp5.year)
happydraft$M_gini.of.household.income.reported.in.Gallup..by.wp5.year<-as.factor(ifelse(is.na(happydraft$gini.of.household.income.reported.in.Gallup..by.wp5.year), 1, 0))
happydraft$gini.of.household.income.reported.in.Gallup..by.wp5.year[is.na(happydraft$gini.of.household.income.reported.in.Gallup..by.wp5.year)]<-median(happydraft$gini.of.household.income.reported.in.Gallup..by.wp5.year, na.rm=TRUE)
summary(happydraft$gini.of.household.income.reported.in.Gallup..by.wp5.year)

#Imputation for confidence in national government:
summary(happydraft$Confidence.in.national.government)
happydraft$M_confidence.in.national.government<-as.factor(ifelse(is.na(happydraft$Confidence.in.national.government), 1, 0))
happydraft$Confidence.in.national.government[is.na(happydraft$Confidence.in.national.government)]<-median(happydraft$Confidence.in.national.government, na.rm=TRUE)
summary(happydraft$Confidence.in.national.government)

#imputation for GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel
summary(happydraft$GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel)
happydraft$M_GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel<-as.factor(ifelse(is.na(happydraft$GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel), 1, 0))
happydraft$GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel[is.na(happydraft$GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel)]<-median(happydraft$GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel, na.rm=TRUE)
summary(happydraft$GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel)

#imputation for Democratic.Quality
summary(happydraft$Democratic.Quality)
happydraft$M_Democratic.Quality<-as.factor(ifelse(is.na(happydraft$Democratic.Quality), 1, 0))
happydraft$Democratic.Quality[is.na(happydraft$Democratic.Quality)]<-median(happydraft$Democratic.Quality, na.rm=TRUE)
summary(happydraft$Democratic.Quality)

#imputation for DeliveryQuality
summary(happydraft$Delivery.Quality)
happydraft$M_Delivery.Quality<-as.factor(ifelse(is.na(happydraft$Delivery.Quality), 1, 0))
happydraft$Delivery.Quality[is.na(happydraft$Delivery.Quality)]<-median(happydraft$Delivery.Quality, na.rm=TRUE)
summary(happydraft$Delivery.Quality)

#imputation for perceptions of corruption:
summary(happydraft$Perceptions.of.corruption)
happydraft$M_Perceptions.of.corruption<-as.factor(ifelse(is.na(happydraft$Perceptions.of.corruption), 1, 0))
happydraft$Perceptions.of.corruption[is.na(happydraft$Perceptions.of.corruption)]<-median(happydraft$Perceptions.of.corruption, na.rm=TRUE)
summary(happydraft$Perceptions.of.corruption)

#imputation for generosity
summary(happydraft$Generosity)
happydraft$M_Generosity<-as.factor(ifelse(is.na(happydraft$Generosity), 1, 0))
happydraft$Generosity[is.na(happydraft$Generosity)]<-median(happydraft$Generosity, na.rm=TRUE)
summary(happydraft$Generosity)

#imputation for healthy life expectancy at birth:
summary(happydraft$Healthy.life.expectancy.at.birth)
happydraft$M_Healthy.life.expectancy.at.birth<-as.factor(ifelse(is.na(happydraft$Healthy.life.expectancy.at.birth), 1, 0))
happydraft$Healthy.life.expectancy.at.birth[is.na(happydraft$Healthy.life.expectancy.at.birth)]<-median(happydraft$Healthy.life.expectancy.at.birth, na.rm=TRUE)
summary(happydraft$Healthy.life.expectancy.at.birth)

#imputation for freedom to make life choices
summary(happydraft$Freedom.to.make.life.choices)
happydraft$M_Freedom.to.make.life.choices<-as.factor(ifelse(is.na(happydraft$Freedom.to.make.life.choices), 1, 0))
happydraft$Freedom.to.make.life.choices[is.na(happydraft$Freedom.to.make.life.choices)]<-median(happydraft$Freedom.to.make.life.choices, na.rm=TRUE)
summary(happydraft$Freedom.to.make.life.choices)

#imputation for Log.GDP per capita
summary(happydraft$Log.GDP.per.capita)
happydraft$M_Log.GDP.per.capita<-as.factor(ifelse(is.na(happydraft$Log.GDP.per.capita), 1, 0))
happydraft$Log.GDP.per.capita[is.na(happydraft$Log.GDP.per.capita)]<-median(happydraft$Log.GDP.per.capita, na.rm=TRUE)
summary(happydraft$Log.GDP.per.capita)

#imputation for positive affect
summary(happydraft$Positive.affect)
happydraft$M_Positive.affect<-as.factor(ifelse(is.na(happydraft$Positive.affect), 1, 0))
happydraft$Positive.affect[is.na(happydraft$Positive.affect)]<-median(happydraft$Positive.affect, na.rm=TRUE)
summary(happydraft$Positive.affect)

#imputation for negative affect:
summary(happydraft$Negative.affect)
happydraft$M_Negative.affect<-as.factor(ifelse(is.na(happydraft$Negative.affect), 1, 0))
happydraft$Negative.affect[is.na(happydraft$Negative.affect)]<-median(happydraft$Negative.affect, na.rm=TRUE)
summary(happydraft$Negative.affect)

#imputation for social support
summary(happydraft$Social.support)
happydraft$M_Social.support<-as.factor(ifelse(is.na(happydraft$Social.support), 1, 0))
happydraft$Social.support[is.na(happydraft$Social.support)]<-median(happydraft$Social.support, na.rm=TRUE)
summary(happydraft$Social.support)

#library(Hmisc)
#impute(happydraft$Social.support, mode)  # replace with mode
#summary(happydraft$Social.support)

#mode for social support
#summary(happydraft$Social.support)
#happydraft$M_Social.support<-as.factor(ifelse(is.na(happydraft$Social.support), 1, 0))
#happydraft$Social.support[is.na(happydraft$Social.support)]<-mode(happydraft$Social.support)
#summary(happydraft$Social.support)

#remove M columns after imputation
library(dplyr)
happydraft <- select(happydraft, -M_Social.support, -M_Negative.affect, -M_Positive.affect, -M_Log.GDP.per.capita, -M_Freedom.to.make.life.choices, -M_Healthy.life.expectancy.at.birth, -M_Generosity, -M_Perceptions.of.corruption, -M_Delivery.Quality, -M_Democratic.Quality, -M_GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel, -M_confidence.in.national.government, -M_gini.of.household.income.reported.in.Gallup..by.wp5.year) 

#the rest of the variables have 0 missing data
plot_missing(happydraft)

#regression plot:
happydraft.reg<-lm(Life.Ladder~.-year-Country.name, data=happydraft)
summary(happydraft.reg)

#regression plot with just the significant factors at alpha = 0.05
reg.final<-lm(Life.Ladder~Social.support+Log.GDP.per.capita+Freedom.to.make.life.choices+Generosity+Perceptions.of.corruption+Positive.affect+Negative.affect+Confidence.in.national.government+Democratic.Quality+Delivery.Quality+GINI.index..World.Bank.estimate...average.2000.2017..unbalanced.panel+gini.of.household.income.reported.in.Gallup..by.wp5.year+Standard.deviation.of.ladder.by.country.year, data=happydraft)
summary(reg.final)
#lower multiple r2 than model1, but model 1 has 1 variable that is highly correlated with the y variable

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(reg.final)

plot(reg.final)

plot(reg.final$coefficients)

install.packages("coefplot")
library(coefplot)
coefplot(reg.final)
coefplot(reg.final, xlab = "Coefficient Confidence Intervals", varnames=NULL, intercept=FALSE)
