##set working directory

setwd("C:/Users/acer/Desktop/QDA_Assignment1")

##read csv file

sus_rtlx <- read.csv("SusRtlx.csv")

##inspect first 6 and last 6 rows

head(sus_rtlx)
tail(sus_rtlx)

## Getting rid of ID column

sus_rtlx$ID = NULL

##inspect whole dataset

View(sus_rtlx)

# Descriptive statistics for both continuous variables

mean(sus_rtlx$SUS.Score)
sd(sus_rtlx$SUS.Score)
max(sus_rtlx$SUS.Score)
min(sus_rtlx$SUS.Score)
median(sus_rtlx$SUS.Score)
IQR(sus_rtlx$SUS.Score)

mean(sus_rtlx$RTLX.Score)
sd(sus_rtlx$RTLX.Score)
max(sus_rtlx$RTLX.Score)
min(sus_rtlx$RTLX.Score)
median(sus_rtlx$RTLX.Score)
IQR(sus_rtlx$RTLX.Score)

## System Usability Scale negative value retrieved

which(sus_rtlx$SUS.Score<0)

## System Usability Scale max value retrieved

which(sus_rtlx$SUS.Score==max(sus_rtlx$SUS.Score))

## Dataset clean by getting rid of the two datapoints outside SUS range

sus_rtlx_negr <- sus_rtlx[-92,]

sus_rtlx_clean <- sus_rtlx_negr[-88,]

View(sus_rtlx_clean)

## Descriptive statistics of clean dataset for the two continuous variables

mean(sus_rtlx_clean$SUS.Score)
sd(sus_rtlx_clean$SUS.Score)
max(sus_rtlx_clean$SUS.Score)
min(sus_rtlx_clean$SUS.Score)
median(sus_rtlx_clean$SUS.Score)
IQR(sus_rtlx_clean$SUS.Score)

mean(sus_rtlx_clean$RTLX.Score)
sd(sus_rtlx_clean$RTLX.Score)
max(sus_rtlx_clean$RTLX.Score)
min(sus_rtlx_clean$RTLX.Score)
median(sus_rtlx_clean$RTLX.Score)
IQR(sus_rtlx_clean$RTLX.Score)

## Histogram of System Usability Scale. Histogram show System Usability as normally distributed

hist(sus_rtlx_clean$SUS.Score, main ="Histogram of System Usability Scale", xlab = 'System Usability Scale', breaks = c(0,20,40,60,80,100))

## Box plot of System Usability Scale to check for outliers

boxplot(sus_rtlx_clean$SUS.Score, main = "Boxplot of System Usability Scale", ylim = c(0,100), ylab = "System Usability Scale")

## Histogram of Raw NASA Task Load Index. Histogram show Raw NASA Tasks Load Index as normally distributed

hist(sus_rtlx_clean$RTLX.Score, main = "Histogram of Raw NASA Tasks Load Index", xlab = 'Raw NASA Tasks Load Index', breaks = c(10,20,30,40,50,60,70))

## Box plot of Raw NASA Tasks Load Index

boxplot(sus_rtlx_clean$RTLX.Score, main = "Boxplot of Raw NASA Tasks Load Index", ylim = c(10,70), ylab = "Raw NASA Tasks Load Index")

## Pearson's correlation of the two continuous variables

cor.test(sus_rtlx_clean$SUS.Score, sus_rtlx_clean$RTLX.Score)

## Loading ggplot2

library(tidyverse)

## Scatter plot of association between the two continuous variables

sus_rtlx_assoc <- ggplot(data = sus_rtlx_clean,
                         mapping = aes(x = SUS.Score,
                                       y = RTLX.Score))
sus_rtlx_assoc + geom_point(color = "blue", alpha = 0.6, size = 2)+
                 geom_smooth(color = "red", se = FALSE, method = "lm")+
                 labs(x = "System Usability Scale", 
                      y = "Raw NASA Tasks Load Index",
                      title = "Scatter Plot of System Usability Scale and Raw NASA Tasks Load Index")+
                  theme_set(theme_test(base_size = 9))

ggsave(filename = "SUSRTLX_figure.pdf")


## Normal distribution curve on the histogram of Raw NASA Tasks Load Index to show nomality

sus_den <- ggplot(data = sus_rtlx_clean, aes(SUS.Score))+
                    geom_histogram(aes(y = ..density..), color = "blue", fill = "lightblue", alpha = 0.6) +
                    labs(x = "System Usability Scale score", y = "Density") +
                    stat_function(fun = dnorm, 
                    args = list(mean = mean(sus_rtlx_clean$SUS.Score), sd = sd(sus_rtlx_clean$SUS.Score)),
                    color = "black", size = 1)
sus_den

## Save file

ggsave(filename = "SUS_den_figure.pdf")

## Normal distribution curve on the histogram of System Usability Scale to show nomality

rtlx_den <- ggplot(data = sus_rtlx_clean, aes(RTLX.Score))+
                    geom_histogram(aes(y = ..density..), color = "green", fill = "lightgreen", alpha = 0.6) +
                    labs(x = "Raw NASA Tasks Load Index score", y = "Density") +
                    stat_function(fun = dnorm, 
                    args = list(mean = mean(sus_rtlx_clean$RTLX.Score), sd = sd(sus_rtlx_clean$RTLX.Score)),
                    color = "black", size = 1)
rtlx_den

## Save pdf file

ggsave(filename = "RTLX_den_figure.pdf")


