# Code for Homework 2

rm(list = ls())

library(zoo)
library(lmtest)
library(sandwich)
require(graphics)
library(dplyr)
library(ggplot2)
library(stargazer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Question 1
FF_data <- read.csv("F-F_Research_Data_5_Factors_2x3.csv",
                    skip = 3, stringsAsFactors=FALSE)
RMW <- FF_data[1:712,c(1,5)] %>%
  mutate(Date = as.Date(paste0(Date,"01"), "%Y%m%d"),
         RMW = as.numeric(RMW))

ggplot(RMW, aes(x=Date, y=RMW)) +
  geom_line() + 
  theme_bw() +
  xlab("") +
  scale_x_date(date_labels = "%m-%Y")

annualized_mean<-12*mean(RMW$RMW)
annaulized_std<-sqrt(12)*sd(RMW$RMW)

autocorr<-acf(RMW$RMW,lag.max=60, type = c("correlation"))
cumcorr<-cumsum(autocorr$acf)-1

plot(autocorr)
plot(cumcorr,type="l")

Box.test(RMW$RMW, lag = 6, type = "Ljung-Box")

# regression
RMW$sums40 <- rollsumr(RMW$RMW, k = 40, fill = NA)

RMW$lead<-lead(RMW$RMW,1)

a<-lm(RMW$lead~RMW$RMW+RMW$sums40)

stargazer(a, title="Results", align=TRUE)

coeftest(a, vcov = vcovHC(a, type = "HC0"))
H<-vcovHC(a,type="HC0")
plot(12*predict(a),type="l")

## Question 2
GDP_data <- read.csv("Table.csv",skip = 1) %>%
  select(year, quarter, Gross.domestic.product.1) %>%
  rename(GDP = Gross.domestic.product.1) %>%
  mutate(gdp_growth = log(GDP) - log(lag(GDP))) %>%
  filter(!is.na(gdp_growth)) %>%
  mutate(Date = as.Date(as.character(as.numeric(substr(quarter,2,2))*300 +
                                       1 + year*10000), "%Y%m%d"))

mean_gdp_growth <- mean(GDP_data$gdp_growth)
std_gdp_growth <- sd(GDP_data$gdp_growth)
skew_gdp_growth <- skewness(GDP_data$gdp_growth, type = 2)
kurt_gdp_growth <- kurtosis(GDP_data$gdp_growth, type = 2)

ggplot(GDP_data, aes(x=Date, y=gdp_growth)) +
  geom_line() + 
  theme_bw() +
  xlab("") +
  scale_x_date(date_labels = "%m-%Y")

AutoCorrelation <- acf(GDP_data$gdp_growth,
                       lag.max = 20, plot = FALSE)
plot(AutoCorrelation, main = "GDP Growth ACF (including COVID)")

AutoCorrelation <- acf(GDP_data$gdp_growth[GDP_data$year < 2020],
                       lag.max = 20, plot = FALSE)
plot(AutoCorrelation, main = "GDP Growth ACF (excluding COVID)")
