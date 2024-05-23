# Code for Homework 1, Quantitative Asset Management

rm(list = ls())

library(zoo)
library(lmtest)
library(sandwich)
require(graphics)
library(dplyr)
library(e1071)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)

## Question 1

FF_data <- read.csv("FF_data_HW1.csv")

mktrf <-  FF_data$Mkt.RF

mean_Mkt <- mean(mktrf)
std_Mkt <- sd(mktrf)
skew_Mkt <- skewness(mktrf, type = 2)
kurt_Mkt <- kurtosis(mktrf, type = 2)
SR_mkt <- mean_Mkt / std_Mkt

Ann_mean_mkt <- mean_Mkt * 12
Ann_std_mkt <- std_Mkt * sqrt(12)
Ann_SR_mkt <- Ann_mean_mkt / Ann_std_mkt

his_Mkt <- hist(mktrf,breaks=30)
data_pdf <- his_Mkt$counts / length(mktrf)
min3sd_cutoff <-  mean_Mkt - 3*std_Mkt
idx <- his_Mkt$breaks < min3sd_cutoff
Prob_min3 <- sum(data_pdf[idx])


hist(mktrf, density=20, breaks=30, prob=TRUE)
curve(dnorm(x, mean = mean_Mkt, sd = std_Mkt), 
      col="red", lwd=1, add=TRUE, yaxt="n")

# now use daily data

FF_data <- read.csv("FF_daily.csv")

mktrf <-  FF_data$Mkt.RF

mean_Mkt <- mean(mktrf)
std_Mkt <- sd(mktrf)
skew_Mkt <- skewness(mktrf, type = 2)
kurt_Mkt <- kurtosis(mktrf, type = 2)
SR_mkt <- mean_Mkt / std_Mkt

Ann_mean_mkt <- mean_Mkt * 12
Ann_std_mkt <- std_Mkt * sqrt(12)
Ann_SR_mkt <- Ann_mean_mkt / Ann_std_mkt

his_Mkt <- hist(mktrf,breaks=30)
data_pdf <- his_Mkt$counts / length(mktrf)
min3sd_cutoff <-  mean_Mkt - 3*std_Mkt
idx <- his_Mkt$breaks < min3sd_cutoff
Prob_min3 <- sum(data_pdf[idx])


hist(mktrf, density=20, breaks=30, prob=TRUE)
curve(dnorm(x, mean = mean_Mkt, sd = std_Mkt), 
      col="red", lwd=1, add=TRUE, yaxt="n")

## Question 2
# now revert to monthly data and plot drawdown and max drawdown

FF_data <- read.csv("FF_data_HW1.csv")

mktrf <-  FF_data$Mkt.RF/100
rf <-  FF_data$RF/100
# note, we have to divide by 100 as we need decimal returns.

# portfolio return, be careful to make returns 
rp <- 0.5 * (mktrf + rf) + 0.5*rf;
SR_5050 <- sqrt(12) * mean(rp-rf) / sd(rp-rf)

# Value of portfolio over time is then, assuming we start with $1:
tt <- length(rf)
Value <- c()

DD <- c(0)
MaxDD <- c(0)
Value[1] <- 1

for (jt in 1:tt){
  Value[jt+1] <-  Value[jt] * (1+rp[jt])
  DD[jt+1] <- (max(Value) - Value[jt+1]) /max(Value)
  MaxDD[jt+1] <- max(DD)
}

df <- data.frame(x = 1:length(DD), DD = DD, MaxDD = MaxDD) %>%
  gather(key = "variable", value = "value", -x)

p1 <- ggplot(df, aes(x = x, y = value)) + 
  theme_bw() +
  geom_line(aes(color = variable)) +#, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme(legend.position="bottom", legend.title = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())

# redo with leverage 
# portfolio return, be careful to make returns 
rp <- 3 * (mktrf +rf) - 2*rf # = 3*mktrf + rf
SR_300 <- sqrt(12) * mean(rp-rf) / sd(rp-rf)
# Value of portfolio over time is then, assuming we start with $1:
tt <- length(rf)
Value_lev <- c()

DD_lev <- c(0)
MaxDD_lev <- c(0)
Value_lev[1] <- 1

for (jt in 1:tt){
  Value_lev[jt+1] <-  Value_lev[jt] * (1+rp[jt])
  DD_lev[jt+1] <- (max(Value_lev) - Value_lev[jt+1]) /max(Value_lev)
  MaxDD_lev[jt+1] <- max(DD_lev)
}

df_lev <- data.frame(x = 1:length(DD_lev), DD_lev = DD_lev, MaxDD_lev = MaxDD_lev) %>%
  gather(key = "variable", value = "value", -x)

p2 <- ggplot(df_lev, aes(x = x, y = value)) + 
  theme_bw() +
  geom_line(aes(color = variable)) +#, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme(legend.position="bottom", legend.title = element_blank(),
        axis.title.x=element_blank(), axis.title.y=element_blank())


grid.arrange(p1, p2, nrow=2)

## Question 3
# case1
T=600
N=10000
M=1000
beta=rep(0,N)
t_stat=rep(0,N)
for(n in 1:N){
  r1=rnorm(M+T,mean = 0.005,sd = 0.04)
  r2=rnorm(M+T,mean = 0.005,sd = 0.04)
  r1=r1[(M+1):(M+T)]
  r2=r2[(M+1):(M+T)]
  output=lm(r1~r2)
  beta[n]=output$coefficients[2]
  HAC_se=sqrt(NeweyWest(output,lag = 5)[2,2])
  t_stat[n]=beta[n]/HAC_se
}

hist(beta,breaks = 100,main = "beta-case1")
mean(HAC_se)
hist(t_stat,breaks = 100, main = "t_stat-case1")
# case2
T=600
N=10000
M=1000
beta=rep(0,N)
t_stat=rep(0,N)
for (n in 1:N){
  p1=rep(0,M+T)
  p2=rep(0,M+T)
  r1=rnorm(M+T-1,mean = 0.005,sd = 0.04)
  r2=rnorm(M+T-1,mean = 0.005,sd = 0.04)
  for (t in 1:(M+T-1)){
    p1[t+1]=p1[t]+r1[t]
    p2[t+1]=p2[t]+r2[t]
  }
  p1=p1[(M+1):(M+T)]
  p2=p2[(M+1):(M+T)]
  output=lm(p1~p2)
  beta[n]=output$coefficients[2]
  HAC_se=sqrt(NeweyWest(output,lag = 5)[2,2])
  t_stat[n]=beta[n]/HAC_se
}
hist(beta,breaks=100,main = "beta-case2")
hist(t_stat,breaks=100,main = "t_stat-case2")
