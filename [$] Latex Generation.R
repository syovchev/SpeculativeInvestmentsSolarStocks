library(stargazer)
library(olsrr)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

# IEA Solar Share Table
solar_share <- read.csv('input-data/iea-sources/solar_share.csv')
solar_share
stargazer(solar_share, type = 'latex', summary = FALSE)

# OLS Model
endogen <- read.csv('./endogen.csv') %>% dplyr::select('X0')
exogen  <- read.csv('./exogen.csv')  %>% dplyr::select(-c(Date, const))

stargazer(endogen, summary.stat  = c('n', 'mean', 'sd', 'min', 'median', 'max'))
stargazer(exogen, summary.stat  = c('n', 'mean', 'sd', 'min', 'median', 'max'))

#Sundix
sundix <- read.csv('./input-data/SUNDIX Stocks.csv')
stargazer(sundix, summary = F)

model <- lm(endogen$X0 ~ ., data = exogen)
summary(model)
stargazer(coef(summary(model)), type = 'latex')


prices <- read.csv('./output-data/prices.csv')
stargazer(prices, summary = T, summary.stat  = c('n', 'mean', 'sd', 'min', 'median', 'max'))
