rm(list=ls())
library(exuber)
library(tidyverse)
library(tseries)
library(ggplot2)
library(ggfortify)

prices <- read.csv('./output-data/prices.csv')
prices$TAN <- as.numeric(prices$TAN)
prices$Date <- as.Date(prices$Date)

sample_size <- nrow(prices)
r0 <- 0.1

minw <- sample_size * r0
minw <- as.integer(minw)

radf_results <- radf(prices, minw = minw, lag = 1)
mc_critical_values <- radf_mc_cv(sample_size, minw = minw, seed = 145)

summary_results <- summary(radf_results, mc_critical_values)
summary_results

option <- 'gsadf'

autoplot(radf_results, mc_critical_values, option = option, legendLabs = NULL) +
  labs(x = 'Date', y = 't-stat') +
  scale_color_manual(values=c("red", "black")) +
  theme_classic() +
  theme(text=element_text(size=12,  family="serif")) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months")

ggsave(glue::glue('./figures/', option, '_prediciton.png'), width = 20, height = 8, units = 'cm')
stargazer(as.data.frame(summary_results), summary = F)
summary_results
diagnostics(radf_results, mc_critical_values, option = option)

stargazer(as.data.frame(datestamp(radf_results, mc_critical_values, option = option)), summary = F)
