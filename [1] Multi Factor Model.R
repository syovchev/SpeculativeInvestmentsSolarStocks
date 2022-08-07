rm(list=ls())
library(stargazer)

# OLS Model
endogen <- read.csv('./lm-data/endogen.csv') %>% dplyr::select('X0')
exogen  <- read.csv('./lm-data/exogen.csv')  %>% dplyr::select(-c(Date, const))

model <- lm(endogen$X0 ~ ., data = exogen)

summary(model)
stargazer(coef(summary(model)), type = 'latex')