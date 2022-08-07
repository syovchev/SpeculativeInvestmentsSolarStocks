library(tidyverse)
library(ggplot2)
library(directlabels)
library(wesanderson)
library(quantmod)
library(priceR)

# Figure 1. 
# Solar energy returns vs the broad market
df <- read.csv('output-data/solar_energy_returns_vs_market.csv') %>%
  pivot_longer(cols = c('TAN', 'SPY', 'URTH'),
                        names_to  = 'ETF',
                        values_to = 'Returns')

plt <- ggplot(df, aes(x=as.Date(Date), y = Returns, group = ETF, linetype = ETF)) +
  geom_line(size = 0.5) +
  labs(title='Solar energy stocks vs the broader market',
       subtitle = '(January 2017 - January 2022)') +
  xlab('Date') + 
  ylab('Returns') +
  theme_classic() +
  theme(text=element_text(size=12,  family="serif")) +
  scale_color_brewer(palette="Greys") +
  # scale_color_manual(values=wes_palette(n=3, name="Moonrise1")) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months")

plt
ggsave(filename = 'figures/Solar energy stocks vs the broader market.png', plot = plt, width = 20, height = 12, units = 'cm')

# Solar energy share of renewable electricity mix
df <- read.csv('input-data/iea-sources/solar_share_long.csv')
plt <- ggplot(df, aes(x = Year, y = Share, group = Country, linetype = Country)) +
  labs(title='Solar PV share of renewable electricity mix',
       subtitle = '(1990 - 2020)') +
  ylab('Share (%)') +
  geom_line(size = 0.5) +
  theme_classic() +
  theme(text=element_text(size=12,  family="serif"))
ggsave(filename = 'figures/Solar share of renewable electricity mix.png', plot = plt, width = 20, height = 12, units = 'cm')

df <- read.csv('input-data/lcoe.csv')
plt <- ggplot(df, aes(x = Year, y = LCOE, group = Technology, linetype = Technology)) +
  labs(title='Global weighted-average LCOE',
       subtitle = '(2010 - 2020)') +
  ylab('LCOE ($/kWh)') +
  geom_line(size = 0.5, show.legend = FALSE) +
  theme_classic() +
  theme(text=element_text(size=12,  family="serif")) +
  xlim(2008,2022) +
  geom_dl(aes(label = Technology), method = list(dl.combine("first.points", "last.points"), cex = 0.9, fontfamily = 'serif'))
plt
ggsave(filename = 'figures/LCOE.png', plot = plt, width = 20, height = 17, units = 'cm')

df <- read.csv('./lm-data/endogen.csv') 
#df <- read.csv('output-data/solar_energy_returns_vs_market.csv')
# df$X0 <- c(-diff(df$TAN)/df$TAN[-1] *  100, NA)

ggplot(df, aes(x = X0)) +
  geom_histogram(binwidth = 1, color="black", fill="gray") +
  geom_vline(aes(xintercept=mean(X0, na.rm = TRUE)),
             color="blue", linetype="dashed", size=1.05) +
  xlab('TAN Weekly Returns') +
  ylab('') +
  theme_classic() +
  theme(text=element_text(size=12,  family="serif"))

ggsave(filename = 'figures/Weekly returns histogram.png', width = 20, height = 17, units = 'cm')


df <- read.csv('output-data/prices.csv')
plt <- ggplot(df, aes(x = as.Date(Date), y = TAN, group = 1)) +
  labs(title='TAN Price Movements',
       subtitle = '(January 2017 - January 2022)') +
  xlab('Date') +
  ylab('USD') +
  geom_line(size = 0.5, show.legend = FALSE) +
  theme_classic() +
  theme(text=element_text(size=12,  family="serif")) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months")
plt
ggsave(filename = 'figures/TAN prices.png', width = 20, height = 12, units = 'cm')
