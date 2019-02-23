library(readr)
library(dplyr)
library(readr)
library(stringr)
library(caret)
library(tm)
library(ggplot2)

indicators <- read.csv("~/world-development-indicators/Indicators.csv")
str(indicators)


south_asia <- c("AFG","BGD", "BTN", "IND", "MDV", "NPL", "PAK", "LKA")
south_asia_indicators <- subset(indicators, indicators$CountryCode %in% south_asia)


population <- subset(south_asia_indicators, south_asia_indicators$IndicatorCode == "SP.POP.GROW")

my_theme <-  theme(plot.title = element_text(hjust = 0.5),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(color = "black", size = 1))

ggplot(data = population, aes(Year, Value)) +
  geom_line(aes(col = CountryName)) +
  scale_x_continuous(breaks = seq(1960, 2014, 5)) +
  ggtitle("Population Growth Rate") +
  ylab("Population Growth Rate (%)") + 
  scale_color_discrete(name = "Country") + my_theme
 
birth_death <- subset(south_asia_indicators, south_asia_indicators$IndicatorCode %in% c("SP.DYN.CBRT.IN", "SP.DYN.CDRT.IN"))
  
ggplot(data = birth_death, aes(Year, Value)) +
  geom_point(aes(col = IndicatorName)) +
  scale_x_continuous(breaks = seq(1960, 2014, 10)) +
  facet_wrap(~CountryName, ncol = 2) +
  scale_color_discrete(name = "Indicator") + my_theme +
  ggtitle("Birth and Death Rates")

gdp_per_capita <- subset(south_asia_indicators, south_asia_indicators$IndicatorCode == "NY.GDP.PCAP.CD")

ggplot(data = gdp_per_capita, aes(Year, Value)) +
  geom_line(aes(col = CountryName)) +
  geom_smooth(stat = "summary", fun.y = mean, linetype = 1, size = 2) +
  scale_x_continuous(breaks = seq(1960, 2014, 10)) +
  ggtitle("GDP Per Capita (USD)") +
  ylab("GDP Per Capita") + 
  scale_color_discrete(name = "Country") + my_theme

sectors <- subset(south_asia_indicators, south_asia_indicators$IndicatorCode %in% c("NV.AGR.TOTL.ZS", "NV.IND.MANF.ZS","NV.SRV.TETC.ZS"))
ggplot(data = sectors, aes(Year, Value)) +
  geom_line(aes(col = IndicatorName)) +
  scale_x_continuous(breaks = seq(1960, 2014, 10)) +
  facet_wrap(~CountryName, ncol = 2) +
  scale_color_discrete(name = "Indicator") + my_theme

tel_mobile <- subset(south_asia_indicators, south_asia_indicators$IndicatorCode %in% c("IT.MLT.MAIN.P2", "IT.CEL.SETS.P2"))
ggplot(data = tel_mobile, aes(Year, Value)) +
  geom_line(aes(col = IndicatorName)) +
  scale_x_continuous(breaks = seq(1960, 2014, 10)) +
  facet_wrap(~CountryName, ncol = 2) +
  scale_color_discrete(name = "Indicator") + my_theme

co2 <- subset(south_asia_indicators, south_asia_indicators$IndicatorCode == "EN.ATM.CO2E.PC")
ggplot(data = co2, aes(Year, Value)) +
  geom_line(aes(col = CountryName)) +
  geom_smooth(stat = "summary", fun.y = mean, linetype = 1, size = 2) +
  scale_x_continuous(breaks = seq(1960, 2014, 10)) +
  ggtitle("CO2 Emissions") +
  ylab("CO2 Emissions (Metiric Tons Per Capita") + 
  scale_color_discrete(name = "Country") + my_theme 