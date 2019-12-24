library(gapminder)
library(tidyverse)
library(ggplot2)

#Viz from the task

ggplot(gapminder, aes(x=gdpPercap,
                      y=lifeExp,
                      color=continent,
                      size=pop)) +
  geom_point()+
  facet_wrap(.~year)+
  scale_x_log10()



#Let's see how countries of former Czechoslovakia(1918 - 1993) are represented in df:

df <- gapminder

df %>% 
  filter(country == c('Slovak Republic', 'Czech Republic')) %>%
  ggplot(aes(x=year,
             y=pop,
             color=country))+
  geom_line(size=3)+
  ggtitle("Czechoslovakia or Czech + Slovak Republic?")+
  xlab("Year") + ylab("Population") +
  theme_classic()
