
rm(list=ls()) # Caution: this clears the Environment

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,
       lme4,
       merTools)

dat <- readRDS("mergeall.rds")

dat %>%
  group_by(year) %>%
  summarise(mean = mean(yield, na.rm=T))

mergeall$GM <- 0
mergeall$GM <- ifelse(mergeall$technology != "conv", 1, 0)

pre <- mergeall[mergeall$year < 2010,]

reg1 <- glm(yield ~ provence + factor(year) + GM + color, data=pre)
summary(reg1)

reg2 <- glm(yield ~ provence + factor(year)+ GM + year*GM + yearsq*GM + color, data=mergeall)
summary(reg2)

reg3 <- glm(yield ~ provence + factor(year)+ GM + provence*year*GM + provence*yearsq*GM + color, data=mergeall)
summary(reg3)

## different specifications - quadratic, sin(year) and cosine(year)
# run for bt and bt stacked only
# only bt

mergeall$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * mergeall$year

mergeall$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * mergeall$year + reg2$coefficients["GM:yearsq"] * mergeall$yearsq

ggplot(data=mergeall)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))
