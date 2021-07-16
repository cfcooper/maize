
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

dat$GM <- 0
dat$GM <- ifelse(dat$technology != "conv", 1, 0)
dat$dry <- ifelse(dat$land_type=="Dryland", 1, 0)

pre <- dat[dat$year < 2011,]

reg1 <- glm(yield ~ provence + factor(year) + GM + color, data=pre)
summary(reg1)

reg2 <- glm(yield ~ provence + factor(year)+ GM + year*GM + yearsq*GM + color, data=mergeall)
summary(reg2)

reg3 <- glm(yield ~ provence + factor(year)+ GM + provence*year*GM + provence*yearsq*GM + color, data=mergeall)
summary(reg3)

## different specifications - quadratic, sin(year) and cosine(year)
# run for bt and bt stacked only
# only bt

dat$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * mergeall$year

dat$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * mergeall$year + reg2$coefficients["GM:yearsq"] * mergeall$yearsq

ggplot(data=dat)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

# Mixed Effects Models

dat <- dat[dat$provence != "nam",]

dat %>% 
  group_by(provence) %>%
  summarise(n = n(),
            na = sum(is.na(provence)))

me_loc <- lmer(yield ~ factor(year) + GM + ( GM -1 | locality), data=dat)
summary(me_loc)
re_loc_gm <- ranef(me_loc)$locality

gg <- ggplot()
gg + geom_histogram(data=re_loc_gm, aes(GM))

dat$trend <- dat$year - min(dat$year) + 1

me_pr <- lmer(yield ~ factor(year) + color + provence + dry +
                GM + trend*GM +( trend*GM -1-trend | provence), data=dat)
ranef(me_pr)$provence
re_pr_gm <- ranef(me_pr)$provence
