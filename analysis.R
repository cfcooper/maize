
rm(list=ls()) # Caution: this clears the Environment

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(sandwich,
       lmtest,
       gtsummary,
       segmented,
       Rcpp)

dat <- readRDS("data/finalpanel.rds")

dat %>%
  group_by(year) %>%
  summarise(mean = mean(yield, na.rm=T))

dat$GM <- 0
dat$GM <- ifelse(dat$technology != "conv", 1, 0)
dat$bt <- ifelse(dat$technology %in% c("B", "BR"), 1, 0)

pre <- dat[dat$year < 2011,]
post <- dat[dat$year > "1999",]
allb <- dat[dat$bt == 1,]
bonly <- dat[dat$technology == "B",]

summarynew <- dat %>% group_by(provence,color, technology, year, .add = FALSE) %>% 
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))

p <- ggplot(summarynew, aes(year, mean))
p + geom_smooth(aes(color=color), size = 1) +
  facet_wrap(~provence)


pre_reg <- glm(yield ~ provence + factor(year) + GM + color, data=pre)
sum_prereg <- summary(pre_reg)
sum_prereg

reg1 <- glm(yield ~ provence + factor(year) + GM + color + irrigated, data=dat)
summary(reg1)

#summ(reg1)

dat$yearsq <- dat$year*dat$year

reg2 <- glm(yield ~ provence + factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=dat)
summary(reg2)

dat$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * dat$year
dat$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * dat$year + reg2$coefficients["GM:yearsq"] * dat$yearsq

post$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * post$year
post$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * post$year + reg2$coefficients["GM:yearsq"] * post$yearsq

# Provence by year by GM effects in one model
## Need to add robust standard errors using jtools, sandwich, and lmtest packages
reg3 <- glm(yield ~ provence + factor(year)+ GM + provence*year*GM + provence*yearsq*GM + color + irrigated, data=dat)
summary(reg3)

## subset dat by provence and run new regs.

#1
FS <- dat[dat$provence == "FS",]
fs_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=dat[dat$provence == "FS",])
summary(fs_reg)

FS$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * FS$year
FS$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * FS$year + reg2$coefficients["GM:yearsq"] * FS$yearsq


#2
GP <- dat[dat$provence == "GP",]
gp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=GP)
summary(gp_reg)

GP$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * GP$year
GP$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * GP$year + reg2$coefficients["GM:yearsq"] * GP$yearsq

#3
MP <- dat[dat$provence == "MP",]
mp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=MP)
summary(mp_reg)

MP$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * MP$year
MP$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * MP$year + reg2$coefficients["GM:yearsq"] * MP$yearsq

#4
NW <- dat[dat$provence == "NW",]
nw_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=NW)
summary(nw_reg)

NW$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * NW$year
NW$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * NW$year + reg2$coefficients["GM:yearsq"] * NW$yearsq

#5
KZN <- dat[dat$provence == "KZN",]
kzn_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=KZN)
summary(kzn_reg)

KZN$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * KZN$year
KZN$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * KZN$year + reg2$coefficients["GM:yearsq"] * KZN$yearsq

#6
EC <- dat[dat$provence == "EC",]
ec_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=EC)
summary(ec_reg)

EC$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * EC$year
EC$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * EC$year + reg2$coefficients["GM:yearsq"] * EC$yearsq

#7
LP <- dat[dat$provence == "LP",]
lp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=LP)
summary(lp_reg)

LP$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * LP$year
LP$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * LP$year + reg2$coefficients["GM:yearsq"] * LP$yearsq

#8
NC <- dat[dat$provence == "NC",]
nc_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=NC)
summary(nc_reg)

NC$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * NC$year
NC$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * NC$year + reg2$coefficients["GM:yearsq"] * NC$yearsq
      
#9
WC <- dat[dat$provence == "WC",]
wc_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=WC)
summary(wc_reg)

WC$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * WC$year
WC$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * WC$year + reg2$coefficients["GM:yearsq"] * WC$yearsq

prov <- rbind(FS, GP, MP, NW, KZN, EC, LP, NC, WC)

### derivative of each quadratic curve







install.packages("polynom")
library(polynom)

install.packages("Deriv")
library(Deriv)

deriv(fs_reg)


optimize(reg3, interval=c(2000, 2015), maximum=TRUE)

pc <- coef(reg3)
D1 = pc[1] + 2*pc[3]*pred$P + 3*cf[4]*pred$P^2


x <- with(dat, year[year == 1])

max(prov$ysq_effect)







## different specifications - quadratic, sin(year) and cosine(year)
# run for bt and bt stacked only

breg1 <- glm(yield ~ provence + factor(year) + GM + color, data=allb)
summary(breg1)

breg2 <- glm(yield ~ provence + factor(year)+ GM + year*GM + yearsq*GM + color, data=allb)
summary(breg2)

breg3 <- glm(yield ~ provence + factor(year)+ GM + provence*year*GM + provence*yearsq*GM + color, data=allb)
summary(breg3)



# only bt



## peak graphs

## peak graphs
ggplot(data=prov)+
  geom_line(aes(year, ysq_effect, color=provence))

ggplot(data=FS)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=GP)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=MP)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=NW)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=KZN)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=EC)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=LP)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=NC)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

ggplot(data=WC)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect))

###################################
# robustness test

coeftest(reg1, vcov = vcovHC(reg1, type="HC1"))
coeftest(reg2, vcov = vcovHC(reg1, type="HC1"))

######################################

###########################################
# Tables for presentation

reg1t <- summary(reg1, scale = TRUE)


t1 <- tbl_regression(reg1, exponentiate = TRUE, )
t2 <- tbl_regression(reg2, exponentiate = TRUE, include = -c(year, yearsq))

mergedt1 <- tbl_merge(tbls = list(t1, t2), tab_spanner = c("**Linear**", "**Quadratic**"))
tbl_summary(mergedt1) %>% as_flex_table()


#########################################
## ATTEMPTS AT SE GRAPHS ##

sumdata <- plyr::ddply(post, c("year", "provence"), summarise,
               N    = length(yield),
               mean = mean(yield),
               sd   = sd(yield),
               se   = sd / sqrt(N))
sumdata
postSE <- merge(post, sumdata, by=c("year","provence"))

ggplot(postSE, aes(x=year, y=ysq_effect, group=provence, color=provence)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=ysq_effect-se, ymax=ysq_effect+se), width=.2,
                position=position_dodge(0.05)) +
  facet_wrap(~provence)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


#shortened graph (by year)
ggplot(data=post)+
  #geom_line(aes(year, y_effect))+
  geom_line(aes(year, ysq_effect, color=provence))


#shortened graph + error bars

FS$sd <- sd(FS$ysq_effect, na.rm = TRUE)
ggplot(FS, aes(x=year, y=ysq_effect)) + 
  geom_line() +
  geom_errorbar(aes(ymin=ysq_effect-sd, ymax=ysq_effect+sd), width=.2,
                position=position_dodge(0.05)) 

######################
#graphs for presentation

#box plot for yield by year, bt/non bt
post$bt <- if_else(post$bt == 1, "bt", "conv", missing=NULL)
btgg <- ggplot(post, aes(year, yield, group= interaction(bt, year))) + 
  geom_boxplot(aes(color=bt), size = 1)

btgg2 <- btgg +
  theme_bw() +
  labs(
    x = "Year",
    y = "Yield",
    colour = "Technology Type",
    title = "Yield Differences Between Bt and Conventional Over Time"
  ) +
  scale_colour_brewer(type = "seq", palette = "Set2")
btgg2

#box plot for yield by year, yellow/white
colgg <- ggplot(post, aes(year, yield, group= interaction(color, year))) + 
  geom_boxplot(aes(color=color), size = 1)
colgg2 <- colgg +
  theme_bw() +
  labs(
    x = "Year",
    y = "Yield",
    colour = "Color",
    title = "Yield Differences Between Yellow and White Over Time"
  ) +
  scale_colour_brewer(type = "seq", palette = "Set2")
colgg2



######################

#################################################################################

# Breakpoint Analysis / Piecewise Linear Regression / Segmented Regression

breakreg3 <- segmented.glm(reg3, seg.Z =~year)
summary(breakreg2)

breakfs <- segmented.glm(fs_reg, seg.Z =~ysq_effect)
summary(breakfs)


#################################################################################

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

###########################################################################
# Code from JB Tack to extract mixed effects models components
estimates <- coef(lmer3)
fipsest <- estimates$fips
fipsest <- data.frame(fips=row.names(fipsest), fipsest)
fipsest$REfips <- fipsest$X.Intercept.
fipsest <- fipsest[,c("fips","REfips")]
fipsest$fips <- as.numeric(as.character(fipsest$fips))

fipsest <- data.table(fipsest)
setkey(fipsest,fips)
setkey(trend_est,fips)
trend_est <- merge(trend_est,fipsest,x.all=TRUE)

distfipsest <- estimates$distfips
distfipsest <- data.frame(distfips=row.names(distfipsest), distfipsest)
distfipsest <- distfipsest[,c("distfips","trend","spline")]
distfipsest$distfips <- as.numeric(as.character(distfipsest$distfips))
distfipsest <- data.table(distfipsest)
setkey(distfipsest,distfips)
setkey(trend_est,distfips)
trend_est <- merge(trend_est,distfipsest,x.all=TRUE)
trend_est <- as.data.frame(trend_est)
trend_est$trend1 <- 100*trend_est$trend #convert to percent
trend_est$trend2 <- 100*(trend_est$trend + trend_est$spline) #convert to perc