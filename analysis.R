
rm(list=ls()) # Caution: this clears the Environment

if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(sandwich,
       lmtest,
       gtsummary,
       segmented,
       Rcpp)
install.packages("RColorBrewer")
library(RColorBrewer)

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


dat$yearsq <- dat$year*dat$year

reg2 <- glm(yield ~ provence + factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=dat)
summary(reg2)

dat$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * dat$year
dat$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * dat$year + reg2$coefficients["GM:yearsq"] * dat$yearsq

max(dat$ysq_effect)



# Provence by year by GM effects in one model
## Need to add robust standard errors using jtools, sandwich, and lmtest packages
reg3 <- glm(yield ~ provence + factor(year)+ GM + provence*year*GM + provence*yearsq*GM + color + irrigated, data=dat)
summary(reg3)

## require 5 obs per year per provence
summaryobs <- dat %>% group_by(year, provence, color, bt) %>%
  summarise(count = n()) # check bt if in more than one obersvation per provence/year
summaryobs <- summaryobs[summaryobs$count < 5,]
summaryobs$count <- "1"
dat <- merge(dat,summaryobs, by = c("year","provence", "bt","color"),all = TRUE, no.dups= TRUE)
dat$count <- if_else(dat$count %in% c(NA), "0", "1")
dat <- dat[!dat$count== "1", ]




#1
FS <- dat[dat$provence == "FS",]
##FS <- FS[!FS$year == "2004",]
fs_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=FS)
summary(fs_reg)

FS$y_effect <- fs_reg$coefficients["GM"] + fs_reg$coefficients["GM:year"] * FS$year
FS$ysq_effect <- fs_reg$coefficients["GM"] + fs_reg$coefficients["GM:year"] * FS$year + fs_reg$coefficients["GM:yearsq"] * FS$yearsq


#2
GP <- dat[dat$provence == "GP",]
gp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=GP)
summary(gp_reg)

GP$y_effect <- gp_reg$coefficients["GM"] + gp_reg$coefficients["GM:year"] * GP$year
GP$ysq_effect <- gp_reg$coefficients["GM"] + gp_reg$coefficients["GM:year"] * GP$year + gp_reg$coefficients["GM:yearsq"] * GP$yearsq

#3
MP <- dat[dat$provence == "MP",]
mp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=MP)
summary(mp_reg)

MP$y_effect <- mp_reg$coefficients["GM"] + mp_reg$coefficients["GM:year"] * MP$year
MP$ysq_effect <- mp_reg$coefficients["GM"] + mp_reg$coefficients["GM:year"] * MP$year + mp_reg$coefficients["GM:yearsq"] * MP$yearsq

#4
NW <- dat[dat$provence == "NW",]
nw_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=NW)
summary(nw_reg)

NW$y_effect <- nw_reg$coefficients["GM"] + nw_reg$coefficients["GM:year"] * NW$year
NW$ysq_effect <- nw_reg$coefficients["GM"] + nw_reg$coefficients["GM:year"] * NW$year + nw_reg$coefficients["GM:yearsq"] * NW$yearsq

#5
KZN <- dat[dat$provence == "KZN",]
kzn_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=KZN)
summary(kzn_reg)

KZN$y_effect <- kzn_reg$coefficients["GM"] + kzn_reg$coefficients["GM:year"] * KZN$year
KZN$ysq_effect <- kzn_reg$coefficients["GM"] + kzn_reg$coefficients["GM:year"] * KZN$year + kzn_reg$coefficients["GM:yearsq"] * KZN$yearsq

#6
EC <- dat[dat$provence == "EC",]
ec_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=EC)
summary(ec_reg)

EC$y_effect <- ec_reg$coefficients["GM"] + ec_reg$coefficients["GM:year"] * EC$year
EC$ysq_effect <- ec_reg$coefficients["GM"] + ec_reg$coefficients["GM:year"] * EC$year + ec_reg$coefficients["GM:yearsq"] * EC$yearsq

#7
LP <- dat[dat$provence == "LP",]
lp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=LP)
summary(lp_reg)

LP$y_effect <- lp_reg$coefficients["GM"] + lp_reg$coefficients["GM:year"] * LP$year
LP$ysq_effect <- lp_reg$coefficients["GM"] + lp_reg$coefficients["GM:year"] * LP$year + lp_reg$coefficients["GM:yearsq"] * LP$yearsq

#8
NC <- dat[dat$provence == "NC",]
nc_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=NC)
summary(nc_reg)

NC$y_effect <- nc_reg$coefficients["GM"] + nc_reg$coefficients["GM:year"] * NC$year
NC$ysq_effect <- nc_reg$coefficients["GM"] + nc_reg$coefficients["GM:year"] * NC$year + nc_reg$coefficients["GM:yearsq"] * NC$yearsq
      
#9
WC <- dat[dat$provence == "WC",]
wc_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=WC)
summary(wc_reg)

WC$y_effect <- wc_reg$coefficients["GM"] + wc_reg$coefficients["GM:year"] * WC$year
WC$ysq_effect <- wc_reg$coefficients["GM"] + wc_reg$coefficients["GM:year"] * WC$year + wc_reg$coefficients["GM:yearsq"] * WC$yearsq

#combine NC, NW, FS
northregion <- bind_rows(NC, NW, FS)
north_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + color + irrigated, data=northregion)
summary(north_reg)

northregion$y_effect <- north_reg$coefficients["GM"] + north_reg$coefficients["GM:year"] * northregion$year
northregion$ysq_effect <- north_reg$coefficients["GM"] + north_reg$coefficients["GM:year"] * northregion$year + north_reg$coefficients["GM:yearsq"] * northregion$yearsq

northregion$provence <- "northregion"
prov <- rbind(northregion, GP, MP, KZN, LP, WC)
#prov <- rbind(FS, GP, MP, NW, KZN, LP, NC, WC)

### yield gains peak

breakpoint <- data.frame(0,0,0)
colnames(breakpoint) <- c('provence', 'ysq_effectmax')
provence <- c("northregion", "GP", "MP", "KZN", "LP", "WC")

ysq_effect <- c(max(northregion$ysq_effect), 
                max(GP$ysq_effect), 
                max(MP$ysq_effect), 
                max(KZN$ysq_effect), 
                max(LP$ysq_effect), 
                max(WC$ysq_effect))

breakpoint <- data.frame(provence, ysq_effect)
rm(provence, ysq_effect)
prov2 <- prov[,c("provence", "ysq_effect", "year")]
breakpoint <- merge(breakpoint, prov2, by = c("ysq_effect","provence"), no.dups = TRUE)
breakpoint <- breakpoint[!duplicated(breakpoint), ]

summaryb <- prov %>% filter(bt == 1) %>% group_by(color, year, provence, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))

breakpoint <- merge(breakpoint, summaryb, by = c("provence","year"), no.dups = TRUE)

summaryb2 <- prov %>% filter(bt == 1) %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(count = n())

breakpoint2 <- merge(breakpoint, summaryb2, by = c("provence","year","color"), no.dups = TRUE)
breakpoint2 <- breakpoint2[,c("year", "provence", "color", "ysq_effect", "mean", "SD", "count")]
breakpoint2 <- breakpoint2[!duplicated(breakpoint2), ]


proveffects <- breakpoint2 %>%
  gt(rowname_col = "province") %>%
  tab_header(
    title = "Province Effects",
    subtitle = glue::glue("observations included in sample")
  ) %>%
  cols_label(
    year = html("Year"),
    provence = html("Province"),
    ysq_effect = html("Yield Gain Peak"),
    mean = html("Average Yield"),
    SD = html("Yield SD"),
    count = html("num of observations")) %>%
  cols_align(
    align = "center",
    columns = everything())  %>%
  fmt_number(
    columns = 4:6,
    decimals = 3)

print(proveffects)

##finding lost gains BY PROV
gploss <- GP %>% filter(bt == 1) %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))
gploss <- merge(GP, gploss, by = c("provence","year","color"), no.dups = TRUE)
gploss <- gploss[,c("year", "provence", "color","technology", "ysq_effect", "mean", "SD")]

gploss <- gploss[gploss$technology == "B",]
gploss <- gploss[!duplicated(gploss), ]
gploss$maxysq <- ifelse(gploss$year<2010, NA, max(gploss$ysq_effect))
gploss$gain_loss <- gploss$ysq_effect - gploss$maxysq


ggplot(data=gploss)+
  geom_line(aes(year, ysq_effect)) + 
  geom_line(aes(year, maxysq)) + 
  coord_cartesian(xlim= c(1998,2020), ylim = c(.0,.5), clip = "on")

##finding lost gains ALL
allb <- dat[dat$technology == "B",]
totalloss <- allb %>% filter(technology == "B") %>%
  group_by(year, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))
totalloss <- merge(allb, totalloss, by = c("year"), no.dups = TRUE)
totalloss <- totalloss[,c("year", "ysq_effect", "mean", "SD")]
totalloss <- totalloss[!duplicated(totalloss), ]
totalloss$maxysq <- ifelse(totalloss$year<2011, NA, max(totalloss$ysq_effect))
totalloss$gain_loss <- totalloss$ysq_effect - totalloss$maxysq

ggplot(data=totalloss)+
  geom_line(aes(year, ysq_effect)) + 
  geom_line(aes(year, maxysq)) + 
  coord_cartesian(xlim= c(1998,2020), ylim = c(.0,.5), clip = "on")


maizeprod <- read.csv("data/metadata/maizeproduction.csv")

names(maizeprod)[2:6] <- c("percentBt","1000ha", "totalmaizehectare", "priceton", "totalmaizevalue")
maizeprod$percentBt <- ifelse(is.na(maizeprod$percentBt), .29, maizeprod$percentBt)
maizeprod$Bthectare <- maizeprod$percentBt*maizeprod$totalmaizehectare

totalloss <- merge(totalloss, maizeprod, by = c("year"), no.dups = TRUE)

##totalloss <- totalloss[,c("year", "ysq_effect","maxysq","gain_loss", "Bthectare","priceton")]

totalloss$mtloss <- (totalloss$gain_loss*totalloss$Bthectare)*-1
totalloss$yearlyloss <- totalloss$mtloss*totalloss$priceton
totalloss$losssum <- lag(totalloss$yearlyloss) + totalloss$yearlyloss
totalloss <- totalloss[,c("year", "ysq_effect","maxysq","gain_loss", "mtloss", "yearlyloss", "losssum", "totalmaizevalue")]

write.csv(totalloss, "output/totalloss.csv")

## Cleaned Tables

cleanedloss <- totalloss[totalloss$year > 2010,]
cleanedloss[is.na(cleanedloss)] <- 0  



ggloss <- ggplot(totalloss,aes(x = year, y = yearlyloss, fill = year))+
  theme_bw() +
  geom_bar(fill="#3CA9CA", width=.8, stat="identity") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  labs(title = "Economic Loss") +
  coord_cartesian(xlim= c(2010,2020), clip = "on")
ggloss

ggloss2 <- ggplot(totalloss)+
  theme_bw() +
  geom_bar(aes(x = year, y = totalmaizevalue), fill="#3CA9CA", width=.8, stat="identity") +
  geom_bar(aes(x = year, y = yearlyloss), fill="#95427B", width=.8, stat="identity") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  labs(title = "Economic Loss") +
  coord_cartesian(xlim= c(2010,2020), clip = "on")
ggloss2


#### summary of of Bt observations in FS
summaryfs <- bonly %>% filter(provence == "FS") %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(count = n())

write.csv(summaryfs, "output/btFSsummary.csv")

#### summary of of Bt observations in EC
summaryec <- bonly %>% filter(provence == "EC") %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(count = n())

### predict economic loss




## different specifications - quadratic, sin(year) and cosine(year)
# run for bt and bt stacked only


## peak graphs
ggplot(data=dat)+
  geom_line(aes(year, ysq_effect)) + 
  coord_cartesian(xlim= c(1998,2020), ylim = c(-.05,.5), clip = "on")


ggplot(data=prov)+
  geom_line(aes(year, ysq_effect, color=provence), size=1) + 
  scale_color_brewer(palette = "Paired") +
  labs(title = "Yield Gains Due to GM Technology") +
  coord_cartesian(xlim= c(1998,2020), ylim = c(-.05,.5), clip = "on")




ggplot(data=FS)+
  geom_line(aes(year, ysq_effect))

ggplot(data=GP)+
  geom_line(aes(year, ysq_effect))

ggplot(data=MP)+
  geom_line(aes(year, ysq_effect))

ggplot(data=NW)+
  geom_line(aes(year, ysq_effect))

ggplot(data=KZN)+
  geom_line(aes(year, ysq_effect))

ggplot(data=EC)+
  geom_line(aes(year, ysq_effect))

ggplot(data=LP)+
  geom_line(aes(year, ysq_effect))

ggplot(data=NC)+
  geom_line(aes(year, ysq_effect))

ggplot(data=WC)+
  geom_line(aes(year, ysq_effect))

###################################
# robustness test

coeftest(reg1, vcov = vcovHC(reg1, type="HC1"))
coeftest(reg2, vcov = vcovHC(reg1, type="HC1"))

######################################

###########################################
# Tables for presentation

reg1t <- summary(reg1, scale = TRUE)


t1 <- tbl_regression(reg1, exponentiate = TRUE)
t2 <- tbl_regression(reg2, exponentiate = TRUE, include = -c(year, yearsq))

mergedt1 <- tbl_merge(tbls = list(t1, t2), tab_spanner = c("**Linear**", "**Quadratic**"))
tbl_summary(mergedt1) %>% as_flex_table()


######################
#graphs for presentation

#box plot for yield by year, bt/non bt
post <- prov[prov$year > "1999",]

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
  scale_colour_brewer(palette = "Paired")
btgg2

provboxplot <- ggplot(post, aes(year, yield, group= interaction(bt, year))) + 
  geom_boxplot(aes(color=bt), size = 1, outlier.shape = NA) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Yield",
    colour = "Technology Type",
    title = "Yield Differences Between Bt and Conventional Over Time") +
  scale_colour_brewer(palette = "Paired")+
  facet_wrap(~provence, scale="free")
provboxplot



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
  scale_colour_brewer(palette = "Paired")
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