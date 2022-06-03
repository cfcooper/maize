
rm(list=ls()) # Caution: this clears the Environment

dat <- readRDS("data/finalpanel.rds")

dat %>%
  group_by(year) %>%
  summarise(mean = mean(yield, na.rm=T))

dat$GM <- 0
dat$GM <- ifelse(dat$technology != "conv", 1, 0)

dat <- dat[!dat$color == "yellow",]
bandconv <- dat[dat$technology %in% c("B", "conv"),]


dat <- bandconv

reg1 <- glm(yield ~ provence + factor(year) + GM + irrigated, data=dat)
summary(reg1)


dat$yearsq <- dat$year*dat$year

reg2 <- glm(yield ~ provence + factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=dat)
summary(reg2)

dat$y_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * dat$year
dat$ysq_effect <- reg2$coefficients["GM"] + reg2$coefficients["GM:year"] * dat$year + reg2$coefficients["GM:yearsq"] * dat$yearsq

max(dat$ysq_effect)



# Provence by year by GM effects in one model
## Need to add robust standard errors using jtools, sandwich, and lmtest packages
reg3 <- glm(yield ~ provence + factor(year)+ GM + provence*year*GM + provence*yearsq*GM + irrigated, data=dat)
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
fs_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=FS)
summary(fs_reg)

FS$y_effect <- fs_reg$coefficients["GM"] + fs_reg$coefficients["GM:year"] * FS$year
FS$ysq_effect <- fs_reg$coefficients["GM"] + fs_reg$coefficients["GM:year"] * FS$year + fs_reg$coefficients["GM:yearsq"] * FS$yearsq


#2
GP <- dat[dat$provence == "GP",]
gp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=GP)
summary(gp_reg)

GP$y_effect <- gp_reg$coefficients["GM"] + gp_reg$coefficients["GM:year"] * GP$year
GP$ysq_effect <- gp_reg$coefficients["GM"] + gp_reg$coefficients["GM:year"] * GP$year + gp_reg$coefficients["GM:yearsq"] * GP$yearsq

#3
MP <- dat[dat$provence == "MP",]
mp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=MP)
summary(mp_reg)

MP$y_effect <- mp_reg$coefficients["GM"] + mp_reg$coefficients["GM:year"] * MP$year
MP$ysq_effect <- mp_reg$coefficients["GM"] + mp_reg$coefficients["GM:year"] * MP$year + mp_reg$coefficients["GM:yearsq"] * MP$yearsq

#4
NW <- dat[dat$provence == "NW",]
nw_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=NW)
summary(nw_reg)

NW$y_effect <- nw_reg$coefficients["GM"] + nw_reg$coefficients["GM:year"] * NW$year
NW$ysq_effect <- nw_reg$coefficients["GM"] + nw_reg$coefficients["GM:year"] * NW$year + nw_reg$coefficients["GM:yearsq"] * NW$yearsq

#5
KZN <- dat[dat$provence == "KZN",]
kzn_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=KZN)
summary(kzn_reg)

KZN$y_effect <- kzn_reg$coefficients["GM"] + kzn_reg$coefficients["GM:year"] * KZN$year
KZN$ysq_effect <- kzn_reg$coefficients["GM"] + kzn_reg$coefficients["GM:year"] * KZN$year + kzn_reg$coefficients["GM:yearsq"] * KZN$yearsq

#6
EC <- dat[dat$provence == "EC",]
ec_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=EC)
summary(ec_reg)

EC$y_effect <- ec_reg$coefficients["GM"] + ec_reg$coefficients["GM:year"] * EC$year
EC$ysq_effect <- ec_reg$coefficients["GM"] + ec_reg$coefficients["GM:year"] * EC$year + ec_reg$coefficients["GM:yearsq"] * EC$yearsq

#7
LP <- dat[dat$provence == "LP",]
lp_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=LP)
summary(lp_reg)

LP$y_effect <- lp_reg$coefficients["GM"] + lp_reg$coefficients["GM:year"] * LP$year
LP$ysq_effect <- lp_reg$coefficients["GM"] + lp_reg$coefficients["GM:year"] * LP$year + lp_reg$coefficients["GM:yearsq"] * LP$yearsq

#8
NC <- dat[dat$provence == "NC",]
nc_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=NC)
summary(nc_reg)

NC$y_effect <- nc_reg$coefficients["GM"] + nc_reg$coefficients["GM:year"] * NC$year
NC$ysq_effect <- nc_reg$coefficients["GM"] + nc_reg$coefficients["GM:year"] * NC$year + nc_reg$coefficients["GM:yearsq"] * NC$yearsq

#9
WC <- dat[dat$provence == "WC",]
wc_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=WC)
summary(wc_reg)

WC$y_effect <- wc_reg$coefficients["GM"] + wc_reg$coefficients["GM:year"] * WC$year
WC$ysq_effect <- wc_reg$coefficients["GM"] + wc_reg$coefficients["GM:year"] * WC$year + wc_reg$coefficients["GM:yearsq"] * WC$yearsq

#combine NC, NW, FS
northregion <- bind_rows(NC, NW, FS)
north_reg <- glm(yield ~ factor(year)+ GM + year*GM + yearsq*GM + irrigated, data=northregion)
summary(north_reg)

northregion$y_effect <- north_reg$coefficients["GM"] + north_reg$coefficients["GM:year"] * northregion$year
northregion$ysq_effect <- north_reg$coefficients["GM"] + north_reg$coefficients["GM:year"] * northregion$year + north_reg$coefficients["GM:yearsq"] * northregion$yearsq

northregion$provence <- "northregion"
prov <- rbind(northregion, GP, MP, KZN, LP, WC)
prov2 <- rbind(EC, FS, GP, MP, NW, KZN, LP, NC, WC)
prov2 <- prov2[!prov2$provence == "EC",]
prov2 <- prov2[!prov2$provence == "LP",]

### yield gains peak

breakpoint <- data.frame(0,0,0)
colnames(breakpoint) <- c('provence', 'ysq_effectmax')
provence <- c("northregion", 'GP', 'MP', 'KZN', 'WC')

ysq_effect <- c(max(northregion$ysq_effect), 
                max(GP$ysq_effect), 
                max(MP$ysq_effect), 
                max(KZN$ysq_effect), 
                max(WC$ysq_effect))


breakpoint <- data.frame(provence, ysq_effect)
rm(provence, ysq_effect)
prov4 <- prov[,c("provence", "ysq_effect", "year")]
breakpoint <- merge(breakpoint, prov4, by = c("ysq_effect","provence"), no.dups = TRUE)
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

write.csv(breakpoint2, "output/whitebreakpoint.csv")

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

summtbl <- prov2 %>% filter(technology == "B") %>% 
  group_by(provence, technology, year,  .add = FALSE) %>% 
  summarise(yield = mean(yield, na.rm = T),
            SD = sd(yield, na.rm = T))
summtbl2 <- prov2 %>% filter(technology == "B") %>%
  group_by(year, provence, technology) %>%
  count()



##finding lost gains BY PROV
maizeprod <- read.csv("data/metadata/maizeproduction.csv")

names(maizeprod)[2:7] <- c("percentBt","percentwhite", "percentyellow", "whiteprice", "yellowprice", "totalmaizeht")
maizeprod$percentBt <- ifelse(is.na(maizeprod$percentBt), .29, maizeprod$percentBt)
maizeprod$Bthectare <- maizeprod$percentBt*maizeprod$totalmaizeht
maizeprod$totalprice <- maizeprod$percentwhite*maizeprod$whiteprice + maizeprod$percentyellow*maizeprod$yellowprice

###########################
# GP Loss
gploss <- GP %>% filter(bt == 1) %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))
gploss <- merge(GP, gploss, by = c("provence","year","color"), no.dups = TRUE)
gploss <- gploss[,c("year", "provence", "color","technology", "ysq_effect", "mean", "SD")]

gploss <- gploss[gploss$technology == "B",]
gploss <- gploss[!duplicated(gploss), ]
gploss$maxysq <- ifelse(gploss$year<2009, NA, max(gploss$ysq_effect))
gploss$gain_loss <- gploss$ysq_effect - gploss$maxysq

gploss <- merge(gploss, maizeprod, by = c("year"), no.dups = TRUE)

gploss$mtloss <- (gploss$gain_loss*gploss$Bthectare)*-1
gploss$yearlyloss <- gploss$mtloss*gploss$totalprice
gploss$dollarlossperht <- gploss$yearlyloss/gploss$Bthectare
gploss <- gploss[,c("year","provence", "color", "ysq_effect","maxysq","gain_loss", "mtloss", "yearlyloss", "dollarlossperht")]

# KZN Loss
kznloss <- KZN %>% filter(bt == 1) %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))
kznloss <- merge(KZN, kznloss, by = c("provence","year","color"), no.dups = TRUE)
kznloss <- kznloss[,c("year", "provence", "color","technology", "ysq_effect", "mean", "SD")]

kznloss <- kznloss[kznloss$technology == "B",]
kznloss <- kznloss[!duplicated(kznloss), ]
kznloss$maxysq <- ifelse(kznloss$year<2010, NA, max(kznloss$ysq_effect))
kznloss$gain_loss <- kznloss$ysq_effect - kznloss$maxysq

kznloss <- merge(kznloss, maizeprod, by = c("year"), no.dups = TRUE)

kznloss$mtloss <- (kznloss$gain_loss*kznloss$Bthectare)*-1
kznloss$yearlyloss <- kznloss$mtloss*kznloss$totalprice
kznloss$dollarlossperht <- kznloss$yearlyloss/kznloss$Bthectare
kznloss <- kznloss[,c("year", "provence", "color", "ysq_effect","maxysq","gain_loss", "mtloss", "yearlyloss", "dollarlossperht")]

# MP Loss
mploss <- MP %>% filter(bt == 1) %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))
mploss <- merge(MP, mploss, by = c("provence","year","color"), no.dups = TRUE)
mploss <- mploss[,c("year", "provence", "color","technology", "ysq_effect", "mean", "SD")]

mploss <- mploss[mploss$technology == "B",]
mploss <- mploss[!duplicated(mploss), ]
mploss$maxysq <- ifelse(mploss$year<2018, NA, max(mploss$ysq_effect))
mploss$gain_loss <- mploss$ysq_effect - mploss$maxysq

mploss <- merge(mploss, maizeprod, by = c("year"), no.dups = TRUE)

mploss$mtloss <- (mploss$gain_loss*mploss$Bthectare)*-1
mploss$yearlyloss <- mploss$mtloss*mploss$totalprice
mploss$dollarlossperht <- mploss$yearlyloss/mploss$Bthectare
mploss <- mploss[,c("year", "provence", "color", "ysq_effect","maxysq","gain_loss", "mtloss", "yearlyloss", "dollarlossperht")]

# North Region Loss
northloss <- northregion %>% filter(bt == 1) %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))
northloss <- merge(northregion, northloss, by = c("provence","year","color"), no.dups = TRUE)
northloss <- northloss[,c("year", "provence", "provence", "color","technology", "ysq_effect", "mean", "SD")]

northloss <- northloss[northloss$technology == "B",]
northloss <- northloss[!duplicated(northloss), ]
northloss$maxysq <- ifelse(northloss$year<2008, NA, max(northloss$ysq_effect))
northloss$gain_loss <- northloss$ysq_effect - northloss$maxysq

northloss <- merge(northloss, maizeprod, by = c("year"), no.dups = TRUE)

northloss$mtloss <- (northloss$gain_loss*northloss$Bthectare)*-1
northloss$yearlyloss <- northloss$mtloss*northloss$totalprice
northloss$dollarlossperht <- northloss$yearlyloss/northloss$Bthectare
northloss <- northloss[,c("year", "provence", "color", "ysq_effect","maxysq","gain_loss", "mtloss", "yearlyloss", "dollarlossperht")]

# WC Loss
wcloss <- WC %>% filter(bt == 1) %>%
  group_by(color, year, provence, .add = FALSE) %>%
  summarise(mean = mean(yield, na.rm = T), 
            SD = sd(yield, na.rm = T))
wcloss <- merge(WC, wcloss, by = c("provence","year","color"), no.dups = TRUE)
wcloss <- wcloss[,c("year", "provence", "color","technology", "ysq_effect", "mean", "SD")]

wcloss <- wcloss[wcloss$technology == "B",]
wcloss <- wcloss[!duplicated(wcloss), ]
wcloss$maxysq <- ifelse(wcloss$year<2018, NA, max(wcloss$ysq_effect))
wcloss$gain_loss <- wcloss$ysq_effect - wcloss$maxysq

wcloss <- merge(wcloss, maizeprod, by = c("year"), no.dups = TRUE)

wcloss$mtloss <- (wcloss$gain_loss*wcloss$Bthectare)*-1
wcloss$yearlyloss <- wcloss$mtloss*wcloss$totalprice
wcloss$dollarlossperht <- wcloss$yearlyloss/wcloss$Bthectare
wcloss <- wcloss[,c("year", "provence", "color", "ysq_effect","maxysq","gain_loss", "mtloss", "yearlyloss", "dollarlossperht")]

## Combining Prov Level Gains Lost
provloss <- rbind(northloss, gploss, mploss, kznloss, wcloss)
provloss <- na.omit(provloss)

provloss2 <- provloss %>%
  group_by(year, .add = FALSE) %>%
  summarise(averagemtlost = mean(mtloss, na.rm = T),
            totalhalost = sum(mtloss, na.rm = T),
            yearlyloss = sum(yearlyloss, na.rm = T)
            )


#############################


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




totalloss <- merge(totalloss, maizeprod, by = c("year"), no.dups = TRUE)

totalloss$mtloss <- (totalloss$gain_loss*totalloss$Bthectare)*-1
totalloss$yearlyloss <- totalloss$mtloss*totalloss$totalprice
totalloss$dollarlossperht <- totalloss$yearlyloss/totalloss$Bthectare
totalloss <- totalloss[,c("year", "ysq_effect","maxysq","gain_loss", "mtloss", "yearlyloss", "dollarlossperht", "consumption")]

totalloss$rationloss <- (totalloss$mtloss*1000)/totalloss$consumption
write.csv(totalloss, "output/totalloss.csv")

