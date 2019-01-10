
# gini_scoring.R
# Morris Trachsler, 2016
#
# "Just how important is secondary scoring in ice hockey?" I wrote this script
# to adress this question. I first collect and clean ten years of NHL data 
# ("Read data", "Data Treatment" sections). I then propose a new statistic to 
# measure any team's "star - dependency", the Gini coefficient of scoring. This
# metric measures the degree of inequality in a given distribution and fits our 
# purpose well ("Gini coefficient" section). The subsequent analysis,
# based on our data, clearly indicates a strong POSITIVE relationship between 
# the secondary scoring of a team and this team's success.
#

library("RCurl", lib.loc="~/R/win-library/3.1")
library("rjson", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
library("lmtest", lib.loc="~/R/win-library/3.1")
library("rstudio", lib.loc="~/R/win-library/3.1")
library("graphics", lib.loc="~/R/win-library/3.1")
library("stats", lib.loc="~/R/win-library/3.1")
library("plyr", lib.loc="~/R/win-library/3.1")
library("psych", lib.loc="~/R/win-library/3.1")
library("MASS", lib.loc="C:/Program Files/R/R-3.1.1/library")
library("reshape2", lib.loc="~/R/win-library/3.1")
library("ineq", lib.loc="~/R/win-library/3.1")
library("readr", lib.loc="~/R/win-library/3.1")
library(stringr)
library(chron)
library(abind)
# library("biglm", lib.loc="~/R/win-library/3.1")
# library("bitops", lib.loc="~/R/win-library/3.1")
# library("nhlscrapr", lib.loc="~/R/win-library/3.1")
# library("dplyr", lib.loc="~/R/win-library/3.1")

# Set working directory
setwd("C:/Users/morris/Desktop/RR/2ndscoring")

# READ DATA --------------------------------------------------------------------
nhlTm <- read_delim("nhlTm.txt","\t", escape_double = FALSE, col_names = FALSE)
# Individual player stats for the NHL seasons 0506 until 1415
statspl05061415 <- read_delim(
  "statspl05061415.csv", ";", escape_double=F,
  col_types=cols(Goalie.GAR=col_double(), drawn.GAR=col_double()))
# Individual goalie stats for the NHL seasons 0506 until 1415
# statsgo05061415 <- read_delim("statsgo05061415.csv", ";", escape_double=F)
# League standings at end of regular season for each year from 0506 until 1415
nhlstandings0506 <- read_csv("./standings/nhl200506standings.txt")
nhlstandings0607 <- read_csv("./standings/nhl200607standings.txt")
nhlstandings0708 <- read_csv("./standings/nhl200708standings.txt")
nhlstandings0809 <- read_csv("./standings/nhl200809standings.txt")
nhlstandings0910 <- read_csv("./standings/nhl200910standings.txt")
nhlstandings1011 <- read_csv("./standings/nhl201011standings.txt")
nhlstandings1112 <- read_csv("./standings/nhl201112standings.txt")
nhlstandings1213 <- read_csv("./standings/nhl201213standings.txt")
nhlstandings1314 <- read_csv("./standings/nhl201314standings.txt")
nhlstandings1415 <- read_csv("./standings/nhl201415standings.txt")
nhlstandings0506$season <- 20052006
nhlstandings0607$season <- 20062007
nhlstandings0708$season <- 20072008
nhlstandings0809$season <- 20082009
nhlstandings0910$season <- 20092010
nhlstandings1011$season <- 20102011
nhlstandings1112$season <- 20112012
nhlstandings1213$season <- 20122013
nhlstandings1314$season <- 20132014
nhlstandings1415$season <- 20142015
nhlstandings <- rbind(nhlstandings0506, nhlstandings0607, nhlstandings0708,
                      nhlstandings0809, nhlstandings0910, nhlstandings1011,
                      nhlstandings1112,nhlstandings1213,nhlstandings1314,
                      nhlstandings1415)
rm(nhlstandings0506, nhlstandings0607, nhlstandings0708, nhlstandings0809,
   nhlstandings0910, nhlstandings1011, nhlstandings1112, nhlstandings1213,
   nhlstandings1314, nhlstandings1415)
# Playoff "standings" for each season from 0506 until 1415
nhlstandings0506po <- read_csv("./standings/nhl200506standingspo.txt")
nhlstandings0607po <- read_csv("./standings/nhl200607standingspo.txt")
nhlstandings0708po <- read_csv("./standings/nhl200708standingspo.txt")
nhlstandings0809po <- read_csv("./standings/nhl200809standingspo.txt")
nhlstandings0910po <- read_csv("./standings/nhl200910standingspo.txt")
nhlstandings1011po <- read_csv("./standings/nhl201011standingspo.txt")
nhlstandings1112po <- read_csv("./standings/nhl201112standingspo.txt")
nhlstandings1213po <- read_csv("./standings/nhl201213standingspo.txt")
nhlstandings1314po <- read_csv("./standings/nhl201314standingspo.txt")
nhlstandings1415po <- read_csv("./standings/nhl201415standingspo.txt")
nhlstandings0506po$season <- 20052006
nhlstandings0607po$season <- 20062007
nhlstandings0708po$season <- 20072008
nhlstandings0809po$season <- 20082009
nhlstandings0910po$season <- 20092010
nhlstandings1011po$season <- 20102011
nhlstandings1112po$season <- 20112012
nhlstandings1213po$season <- 20122013
nhlstandings1314po$season <- 20132014
nhlstandings1415po$season <- 20142015
nhlstandingspo <- rbind(
  nhlstandings0506po, nhlstandings0607po, nhlstandings0708po,
  nhlstandings0809po, nhlstandings0910po, nhlstandings1011po, 
  nhlstandings1112po, nhlstandings1213po, nhlstandings1314po,
  nhlstandings1415po)#,nhlstandings1516po)
rm(nhlstandings0506po, nhlstandings0607po, nhlstandings0708po,
   nhlstandings0809po, nhlstandings0910po, nhlstandings1011po,
   nhlstandings1112po, nhlstandings1213po, nhlstandings1314po,
   nhlstandings1415po)#, nhlstandings1516po)
# Voilà, we are going to work on these data frames:
# "nhlstandings": League standings for regular seasons 0506-1415
# "nhlstandingspo": Playoff "standings" for seasons 0506-1415
# "nhlTm": df containing names and suffixes of NHL Teams, later removed
# "statspl05061415": df containing player stats for seasons 0506-1415

# DATA TREATMENT ---------------------------------------------------------------
# G_GP, A_GP, PTS_GP: a player's Goals, Assists and Points per games
statspl05061415$G_GP <- statspl05061415$G / statspl05061415$GP
statspl05061415$A_GP <- statspl05061415$A / statspl05061415$GP
statspl05061415$PTS_GP <- statspl05061415$PTS / statspl05061415$GP
colnames(nhlTm) <- c("Full Name","Tm")
colnames(nhlstandings)[2] <- "Full Name"
colnames(nhlstandingspo)[2] <- "Full Name"
nhlstandings <- nhlstandings[!(nhlstandings$`Full Name`=="League Average"), ]
nhlstandingspo <- nhlstandingspo[
  !(nhlstandingspo$`Full Name`=="League Average"), ]
nhlstandings <- merge(nhlstandings, nhlTm, by="Full Name")
nhlstandingspo <- merge(nhlstandingspo, nhlTm, by="Full Name")
rm(nhlTm)
# RDS: How many playoff rounds did the team go?
nhlstandingspo$RDS <- 1
nhlstandingspo$RDS[nhlstandingspo$W>3] <- 2
nhlstandingspo$RDS[nhlstandingspo$W>7] <- 3
nhlstandingspo$RDS[nhlstandingspo$W>11] <- 4
nhlstandingspo$RDS[nhlstandingspo$W==16] <- 5
# Adjusting for franchise changes (Atlanta -> Winnipeg etc.)
nhlstandings$Tm2 <- nhlstandings$Tm
nhlstandings$Tm2 <- replace(nhlstandings$Tm2, which(nhlstandings$Tm2=="PHX"),
                            "ARI")
nhlstandings$Tm2 <- replace(nhlstandings$Tm2, which(nhlstandings$Tm2=="ATL"),
                            "WPG")
nhlstandingspo$Tm2 <- nhlstandingspo$Tm
nhlstandingspo$Tm2 <- replace(nhlstandingspo$Tm2, which(
  nhlstandingspo$Tm2=="PHX"),"ARI")
nhlstandingspo$Tm2 <- replace(nhlstandingspo$Tm2, which(
  nhlstandingspo$Tm2=="ATL"),"WPG")
statsgo05061415$Tm2 <- statsgo05061415$tm.fin
statsgo05061415$Tm2 <- replace(statsgo05061415$Tm2, which(
  statsgo05061415$Tm2=="PHX"),"ARI")
statsgo05061415$Tm2 <- replace(statsgo05061415$Tm2, which(
  statsgo05061415$Tm2=="ATL"),"WPG")
statspl05061415$Tm2 <- statspl05061415$tmfin
statspl05061415$Tm2 <- replace(statspl05061415$Tm2, which(
  statspl05061415$Tm2=="PHX"),"ARI")
statspl05061415$Tm2 <- replace(statspl05061415$Tm2, which(
  statspl05061415$Tm2=="ATL"),"WPG")
# DIFF, PTSG: a team's Goal Differential and Points per game in a season
# DIFFG: a team's Goal Differential per Game in a season 
nhlstandings$DIFF <- nhlstandings$GF - nhlstandings$GA 
nhlstandings$DIFFG <- nhlstandings$DIFF / nhlstandings$GP
nhlstandings$PTSG <- nhlstandings$PTS / nhlstandings$GP
nhlstandings$d1213 <- 0 # Dummy for Lockout Season 20122013
nhlstandings$d1213[nhlstandings$season==20122013] <- 1
# ATOI2: transform ATOI from text to time
dfatoi2 <- data.frame(str_split_fixed(statspl05061415$ATOI, ":", 3))
dfatoi2$X3 <- 00
dfatoi2$X4 <- do.call(paste, c(dfatoi2[c("X3","X1", "X2")], sep = ":"))
dfatoi2$X5 <- chron(times=dfatoi2$X4,format = c("h:m:s"),out.format=c("h:m:s"))
statspl05061415$ATOI2 <- dfatoi2$X5
rm(dfatoi2)
#Positions
statspl05061415$pos2 <- "F"
statspl05061415$pos2[statspl05061415$pos=="D"] <- "D"
statspl05061415$pos2[statspl05061415$pos=="G"] <- "G"

# GINI Coefficients ------------------------------------------------------------
# ginis, ginispo: Create Gini coefficients for teams per season. 0 = perfect 
# scoring equality since every player contributes the same amount of goals, 1 = 
# perfect scoring inequality since one player alone scores all the goals.
ginis<-ddply(
  subset(statspl05061415, session=="Regular"),
  .(Tm2, season), summarise,
  gG=ineq(G),
  gA=ineq(A),
  gPTS=ineq(PTS),
  gG_GP=ineq(G_GP),
  gA_GP=ineq(A_GP),
  gPTS_GP=ineq(PTS_GP))
ginispo<-ddply(
  subset(statspl05061415, session=="Playoffs"),
  .(Tm2, season), summarise,
  gG=ineq(G),
  gA=ineq(A),
  gPTS=ineq(PTS),
  gG_GP=ineq(G_GP),
  gA_GP=ineq(A_GP),
  gPTS_GP=ineq(PTS_GP))
# nhlstandings: add gini coefficients of any team and any season
nhlstandings <- merge(nhlstandings, ginis, by=c("Tm2", "season"))
nhlstandingspo <- merge(nhlstandingspo, ginispo, by=c("Tm2", "season"))
rm(ginis, ginispo)

# GINI PLOTS -------------------------------------------------------------------
# I play a bit around looking at different relationships and different Gini
# coefficients. Later, I zoom in on the Gini Goal coefficient and run some
# regressions
# Scatter of Gini Goal and Goal Differential per Game
ggplot(nhlstandings) + 
  geom_jitter(aes(gG,DIFFG), colour="red",shape=1,na.rm=TRUE) + 
  geom_smooth(aes(gG,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  # geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + 
  # geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")
# Scatter of Gini Goal and Points won per Game
ggplot(nhlstandings) + 
  geom_jitter(aes(gG, PTSG), colour="red", shape=1, na.rm=TRUE) + 
  geom_smooth(aes(gG, PTSG), method=lm, se=FALSE, colour="orange", na.rm=TRUE) +
  # geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + 
  # geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Goals", y = "Points per Game won")
# Scatter of Gini Assist and Goal Differential per Game
ggplot(nhlstandings) + 
  geom_jitter(aes(gA,DIFFG), colour="red",shape=1,na.rm=TRUE) + 
  geom_smooth(aes(gA,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  # geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + 
  # geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Assists", y = "Goal Diff. per Game")
# Scatter of Gini Points and Goal Differential per Game
ggplot(nhlstandings) + 
  geom_jitter(aes(gPTS,DIFFG), colour="red",shape=1,na.rm=TRUE) + 
  geom_smooth(aes(gPTS,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  # geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + 
  # geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Points", y = "Goal Diff. per Game")

# Scatters of Gini Goal and Goal Differential per Game
ggplot(nhlstandings) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(gG,DIFFG, colour=as.character(season))) + 
  geom_smooth(aes(gG,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +  #, scales="free_x") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")
# Scatters of Gini Goal and Points per Game
ggplot(nhlstandings) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(gG,PTSG, colour=as.character(season))) +
  geom_smooth(aes(gG,PTSG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) + #, scales="free_x") +
  labs(x = "Gini Goals", y = "Points per Game")

# The scatterplots clearly illustrate a solid negative relationship between 
# scoring inequality and team success. Time to run some regressions:

# GINI REGRESSIONS--------------------------------------------------------------
# Linear Regression of DIFFG, PTSG on gini goals
lmgG1 <- lm(DIFFG~gG,data=nhlstandings)
summary(lmgG1)
lmgG2 <- lm(PTSG~gG,data=nhlstandings)
summary(lmgG2)
# The linear regressions prove the point: There's a solid negative relationship
# between the gini coefficient of scoring and team success. To interpret the
# estimate is not straightforward since our independent var is a coefficient
# (what does a 0.1 point decrease in our gini coeff. mean?). Nonetheless, our
# results imply that secondary scoring contributes to a team's overall success.



