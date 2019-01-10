
# weaklink_stronglink.R
# Morris Trachsler, 2016
#
# "What's the best way to make my team better?" NHL team owners and GMs: this 
# script is for you. I try to make your life easier by tackling an interesting 
# question: Is it better to improve your worst player(s) or should one rather 
# buy another star?  To evaluate players, I use the Goals-above-Replacement 
# metric (GAR) found on originalsixanalytics.com. After the usual data reading 
# and cleaning process ("READ DATA", "DATA TREATMENT" sections), I reckon 
# several GARs for every team in the seasons under consideration ("PLAYERS' 
# GARs", "GOALIES' GARs" sect.). The following analysis ("PLOTS", "REGRESSIONS") 
# suggests that improving your lower-end players should be the way to go. But
# enough of me, now go and buy another star!
# 
# Chris Anderson and David Sally ask this very question in the context of 
# european football in their very recommendable book "The Numbers Game". I was 
# intrigued by their analysis and wondered if ice hockey would lead to similar 
# results


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
# library("dplyr", lib.loc="~/R/win-library/3.1")
library(stringr)
library(chron)
library(abind)

# Set working directory
setwd("C:/Users/morris/Desktop/RR/2ndscoring")

# READ DATA --------------------------------------------------------------------
nhlTm <- read_delim("nhlTm.txt","\t", escape_double = FALSE, col_names = FALSE)
# Individual player stats for the NHL seasons 0506 until 1415
statspl05061415 <- read_delim(
  "statspl05061415.csv", ";", escape_double=F,
  col_types=cols(Goalie.GAR=col_double(), drawn.GAR=col_double()))
# Individual goalie stats for the NHL seasons 0506 until 1415
statsgo05061415 <- read_delim("statsgo05061415.csv", ";", escape_double=F)
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
# "statsgo05061415": df containing goalie stats for seasons 0506-1415
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
# DIFF, PTSG: a team's Goal and Points Differential in a season
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
# statspl05061415ord: games played and av.time on ice as ordering factors
# This makes sense since we are interested in each teams most important, most
# utilized players
statspl05061415ord <- statspl05061415[with(statspl05061415,order(-GP,-ATOI2)), ]
# ordered list for goalies, games played and av.time on ice as ordering factors
statsgo05061415ord <- statsgo05061415[with(statsgo05061415, order(-GP)), ] 

# PLAYERS' GARs ----------------------------------------------------------------
# dffreg, dfdreg, dffpo, dfdpo: dfs with GARs (off., def., overall) for
# individual players in every season. We are looking for every team's most
# utilized players (12 Fs, 6 Ds, 2Gs) in each of the seasons
# Forwards, Regular Season
dffreg <- ddply(
  subset(statspl05061415ord, statspl05061415ord$session=="Regular" &
           statspl05061415ord$pos2=="F"),
  .(season, Tm2, pos2), summarize,
  pos     = pos,
  GP      = GP,
  ATOI2   = ATOI2,
  n       = player,
  GAR.Off = GAR.Off,
  GAR.Def = GAR.Def,
  GAR     = GAR) 
dffreg <- split(dffreg, list(dffreg$Tm2, dffreg$season))
dffreg <- lapply(dffreg, function(l) l[1:12,])  # 12 most-used (GP, ATOI2) F's 
dffreg <- do.call(rbind, dffreg)                # per team
# Defensemen, regular season
dfdreg <- ddply(
  subset(statspl05061415ord, statspl05061415ord$session=="Regular" &
           statspl05061415ord$pos2=="D"),
  .(season, Tm2, pos2), summarize,
  pos     = pos,
  GP      = GP,
  ATOI2   = ATOI2,
  n       = player,
  GAR.Off = GAR.Off,
  GAR.Def = GAR.Def,
  GAR     = GAR) #Defensemen, Regular Season
dfdreg <- split(dfdreg, list(dfdreg$Tm2, dfdreg$season))
dfdreg <- lapply(dfdreg, function(l) l[1:6,])  # 6 most-used (GP, ATOI2) D's per 
dfdreg <- do.call(rbind, dfdreg)               # team
#Forwards, Playoffs
dffpo <- ddply(
  subset(statspl05061415ord, statspl05061415ord$session=="Playoffs" &
           statspl05061415ord$pos2=="F"),
  .(season, Tm2, pos2), summarize,
  pos     = pos,
  GP      = GP,
  ATOI2   = ATOI2,
  n       = player,
  GAR.Off = GAR.Off,
  GAR.Def = GAR.Def,
  GAR     = GAR) 
dffpo <- split(dffpo, list(dffpo$Tm2, dffpo$season))
dffpo <- lapply(dffpo, function(l) l[1:12,])  # 12 most-used (GP, ATOI2) F's
dffpo <- do.call(rbind, dffpo)
#Defensemen, Playoffs
dfdpo <- ddply(
  subset(statspl05061415ord, statspl05061415ord$session=="Playoffs" &
           statspl05061415ord$pos2=="D"),
  .(season,Tm2,pos2), summarize,
  pos     = pos,
  GP      = GP,
  ATOI2   = ATOI2,
  n       = player,
  GAR.Off = GAR.Off,
  GAR.Def = GAR.Def,
  GAR     = GAR) 
dfdpo <- split(dfdpo, list(dfdpo$Tm2, dfdpo$season))
dfdpo <- lapply(dfdpo, function(l) l[1:6,])  # 6 most-used (GP, ATOI2) D's per 
dfdpo <- do.call(rbind, dfdpo)               # team

# dfdregmax, dffregmax, dfdpomax, dffpomax: df with max GARs (Off., Def., 
# Overall) for seasons and positions
dfdregmax <- ddply(
  subset(statspl05061415, statspl05061415$pos2=="D" &
           statspl05061415$session=="Regular"),
  .(season), summarize,
  maxGAR.Off = max(GAR.Off, na.rm=TRUE),
  maxGAR.Def = max(GAR.Def,na.rm=TRUE),  
  maxGAR     = max(GAR,na.rm=TRUE))  # 3 max GARs for every season (10 x 4 df)
dffregmax <- ddply(
  subset(statspl05061415, statspl05061415$pos2=="F" &
           statspl05061415$session=="Regular"),
  .(season), summarize,
  maxGAR.Off = max(GAR.Off,na.rm=TRUE),
  maxGAR.Def = max(GAR.Def,na.rm=TRUE),
  maxGAR     = max(GAR,na.rm=TRUE))
dfdpomax <- ddply(
  subset(statspl05061415, statspl05061415$pos2=="D" &
           statspl05061415$session=="Playoffs"),
  .(season), summarize,
  maxGAR.Off = max(GAR.Off,na.rm=TRUE),
  maxGAR.Def = max(GAR.Def,na.rm=TRUE),
  maxGAR     = max(GAR,na.rm=TRUE))
dffpomax <- ddply(
  subset(statspl05061415, statspl05061415$pos2=="F" &
           statspl05061415$session=="Playoffs"),
  .(season), summarize,
  maxGAR.Off = max(GAR.Off,na.rm=TRUE),
  maxGAR.Def = max(GAR.Def,na.rm=TRUE),
  maxGAR     = max(GAR,na.rm=TRUE))

# merging max values in dfdreg, dffreg, dfdpo, dffpo and computing relative GAR
# values of individual players
dfdreg <- merge(dfdreg, dfdregmax, by="season")
dfdreg$relgaroff <- dfdreg$GAR.Off / dfdreg$maxGAR.Off
dfdreg$relgardef <- dfdreg$GAR.Def / dfdreg$maxGAR.Def
dfdreg$relgar <- dfdreg$GAR / dfdreg$maxGAR
dfdpo <- merge(dfdpo, dfdpomax, by="season")
dfdpo$relgaroff <- dfdpo$GAR.Off / dfdpo$maxGAR.Off
dfdpo$relgardef <- dfdpo$GAR.Def / dfdpo$maxGAR.Def
dfdpo$relgar <- dfdpo$GAR / dfdpo$maxGAR
dffreg <- merge(dffreg, dffregmax, by="season")
dffreg$relgaroff <- dffreg$GAR.Off / dffreg$maxGAR.Off
dffreg$relgardef <- dffreg$GAR.Def / dffreg$maxGAR.Def
dffreg$relgar <- dffreg$GAR / dffreg$maxGAR
dffpo <- merge(dffpo, dffpomax, by="season")
dffpo$relgaroff <- dffpo$GAR.Off / dffpo$maxGAR.Off
dffpo$relgardef <- dffpo$GAR.Def / dffpo$maxGAR.Def
dffpo$relgar <- dffpo$GAR / dffpo$maxGAR
# merge: an index var for subsequent merging
dfdreg$merge <- do.call(paste, c(dfdreg[c("Tm2","season")], sep = "."))
dfdpo$merge <- do.call(paste, c(dfdpo[c("Tm2","season")], sep = "."))
dffreg$merge <- do.call(paste, c(dffreg[c("Tm2","season")], sep = "."))
dffpo$merge <- do.call(paste, c(dffpo[c("Tm2","season")], sep = "."))
nhlstandings$merge <- do.call(paste, c(nhlstandings[c("Tm2","season")],
                                       sep = "."))
nhlstandingspo$merge <- do.call(paste, c(nhlstandingspo[c("Tm2","season")],
                                         sep = "."))

# dfs with all relevant GARs for teams in every season
dfdreg <- ddply(dfdreg,   .(season,  Tm2),  summarize,  
              maxGAR.Off=max(GAR.Off,  na.rm=TRUE),  
              minGAR.Off=min(GAR.Off, na.rm=TRUE), 
              maxGAR.Def=max(GAR.Def, na.rm=TRUE), 
              minGAR.Def=min(GAR.Def, na.rm=TRUE), 
              maxGAR=max(GAR, na.rm=TRUE), 
              minGAR=min(GAR, na.rm=TRUE), 
              maxrelgaroff=max(relgaroff, na.rm=TRUE), 
              minrelgaroff=min(relgaroff, na.rm=TRUE), 
              maxrelgardef=max(relgardef, na.rm=TRUE), 
              minrelgardef=min(relgardef, na.rm=TRUE), 
              maxrelgar=max(relgar, na.rm=TRUE), 
              minrelgar=min(relgar, na.rm=TRUE), 
              merge=head(merge, 1))
dfdpo <- ddply(dfdpo, .(season, Tm2), summarize, 
             maxGAR.Off=max(GAR.Off, na.rm=TRUE), 
             minGAR.Off=min(GAR.Off, na.rm=TRUE), 
             maxGAR.Def=max(GAR.Def, na.rm=TRUE), 
             minGAR.Def=min(GAR.Def, na.rm=TRUE), 
             maxGAR=max(GAR, na.rm=TRUE), 
             minGAR=min(GAR, na.rm=TRUE), 
             maxrelgaroff=max(relgaroff, na.rm=TRUE), 
             minrelgaroff=min(relgaroff, na.rm=TRUE), 
             maxrelgardef=max(relgardef, na.rm=TRUE), 
             minrelgardef=min(relgardef, na.rm=TRUE), 
             maxrelgar=max(relgar, na.rm=TRUE), 
             minrelgar=min(relgar, na.rm=TRUE), 
             merge=head(merge, 1))
dffreg <- ddply(dffreg, .(season, Tm2), summarize, 
              maxGAR.Off=max(GAR.Off, na.rm=TRUE), 
              minGAR.Off=min(GAR.Off, na.rm=TRUE), 
              maxGAR.Def=max(GAR.Def, na.rm=TRUE), 
              minGAR.Def=min(GAR.Def, na.rm=TRUE), 
              maxGAR=max(GAR, na.rm=TRUE), 
              minGAR=min(GAR, na.rm=TRUE), 
              maxrelgaroff=max(relgaroff, na.rm=TRUE), 
              minrelgaroff=min(relgaroff, na.rm=TRUE), 
              maxrelgardef=max(relgardef, na.rm=TRUE), 
              minrelgardef=min(relgardef, na.rm=TRUE), 
              maxrelgar=max(relgar, na.rm=TRUE), 
              minrelgar=min(relgar, na.rm=TRUE), 
              merge=head(merge, 1))
dffpo <- ddply(dffpo, .(season, Tm2), summarize, 
             maxGAR.Off=max(GAR.Off, na.rm=TRUE), 
             minGAR.Off=min(GAR.Off, na.rm=TRUE), 
             maxGAR.Def=max(GAR.Def, na.rm=TRUE), 
             minGAR.Def=min(GAR.Def, na.rm=TRUE), 
             maxGAR=max(GAR, na.rm=TRUE), 
             minGAR=min(GAR, na.rm=TRUE), 
             maxrelgaroff=max(relgaroff, na.rm=TRUE), 
             minrelgaroff=min(relgaroff, na.rm=TRUE), 
             maxrelgardef=max(relgardef, na.rm=TRUE), 
             minrelgardef=min(relgardef, na.rm=TRUE), 
             maxrelgar=max(relgar, na.rm=TRUE), 
             minrelgar=min(relgar, na.rm=TRUE), 
             merge=head(merge, 1))

# nhlstandings: merge these dfs with standings
nhlstandings <- merge(nhlstandings, dfdreg, by="merge")
names(nhlstandings) <- gsub("max", "Dstrong", names(nhlstandings))
names(nhlstandings) <- gsub("min", "Dweak", names(nhlstandings))
nhlstandings$season.y <- NULL
nhlstandings$Tm2.y <- NULL
nhlstandings <- merge(nhlstandings, dffreg, by="merge")
names(nhlstandings) <- gsub("max", "Fstrong", names(nhlstandings))
names(nhlstandings) <- gsub("min", "Fweak", names(nhlstandings))
nhlstandings$season <- NULL
nhlstandings$Tm2 <- NULL
nhlstandingspo <- merge(nhlstandingspo, dfdpo, by="merge")
names(nhlstandingspo) <- gsub("max", "Dstrong", names(nhlstandingspo))
names(nhlstandingspo) <- gsub("min", "Dweak", names(nhlstandingspo))
nhlstandingspo$season.y <- NULL
nhlstandingspo$Tm2.y <- NULL
nhlstandingspo <- merge(nhlstandingspo, dffpo, by="merge")
names(nhlstandingspo) <- gsub("max", "Fstrong", names(nhlstandingspo))
names(nhlstandingspo) <- gsub("min", "Fweak", names(nhlstandingspo))
nhlstandingspo$season <- NULL
nhlstandingspo$Tm2 <- NULL
# Remove unnecessary vars
rm(dfdpomax, dfdregmax, dffregmax, dffpomax)
rm(dfdpo, dfdreg, dffpo, dffreg)

# GOALIES' GARs ----------------------------------------------------------------
# dfgreg, dfgpo: dfs containing GARs every goalie in each year
# We are only interested in each team's two most used goalies
# Goalies, Regular Season
dfgreg <- ddply(
  subset(statsgo05061415ord, statsgo05061415ord$session=="Regular" &
           statsgo05061415ord$Pos=="G"),
  .(season,Tm2,Pos), summarize,
  Pos = Pos,
  GP = GP,
  n = player,
  GAR = GAR) 
dfgreg <- split(dfgreg, list(dfgreg$Tm2, dfgreg$season))
dfgreg <- lapply(dfgreg, function(l) l[1:2,])  # 2 most-used (GP, ATOI2) G's
dfgreg <- do.call(rbind, dfgreg)
# Goalies, Playoffs
dfgpo <- ddply(
  subset(statsgo05061415ord, statsgo05061415ord$session=="Playoffs" &
           statsgo05061415ord$Pos=="G"),
  .(season, Tm2, Pos), summarize,
  Pos = Pos,
  GP = GP,
  n = player,
  GAR = GAR) 
dfgpo <- split(dfgpo, list(dfgpo$Tm2, dfgpo$season))
dfgpo <- lapply(dfgpo, function(l) l[1:2,])  # 2 most-used (GP, ATOI2) G's
dfgpo <- do.call(rbind, dfgpo)

# dfgregmax, dfgpomax: max values for goalies every year
dfgregmax <- ddply(subset(statsgo05061415, statsgo05061415$session=="Regular"),
                   .(season), summarize,
                   maxGAR=max(GAR, na.rm=TRUE))
dfgpomax <- ddply(subset(statsgo05061415, statsgo05061415$session=="Playoffs"),
                  .(season), summarize,
                  maxGAR=max(GAR,na.rm=TRUE))
# merge this with relevant dfs
dfgreg <- merge(dfgreg, dfgregmax, by="season")
dfgreg$relgar <- dfgreg$GAR / dfgreg$maxGAR
dfgpo <- merge(dfgpo, dfgpomax, by="season")
dfgpo$relgar <- dfgpo$GAR / dfgpo$maxGAR
# Creating index-var: merge
dfgreg$merge <- do.call(paste, c(dfgreg[c("Tm2", "season")], sep = "."))
dfgpo$merge <- do.call(paste, c(dfgpo[c("Tm2","season")], sep = "."))

# dfgreg, dfgpo: Goalie GARs for every team every season
dfgreg <- ddply(dfgreg, .(season,Tm2), summarize,
              maxGAR=max(GAR),#,na.rm=TRUE),
              minGAR=min(GAR),#,na.rm=TRUE),
              maxrelgar=max(relgar),#,na.rm=TRUE),
              minrelgar=min(relgar),#,na.rm=TRUE),
              merge=head(merge, 1))
dfgpo <- ddply(dfgpo, .(season, Tm2), summarize,
             maxGAR=max(GAR),#,na.rm=TRUE),
             minGAR=min(GAR),#,na.rm=TRUE),
             maxrelgar=max(relgar),#,na.rm=TRUE),
             minrelgar=min(relgar),#,na.rm=TRUE),
             merge=head(merge, 1))
# again, merge this with standings
nhlstandings <- merge(nhlstandings, dfgreg, by="merge")
names(nhlstandings) <- gsub("max", "Gstrong", names(nhlstandings))
names(nhlstandings) <- gsub("min", "Gweak", names(nhlstandings))
nhlstandings$season <- NULL
nhlstandings$Tm2 <- NULL
nhlstandings <- rename(nhlstandings, c("season.x"="season", "Tm2.x"="Tm2"))
nhlstandingspo <- merge(nhlstandingspo, dfgpo, by="merge")
names(nhlstandingspo) <- gsub("max", "Gstrong", names(nhlstandingspo))
names(nhlstandingspo) <- gsub("min", "Gweak", names(nhlstandingspo))
nhlstandingspo$season <- NULL
nhlstandingspo$Tm2 <- NULL
nhlstandingspo <- rename(nhlstandingspo,
                          c("season.x"="season", "Tm2.x"="Tm2"))
# remove vars
rm(dfgpo, dfgreg, dfgregmax, dfgpomax)

# Regular season:
# strongrelgaroff: strongest relative off. GAR per team and year
nhlstandings[, "strongrelgaroff"] <- apply(
  nhlstandings[, c("Dstrongrelgaroff", "Fstrongrelgaroff")], 1, max)
# weakrelgaroff: weakest relative off. GAR per team and year
nhlstandings[, "weakrelgaroff"] <- apply(
  nhlstandings[, c("Dweakrelgaroff", "Fweakrelgaroff")], 1, min)
# strongrelgardef: strongest relative def. GAR
nhlstandings[, "strongrelgardef"] <- apply(
  nhlstandings[, c("Dstrongrelgardef", "Fstrongrelgardef")], 1, max)
# weakrelgardef: weakest relative def. GAR
nhlstandings[, "weakrelgardef"] <- apply(
  nhlstandings[, c("Dweakrelgardef", "Fweakrelgardef")], 1, min)
# strongrelgar: strongest relative GAR (player OR goalie)
nhlstandings[, "strongrelgar"] <- apply(
  nhlstandings[, c("Dstrongrelgar", "Fstrongrelgar", "Gstrongrelgar")], 1, max,
  na.rm=TRUE)
# weakrelgar: weakest relative GAR (player OR goalie)
nhlstandings[, "weakrelgar"] <- apply(
  nhlstandings[, c("Dweakrelgar", "Fweakrelgar", "Gweakrelgar")], 1, min,
  na.rm=TRUE)
# strongGAR: strongest absolute GAR (player or goalie)
nhlstandings[, "strongGAR"] <- apply(
  nhlstandings[, c("DstrongGAR", "FstrongGAR", "GstrongGAR")], 1, max,
  na.rm=TRUE)
# weakGAR: weakest absolute GAR (player or goalie)
nhlstandings[, "weakGAR"] <- apply(
  nhlstandings[, c("DweakGAR", "FweakGAR", "GweakGAR")], 1, min,
  na.rm=TRUE)

# Playoffs:
# strongrelgaroff: strongest relative off. GAR per team and year
nhlstandingspo[, "strongrelgaroff"] <- apply(
  nhlstandingspo[, c("Dstrongrelgaroff", "Fstrongrelgaroff")], 1,
  max)
# weakrelgaroff: weakest relative off. GAR players
nhlstandingspo[, "weakrelgaroff"] <- apply(
  nhlstandingspo[, c("Dweakrelgaroff", "Fweakrelgaroff")], 1,
  min)
# strongrelgardef: strongest relative def. GAR
nhlstandingspo[, "strongrelgardef"] <- apply(
  nhlstandingspo[, c("Dstrongrelgardef", "Fstrongrelgardef")], 1,
  max)
# weakrelgardef: weakest relative def. GAR
nhlstandingspo[, "weakrelgardef"] <- apply(
  nhlstandingspo[, c("Dweakrelgardef", "Fweakrelgardef")], 1,
  min)
# strongrelgar: strongest relative GAR (players and goalies)
nhlstandingspo[, "strongrelgar"] <- apply(
  nhlstandingspo[, c("Dstrongrelgar", "Fstrongrelgar", "Gstrongrelgar")], 1,
  max, na.rm=TRUE)
# weakrelgar: weakest relative GAR (players, goalies)
nhlstandingspo[, "weakrelgar"] <- apply(
  nhlstandingspo[, c("Dweakrelgar", "Fweakrelgar", "Gweakrelgar")], 1,
  min, na.rm=TRUE)
# strongGAR: strongest absolute GAR (players and goalies)
nhlstandingspo[, "strongGAR"] <- apply(
  nhlstandingspo[, c("DstrongGAR", "FstrongGAR", "GstrongGAR")], 1,
  max, na.rm=TRUE)
# weakGAR: weakest absolute GAR (players and goalies)
nhlstandingspo[, "weakGAR"] <- apply(
  nhlstandingspo[, c("DweakGAR", "FweakGAR", "GweakGAR")], 1,
  min, na.rm=TRUE)
# GARdiff: Difference between strongest and weakest players in a team
nhlstandings$GARdiff <- nhlstandings$strongGAR - nhlstandings$weakGAR
nhlstandingspo$GARdiff <- nhlstandingspo$strongGAR - nhlstandingspo$weakGAR

# PLOTS ------------------------------------------------------------------------
# is there clustering of strong links/weak links?
# Not surprisingly, Anderson and Sally found strong clustering in european
# football: there's a strong positive relationship between the strengths of the
# teams' best and worst players (thank you Champions League). Is this also the 
# case in the NHL?
ggplot(nhlstandings) + 
  geom_jitter(aes(strongGAR,weakGAR), shape=1) +
  geom_smooth(aes(strongGAR,weakGAR), method=lm, se=FALSE,colour="orange") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
ggplot(nhlstandings) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(strongGAR,weakGAR, colour=as.character(season))) +
  geom_smooth(aes(strongGAR,weakGAR, colour=as.character(season)),
              method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
# The effect is present but only very moderate

# positionwise clustering? any combinations (e.g. strong d/weak g)?
# Dstrong/Dweak
ggplot(nhlstandings) + 
  geom_jitter(aes(DstrongGAR,DweakGAR),shape=1) +
  geom_smooth(aes(DstrongGAR,DweakGAR), method=lm, se=FALSE,colour="orange") #+
  #labs(x = "Dstrong Link GAR-Score", y = "Dweak Link GAR-Score")
ggplot(nhlstandings) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(DstrongGAR,DweakGAR, colour=as.character(season))) +
  geom_smooth(aes(DstrongGAR,DweakGAR, colour=as.character(season)),
              method=lm, se=FALSE) +
  facet_wrap(~season)  # +  , scales="free_x")  +
  #labs(x = "Dstrong Link GAR-Score", y = "Dweak Link GAR-Score")
#Fstrong/Fweak
ggplot(nhlstandings) + 
  geom_jitter(aes(FstrongGAR,FweakGAR),shape=1) +
  geom_smooth(aes(FstrongGAR,FweakGAR), method=lm, se=FALSE,colour="orange") 
  #labs(x = "Fstrong Link GAR-Score", y = "Fweak Link GAR-Score")
ggplot(nhlstandings) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(FstrongGAR,FweakGAR, colour=as.character(season))) +
  geom_smooth(aes(FstrongGAR,FweakGAR, colour=as.character(season)),
              method=lm, se=FALSE) +
  facet_wrap(~season)  # + , scales="free_x") +
  #labs(x = "Fstrong Link GAR-Score", y = "Fweak Link GAR-Score")
# There also is some "positional clustering", meaning that stronger teams have 
# a better star AND a better weakest player

# DIFFG~weak/strong link
# Now we start to look at the relationship between the weakest/strongest 
# players and team success:
ggplot(nhlstandings) + 
  geom_jitter(aes(strongGAR,DIFFG), colour="red",shape=1) +
  geom_smooth(aes(strongGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) +
  geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Weak Link/Strong Link GAR-Score", y = "Goal Differential per Game")
ggplot(nhlstandings) +
  geom_jitter(aes(strongGAR,DIFFG, colour=as.character(season))) +
  geom_smooth(aes(strongGAR,DIFFG, colour=as.character(season)),
              method=lm, se=FALSE) +
  geom_jitter(aes(weakGAR,DIFFG, colour=as.character(season))) +
  geom_smooth(aes(weakGAR,DIFFG, colour=as.character(season)),
              method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
# PTSG~weak/strong link
ggplot(nhlstandings) + 
  geom_jitter(aes(strongGAR,PTSG), colour="red",shape=1) + 
  geom_smooth(aes(strongGAR,PTSG), method=lm, se=FALSE,colour="orange") +
  geom_jitter(aes(weakGAR,PTSG), colour="blue",shape=1) + 
  geom_smooth(aes(weakGAR,PTSG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Weak Link/Strong Link GAR-Score", y = "Points per Game")
ggplot(nhlstandings) +
  geom_jitter(aes(strongGAR,PTSG, colour=as.character(season))) +
  geom_smooth(aes(strongGAR,PTSG, colour=as.character(season)),
              method=lm, se=FALSE) +
  geom_jitter(aes(weakGAR,PTSG, colour=as.character(season))) +
  geom_smooth(aes(weakGAR,PTSG, colour=as.character(season)), 
              method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
# Irrespective of the measure (Goal differential per game, points per game),
# there is, as expected, a strong positive relationship between the strength
# of a team's weakest/best player and team success. But which relationship is 
# stronger? Put differently: Should the GM rather improve his weakest or his 
# strongest link? Regression to the rescue:

# REGRESSIONS------------------------------------------------------------------
lmlink1 <- lm(DIFFG ~ weakGAR + strongGAR, data=nhlstandings)
summary(lmlink1)
lmlink2 <- lm(PTSG ~ weakGAR + strongGAR, data=nhlstandings)
summary(lmlink2)
# Again, both estimation coefficients are highly significant and positive. They
# are also similar in scope, with a slight advantage for the weakGAR coefficient
# (in both regressions). This, and the fact that weaker players usually come at 
# a fraction of the price of another star player give us the answer: Dear team 
# owner, thou shalt improve your team from the bottom-up
#



