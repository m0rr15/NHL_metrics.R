#Load Packages, set working Dir, load data

# library("biglm", lib.loc="~/R/win-library/3.1")
# library("bitops", lib.loc="~/R/win-library/3.1")
#library("nhlscrapr", lib.loc="~/R/win-library/3.1")
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
#library("dplyr", lib.loc="~/R/win-library/3.1")
library(stringr)
library(chron)
library(abind)

setwd("C:/Users/morris/Desktop/RR/2ndscoring")



# READ DATA ---------------------------------------------------------------


# garmtr <- read_delim("garmtr.csv",";", escape_double = FALSE)
nhlTm <- read_delim("nhlTm.txt","\t", escape_double = FALSE, col_names = FALSE)

# nhlstats0203to1415 <- read_delim("./playerstats/nhlstats0203to1415.csv", ";", escape_double = FALSE)

statspl05061415 <- read_delim("statspl05061415.csv", ";", escape_double = FALSE, col_types = cols(Goalie.GAR = col_double(),drawn.GAR = col_double()))
statsgo05061415 <- read_delim("statsgo05061415.csv", ";", escape_double = FALSE)

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
# nhlstandings1516 <- read_csv("./standings/nhl201516standings.txt")
# nhlstandings1617 <- read_csv("./standings/nhl201617standings.txt")
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
# nhlstandings1516$season <- 20152016
# nhlstandings1617$season <- 20162017

# nhlstats1011 <- read_csv("./playerstats/Skaters/nhlstats201011.txt")
# nhlstats1112 <- read_csv("./playerstats/Skaters/nhlstats201112.txt")
# nhlstats1213 <- read_csv("./playerstats/Skaters/nhlstats201213.txt")
# nhlstats1314 <- read_csv("./playerstats/Skaters/nhlstats201314.txt")
# nhlstats1415 <- read_csv("./playerstats/Skaters/nhlstats201415.txt")
# nhlstats1516 <- read_csv("./playerstats/Skaters/nhlstats201516.txt")
# nhlstats1617 <- read_csv("./playerstats/Skaters/nhlstats201617.txt")
# nhlstats1011po <- read_csv("./playerstats/Skaters/nhlstats1011po.txt")
# nhlstats1112po <- read_csv("./playerstats/Skaters/nhlstats1112po.txt")
# nhlstats1213po <- read_csv("./playerstats/Skaters/nhlstats1213po.txt")
# nhlstats1314po <- read_csv("./playerstats/Skaters/nhlstats1314po.txt")
# nhlstats1415po <- read_csv("./playerstats/Skaters/nhlstats1415po.txt")
# nhlstats1516po <- read_csv("./playerstats/Skaters/nhlstats1516po.txt")


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
# nhlstandings1516po <- read_csv("./standings/nhl201516standingspo.txt")
# nhlstandings1617po <- read_csv("./standings/nhl201617standingspo.txt")
nhlstandings0506po$season<-20052006
nhlstandings0607po$season<-20062007
nhlstandings0708po$season<-20072008
nhlstandings0809po$season<-20082009
nhlstandings0910po$season<-20092010
nhlstandings1011po$season<-20102011
nhlstandings1112po$season<-20112012
nhlstandings1213po$season<-20122013
nhlstandings1314po$season<-20132014
nhlstandings1415po$season<-20142015
# nhlstandings1516po$season<-20152016
# nhlstandings1617po$season<-20162017

# nhlstats<-rbind(nhlstats1011,nhlstats1112,nhlstats1213,nhlstats1314,nhlstats1415,nhlstats1516,nhlstats1617)
nhlstandings<-rbind(nhlstandings0506,nhlstandings0607,nhlstandings0708,nhlstandings0809,nhlstandings0910,nhlstandings1011,nhlstandings1112,nhlstandings1213,nhlstandings1314,nhlstandings1415)#,nhlstandings1516,nhlstandings1617)
# nhlstandings$GD<-(nhlstandings$GF-nhlstandings$GA)
# nhlstatspo<-rbind(nhlstats1011po,nhlstats1112po,nhlstats1213po,nhlstats1314po,nhlstats1415po,nhlstats1516po)
nhlstandingspo<-rbind(nhlstandings0506po,nhlstandings0607po,nhlstandings0708po,nhlstandings0809po,nhlstandings0910po,nhlstandings1011po,nhlstandings1112po,nhlstandings1213po,nhlstandings1314po,nhlstandings1415po)#,nhlstandings1516po)

# rm(nhlstats1011,nhlstats1112,nhlstats1213,nhlstats1314,nhlstats1415,nhlstats1516,nhlstats1617)
rm(nhlstandings0506,nhlstandings0607,nhlstandings0708,nhlstandings0809,nhlstandings0910,nhlstandings1011,nhlstandings1112,nhlstandings1213,nhlstandings1314,nhlstandings1415)#,nhlstandings1516,nhlstandings1617)
# rm(nhlstats1011po,nhlstats1112po,nhlstats1213po,nhlstats1314po,nhlstats1415po,nhlstats1516po)
rm(nhlstandings0506po,nhlstandings0607po,nhlstandings0708po,nhlstandings0809po,nhlstandings0910po,nhlstandings1011po,nhlstandings1112po,nhlstandings1213po,nhlstandings1314po,nhlstandings1415po)#,nhlstandings1516po)



# DATA TREATMENT ---------------------------------------------------------

# garmtr<-garmtr[!(garmtr$season==20022003),]
# garmtr<-garmtr[!(garmtr$season==20032004),]
# garmtr<-garmtr[!(garmtr$season==20022003),]
# garmtr<-garmtr[!(is.na(garmtr$team)), ]

# ddply(subset(garmtr,season==20142015),.(teamEND,position),summarize,maxgar=max(totalgar),mingar=min(totalgar),N=length(totalgar))



# nhlstats1011$G_GP=nhlstats1011$G/nhlstats1011$GP
# nhlstats1011$A_GP=nhlstats1011$A/nhlstats1011$GP
# nhlstats1011$PTS_GP=nhlstats1011$PTS/nhlstats1011$GP
# nhlstats1112$G_GP=nhlstats1112$G/nhlstats1112$GP
# nhlstats1112$A_GP=nhlstats1112$A/nhlstats1112$GP
# nhlstats1112$PTS_GP=nhlstats1112$PTS/nhlstats1112$GP
# nhlstats1213$G_GP=nhlstats1213$G/nhlstats1213$GP
# nhlstats1213$A_GP=nhlstats1213$A/nhlstats1213$GP
# nhlstats1213$PTS_GP=nhlstats1213$PTS/nhlstats1213$GP
# nhlstats1314$G_GP=nhlstats1314$G/nhlstats1314$GP
# nhlstats1314$A_GP=nhlstats1314$A/nhlstats1314$GP
# nhlstats1314$PTS_GP=nhlstats1314$PTS/nhlstats1314$GP
# nhlstats1415$G_GP=nhlstats1415$G/nhlstats1415$GP
# nhlstats1415$A_GP=nhlstats1415$A/nhlstats1415$GP
# nhlstats1415$PTS_GP=nhlstats1415$PTS/nhlstats1415$GP
# nhlstats1516$G_GP=nhlstats1516$G/nhlstats1516$GP
# nhlstats1516$A_GP=nhlstats1516$A/nhlstats1516$GP
# nhlstats1516$PTS_GP=nhlstats1516$PTS/nhlstats1516$GP
# nhlstats1617$G_GP=nhlstats1617$G/nhlstats1617$GP
# nhlstats1617$A_GP=nhlstats1617$A/nhlstats1617$GP
# nhlstats1617$PTS_GP=nhlstats1617$PTS/nhlstats1617$GP
# # nhlstats1415po$G_GP=nhlstats1415po$G/nhlstats1415po$GP
# # nhlstats1415po$A_GP=nhlstats1415po$A/nhlstats1415po$GP
# # nhlstats1415po$PTS_GP=nhlstats1415po$PTS/nhlstats1415po$GP
# # nhlstats1516po$G_GP=nhlstats1516po$G/nhlstats1516po$GP
# # $A_GP=nhlstats1516po$A/nhlstats1516po$GP
# # nhlstats1516po$PTS_GP=nhlstats1516po$PTS/nhlstats1516po$GP
statspl05061415$G_GP=statspl05061415$G/statspl05061415$GP
statspl05061415$A_GP=statspl05061415$A/statspl05061415$GP
statspl05061415$PTS_GP=statspl05061415$PTS/statspl05061415$GP


colnames(nhlTm)<-c("Full Name","Tm")
# colnames(nhlstandings1011)[2]<-"Full Name"
# colnames(nhlstandings1112)[2]<-"Full Name"
# colnames(nhlstandings1213)[2]<-"Full Name"
# colnames(nhlstandings1314)[2]<-"Full Name"
# colnames(nhlstandings1415)[2]<-"Full Name"
# colnames(nhlstandings1516)[2]<-"Full Name"
# colnames(nhlstandings1617)[2]<-"Full Name"
colnames(nhlstandings)[2]<-"Full Name"
colnames(nhlstandingspo)[2]<-"Full Name"

# nhlstats1011<-nhlstats1011[!(nhlstats1011$Tm=="TOT"),]
# nhlstats1112<-nhlstats1112[!(nhlstats1112$Tm=="TOT"),]
# nhlstats1213<-nhlstats1213[!(nhlstats1213$Tm=="TOT"),]
# nhlstats1314<-nhlstats1314[!(nhlstats1314$Tm=="TOT"),]
# nhlstats1415<-nhlstats1415[!(nhlstats1415$Tm=="TOT"),]
# nhlstats1516<-nhlstats1516[!(nhlstats1516$Tm=="TOT"),]
# nhlstats1617<-nhlstats1617[!(nhlstats1617$Tm=="TOT"),]
# nhlstats<-nhlstats[!(nhlstats$Tm=="TOT"),]

# nhlstandings1011<-nhlstandings1011[!(nhlstandings1011$`Full Name`=="League Average"),]
# nhlstandings1112<-nhlstandings1112[!(nhlstandings1112$`Full Name`=="League Average"),]
# nhlstandings1213<-nhlstandings1213[!(nhlstandings1213$`Full Name`=="League Average"),]
# nhlstandings1314<-nhlstandings1314[!(nhlstandings1314$`Full Name`=="League Average"),]
# nhlstandings1415<-nhlstandings1415[!(nhlstandings1415$`Full Name`=="League Average"),]
# nhlstandings1516<-nhlstandings1516[!(nhlstandings1516$`Full Name`=="League Average"),]
# nhlstandings1617<-nhlstandings1617[!(nhlstandings1617$`Full Name`=="League Average"),]
nhlstandings<-nhlstandings[!(nhlstandings$`Full Name`=="League Average"),]
nhlstandingspo<-nhlstandingspo[!(nhlstandingspo$`Full Name`=="League Average"),]

# nhlstandings1011<-merge(nhlstandings1011,nhlTm, by="Full Name")
# nhlstandings1112<-merge(nhlstandings1112,nhlTm, by="Full Name")
# nhlstandings1213<-merge(nhlstandings1213,nhlTm, by="Full Name")
# nhlstandings1314<-merge(nhlstandings1314,nhlTm, by="Full Name")
# nhlstandings1415<-merge(nhlstandings1415,nhlTm, by="Full Name")
# nhlstandings1516<-merge(nhlstandings1516,nhlTm, by="Full Name")
# nhlstandings1617<-merge(nhlstandings1617,nhlTm, by="Full Name")
nhlstandings<-merge(nhlstandings,nhlTm, by="Full Name")
nhlstandingspo<-merge(nhlstandingspo,nhlTm, by="Full Name")

# rm(nhlTm)


nhlstandingspo$RDS<-1
nhlstandingspo$RDS[nhlstandingspo$W>3]<-2
nhlstandingspo$RDS[nhlstandingspo$W>7]<-3
nhlstandingspo$RDS[nhlstandingspo$W>11]<-4
nhlstandingspo$RDS[nhlstandingspo$W==16]<-5

nhlstandings$Tm2<-nhlstandings$Tm
nhlstandings$Tm2<-replace(nhlstandings$Tm2,which(nhlstandings$Tm2=="PHX"),"ARI")
nhlstandings$Tm2<-replace(nhlstandings$Tm2,which(nhlstandings$Tm2=="ATL"),"WPG")
nhlstandingspo$Tm2<-nhlstandingspo$Tm
nhlstandingspo$Tm2<-replace(nhlstandingspo$Tm2,which(nhlstandingspo$Tm2=="PHX"),"ARI")
nhlstandingspo$Tm2<-replace(nhlstandingspo$Tm2,which(nhlstandingspo$Tm2=="ATL"),"WPG")
statsgo05061415$Tm2<-statsgo05061415$tm.fin
statsgo05061415$Tm2<-replace(statsgo05061415$Tm2,which(statsgo05061415$Tm2=="PHX"),"ARI")
statsgo05061415$Tm2<-replace(statsgo05061415$Tm2,which(statsgo05061415$Tm2=="ATL"),"WPG")
statspl05061415$Tm2<-statspl05061415$tmfin
statspl05061415$Tm2<-replace(statspl05061415$Tm2,which(statspl05061415$Tm2=="PHX"),"ARI")
statspl05061415$Tm2<-replace(statspl05061415$Tm2,which(statspl05061415$Tm2=="ATL"),"WPG")
# nhlstatspo$Tm2<-nhlstatspo$Tm
# nhlstatspo$Tm2<-replace(nhlstatspo$Tm2,which(nhlstatspo$Tm2=="PHX"),"ARI")
# nhlstatspo$Tm2<-replace(nhlstatspo$Tm2,which(nhlstatspo$Tm2=="ATL"),"WPG")
# count(nhlstandings$Tm2=="PHX"|nhlstandings$Tm2=="ATL")

nhlstandings$DIFF<-nhlstandings$GF-nhlstandings$GA #Goal Differential
nhlstandings$DIFFG<-nhlstandings$DIFF/nhlstandings$GP
nhlstandings$PTSG<-nhlstandings$PTS/nhlstandings$GP
nhlstandings$d1213<-0 #Dummy for Lockout Season 20122013
nhlstandings$d1213[nhlstandings$season==20122013]<-1

# nhlstats$Player<-gsub("\\\\",";",nhlstats$Player) # Vor- und Nachnamen trennen  (\-getrennt) und in versch. Variablen speichern
# dfnames<-data.frame(str_split_fixed(nhlstats$Player, ";", 2))
# nhlstats$Player<-dfnames$X1
# rm(dfnames)
# dfnames<-data.frame(str_split_fixed(nhlstats$Player, " ", 4))
# dfnames$fname<-tolower(dfnames$X1)
# dfnames$X5<-"xxxx"
# dfnames$lname<-do.call(paste, c(dfnames[c("X2", "X3","X4","X5")], sep = "")) 
# dfnames$lname<-tolower(dfnames$lname)
# dfnames$fname<-substr(dfnames$fname,1,2)
# dfnames$lname<-substr(dfnames$lname,1,5)
# dfnames$woi.id<-do.call(paste, c(dfnames[c("lname","fname")], sep = ""))
# nhlstats<-cbind(nhlstats,dfnames$woi.id)
# rm(dfnames)
# df <- data.frame(ID=11:13, FOO=c('a|b','b|c','x|y'))
# df<-within(df, FOO<-data.frame(do.call('rbind', strsplit(as.character(FOO), '|', fixed=TRUE))))


# STRONG LINK/WEAK LINK Players---------------------------------------------------

dfatoi2<-data.frame(str_split_fixed(statspl05061415$ATOI, ":", 3))
dfatoi2$X3<-00
dfatoi2$X4<-do.call(paste, c(dfatoi2[c("X3","X1", "X2")], sep = ":"))
dfatoi2$X5<-chron(times=dfatoi2$X4,format = c("h:m:s"),out.format=c("h:m:s"))
statspl05061415$ATOI2<-dfatoi2$X5
rm(dfatoi2)

statspl05061415$pos2<-"F"
statspl05061415$pos2[statspl05061415$pos=="D"]<-"D"
statspl05061415$pos2[statspl05061415$pos=="G"]<-"G"

statspl05061415ord<-statspl05061415[with(statspl05061415,order(-GP,-ATOI2)),] #ordered list, games played and av.time on ice as ordering factors

# dfreg<-ddply(subset(statspl05061415ord,statspl05061415ord$session=="Regular"),.(season,pos2,Tm2),summarize,
#              max=max(GAR.Off,na.rm=TRUE),
#              min=min(GAR.Off,na.rm=TRUE),
#              N=length(GAR.Off)) #summary of used players per team and season, regular season only
# nrow(subset(dfreg,pos2=="F"&N<12))
# nrow(subset(dfreg,pos2=="D"&N<6))
# dfpo<-ddply(subset(statspl05061415ord,statspl05061415ord$session=="Playoffs"),.(season,pos2,Tm2),summarize,
#              max=max(GAR.Off,na.rm=TRUE),
#              min=min(GAR.Off,na.rm=TRUE),
#              N=length(GAR.Off)) #summary of used players per team and season, playoffs only
# nrow(subset(dfpo,pos2=="F"&N<12))
# nrow(subset(dfpo,pos2=="D"&N<6))

dffreg<-ddply(subset(statspl05061415ord,statspl05061415ord$session=="Regular"&statspl05061415ord$pos2=="F"),.(season,Tm2,pos2),summarize,
              pos=pos,
              GP=GP,
              ATOI2=ATOI2,
              n=player,
              GAR.Off=GAR.Off,
              GAR.Def=GAR.Def,
              GAR=GAR) #Forwards, Regular Season
dffreg<-split(dffreg,list(dffreg$Tm2,dffreg$season))
dffreg<-lapply(dffreg, function(l) l[1:12,])#12 most-used (GP, ATOI2) F's
# dffreg<-ldply(dffreg, data.frame)
dffreg <- do.call(rbind, dffreg)
# identical(dffreg3,dffreg)
dfdreg<-ddply(subset(statspl05061415ord,statspl05061415ord$session=="Regular"&statspl05061415ord$pos2=="D"),.(season,Tm2,pos2),summarize,
              pos=pos,
              GP=GP,
              ATOI2=ATOI2,
              n=player,
              GAR.Off=GAR.Off,
              GAR.Def=GAR.Def,
              GAR=GAR) #Defensemen, Regular Season
dfdreg<-split(dfdreg,list(dfdreg$Tm2,dfdreg$season))
dfdreg<-lapply(dfdreg, function(l) l[1:6,])#6 most-used (GP, ATOI2) D's per team
# dfdreg<-ldply(dfdreg, data.frame)
dfdreg <- do.call(rbind, dfdreg)
# identical(dfdreg3,dfdreg)
dffpo<-ddply(subset(statspl05061415ord,statspl05061415ord$session=="Playoffs"&statspl05061415ord$pos2=="F"),.(season,Tm2,pos2),summarize,
              pos=pos,
              GP=GP,
              ATOI2=ATOI2,
              n=player,
              GAR.Off=GAR.Off,
              GAR.Def=GAR.Def,
              GAR=GAR) #Forwards, Regular Season
dffpo<-split(dffpo,list(dffpo$Tm2,dffpo$season))
dffpo<-lapply(dffpo, function(l) l[1:12,])#12 most-used (GP, ATOI2) F's
# dffpo<-ldply(dffpo, data.frame)
dffpo <- do.call(rbind, dffpo)
# identical(dffpo3,dffpo)
dfdpo<-ddply(subset(statspl05061415ord,statspl05061415ord$session=="Playoffs"&statspl05061415ord$pos2=="D"),.(season,Tm2,pos2),summarize,
              pos=pos,
              GP=GP,
              ATOI2=ATOI2,
              n=player,
              GAR.Off=GAR.Off,
              GAR.Def=GAR.Def,
              GAR=GAR) #Defensemen, Playoffs
dfdpo<-split(dfdpo,list(dfdpo$Tm2,dfdpo$season))
dfdpo<-lapply(dfdpo, function(l) l[1:6,])#6 most-used (GP, ATOI2) D's per team
# dfdpo<-ldply(dfdpo, data.frame)
dfdpo <- do.call(rbind, dfdpo)
# identical(dfdpo3,dfdpo)

dfdregmax<-ddply(subset(statspl05061415,statspl05061415$pos2=="D"&statspl05061415$session=="Regular"),.(season),summarize,
                 maxGAR.Off=max(GAR.Off,na.rm=TRUE),
                 maxGAR.Def=max(GAR.Def,na.rm=TRUE),
                 maxGAR=max(GAR,na.rm=TRUE))
dffregmax<-ddply(subset(statspl05061415,statspl05061415$pos2=="F"&statspl05061415$session=="Regular"),.(season),summarize,
                 maxGAR.Off=max(GAR.Off,na.rm=TRUE),
                 maxGAR.Def=max(GAR.Def,na.rm=TRUE),
                 maxGAR=max(GAR,na.rm=TRUE))
dfdpomax<-ddply(subset(statspl05061415,statspl05061415$pos2=="D"&statspl05061415$session=="Playoffs"),.(season),summarize,
                 maxGAR.Off=max(GAR.Off,na.rm=TRUE),
                 maxGAR.Def=max(GAR.Def,na.rm=TRUE),
                 maxGAR=max(GAR,na.rm=TRUE))
dffpomax<-ddply(subset(statspl05061415,statspl05061415$pos2=="F"&statspl05061415$session=="Playoffs"),.(season),summarize,
                 maxGAR.Off=max(GAR.Off,na.rm=TRUE),
                 maxGAR.Def=max(GAR.Def,na.rm=TRUE),
                 maxGAR=max(GAR,na.rm=TRUE))

dfdreg<-merge(dfdreg,dfdregmax,by="season")
dfdreg$relgaroff<-dfdreg$GAR.Off/dfdreg$maxGAR.Off
dfdreg$relgardef<-dfdreg$GAR.Def/dfdreg$maxGAR.Def
dfdreg$relgar<-dfdreg$GAR/dfdreg$maxGAR
dfdpo<-merge(dfdpo,dfdpomax,by="season")
dfdpo$relgaroff<-dfdpo$GAR.Off/dfdpo$maxGAR.Off
dfdpo$relgardef<-dfdpo$GAR.Def/dfdpo$maxGAR.Def
dfdpo$relgar<-dfdpo$GAR/dfdpo$maxGAR
dffreg<-merge(dffreg,dffregmax,by="season")
dffreg$relgaroff<-dffreg$GAR.Off/dffreg$maxGAR.Off
dffreg$relgardef<-dffreg$GAR.Def/dffreg$maxGAR.Def
dffreg$relgar<-dffreg$GAR/dffreg$maxGAR
dffpo<-merge(dffpo,dffpomax,by="season")
dffpo$relgaroff<-dffpo$GAR.Off/dffpo$maxGAR.Off
dffpo$relgardef<-dffpo$GAR.Def/dffpo$maxGAR.Def
dffpo$relgar<-dffpo$GAR/dffpo$maxGAR

dfdreg$merge<-do.call(paste, c(dfdreg[c("Tm2","season")], sep = "."))
dfdpo$merge<-do.call(paste, c(dfdpo[c("Tm2","season")], sep = "."))
dffreg$merge<-do.call(paste, c(dffreg[c("Tm2","season")], sep = "."))
dffpo$merge<-do.call(paste, c(dffpo[c("Tm2","season")], sep = "."))
nhlstandings$merge<-do.call(paste, c(nhlstandings[c("Tm2","season")], sep = "."))
nhlstandingspo$merge<-do.call(paste, c(nhlstandingspo[c("Tm2","season")], sep = "."))

dfdreg<-ddply(dfdreg,.(season,Tm2),summarize,
              maxGAR.Off=max(GAR.Off,na.rm=TRUE),
              minGAR.Off=min(GAR.Off,na.rm=TRUE),
              maxGAR.Def=max(GAR.Def,na.rm=TRUE),
              minGAR.Def=min(GAR.Def,na.rm=TRUE),
              maxGAR=max(GAR,na.rm=TRUE),
              minGAR=min(GAR,na.rm=TRUE),
              maxrelgaroff=max(relgaroff,na.rm=TRUE),
              minrelgaroff=min(relgaroff,na.rm=TRUE),
              maxrelgardef=max(relgardef,na.rm=TRUE),
              minrelgardef=min(relgardef,na.rm=TRUE),
              maxrelgar=max(relgar,na.rm=TRUE),
              minrelgar=min(relgar,na.rm=TRUE),
              merge=head(merge,1))
dfdpo<-ddply(dfdpo,.(season,Tm2),summarize,
             maxGAR.Off=max(GAR.Off,na.rm=TRUE),
             minGAR.Off=min(GAR.Off,na.rm=TRUE),
             maxGAR.Def=max(GAR.Def,na.rm=TRUE),
             minGAR.Def=min(GAR.Def,na.rm=TRUE),
             maxGAR=max(GAR,na.rm=TRUE),
             minGAR=min(GAR,na.rm=TRUE),
             maxrelgaroff=max(relgaroff,na.rm=TRUE),
             minrelgaroff=min(relgaroff,na.rm=TRUE),
             maxrelgardef=max(relgardef,na.rm=TRUE),
             minrelgardef=min(relgardef,na.rm=TRUE),
             maxrelgar=max(relgar,na.rm=TRUE),
             minrelgar=min(relgar,na.rm=TRUE),
             merge=head(merge,1))
dffreg<-ddply(dffreg,.(season,Tm2),summarize,
              maxGAR.Off=max(GAR.Off,na.rm=TRUE),
              minGAR.Off=min(GAR.Off,na.rm=TRUE),
              maxGAR.Def=max(GAR.Def,na.rm=TRUE),
              minGAR.Def=min(GAR.Def,na.rm=TRUE),
              maxGAR=max(GAR,na.rm=TRUE),
              minGAR=min(GAR,na.rm=TRUE),
              maxrelgaroff=max(relgaroff,na.rm=TRUE),
              minrelgaroff=min(relgaroff,na.rm=TRUE),
              maxrelgardef=max(relgardef,na.rm=TRUE),
              minrelgardef=min(relgardef,na.rm=TRUE),
              maxrelgar=max(relgar,na.rm=TRUE),
              minrelgar=min(relgar,na.rm=TRUE),
              merge=head(merge,1))
dffpo<-ddply(dffpo,.(season,Tm2),summarize,
             maxGAR.Off=max(GAR.Off,na.rm=TRUE),
             minGAR.Off=min(GAR.Off,na.rm=TRUE),
             maxGAR.Def=max(GAR.Def,na.rm=TRUE),
             minGAR.Def=min(GAR.Def,na.rm=TRUE),
             maxGAR=max(GAR,na.rm=TRUE),
             minGAR=min(GAR,na.rm=TRUE),
             maxrelgaroff=max(relgaroff,na.rm=TRUE),
             minrelgaroff=min(relgaroff,na.rm=TRUE),
             maxrelgardef=max(relgardef,na.rm=TRUE),
             minrelgardef=min(relgardef,na.rm=TRUE),
             maxrelgar=max(relgar,na.rm=TRUE),
             minrelgar=min(relgar,na.rm=TRUE),
             merge=head(merge,1))

nhlstandings2<-merge(nhlstandings,dfdreg,by="merge")
names(nhlstandings2) <- gsub("max", "Dstrong", names(nhlstandings2))
names(nhlstandings2) <- gsub("min", "Dweak", names(nhlstandings2))
nhlstandings2$season.y<-NULL
nhlstandings2$Tm2.y<-NULL

nhlstandings2<-merge(nhlstandings2,dffreg,by="merge")
names(nhlstandings2) <- gsub("max", "Fstrong", names(nhlstandings2))
names(nhlstandings2) <- gsub("min", "Fweak", names(nhlstandings2))
nhlstandings2$season<-NULL
nhlstandings2$Tm2<-NULL

nhlstandingspo2<-merge(nhlstandingspo,dfdpo,by="merge")
names(nhlstandingspo2) <- gsub("max", "Dstrong", names(nhlstandingspo2))
names(nhlstandingspo2) <- gsub("min", "Dweak", names(nhlstandingspo2))
nhlstandingspo2$season.y<-NULL
nhlstandingspo2$Tm2.y<-NULL

nhlstandingspo2<-merge(nhlstandingspo2,dffpo,by="merge")
names(nhlstandingspo2) <- gsub("max", "Fstrong", names(nhlstandingspo2))
names(nhlstandingspo2) <- gsub("min", "Fweak", names(nhlstandingspo2))
nhlstandingspo2$season<-NULL
nhlstandingspo2$Tm2<-NULL

rm(dfdpomax,dfdregmax,dffregmax,dffpomax)
rm(dfdpo,dfdreg,dffpo,dffreg)




statsgo05061415ord<-statsgo05061415[with(statsgo05061415,order(-GP)),] #ordered list, games played and av.time on ice as ordering factors

# dfreg<-ddply(subset(statsgo05061415ord,statsgo05061415ord$session=="Regular"),.(season,Pos,Tm2),summarize,
#              max=max(GAR,na.rm=TRUE),
#              min=min(GAR,na.rm=TRUE),
#              N=length(GAR)) #summary of used goalies per team and season, regular season only
# nrow(subset(dfreg,N<2))

# dfpo<-ddply(subset(statsgo05061415ord,statsgo05061415ord$session=="Playoffs"),.(season,Pos,Tm2),summarize,
#              max=max(GAR),
#              min=min(GAR),#,na.rm=TRUE),
#              N=length(GAR)) #summary of used players per team and season, playoffs only
# nrow(subset(dfpo,N<2))
# nrow(subset(dfpo,pos2=="D"&N<6))

dfgreg<-ddply(subset(statsgo05061415ord,statsgo05061415ord$session=="Regular"&statsgo05061415ord$Pos=="G"),.(season,Tm2,Pos),summarize,
              Pos=Pos,
              GP=GP,
              n=player,
              GAR=GAR) #Forwards, Regular Season
dfgreg<-split(dfgreg,list(dfgreg$Tm2,dfgreg$season))
dfgreg<-lapply(dfgreg, function(l) l[1:2,])#12 most-used (GP, ATOI2) F's
# dfgreg<-ldply(dfgreg, data.frame)
dfgreg <- do.call(rbind, dfgreg)
# identical(dfgreg3,dfgreg)
dfgpo<-ddply(subset(statsgo05061415ord,statsgo05061415ord$session=="Playoffs"&statsgo05061415ord$Pos=="G"),.(season,Tm2,Pos),summarize,
              Pos=Pos,
              GP=GP,
              n=player,
              GAR=GAR) #Forwards, Regular Season
dfgpo<-split(dfgpo,list(dfgpo$Tm2,dfgpo$season))
dfgpo<-lapply(dfgpo, function(l) l[1:2,])#12 most-used (GP, ATOI2) F's
# dfgpo<-ldply(dfgpo, data.frame)
dfgpo <- do.call(rbind, dfgpo)
# identical(dfgpo3,dfgpo)

# dfg<-merge(dfgreg,dfgpo,by=c("season","n"))

dfgregmax<-ddply(subset(statsgo05061415,statsgo05061415$session=="Regular"),.(season),summarize,
                 maxGAR=max(GAR,na.rm=TRUE))
dfgpomax<-ddply(subset(statsgo05061415,statsgo05061415$session=="Playoffs"),.(season),summarize,
                 maxGAR=max(GAR,na.rm=TRUE))


dfgreg<-merge(dfgreg,dfgregmax,by="season")
# dfgreg$relgaroff<-dfgreg$GAR.Off/dfgreg$maxGAR.Off
# dfgreg$relgardef<-dfgreg$GAR.Def/dfgreg$maxGAR.Def
dfgreg$relgar<-dfgreg$GAR/dfgreg$maxGAR
dfgpo<-merge(dfgpo,dfgpomax,by="season")
# dfgpo$relgaroff<-dfgpo$GAR.Off/dfgpo$maxGAR.Off
# dfgpo$relgardef<-dfgpo$GAR.Def/dfgpo$maxGAR.Def
dfgpo$relgar<-dfgpo$GAR/dfgpo$maxGAR


dfgreg$merge<-do.call(paste, c(dfgreg[c("Tm2","season")], sep = "."))
dfgpo$merge<-do.call(paste, c(dfgpo[c("Tm2","season")], sep = "."))

dfgreg<-ddply(dfgreg,.(season,Tm2),summarize,
              # maxGAR.Off=max(GAR.Off,na.rm=TRUE),
              # minGAR.Off=min(GAR.Off,na.rm=TRUE),
              # maxGAR.Def=max(GAR.Def,na.rm=TRUE),
              # minGAR.Def=min(GAR.Def,na.rm=TRUE),
              maxGAR=max(GAR),#,na.rm=TRUE),
              minGAR=min(GAR),#,na.rm=TRUE),
              # maxrelgaroff=max(relgaroff,na.rm=TRUE),
              # minrelgaroff=min(relgaroff,na.rm=TRUE),
              # maxrelgardef=max(relgardef,na.rm=TRUE),
              # minrelgardef=min(relgardef,na.rm=TRUE),
              maxrelgar=max(relgar),#,na.rm=TRUE),
              minrelgar=min(relgar),#,na.rm=TRUE),
              merge=head(merge,1))
dfgpo<-ddply(dfgpo,.(season,Tm2),summarize,
             # maxGAR.Off=max(GAR.Off,na.rm=TRUE),
             # minGAR.Off=min(GAR.Off,na.rm=TRUE),
             # maxGAR.Def=max(GAR.Def,na.rm=TRUE),
             # minGAR.Def=min(GAR.Def,na.rm=TRUE),
             maxGAR=max(GAR),#,na.rm=TRUE),
             minGAR=min(GAR),#,na.rm=TRUE),
             # maxrelgaroff=max(relgaroff,na.rm=TRUE),
             # minrelgaroff=min(relgaroff,na.rm=TRUE),
             # maxrelgardef=max(relgardef,na.rm=TRUE),
             # minrelgardef=min(relgardef,na.rm=TRUE),
             maxrelgar=max(relgar),#,na.rm=TRUE),
             minrelgar=min(relgar),#,na.rm=TRUE),
             merge=head(merge,1))



nhlstandings2<-merge(nhlstandings2,dfgreg,by="merge")
names(nhlstandings2) <- gsub("max", "Gstrong", names(nhlstandings2))
names(nhlstandings2) <- gsub("min", "Gweak", names(nhlstandings2))
nhlstandings2$season<-NULL
nhlstandings2$Tm2<-NULL
nhlstandings2<-rename(nhlstandings2,c("season.x"="season","Tm2.x"="Tm2"))


nhlstandingspo2<-merge(nhlstandingspo2,dfgpo,by="merge")
names(nhlstandingspo2) <- gsub("max", "Gstrong", names(nhlstandingspo2))
names(nhlstandingspo2) <- gsub("min", "Gweak", names(nhlstandingspo2))
nhlstandingspo2$season<-NULL
nhlstandingspo2$Tm2<-NULL
nhlstandingspo2<-rename(nhlstandingspo2,c("season.x"="season","Tm2.x"="Tm2"))

rm(dfgpo,dfgreg,dfgregmax,dfgpomax)

nhlstandings2[,"strongrelgaroff"]<-apply(nhlstandings2[,c("Dstrongrelgaroff","Fstrongrelgaroff")],1,max)
nhlstandings2[,"weakrelgaroff"]<-apply(nhlstandings2[,c("Dweakrelgaroff","Fweakrelgaroff")],1,min)
nhlstandings2[,"strongrelgardef"]<-apply(nhlstandings2[,c("Dstrongrelgardef","Fstrongrelgardef")],1,max)
nhlstandings2[,"weakrelgardef"]<-apply(nhlstandings2[,c("Dweakrelgardef","Fweakrelgardef")],1,min)
nhlstandings2[,"strongrelgar"]<-apply(nhlstandings2[,c("Dstrongrelgar","Fstrongrelgar","Gstrongrelgar")],1,max,na.rm=TRUE)
nhlstandings2[,"weakrelgar"]<-apply(nhlstandings2[,c("Dweakrelgar","Fweakrelgar","Gweakrelgar")],1,min,na.rm=TRUE)
nhlstandings2[,"strongGAR"]<-apply(nhlstandings2[,c("DstrongGAR","FstrongGAR","GstrongGAR")],1,max,na.rm=TRUE)
nhlstandings2[,"weakGAR"]<-apply(nhlstandings2[,c("DweakGAR","FweakGAR","GweakGAR")],1,min,na.rm=TRUE)

nhlstandingspo2[,"strongrelgaroff"]<-apply(nhlstandingspo2[,c("Dstrongrelgaroff","Fstrongrelgaroff")],1,max)
nhlstandingspo2[,"weakrelgaroff"]<-apply(nhlstandingspo2[,c("Dweakrelgaroff","Fweakrelgaroff")],1,min)
nhlstandingspo2[,"strongrelgardef"]<-apply(nhlstandingspo2[,c("Dstrongrelgardef","Fstrongrelgardef")],1,max)
nhlstandingspo2[,"weakrelgardef"]<-apply(nhlstandingspo2[,c("Dweakrelgardef","Fweakrelgardef")],1,min)
nhlstandingspo2[,"strongrelgar"]<-apply(nhlstandingspo2[,c("Dstrongrelgar","Fstrongrelgar","Gstrongrelgar")],1,max,na.rm=TRUE)
nhlstandingspo2[,"weakrelgar"]<-apply(nhlstandingspo2[,c("Dweakrelgar","Fweakrelgar","Gweakrelgar")],1,min,na.rm=TRUE)
nhlstandingspo2[,"strongGAR"]<-apply(nhlstandingspo2[,c("DstrongGAR","FstrongGAR","GstrongGAR")],1,max,na.rm=TRUE)
nhlstandingspo2[,"weakGAR"]<-apply(nhlstandingspo2[,c("DweakGAR","FweakGAR","GweakGAR")],1,min,na.rm=TRUE)


nhlstandings2$GARdiff<-nhlstandings2$strongGAR-nhlstandings2$weakGAR
nhlstandingspo2$GARdiff<-nhlstandingspo2$strongGAR-nhlstandingspo2$weakGAR

# GINIGINIGINI ------------------------------------------------------------


# Tm1011<-factor(nhlstats1011$Tm)
# g1011<-c(tapply(nhlstats1011$G_GP,Tm1011,ineq),tapply(nhlstats1011$A_GP,Tm1011,ineq),tapply(nhlstats1011$PTS_GP,Tm1011,ineq),
#          tapply(nhlstats1011$G,Tm1011,ineq),tapply(nhlstats1011$A,Tm1011,ineq),tapply(nhlstats1011$PTS,Tm1011,ineq))
# Tm1112<-factor(nhlstats1112$Tm)
# g1112<-c(tapply(nhlstats1112$G_GP,Tm1112,ineq),tapply(nhlstats1112$A_GP,Tm1112,ineq),tapply(nhlstats1112$PTS_GP,Tm1112,ineq),
#          tapply(nhlstats1112$G,Tm1112,ineq),tapply(nhlstats1112$A,Tm1112,ineq),tapply(nhlstats1112$PTS,Tm1112,ineq))
# Tm1213<-factor(nhlstats1213$Tm)
# g1213<-c(tapply(nhlstats1213$G_GP,Tm1213,ineq),tapply(nhlstats1213$A_GP,Tm1213,ineq),tapply(nhlstats1213$PTS_GP,Tm1213,ineq),
#          tapply(nhlstats1213$G,Tm1213,ineq),tapply(nhlstats1213$A,Tm1213,ineq),tapply(nhlstats1213$PTS,Tm1213,ineq))
# Tm1314<-factor(nhlstats1314$Tm)
# g1314<-c(tapply(nhlstats1314$G_GP,Tm1314,ineq),tapply(nhlstats1314$A_GP,Tm1314,ineq),tapply(nhlstats1314$PTS_GP,Tm1314,ineq),
#          tapply(nhlstats1314$G,Tm1314,ineq),tapply(nhlstats1314$A,Tm1314,ineq),tapply(nhlstats1314$PTS,Tm1314,ineq))
# Tm1415<-factor(nhlstats1415$Tm)
# g1415<-c(tapply(nhlstats1415$G_GP,Tm1415,ineq),tapply(nhlstats1415$A_GP,Tm1415,ineq),tapply(nhlstats1415$PTS_GP,Tm1415,ineq),
#          tapply(nhlstats1415$G,Tm1415,ineq),tapply(nhlstats1415$A,Tm1415,ineq),tapply(nhlstats1415$PTS,Tm1415,ineq))
# # Tm1415po<-factor(nhlstats1415po$Tm)
# # g1415po<-c(tapply(nhlstats1415po$G_GP,Tm1415po,ineq),tapply(nhlstats1415po$A_GP,Tm1415po,ineq),tapply(nhlstats1415po$PTS_GP,Tm1415po,ineq))
# Tm1516<-factor(nhlstats1516$Tm)
# g1516<-c(tapply(nhlstats1516$G_GP,Tm1516,ineq),tapply(nhlstats1516$A_GP,Tm1516,ineq),tapply(nhlstats1516$PTS_GP,Tm1516,ineq),
#          tapply(nhlstats1516$G,Tm1516,ineq),tapply(nhlstats1516$A,Tm1516,ineq),tapply(nhlstats1516$PTS,Tm1516,ineq))
# # Tm1516po<-factor(nhlstats1516po$Tm)
# # g1516po<-c(tapply(nhlstats1516po$G_GP,Tm1516po,ineq),tapply(nhlstats1516po$A_GP,Tm1516po,ineq),tapply(nhlstats1516po$PTS_GP,Tm1516po,ineq))
# Tm1617<-factor(nhlstats1617$Tm)
# g1617<-c(tapply(nhlstats1617$G_GP,Tm1617,ineq),tapply(nhlstats1617$A_GP,Tm1617,ineq),tapply(nhlstats1617$PTS_GP,Tm1617,ineq),
#          tapply(nhlstats1617$G,Tm1617,ineq),tapply(nhlstats1617$A,Tm1617,ineq),tapply(nhlstats1617$PTS,Tm1617,ineq))
# #Tm1617po<-factor(nhlstats1617po$Tm)
# #g1617po<-tapply(nhlstats1617po$G_GP,Tm1617po,ineq)
# 
# 
# 
# 
# dim(g1011)<-c(30,6)
# dim(g1112)<-c(30,6)
# dim(g1213)<-c(30,6)
# dim(g1314)<-c(30,6)
# dim(g1415)<-c(30,6)
# # dim(g1415po)<-c(16,3)
# dim(g1516)<-c(30,6)
# # dim(g1516po)<-c(16,3)
# dim(g1617)<-c(30,6)
# 
# g1011<-as.data.frame(g1011)
# g1112<-as.data.frame(g1112)
# g1213<-as.data.frame(g1213)
# g1314<-as.data.frame(g1314)
# g1415<-as.data.frame(g1415)
# # g1415po<-as.data.frame(g1415po)
# g1516<-as.data.frame(g1516)
# # g1516po<-as.data.frame(g1516po)
# g1617<-as.data.frame(g1617)
# 
# g1011<-cbind(g1011,levels(Tm1011))
# g1112<-cbind(g1112,levels(Tm1112))
# g1213<-cbind(g1213,levels(Tm1213))
# g1314<-cbind(g1314,levels(Tm1314))
# g1415<-cbind(g1415,levels(Tm1415))
# # g1415po<-cbind(g1415po,levels(Tm1415po))
# g1516<-cbind(g1516,levels(Tm1516))
# # g1516po<-cbind(g1516po,levels(Tm1516po))
# g1617<-cbind(g1617,levels(Tm1617))
# 
# 
# colnames(g1011)<-c("gG_GP","gA_GP","gPTS_GP","gG","gA","gPTS","Tm")
# colnames(g1112)<-c("gG_GP","gA_GP","gPTS_GP","gG","gA","gPTS","Tm")
# colnames(g1213)<-c("gG_GP","gA_GP","gPTS_GP","gG","gA","gPTS","Tm")
# colnames(g1314)<-c("gG_GP","gA_GP","gPTS_GP","gG","gA","gPTS","Tm")
# colnames(g1415)<-c("gG_GP","gA_GP","gPTS_GP","gG","gA","gPTS","Tm")
# # colnames(g1415po)<-c("gG_GP","gA_GP","gPTS_GP","Tm")
# colnames(g1516)<-c("gG_GP","gA_GP","gPTS_GP","gG","gA","gPTS","Tm")
# # colnames(g1516po)<-c("gG_GP","gA_GP","gPTS_GP","Tm")
# colnames(g1617)<-c("gG_GP","gA_GP","gPTS_GP","gG","gA","gPTS","Tm")
# 
# 
# 
# nhlstandings1011<-merge(nhlstandings1011,g1011,by="Tm",all=TRUE)
# nhlstandings1112<-merge(nhlstandings1112,g1112,by="Tm",all=TRUE)
# nhlstandings1213<-merge(nhlstandings1213,g1213,by="Tm",all=TRUE)
# nhlstandings1314<-merge(nhlstandings1314,g1314,by="Tm",all=TRUE)
# nhlstandings1415<-merge(nhlstandings1415,g1415,by="Tm",all=TRUE)
# nhlstandings1516<-merge(nhlstandings1516,g1516,by="Tm",all=TRUE)
# nhlstandings1617<-merge(nhlstandings1617,g1617,by="Tm",all=TRUE)

ginis<-ddply(subset(statspl05061415,session=="Regular"),.(Tm2, season), summarise,
             gG=ineq(G),
             gA=ineq(A),
             gPTS=ineq(PTS),
             gG_GP=ineq(G_GP),
             gA_GP=ineq(A_GP),
             gPTS_GP=ineq(PTS_GP))
ginispo<-ddply(subset(statspl05061415,session=="Playoffs"),.(Tm2, season), summarise,
               gG=ineq(G),
               gA=ineq(A),
               gPTS=ineq(PTS),
               gG_GP=ineq(G_GP),
               gA_GP=ineq(A_GP),
               gPTS_GP=ineq(PTS_GP))

nhlstandings3<-merge(nhlstandings2,ginis,by=c("Tm2","season"))
nhlstandingspo3<-merge(nhlstandingspo2,ginispo,by=c("Tm2","season"))




  
  

# STRONG/WEAK LINK PLOTS --------------------------------------------------



#is there clustering of strong links/weak links?

# ggplot(nhlstandings2) + 
#   geom_jitter(aes(strongrelgar,weakrelgar),shape=1) + geom_smooth(aes(strongrelgar,weakrelgar), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Strong Link rel. GAR-Score", y = "Weak Link rel. GAR-Score")
ggplot(nhlstandings2) + 
  geom_jitter(aes(strongGAR,weakGAR),shape=1) + geom_smooth(aes(strongGAR,weakGAR), method=lm, se=FALSE,colour="orange") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
# ggplot(subset(nhlstandings2,season==20142015)) + 
#   geom_jitter(aes(strongGAR,weakGAR),shape=1) + geom_smooth(aes(strongGAR,weakGAR), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
ggplot(nhlstandings2) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(strongGAR,weakGAR, colour=as.character(season))) + geom_smooth(aes(strongGAR,weakGAR, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")


#positionwise clustering? any combinations (e.g. strong d/weak g)?
#Dstrong/Dweak
ggplot(nhlstandings2) + 
  geom_jitter(aes(DstrongGAR,DweakGAR),shape=1) + geom_smooth(aes(DstrongGAR,DweakGAR), method=lm, se=FALSE,colour="orange") +
  #labs(x = "Dstrong Link GAR-Score", y = "Dweak Link GAR-Score")
# ggplot(subset(nhlstandings2,season==20142015)) + 
#   geom_jitter(aes(DstrongGAR,DweakGAR),shape=1) + geom_smooth(aes(DstrongGAR,DweakGAR), method=lm, se=FALSE,colour="orange") +
#   #labs(x = "Dstrong Link GAR-Score", y = "Dweak Link GAR-Score")
ggplot(nhlstandings2) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(DstrongGAR,DweakGAR, colour=as.character(season))) + geom_smooth(aes(DstrongGAR,DweakGAR, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  #labs(x = "Dstrong Link GAR-Score", y = "Dweak Link GAR-Score")

#Fstrong/Fweak
ggplot(nhlstandings2) + 
  geom_jitter(aes(FstrongGAR,FweakGAR),shape=1) + geom_smooth(aes(FstrongGAR,FweakGAR), method=lm, se=FALSE,colour="orange") +
  #labs(x = "Fstrong Link GAR-Score", y = "Fweak Link GAR-Score")
# ggplot(subset(nhlstandings2,season==20142015)) + 
#   geom_jitter(aes(FstrongGAR,FweakGAR),shape=1) + geom_smooth(aes(FstrongGAR,FweakGAR), method=lm, se=FALSE,colour="orange") +
#   #labs(x = "Fstrong Link GAR-Score", y = "Fweak Link GAR-Score")
ggplot(nhlstandings2) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(FstrongGAR,FweakGAR, colour=as.character(season))) + geom_smooth(aes(FstrongGAR,FweakGAR, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  #labs(x = "Fstrong Link GAR-Score", y = "Fweak Link GAR-Score")

#Gstrong/Dstrong
ggplot(nhlstandings2) + 
  geom_jitter(aes(GstrongGAR,DstrongGAR),shape=1,na.rm=TRUE) + geom_smooth(aes(GstrongGAR,DstrongGAR), method=lm, se=FALSE,colour="orange",na.rm=TRUE) #+
  #labs(x = "Gstrong Link GAR-Score", y = "Dstrong Link GAR-Score")
ggplot(nhlstandings2) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(GstrongGAR,DstrongGAR, colour=as.character(season)),na.rm=TRUE) + geom_smooth(aes(GstrongGAR,DstrongGAR, colour=as.character(season)), method=lm, se=FALSE,na.rm=TRUE) +
  facet_wrap(~season) +#, scales="free_x") +
  #labs(x = "Gstrong Link GAR-Score", y = "Dstrong Link GAR-Score")
  
  



#DIFFG~weak/strong link

ggplot(nhlstandings2) + 
  geom_jitter(aes(strongGAR,DIFFG), colour="red",shape=1) + geom_smooth(aes(strongGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Weak Link/Strong Link GAR-Score", y = "Goal Differential per Game")
# ggplot(subset(nhlstandings2,season>20122013)) + 
#   geom_jitter(aes(strongGAR,DIFFG), colour="red",shape=1) + geom_smooth(aes(strongGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
#   geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Weak Link/Strong Link GAR-Score", y = "Goal Differential per Game")
ggplot(nhlstandings2) +
  geom_jitter(aes(strongGAR,DIFFG, colour=as.character(season))) + geom_smooth(aes(strongGAR,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  geom_jitter(aes(weakGAR,DIFFG, colour=as.character(season))) + geom_smooth(aes(weakGAR,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")


#DIFFG~GstrongGAR

ggplot(nhlstandings2) + 
  geom_jitter(aes(GstrongGAR,DIFFG), colour="red",shape=1,na.rm=TRUE) + geom_smooth(aes(GstrongGAR,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  #geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gstrong GAR-Score", y = "Goal Differential per Game")
# ggplot(subset(nhlstandings2,season>20122013)) + 
#   geom_jitter(aes(GstrongGAR,DIFFG), colour="red",shape=1) + geom_smooth(aes(GstrongGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
#   geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Weak Link/Strong Link GAR-Score", y = "Goal Differential per Game")
ggplot(nhlstandings2) +
  geom_jitter(aes(GstrongGAR,DIFFG, colour=as.character(season)),na.rm=TRUE) + geom_smooth(aes(GstrongGAR,DIFFG, colour=as.character(season)), method=lm, se=FALSE,na.rm=TRUE) +
  # geom_jitter(aes(weakGAR,DIFFG, colour=as.character(season))) + geom_smooth(aes(weakGAR,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "GStrong Link GAR-Score", y = "Goal Diff per game, avg")


#PTSG~weak/strong link

ggplot(nhlstandings2) + 
  geom_jitter(aes(strongGAR,PTSG), colour="red",shape=1) + geom_smooth(aes(strongGAR,PTSG), method=lm, se=FALSE,colour="orange") +
  geom_jitter(aes(weakGAR,PTSG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,PTSG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Weak Link/Strong Link GAR-Score", y = "Goal Differential per Game")
# ggplot(subset(nhlstandings2,season>20122013)) + 
#   geom_jitter(aes(strongGAR,PTSG), colour="red",shape=1) + geom_smooth(aes(strongGAR,PTSG), method=lm, se=FALSE,colour="orange") +
#   geom_jitter(aes(weakGAR,PTSG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,PTSG), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Weak Link/Strong Link GAR-Score", y = "Goal Differential per Game")
ggplot(nhlstandings2) +
  geom_jitter(aes(strongGAR,PTSG, colour=as.character(season))) + geom_smooth(aes(strongGAR,PTSG, colour=as.character(season)), method=lm, se=FALSE) +
  geom_jitter(aes(weakGAR,PTSG, colour=as.character(season))) + geom_smooth(aes(weakGAR,PTSG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")



#LINEAR REGRESSIONS, DIFFG~,PTSG~

lmlink1<-lm(DIFFG~weakGAR+strongGAR,data=nhlstandings2)
summary(lmlink1)
lmlink1sub<-lm(DIFFG~weakGAR+strongGAR,data=nhlstandings2,subset = (season==20142015))
summary(lmlink1sub)

lmlink2<-lm(PTSG~weakGAR+strongGAR,data=nhlstandings2)
summary(lmlink2)
lmlink2sub<-lm(PTSG~weakGAR+strongGAR,data=nhlstandings2,subset = (season==20142015))
summary(lmlink2sub)





# GINI PLOTS --------------------------------------------------------------



ggplot(nhlstandings3) + 
  geom_jitter(aes(gG,DIFFG), colour="red",shape=1,na.rm=TRUE) + geom_smooth(aes(gG,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  #geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")
ggplot(nhlstandings3) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(gG,DIFFG, colour=as.character(season))) + geom_smooth(aes(gG,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")


ggplot(nhlstandings3) + 
  geom_jitter(aes(gA,DIFFG), colour="red",shape=1,na.rm=TRUE) + geom_smooth(aes(gA,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  #geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")
ggplot(nhlstandings3) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(gA,DIFFG, colour=as.character(season))) + geom_smooth(aes(gA,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")

ggplot(nhlstandings3) + 
  geom_jitter(aes(gPTS,DIFFG), colour="red",shape=1,na.rm=TRUE) + geom_smooth(aes(gPTS,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  #geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")
ggplot(nhlstandings3) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(gPTS,DIFFG, colour=as.character(season))) + geom_smooth(aes(gPTS,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")

ggplot(nhlstandings3) + 
  geom_jitter(aes(gG_GP,DIFFG), colour="red",shape=1,na.rm=TRUE) + geom_smooth(aes(gG_GP,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  #geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")
ggplot(nhlstandings3) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(gG_GP,DIFFG, colour=as.character(season))) + geom_smooth(aes(gG_GP,DIFFG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Gini Goals", y = "Goal Diff. per Game")


lmgG<-lm(DIFFG~gG,data=nhlstandings3)
summary(lmgG)

lmgGGP<-lm(DIFFG~gG_GP,data=nhlstandings3)
summary(lmgGGP)



# m_gini<-melt(ddply(nhlstats,.(yr,Tm2), summarise,
#                    gG=ineq(G)),id=c("yr","Tm2"))
# dcast(m_gini,yr~variable,mean)
# dcast(m_gini,yr~Tm2)
# ggplot(m_gini, aes(yr,value)) + geom_line(aes(colour = Tm2))
# ggplot(subset(nhlstandings,Tm2%in%c("BOS","LAK","CHI","PIT")),aes(yr,AvAge))+geom_line(aes(linetype=Tm2),size=1)#Champions
# ggplot(subset(nhlstandings,Tm2%in%c("VAN","CHI","BOS","NYR","WSH")),aes(yr,AvAge))+geom_line(aes(linetype=Tm2),size=1)#President Trophy
# ddply(nhlstandings,.(yr), summarise,mPDO=median(PDO))
# ddply(nhlstandings,.(yr), summarise,mAvAge=mean(AvAge))
# 








# PTS~DIFF (plots and regressions over seasons) ---------------------------

ggplot(nhlstandings2) + 
  geom_jitter(aes(DIFFG,PTSG), colour="red",shape=1,na.rm=TRUE) + geom_smooth(aes(DIFFG,PTSG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  #geom_jitter(aes(weakGAR,DIFFG), colour="blue",shape=1) + geom_smooth(aes(weakGAR,DIFFG), method=lm, se=FALSE,colour="orange") +
  labs(x = "Goal Diff. per Game", y = "Points per Game")


ggplot(nhlstandings2) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(DIFFG,PTSG, colour=as.character(season))) + geom_smooth(aes(DIFFG,PTSG, colour=as.character(season)), method=lm, se=FALSE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Average Goal Differential per Game", y = "Average Points per Game")


# with(nhlstandings[nhlstandings$yr==1617,],{
#   lm1617<-lm(PTS~DIFF)
#   summary(lm1617)
#   #plot(DIFF,PTS)
#   #abline(coef(lm1617))
# })

nhlstandings2$GT<-nhlstandings2$GF+nhlstandings2$GA
nhlstandings2$`W%`<-nhlstandings2$W/nhlstandings2$GP



lm<-lm(PTSG~DIFFG,data=nhlstandings2)
summary(lm)
lm<-lm(`PTS%`~DIFFG,data=nhlstandings2)
summary(lm)
lm<-lm(`W%`~DIFFG,data=nhlstandings2)
summary(lm)

lm<-lm(log(PTSG)~log(GF)+log(GA),data=nhlstandings2)
summary(lm)
lm<-lm(log(`PTS%`)~log(GF)+log(GT),data=nhlstandings2)
summary(lm)
lm<-lm(log(`W%`)~log(GF)+log(GT),data=nhlstandings2)
summary(lm)
lm<-lm(log(`W%`)~log(GA)+log(GT),data=nhlstandings2)
summary(lm)


# DIFF~GF, DIFF~GA --------------------------------------------------------

plot(nhlstandings$GF,nhlstandings$DIFF)

plot(nhlstandings$GA,nhlstandings$DIFF)

lmgf<-lm(nhlstandings$DIFF~nhlstandings$d1213+nhlstandings$GF)
summary(lmgf)
plot(nhlstandings$GF,nhlstandings$DIFF)
abline(a=coef(lmgf)[1]+1*coef(lmgf)[2],b=coef(lmgf)[3])
abline(a=coef(lmgf)[1]+0*coef(lmgf)[2],b=coef(lmgf)[3])


lmga<-lm(nhlstandings$DIFF~nhlstandings$d1213+nhlstandings$GA)
summary(lmga)
plot(nhlstandings$GA,nhlstandings$DIFF)
abline(a=coef(lmga)[1]+1*coef(lmga)[2],b=coef(lmga)[3])
abline(a=coef(lmga)[1]+0*coef(lmga)[2],b=coef(lmga)[3])



# PTS~GF, PTS~GA ----------------------------------------------------------
plot(nhlstandings$GF,nhlstandings$PTS)

plot(nhlstandings$GA,nhlstandings$PTS)

lmgf2<-lm(nhlstandings$PTS~nhlstandings$d1213+nhlstandings$GF)
summary(lmgf2)
plot(nhlstandings$GF,nhlstandings$PTS)
abline(a=coef(lmgf2)[1]+1*coef(lmgf2)[2],b=coef(lmgf2)[3])
abline(a=coef(lmgf2)[1]+0*coef(lmgf2)[2],b=coef(lmgf2)[3])

with(nhlstandings[nhlstandings$yr!=1213,],{
  lm<-lm(PTS~GF)
  summary(lm)
  #plot(DIFF,PTS)
  #abline(coef(lm1516))
})

lmga2<-lm(nhlstandings$PTS~nhlstandings$d1213+nhlstandings$GA)
summary(lmga2)
plot(nhlstandings$GA,nhlstandings$PTS)
abline(a=coef(lmga2)[1]+1*coef(lmga2)[2],b=coef(lmga2)[3])
abline(a=coef(lmga2)[1]+0*coef(lmga2)[2],b=coef(lmga2)[3])

with(nhlstandings[nhlstandings$yr!=1213,],{
  lm<-lm(PTS~GA)
  summary(lm)
  #plot(DIFF,PTS)
  #abline(coef(lm1516))
})


# DESCRIPTIVE STATISTICS

cor(nhlstandings[c(11,5,33:39)])



plot(nhlstandings$DIFF,nhlstandings$PTS)
plot(nhlstandings$PDO,nhlstandings$`PTS%`)
plot(nhlstandings$AvAge,nhlstandings$`PTS%`)

plot(nhlstandings$gG,nhlstandings$PDO)
# plot(nhlstandings$gG_GP,nhlstandings$PDO)


plot(nhlstandings$gG,nhlstandings$`PTS%`)
abline(a=coef(lm2)[1]+100*coef(lm2)[2],b=coef(lm2)[3])
abline(a=coef(lm2)[1]+98*coef(lm2)[2],b=coef(lm2)[3])
abline(a=coef(lm2)[1]+102*coef(lm2)[2],b=coef(lm2)[3])

# plot(nhlstandings$gA,nhlstandings$`PTS%`)
# plot(nhlstandings$gPTS,nhlstandings$`PTS%`)
plot(nhlstandings$gG_GP,nhlstandings$`PTS%`)
# plot(nhlstandings$gA_GP,nhlstandings$`PTS%`)
# plot(nhlstandings$gPTS_GP,nhlstandings$`PTS%`)



lm1<-lm(nhlstandings$'PTS%'~nhlstandings$PDO+nhlstandings$gG_GP)
summary(lm1)

lm2<-lm(nhlstandings$'PTS%'~nhlstandings$PDO+nhlstandings$gG)
summary(lm2)

lm3<-lm(nhlstandings$'PTS%'~nhlstandings$PDO+nhlstandings$gG+nhlstandings$AvAge)
summary(lm3)

lm<-lm(nhlstandings$`PTS%`~nhlstandings$DIFF)
summary(lm)





plot(nhlstandings1415$gG_GP,nhlstandings1415$PTS,pch=1)
points(nhlstandings1516$gG_GP,nhlstandings1516$PTS,pch=2)
points(nhlstandings1617$gG_GP,nhlstandings1617$PTS,pch=3)

plot(nhlstandings1415$gA_GP,nhlstandings1415$PTS,pch=1)
points(nhlstandings1516$gA_GP,nhlstandings1516$PTS,pch=2)
points(nhlstandings1617$gA_GP,nhlstandings1617$PTS,pch=3)

plot(nhlstandings1415$gPTS_GP,nhlstandings1415$PTS,pch=1)
points(nhlstandings1516$gPTS_GP,nhlstandings1516$PTS,pch=2)
points(nhlstandings1617$gPTS_GP,nhlstandings1617$PTS,pch=3)


# ONE SAMPLE





1415



# PDO ---------------------------------------------------------------------
nhlstandings2$PDO<-as.double(nhlstandings2$PDO)

ggplot(subset(nhlstandings2,Tm2 %in% c("ANA","BOS","NYI","PIT"))) + 
  geom_line(aes(season,PDO, colour=Tm2),na.rm=TRUE) 

ggplot(nhlstandings2) + 
  geom_jitter(aes(PDO,DIFFG),shape=1,na.rm=TRUE) + geom_smooth(aes(PDO,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
# ggplot(subset(nhlstandings2,season==20142015)) + 
#   geom_jitter(aes(strongGAR,weakGAR),shape=1) + geom_smooth(aes(strongGAR,weakGAR), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
ggplot(nhlstandings2) +  #YEARBYYEAR COMPARISON!
  geom_jitter(aes(PDO,DIFFG, colour=as.character(season)),na.rm=TRUE) + geom_smooth(aes(PDO,DIFFG, colour=as.character(season)), method=lm, se=FALSE,na.rm=TRUE) +
  facet_wrap(~season) +#, scales="free_x") +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")

ggplot(nhlstandings3) + 
  geom_jitter(aes(gG,DIFFG),shape=1,na.rm=TRUE) + geom_smooth(aes(gG,DIFFG), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
# ggplot(subset(nhlstandings2,season==20142015)) + 
#   geom_jitter(aes(strongGAR,weakGAR),shape=1) + geom_smooth(aes(strongGAR,weakGAR), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")

ggplot(nhlstandings3) + 
  geom_jitter(aes(gG,PDO),shape=1,na.rm=TRUE) + geom_smooth(aes(gG,PDO), method=lm, se=FALSE,colour="orange",na.rm=TRUE) +
  labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")
# ggplot(subset(nhlstandings2,season==20142015)) + 
#   geom_jitter(aes(strongGAR,weakGAR),shape=1) + geom_smooth(aes(strongGAR,weakGAR), method=lm, se=FALSE,colour="orange") +
#   labs(x = "Strong Link GAR-Score", y = "Weak Link GAR-Score")


