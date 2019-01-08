

#Scorers - True Scoring List based on marginal Probs values



# Fix error in Scoring List!!!! CHECK!
# Traditional Scoring List! CHECK!
# True Scoring List with marg. Probabilities:



library("biglm", lib.loc="~/R/win-library/3.1")
library("bitops", lib.loc="~/R/win-library/3.1")
library("nhlscrapr", lib.loc="~/R/win-library/3.1")
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

setwd("C:/Users/morris/Desktop/RR")

load("C:/Users/morris/Desktop/RR/Season1314.RData")

#Var treatment (renaming, etc.)
grand.data=rename(grand.data,c("ev.team"="eteam","ev.player.1"="eplayer1",
                               "ev.player.2"="eplayer2","ev.player.3"="eplayer3",
                               "home.score"="homescore","away.score"="awayscore",
                               "event.length"="elength","home.skaters"="homeskaters",
                               "away.skaters"="awayskaters","adjusted.distance"="adjdist"))

grand.data$gcode<-as.numeric(grand.data$gcode)

# grand.data$session=ifelse(grand.data$gcode<=1230,"Regular","Playoff")
# table(grand.data$session)


#Regular Season Scoring List based on grand.data

# Erase unneeded rows
# table(grand.data$etype)
# ind=which(with(grand.data,etype!="GOAL"))
# grand.data=grand.data[-ind,]



grand.data$awayscore2=ifelse(grand.data$eteam==grand.data$awayteam&grand.data$etype=="GOAL"&
                               grand.data$gcode<=1230&grand.data$period<5,
                             grand.data$awayscore+1,
                             ifelse(grand.data$eteam==grand.data$awayteam&grand.data$etype=="GOAL"&
                                      grand.data$gcode>1230,
                                    grand.data$awayscore+1,NA))
grand.data$homescore2=ifelse(grand.data$eteam==grand.data$hometeam&grand.data$etype=="GOAL"&
                               grand.data$gcode<=1230&grand.data$period<5,
                             grand.data$homescore+1,
                             ifelse(grand.data$eteam==grand.data$hometeam&grand.data$etype=="GOAL"&
                                      grand.data$gcode>1230,
                                    grand.data$homescore+1,NA))




# Goals Regular and Playoffs ----------------------------------------------


grand.data$greg=ifelse(grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5,
                       grand.data$eplayer1,NA)
grand.data$ass1reg=ifelse(grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5,
                          grand.data$eplayer2,NA)
grand.data$ass2reg=ifelse(grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5,
                          grand.data$eplayer3,NA)

grand.data$gplay=ifelse(grand.data$etype=="GOAL"&grand.data$gcode>1230,grand.data$eplayer1,NA)
grand.data$ass1play=ifelse(grand.data$etype=="GOAL"&grand.data$gcode>1230,grand.data$eplayer2,NA)
grand.data$ass2play=ifelse(grand.data$etype=="GOAL"&grand.data$gcode>1230,grand.data$eplayer3,NA)

greg=as.data.frame(table(grand.data$greg,useNA=NULL))
greg=rename(greg,c("Var1"="player.id","Freq"="greg"))
ass1reg=as.data.frame(table(grand.data$ass1reg,useNA=NULL))
ass1reg=rename(ass1reg,c("Var1"="player.id","Freq"="ass1reg"))
ass2reg=as.data.frame(table(grand.data$ass2reg,useNA=NULL))
ass2reg=rename(ass2reg,c("Var1"="player.id","Freq"="ass2reg"))
reg=merge(greg,ass1reg,by="player.id",all=TRUE)
reg=merge(reg,ass2reg,by="player.id",all=TRUE)
reg[is.na(reg)]<-0
# reg=reg[with(reg, order(player.id)), ]
rm(ass1reg,ass2reg,greg)

gplay=as.data.frame(table(grand.data$gplay, useNA=NULL))
gplay=rename(gplay,c("Var1"="player.id","Freq"="gplay"))
ass1play=as.data.frame(table(grand.data$ass1play, useNA=NULL))
ass1play=rename(ass1play,c("Var1"="player.id","Freq"="ass1play"))
ass2play=as.data.frame(table(grand.data$ass2play, useNA=NULL))
ass2play=rename(ass2play,c("Var1"="player.id","Freq"="ass2play"))
play=merge(gplay,ass1play,by="player.id",all=TRUE)
play=merge(play,ass2play,by="player.id",all=TRUE)
play[is.na(play)]<-0
# play=play[with(play, order(player.id)), ]
rm(ass1play,ass2play,gplay)

roster.master2=merge(roster.master,reg,by="player.id", all=TRUE)
roster.master2=merge(roster.master2,play,by="player.id", all=TRUE)
roster.master2[is.na(roster.master2)]<-0
rm(reg,play)



head(roster.master2[with(roster.master2,order(-greg)),],n=20)
head(roster.master2[with(roster.master2,order(-gplay)),],n=20)





# Goals with marginal Probabilities ---------------------------------------

  
# POINTS & LOG REGRESSION
games$awayscore2=as.numeric(games$awayscore)
games$homescore2=as.numeric(games$homescore)

games$awaypoints=0
games$homepoints=0
games$awaypoints[games$session=="Regular" & games$awayscore2>games$homescore2]=2
games$awaypoints[games$session=="Regular" & games$periods>3 & games$awayscore2<games$homescore2]=1
games$homepoints[games$session=="Regular" & games$awayscore2<games$homescore2]=2
games$homepoints[games$session=="Regular" & games$periods>3 & games$awayscore2>games$homescore2]=1

games$awaypoints[games$session=="Playoffs" & games$awayscore2>games$homescore2]=2
games$homepoints[games$session=="Playoffs" & games$awayscore2<games$homescore2]=2

games$awaypoints=factor(games$awaypoints,ordered=TRUE)
games$homepoints=factor(games$homepoints,ordered=TRUE)


p_sfor.away.logit=polr(awaypoints ~ awayscore2, data=games, method="logistic")

p_sfor.home.logit=polr(homepoints ~ homescore2, data=games, method="logistic")


# DESCRIPTIVE TABLES (0/1,1/2,2/3,....approx integral)
p_s.descr=data.frame(cbind(awayscore2 = 0:10,homescore2=0:10))
p_s.descr=cbind(p_s.descr,predict(p_sfor.away.logit,p_s.descr,type="probs"),
                predict(p_sfor.home.logit,p_s.descr,type="probs"))
p_s.descr=p_s.descr[-c(3,4,6,7)]
p_s.descr=rename(p_s.descr,c("2"="P2away","2.1"="P2home"))

p_s.descr$dP2away=0
p_s.descr$dP2away[p_s.descr$awayscore2==0]=p_s.descr$P2away
p_s.descr$dP2away[p_s.descr$awayscore2>0]=diff(p_s.descr$P2away)

p_s.descr$dP2home=0
p_s.descr$dP2home[p_s.descr$homescore2==0]=p_s.descr$P2home
p_s.descr$dP2home[p_s.descr$homescore2>0]=diff(p_s.descr$P2home)

p_s.away=p_s.descr[c(1,5)]
p_s.home=p_s.descr[c(2,6)]
rm(p_s.descr)

# grand.data2=merge(grand.data,p_s.away,by.x="awayscore2",by.y="awayscore",all=TRUE)
# grand.data2=merge(grand.data2,p_s.home,by.x="homescore2",by.y="homescore",all=TRUE)


# grand.data2$dP2away=predict(p_sfor.away.logit,grand.data2,type="probs")
# grand.data2$dP2home=

grand.data$away1=ifelse(grand.data$awayscore2==1,1,0)
grand.data$away2=ifelse(grand.data$awayscore2==2,1,0)
grand.data$away3=ifelse(grand.data$awayscore2==3,1,0)
grand.data$away4=ifelse(grand.data$awayscore2==4,1,0)
grand.data$away5=ifelse(grand.data$awayscore2==5,1,0)
grand.data$away6=ifelse(grand.data$awayscore2==6,1,0)
grand.data$away7=ifelse(grand.data$awayscore2==7,1,0)
grand.data$away8=ifelse(grand.data$awayscore2==8,1,0)
grand.data$away9=ifelse(grand.data$awayscore2==9,1,0)
grand.data$away10=ifelse(grand.data$awayscore2==10,1,0)

grand.data$home1=ifelse(grand.data$homescore2==1,1,0)
grand.data$home2=ifelse(grand.data$homescore2==2,1,0)
grand.data$home3=ifelse(grand.data$homescore2==3,1,0)
grand.data$home4=ifelse(grand.data$homescore2==4,1,0)
grand.data$home5=ifelse(grand.data$homescore2==5,1,0)
grand.data$home6=ifelse(grand.data$homescore2==6,1,0)
grand.data$home7=ifelse(grand.data$homescore2==7,1,0)
grand.data$home8=ifelse(grand.data$homescore2==8,1,0)
grand.data$home9=ifelse(grand.data$homescore2==9,1,0)
grand.data$home10=ifelse(grand.data$homescore2==10,1,0)


#Regular Season

greg=ddply(grand.data, .(greg), summarize,
            goals=length(greg),
            away1=sum(awayscore2==1,na.rm=TRUE),
            away2=sum(awayscore2==2,na.rm=TRUE),
            away3=sum(awayscore2==3,na.rm=TRUE),
            away4=sum(awayscore2==4,na.rm=TRUE),
            away5=sum(awayscore2==5,na.rm=TRUE),
            away6=sum(awayscore2==6,na.rm=TRUE),
            away7=sum(awayscore2==7,na.rm=TRUE),
            away8=sum(awayscore2==8,na.rm=TRUE),
            away9=sum(awayscore2==9,na.rm=TRUE),
            away10=sum(awayscore2==10,na.rm=TRUE),
            home1=sum(homescore2==1,na.rm=TRUE),
            home2=sum(homescore2==2,na.rm=TRUE),
            home3=sum(homescore2==3,na.rm=TRUE),
            home4=sum(homescore2==4,na.rm=TRUE),
            home5=sum(homescore2==5,na.rm=TRUE),
            home6=sum(homescore2==6,na.rm=TRUE),
            home7=sum(homescore2==7,na.rm=TRUE),
            home8=sum(homescore2==8,na.rm=TRUE),
            home9=sum(homescore2==9,na.rm=TRUE),
            home10=sum(homescore2==10,na.rm=TRUE))



greg$a1=greg$away1*p_s.away[2,2]
greg$a2=greg$away2*p_s.away[3,2]
greg$a3=greg$away3*p_s.away[4,2]
greg$a4=greg$away4*p_s.away[5,2]
greg$a5=greg$away5*p_s.away[6,2]
greg$a6=greg$away6*p_s.away[7,2]
greg$a7=greg$away7*p_s.away[8,2]
greg$a8=greg$away8*p_s.away[9,2]
greg$a9=greg$away9*p_s.away[10,2]
greg$a10=greg$away10*p_s.away[11,2]

greg$h1=greg$home1*p_s.home[2,2]
greg$h2=greg$home2*p_s.home[3,2]
greg$h3=greg$home3*p_s.home[4,2]
greg$h4=greg$home4*p_s.home[5,2]
greg$h5=greg$home5*p_s.home[6,2]
greg$h6=greg$home6*p_s.home[7,2]
greg$h7=greg$home7*p_s.home[8,2]
greg$h8=greg$home8*p_s.home[9,2]
greg$h9=greg$home9*p_s.home[10,2]
greg$h10=greg$home10*p_s.home[11,2]

greg$P2Preg=round(greg$a1+greg$a2+greg$a3+greg$a4+greg$a5+greg$a6+greg$a7+greg$a8+greg$a9+greg$a10+
                 greg$h1+greg$h2+greg$h3+greg$h4+greg$h5+greg$h6+greg$h7+greg$h8+greg$h9+greg$h10,4)

greg2=greg[c(1,43)]
greg2=rename(greg2,c("greg"="player.id"))

roster.master2=merge(roster.master2,greg2,by="player.id", all=TRUE)
roster.master2$P2Preg[is.na(roster.master2$P2Preg)]<-0




#Playoffs
gplay=ddply(grand.data, .(gplay), summarize,
            goals=length(gplay),
            away1=sum(awayscore2==1,na.rm=TRUE),
            away2=sum(awayscore2==2,na.rm=TRUE),
            away3=sum(awayscore2==3,na.rm=TRUE),
            away4=sum(awayscore2==4,na.rm=TRUE),
            away5=sum(awayscore2==5,na.rm=TRUE),
            away6=sum(awayscore2==6,na.rm=TRUE),
            away7=sum(awayscore2==7,na.rm=TRUE),
            away8=sum(awayscore2==8,na.rm=TRUE),
            away9=sum(awayscore2==9,na.rm=TRUE),
            away10=sum(awayscore2==10,na.rm=TRUE),
            home1=sum(homescore2==1,na.rm=TRUE),
            home2=sum(homescore2==2,na.rm=TRUE),
            home3=sum(homescore2==3,na.rm=TRUE),
            home4=sum(homescore2==4,na.rm=TRUE),
            home5=sum(homescore2==5,na.rm=TRUE),
            home6=sum(homescore2==6,na.rm=TRUE),
            home7=sum(homescore2==7,na.rm=TRUE),
            home8=sum(homescore2==8,na.rm=TRUE),
            home9=sum(homescore2==9,na.rm=TRUE),
            home10=sum(homescore2==10,na.rm=TRUE))

gplay$a1=gplay$away1*p_s.away[2,2]
gplay$a2=gplay$away2*p_s.away[3,2]
gplay$a3=gplay$away3*p_s.away[4,2]
gplay$a4=gplay$away4*p_s.away[5,2]
gplay$a5=gplay$away5*p_s.away[6,2]
gplay$a6=gplay$away6*p_s.away[7,2]
gplay$a7=gplay$away7*p_s.away[8,2]
gplay$a8=gplay$away8*p_s.away[9,2]
gplay$a9=gplay$away9*p_s.away[10,2]
gplay$a10=gplay$away10*p_s.away[11,2]

gplay$h1=gplay$home1*p_s.home[2,2]
gplay$h2=gplay$home2*p_s.home[3,2]
gplay$h3=gplay$home3*p_s.home[4,2]
gplay$h4=gplay$home4*p_s.home[5,2]
gplay$h5=gplay$home5*p_s.home[6,2]
gplay$h6=gplay$home6*p_s.home[7,2]
gplay$h7=gplay$home7*p_s.home[8,2]
gplay$h8=gplay$home8*p_s.home[9,2]
gplay$h9=gplay$home9*p_s.home[10,2]
gplay$h10=gplay$home10*p_s.home[11,2]

gplay$P2Pplay=round(gplay$a1+gplay$a2+gplay$a3+gplay$a4+gplay$a5+gplay$a6+gplay$a7+gplay$a8+gplay$a9+gplay$a10+
                 gplay$h1+gplay$h2+gplay$h3+gplay$h4+gplay$h5+gplay$h6+gplay$h7+gplay$h8+gplay$h9+gplay$h10,4)

gplay2=gplay[c(1,43)]
gplay2=rename(gplay2,c("gplay"="player.id"))

roster.master2=merge(roster.master2,gplay2,by="player.id", all=TRUE)
roster.master2$P2Pplay[is.na(roster.master2$P2Pplay)]<-0


rm(p_s.home,p_s.away,p_s.descr,greg,greg2,gplay,gplay2,p_sfor.away.logit,p_sfor.home.logit)

roster.master3=roster.master2[c(1,2,6,13:20)]

head(roster.master3[with(roster.master3,order(-greg)),],n=21)
head(roster.master3[with(roster.master3,order(-gplay)),],n=21)
head(roster.master3[with(roster.master3,order(-P2Preg)),],n=21)
head(roster.master3[with(roster.master3,order(-P2Pplay)),],n=21)




# Goals EVEN STRENGTH ONLY!! ------------------------------------------------

grand.data$gregev=ifelse(grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                         grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                         grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                         grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                         grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                         grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                       grand.data$eplayer1,NA)
grand.data$ass1regev=ifelse(grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                          grand.data$eplayer2,NA)
grand.data$ass2regev=ifelse(grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="GOAL"&grand.data$gcode<=1230&grand.data$period<5&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                          grand.data$eplayer3,NA)

grand.data$gplayev=ifelse(grand.data$etype=="GOAL"&grand.data$gcode>1230&
                          grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                          grand.data$etype=="GOAL"&grand.data$gcode>1230&
                          grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                          grand.data$etype=="GOAL"&grand.data$gcode>1230&
                          grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                        grand.data$eplayer1,NA)

grand.data$ass1playev=ifelse(grand.data$etype=="GOAL"&grand.data$gcode>1230&
                             grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                             grand.data$etype=="GOAL"&grand.data$gcode>1230&
                             grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                             grand.data$etype=="GOAL"&grand.data$gcode>1230&
                             grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                           grand.data$eplayer2,NA)
grand.data$ass2playev=ifelse(grand.data$etype=="GOAL"&grand.data$gcode>1230&
                             grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                             grand.data$etype=="GOAL"&grand.data$gcode>1230&
                             grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                             grand.data$etype=="GOAL"&grand.data$gcode>1230&
                             grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                           grand.data$eplayer3,NA)

gregev=as.data.frame(table(grand.data$gregev,useNA=NULL))
gregev=rename(gregev,c("Var1"="player.id","Freq"="gregev"))
ass1regev=as.data.frame(table(grand.data$ass1regev,useNA=NULL))
ass1regev=rename(ass1regev,c("Var1"="player.id","Freq"="ass1regev"))
ass2regev=as.data.frame(table(grand.data$ass2regev,useNA=NULL))
ass2regev=rename(ass2regev,c("Var1"="player.id","Freq"="ass2regev"))
reg=merge(gregev,ass1regev,by="player.id",all=TRUE)
reg=merge(reg,ass2regev,by="player.id",all=TRUE)
reg[is.na(reg)]<-0
# reg=reg[with(reg, order(player.id)), ]
rm(ass1regev,ass2regev,gregev)

gplayev=as.data.frame(table(grand.data$gplayev, useNA=NULL))
gplayev=rename(gplayev,c("Var1"="player.id","Freq"="gplayev"))
ass1playev=as.data.frame(table(grand.data$ass1playev, useNA=NULL))
ass1playev=rename(ass1playev,c("Var1"="player.id","Freq"="ass1playev"))
ass2playev=as.data.frame(table(grand.data$ass2playev, useNA=NULL))
ass2playev=rename(ass2playev,c("Var1"="player.id","Freq"="ass2playev"))
play=merge(gplayev,ass1playev,by="player.id",all=TRUE)
play=merge(play,ass2playev,by="player.id",all=TRUE)
play[is.na(play)]<-0
# play=play[with(play, order(player.id)), ]
rm(ass1playev,ass2playev,gplayev)

roster.master2=merge(roster.master,reg,by="player.id", all=TRUE)
roster.master2=merge(roster.master2,play,by="player.id", all=TRUE)
roster.master2[is.na(roster.master2)]<-0
rm(reg,play)



# head(roster.master2[with(roster.master2,order(-gregev)),],n=20)
# head(roster.master2[with(roster.master2,order(-gplayev)),],n=20)

roster.master3<-roster.master2[c("player.id","pos","firstlast",
                                 "gregev","ass1regev","ass2regev","gplayev","ass1playev","ass2playev")]

head(roster.master3[with(roster.master2,order(-gregev)),],n=20)
head(roster.master3[with(roster.master2,order(-gplayev)),],n=20)