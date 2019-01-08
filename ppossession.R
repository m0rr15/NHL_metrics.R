

#BEST PUCK-POSSESSION PROXY: FENWICK/CORSI, SHOTS ON GOAL, WON FACEOFFS!, BLOCKED SHOTS (-), 
#CHECKS (-),....)

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
library("stringr", lib.loc="~/R/win-library/3.1")
setwd("C:/Users/morris/Desktop/RR")
load("C:/Users/morris/Desktop/RR/Season1314.RData")

#Var treatment (renaming, etc.)
grand.data=rename(grand.data,c("ev.team"="eteam","ev.player.1"="eplayer1",
                               "ev.player.2"="eplayer2","ev.player.3"="eplayer3",
                               "home.score"="homescore","away.score"="awayscore",
                               "event.length"="elength","home.skaters"="homeskaters",
                               "away.skaters"="awayskaters","adjusted.distance"="adjdist"))
grand.data$gcode<-as.character(grand.data$gcode)
# games$gcode<-as.numeric(games$gcode)
# grand.data$awayscore=ifelse(grand.data$eteam==grand.data$awayteam&grand.data$etype=="GOAL"&
#                                grand.data$gcode<=1230&grand.data$period<5,
#                              grand.data$awayscore+1,
#                              ifelse(grand.data$eteam==grand.data$awayteam&grand.data$etype=="GOAL"&
#                                       grand.data$gcode>1230,
#                                     grand.data$awayscore+1,NA))
# grand.data$homescore=ifelse(grand.data$eteam==grand.data$hometeam&grand.data$etype=="GOAL"&
#                                grand.data$gcode<=1230&grand.data$period<5,
#                              grand.data$homescore+1,
#                              ifelse(grand.data$eteam==grand.data$hometeam&grand.data$etype=="GOAL"&
#                                       grand.data$gcode>1230,
#                                     grand.data$homescore+1,NA))

# Points ------------------------------------------------------------------

games$awayscore=as.numeric(games$awayscore)
games$homescore=as.numeric(games$homescore)
games$awaypoints=0
games$homepoints=0
games$awaypoints[games$session=="Regular" & games$awayscore>games$homescore]=2
games$awaypoints[games$session=="Regular" & games$periods>3 & games$awayscore<games$homescore]=1
games$homepoints[games$session=="Regular" & games$awayscore<games$homescore]=2
games$homepoints[games$session=="Regular" & games$periods>3 & games$awayscore>games$homescore]=1
games$awaypoints[games$session=="Playoffs" & games$awayscore>games$homescore]=2
games$homepoints[games$session=="Playoffs" & games$awayscore<games$homescore]=2
games$awaypoints=factor(games$awaypoints,ordered=TRUE)
games$homepoints=factor(games$homepoints,ordered=TRUE)

games$apointsadj=ifelse(games$homepoints==0,2,
                        ifelse(games$homepoints==1|games$awaypoints==1,1,0))
games$hpointsadj=ifelse(games$awaypoints==0,2,
                        ifelse(games$awaypoints==1|games$homepoints==1,1,0))
games$apointsadj=factor(games$apointsadj,ordered=TRUE)
games$hpointsadj=factor(games$hpointsadj,ordered=TRUE)


# Faceoffs (TOTAL) ------------------------------------------------------------

grand.data$foat=ifelse(grand.data$etype=="FAC"&grand.data$eteam==grand.data$awayteam,1,NA)
grand.data$foht=ifelse(grand.data$etype=="FAC"&grand.data$eteam==grand.data$hometeam,1,NA)

foat=ddply(grand.data, .(gcode), summarize,
             foat=sum(foat==1,na.rm=TRUE))
foht=ddply(grand.data, .(gcode), summarize,
             foht=sum(foht==1,na.rm=TRUE))
fo=merge(foat,foht,by="gcode", all=TRUE)
games=merge(games,fo,by="gcode",all=TRUE)
rm(fo,foat,foht)


# Faceoffs (EVEN) ---------------------------------------------------------

grand.data$foae=ifelse(grand.data$etype=="FAC"&grand.data$eteam==grand.data$awayteam&
                         grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                         grand.data$etype=="FAC"&grand.data$eteam==grand.data$awayteam&
                         grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                         grand.data$etype=="FAC"&grand.data$eteam==grand.data$awayteam&
                         grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                       1,NA)
grand.data$fohe=ifelse(grand.data$etype=="FAC"&grand.data$eteam==grand.data$hometeam&
                         grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                         grand.data$etype=="FAC"&grand.data$eteam==grand.data$hometeam&
                         grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                         grand.data$etype=="FAC"&grand.data$eteam==grand.data$hometeam&
                         grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                       1,NA)

foae=ddply(grand.data, .(gcode), summarize,
           foae=sum(foae==1,na.rm=TRUE))
fohe=ddply(grand.data, .(gcode), summarize,
           fohe=sum(fohe==1,na.rm=TRUE))
fo=merge(foae,fohe,by="gcode", all=TRUE)
games=merge(games,fo,by="gcode",all=TRUE)
rm(fo,foae,fohe)

# FO-Zones (TOTAL)----------------------------------------------------------------

grand.data$fzdt=ifelse(grand.data$etype=="FAC"&grand.data$homezone=="Def",1,NA)
grand.data$fznt=ifelse(grand.data$etype=="FAC"&grand.data$homezone=="Neu",1,NA)
grand.data$fzot=ifelse(grand.data$etype=="FAC"&grand.data$homezone=="Off",1,NA)

fzdt=ddply(grand.data, .(gcode), summarize,
             fzdt=sum(fzdt==1,na.rm=TRUE))
fznt=ddply(grand.data, .(gcode), summarize,
             fznt=sum(fznt==1,na.rm=TRUE))
fzot=ddply(grand.data, .(gcode), summarize,
            fzot=sum(fzot==1,na.rm=TRUE))
fozone=merge(fzdt,fznt,by="gcode", all=TRUE)
fozone=merge(fozone,fzot,by="gcode",all=TRUE)
games=merge(games,fozone,by="gcode",all=TRUE)
rm(fozone,fzdt,fznt,fzot)

# FO-Zones (EVEN) ---------------------------------------------------------

grand.data$fzde=ifelse(grand.data$etype=="FAC"&grand.data$homezone=="Def"&
                         grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                         grand.data$etype=="FAC"&grand.data$homezone=="Def"&
                         grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                         grand.data$etype=="FAC"&grand.data$homezone=="Def"&
                         grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                       1,NA)
grand.data$fzne=ifelse(grand.data$etype=="FAC"&grand.data$homezone=="Neu"&
                         grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                         grand.data$etype=="FAC"&grand.data$homezone=="Neu"&
                         grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                         grand.data$etype=="FAC"&grand.data$homezone=="Neu"&
                         grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                       1,NA)
grand.data$fzoe=ifelse(grand.data$etype=="FAC"&grand.data$homezone=="Off"&
                         grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                         grand.data$etype=="FAC"&grand.data$homezone=="Off"&
                         grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                         grand.data$etype=="FAC"&grand.data$homezone=="Off"&
                         grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                        1,NA)

fzde=ddply(grand.data, .(gcode), summarize,
           fzde=sum(fzde==1,na.rm=TRUE))
fzne=ddply(grand.data, .(gcode), summarize,
           fzne=sum(fzne==1,na.rm=TRUE))
fzoe=ddply(grand.data, .(gcode), summarize,
           fzoe=sum(fzoe==1,na.rm=TRUE))
fozone=merge(fzde,fzne,by="gcode", all=TRUE)
fozone=merge(fozone,fzoe,by="gcode",all=TRUE)
games=merge(games,fozone,by="gcode",all=TRUE)
rm(fozone,fzde,fzne,fzoe)


# TOTAL: Corsi, Fenwick and Shots ----------------------------------

grand.data$cat=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$awayteam,
                            1,NA)
grand.data$cht=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$hometeam,
                            1,NA)

grand.data$fat=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam,
                          1,NA)
grand.data$fht=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam,
                          1,NA)

grand.data$sat=ifelse(grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam,
                            1,NA)
grand.data$sht=ifelse(grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam,
                            1,NA)


cat=ddply(grand.data, .(gcode), summarize,
                cat=sum(cat==1,na.rm=TRUE))
cht=ddply(grand.data, .(gcode), summarize,
                cht=sum(cht==1,na.rm=TRUE))
corsi=merge(cat,cht,by="gcode", all=TRUE)
rm(cat,cht)

fat=ddply(grand.data, .(gcode), summarize,
              fat=sum(fat==1,na.rm=TRUE))
fht=ddply(grand.data, .(gcode), summarize,
              fht=sum(fht==1,na.rm=TRUE))
fen=merge(fat,fht,by="gcode", all=TRUE)
rm(fat,fht)

corsifen=merge(corsi,fen,by="gcode",all=TRUE)
rm(corsi,fen)
games=merge(games,corsifen,by="gcode",all=TRUE)
rm(corsifen)

sat=ddply(grand.data, .(gcode), summarize,
                sat=sum(sat==1,na.rm=TRUE))
sht=ddply(grand.data, .(gcode), summarize,
                sht=sum(sht==1,na.rm=TRUE))
shots=merge(sat,sht,by="gcode", all=TRUE)
rm(sat,sht)
games=merge(games,shots,by="gcode",all=TRUE)
rm(shots)

games$cat.p=games$cat/(games$cat+games$cht)*100
games$cht.p=games$cht/(games$cat+games$cht)*100
games$fat.p=games$fat/(games$fat+games$fht)*100
games$fht.p=games$fht/(games$fat+games$fht)*100
games$foat.p=games$foat/(games$foat+games$foht)*100
games$foht.p=games$foht/(games$foat+games$foht)*100
games$fzdt.p=games$fzdt/(games$fzdt+games$fzot+games$fznt)*100
games$fzot.p=games$fzot/(games$fzdt+games$fzot+games$fznt)*100
games$sat.p=games$sat/(games$sat+games$sht)*100
games$sht.p=games$sht/(games$sat+games$sht)*100


# EVENSTRENGTH: Corsi, Fenwick and Shot(Teams) -----------------------------------------------

grand.data$cae=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                             1,NA)
grand.data$che=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="BLOCK"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                            1,NA)

grand.data$fae=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                          1,NA)
grand.data$fhe=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="MISS"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                            grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                            grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                          1,NA)

grand.data$sae=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$awayteam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                            1,NA)
grand.data$she=ifelse(grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="GOAL"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a6==1&grand.data$a5!=1&grand.data$h6==1&grand.data$h5!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a5==1&grand.data$a4!=1&grand.data$h5==1&grand.data$h4!=1|
                              grand.data$etype=="SHOT"&grand.data$eteam==grand.data$hometeam&
                              grand.data$a4==1&grand.data$a3!=1&grand.data$h4==1&grand.data$h3!=1,
                            1,NA)


cae=ddply(grand.data, .(gcode), summarize,
                cae=sum(cae==1,na.rm=TRUE))
che=ddply(grand.data, .(gcode), summarize,
                che=sum(che==1,na.rm=TRUE))
corsi=merge(cae,che,by="gcode", all=TRUE)
rm(cae,che)

fae=ddply(grand.data, .(gcode), summarize,
                fae=sum(fae==1,na.rm=TRUE))
fhe=ddply(grand.data, .(gcode), summarize,
              fhe=sum(fhe==1,na.rm=TRUE))
fen=merge(fae,fhe,by="gcode", all=TRUE)
rm(fae,fhe)

corsifen=merge(corsi,fen,by="gcode",all=TRUE)
rm(corsi,fen)
games=merge(games,corsifen,by="gcode",all=TRUE)
rm(corsifen)

sae=ddply(grand.data, .(gcode), summarize,
                sae=sum(sae==1,na.rm=TRUE))
she=ddply(grand.data, .(gcode), summarize,
                she=sum(she==1,na.rm=TRUE))
shots=merge(sae,she,by="gcode", all=TRUE)
rm(sae,she)
games=merge(games,shots,by="gcode",all=TRUE)
rm(shots)

games$cae.p=games$cae/(games$cae+games$che)*100
games$che.p=games$che/(games$cae+games$che)*100
games$fae.p=games$fae/(games$fae+games$fhe)*100
games$fhe.p=games$fhe/(games$fae+games$fhe)*100
games$foae.p=games$foae/(games$foae+games$fohe)*100
games$fohe.p=games$fohe/(games$foae+games$fohe)*100
games$fzde.p=games$fzde/(games$fzde+games$fzoe+games$fzne)*100
games$fzoe.p=games$fzoe/(games$fzde+games$fzoe+games$fzne)*100
games$sae.p=games$sae/(games$sae+games$she)*100
games$she.p=games$she/(games$sae+games$she)*100



# data frames -------------------------------------------------------------

games$awaypoints<-as.numeric(levels(games$awaypoints))[games$awaypoints]
games$homepoints<-as.numeric(levels(games$homepoints))[games$homepoints]

r<-data.frame(gcode=c(games$gcode,games$gcode),score=c(games$awayscore,games$homescore),
                    points=c(games$awaypoints,games$homepoints),
                    shots.total=c(games$sat.p,games$sht.p),shots.even=c(games$sae.p,games$she.p),
                    corsi.total=c(games$cat.p,games$cht.p),corsi.even=c(games$cae.p,games$che.p),
                    fenwick.total=c(games$fat.p,games$fht.p),fenwick.even=c(games$fae.p,games$fhe.p),
                    faceoffs.total=c(games$foat.p,games$foht.p),faceoffs.even=c(games$foae.p,games$fohe.p),
                    fozone.total=c(games$fzdt.p,games$fzot.p),fozone.even=c(games$fzde.p,games$fzoe.p))

r$points<-factor(r$points,ordered=TRUE)
games$awaypoints<-factor(games$awaypoints,ordered=TRUE)
games$homepoints<-factor(games$homepoints,ordered=TRUE)
    
      

# Descriptive Stats -------------------------------------------------------

corr<-function(x,y) var(x,y,na.rm=TRUE)/(sd(x,na.rm=TRUE)*sd(y,na.rm=TRUE))




# Principal Component Analysis (PCA) --------------------------------------
pca1<-princomp(~corsi.even+faceoffs.even+fozone.even,data=r,cor=TRUE,na.action=na.exclude)
# summary(pca)
# loadings(pca)
pca1<-data.frame(pca1$scores)
pca1<-rename(pca1,c("Comp.1"="PCA1","Comp.2"="PCA2","Comp.3"="PCA3"))

r$PCA1<-pca1$PCA1
r$PCA2<-pca1$PCA2
r$PCA3<-pca1$PCA3
rm(pca1)

# Regress -----------------------------------------------------------------

pposs.score=polr(points ~ score, data=r, method="logistic")
summary(pposs.score)


pposs.shots=polr(points ~ shots.total, data=r, method="logistic")
summary(pposs.shots)
pposs.shots=polr(points ~ shots.even, data=r, method="logistic")
summary(pposs.shots)

pposs.corsi=polr(points ~ corsi.total, data=r, method="logistic")
summary(pposs.corsi)
pposs.corsi=polr(points ~ corsi.even, data=r, method="logistic")
summary(pposs.corsi)

pposs.fenwick=polr(points ~ fenwick.total, data=r, method="logistic")
summary(pposs.fenwick)
pposs.fenwick=polr(points ~ fenwick.even, data=r, method="logistic")
summary(pposs.fenwick)


pposs.faceoffs=polr(points ~ faceoffs.total, data=r, method="logistic")
summary(pposs.faceoffs)
pposs.faceoffs=polr(points ~ faceoffs.even, data=r, method="logistic")
summary(pposs.faceoffs)


pposs.zone=polr(points ~ fozone.total, data=r, method="logistic")
summary(pposs.zone)
pposs.zone=polr(points ~ fozone.even, data=r, method="logistic")
summary(pposs.zone)



pposs=polr(points ~ shots.even+faceoffs.even+fozone.even, data=r, method="logistic")
summary(pposs)

pposs=polr(points ~ corsi.even+faceoffs.even+fozone.even, data=r, method="logistic")
summary(pposs)
pposs=polr(points ~ fenwick.even+faceoffs.even+fozone.even, data=r, method="logistic")
summary(pposs)


pposs=polr(points ~ PCA1+PCA2, data=r, method="logistic")
summary(pposs)


# Log Regressions ---------------------------------------------------------
#LOG REGRESS (not ordered, just win/loss instead of 0/1/2 points??)

# pposs.corsiaway=polr(awaypoints ~ corsiaway.p, data=games, method="logistic")
# pposs.fenaway=polr(awaypoints ~ fenaway.p, data=games, method="logistic")
# pposs.foaway=polr(awaypoints ~ foaway.p, data=games, method="logistic")
# pposs.fodefaway=polr(awaypoints ~ fodef.p, data=games, method="logistic")
# pposs.corsifoaway=polr(awaypoints ~ corsiaway.p+foaway+fodef, data=games, method="logistic")

pposs.corsiaway2=polr(awaypoints ~ corsiaway.p, data=games, method="logistic")
pposs.fenaway2=polr(awaypoints ~ fenaway.p, data=games, method="logistic")
pposs.foaway2=polr(awaypoints ~ foaway.d, data=games, method="logistic")
pposs.fodefaway2=polr(awaypoints ~ fodef.d, data=games, method="logistic")
pposs1=polr(awaypoints ~ fenaway.d + foaway.d + fodef.d, data=games, method="logistic")
pposs2=polr(homepoints ~ fenaway.d + foaway.d + fodef.d, data=games, method="logistic")


#CHECKS



# Penalties ---------------------------------------------------------------

grand.data$twominaway=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"2 min")&
                               grand.data$eteam==grand.data$awayteam,1,NA)
grand.data$fourminaway=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"4 min")&
                                grand.data$eteam==grand.data$awayteam,1,NA)
grand.data$fiveminaway=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"5 min")&
                                grand.data$eteam==grand.data$awayteam,1,NA)
grand.data$tenminaway=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"10 min")&
                               grand.data$eteam==grand.data$awayteam,1,NA)
grand.data$twominhome=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"2 min")&
                               grand.data$eteam==grand.data$hometeam,1,NA)
grand.data$fourminhome=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"4 min")&
                                grand.data$eteam==grand.data$hometeam,1,NA)
grand.data$fiveminhome=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"5 min")&
                                grand.data$eteam==grand.data$hometeam,1,NA)
grand.data$tenminhome=ifelse(grand.data$etype=="PENL"&str_detect(grand.data$type,"10 min")&
                               grand.data$eteam==grand.data$hometeam,1,NA)

penaway=ddply(grand.data, .(gcode), summarize,
                twominaway=sum(twominaway==1,na.rm=TRUE),
              fourminaway=sum(fourminaway==1,na.rm=TRUE),
              fiveminaway=sum(fiveminaway==1,na.rm=TRUE),
              tenminaway=sum(tenminaway==1,na.rm=TRUE))
penhome=ddply(grand.data, .(gcode), summarize,
              twominhome=sum(twominhome==1,na.rm=TRUE),
              fourminhome=sum(fourminhome==1,na.rm=TRUE),
              fiveminhome=sum(fiveminhome==1,na.rm=TRUE),
              tenminhome=sum(tenminhome==1,na.rm=TRUE))

pen=merge(penaway,penhome,by="gcode", all=TRUE)
rm(penaway,penhome)

games=merge(games,pen,by="gcode", all=TRUE)
rm(pen)

games$penaway=games$twominaway*2+games$fourminaway*4+games$fiveminaway*5
games$penhome=games$twominhome*2+games$fourminhome*4+games$fiveminhome*5
games$penaway.d=games$penaway-games$penhome

# d <- density(games$penaway.d,na.rm=TRUE,bw="nrd0",adjust=2) # returns the density data
# plot(d)
# 
# x <- games$penaway.d
# h<-hist(x, col="red", xlab="PP/PK Differential",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x,na.rm=TRUE),max(x,na.rm=TRUE),length=1000)
# yfit<-dnorm(xfit,mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# lines(xfit, yfit, col="blue", lwd=2)




