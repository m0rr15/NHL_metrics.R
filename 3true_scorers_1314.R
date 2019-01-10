
# true_scorers_1314.R
#
# In this script, I first reconstruct the NHL scoring list from the play-by-
# play data. As an alternative to simply count goals, I then propose a new 
# measure of a player's offensive impact: The weighted goal. Here, I mulitply 
# each goal with its marginal probability that the team wins the game. This 
# produces an alternative scoring list and uncovers under- as well as overrated 
# players.
# Finally, I rank players in function of even strength goals. This, again,
# reveals underrated 5 on 5 players as well as the well known power
# play-monsters.
#
# Morris Trachsler, 2014


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

# Working Directory
setwd("C:/Users/morris/Desktop/RR")

# Load NHL data
load("C:/Users/morris/Desktop/RR/Season1314.RData")
rm(distance.adjust, shot.tables, scoring.models)  # remove unnecessary df

# There are now three df's in our environment we are working with:
# "games" lists all 1335 games played in the 13/14 season, one game per line. 
# "grand.data" is the NHL's play-by-play data for every game played this season.
# "roster.master" lists all players that played games this season.





# VARIABLE TREATMENT -----------------------------------------------------------
grand.data <- rename(grand.data,
                  c("ev.team"="eteam","ev.player.1"="eplayer1",
                    "ev.player.2"="eplayer2","ev.player.3"="eplayer3",
                    "home.score"="homescore","away.score"="awayscore",
                    "event.length"="elength","home.skaters"="homeskaters",
                    "away.skaters"="awayskaters","adjusted.distance"="adjdist"))
grand.data$gcode <- as.numeric(grand.data$gcode)
# grand.data$session <- ifelse(grand.data$gcode<=1230,"Regular","Playoff")
# table(grand.data$session)
grand.data$awayscore2 <- ifelse (
  grand.data$eteam == grand.data$awayteam & grand.data$etype == "GOAL"&
    grand.data$gcode <= 1230 & grand.data$period < 5,
  grand.data$awayscore + 1,
  ifelse (
    grand.data$eteam == grand.data$awayteam & grand.data$etype == "GOAL" &
      grand.data$gcode > 1230,
    grand.data$awayscore + 1, NA))
grand.data$homescore2 <- ifelse (
  grand.data$eteam == grand.data$hometeam & grand.data$etype == "GOAL" &
    grand.data$gcode <= 1230 & grand.data$period < 5,
  grand.data$homescore + 1,
  ifelse (
    grand.data$eteam == grand.data$hometeam & grand.data$etype == "GOAL" &
      grand.data$gcode > 1230,
    grand.data$homescore + 1, NA))

grand.data$greg <- ifelse(
  grand.data$etype == "GOAL" & grand.data$gcode <= 1230 & grand.data$period < 5,
  grand.data$eplayer1, NA)
grand.data$ass1reg <- ifelse (
  grand.data$etype == "GOAL" & grand.data$gcode <= 1230 & grand.data$period < 5,
  grand.data$eplayer2, NA)
grand.data$ass2reg <- ifelse (
  grand.data$etype == "GOAL" & grand.data$gcode <= 1230 & grand.data$period < 5,
  grand.data$eplayer3, NA)

grand.data$gplay <- ifelse (
  grand.data$etype == "GOAL" & grand.data$gcode > 1230,
  grand.data$eplayer1, NA)
grand.data$ass1play <- ifelse (
  grand.data$etype == "GOAL" & grand.data$gcode > 1230,
  grand.data$eplayer2, NA)
grand.data$ass2play <- ifelse (
  grand.data$etype == "GOAL" & grand.data$gcode > 1230,
  grand.data$eplayer3, NA)
# Frequency table Player ID -> Number of points scored in regular season
greg <- as.data.frame(table(grand.data$greg, useNA=NULL))  # frequency table g
greg <- rename(greg, c("Var1"="player.id", "Freq"="greg"))
ass1reg <- as.data.frame(table(grand.data$ass1reg, useNA=NULL))  # fr. table a1
ass1reg <- rename(ass1reg, c("Var1"="player.id", "Freq"="ass1reg"))
ass2reg <- as.data.frame(table(grand.data$ass2reg, useNA=NULL))
ass2reg <- rename(ass2reg,c("Var1"="player.id", "Freq"="ass2reg"))
# merging frequency tables -> points (g, a1, a2) scored in regular season
reg <- merge(greg,ass1reg,by="player.id", all=TRUE)
reg <- merge(reg,ass2reg,by="player.id", all=TRUE)
reg[is.na(reg)] <- 0
 # reg <- reg[with(reg, order(player.id)), ]
rm(ass1reg,ass2reg,greg)
# Frequency table Player ID -> Number of points scored in playoffs
gplay <- as.data.frame(table(grand.data$gplay, useNA=NULL))
gplay <- rename(gplay, c("Var1"="player.id", "Freq"="gplay"))
ass1play <- as.data.frame(table(grand.data$ass1play, useNA=NULL))
ass1play <- rename(ass1play, c("Var1"="player.id", "Freq"="ass1play"))
ass2play <- as.data.frame(table(grand.data$ass2play, useNA=NULL))
ass2play <- rename(ass2play, c("Var1"="player.id", "Freq"="ass2play"))
play <- merge(gplay, ass1play, by="player.id", all=TRUE)
play <- merge(play, ass2play, by="player.id", all=TRUE)
play[is.na(play)] <- 0
 # play <- play[with(play, order(player.id)), ]
rm(ass1play, ass2play, gplay)
# adding points scored in reg. season & playoffs to roster.master: merge()
roster.master <- merge(roster.master, reg, by="player.id", all=TRUE)
roster.master <- merge(roster.master, play, by="player.id", all=TRUE)
roster.master[is.na(roster.master)] <- 0
rm(reg,play)
# Delete duplicate player entries in roster.master
roster.master <- roster.master[!duplicated(roster.master$player.id), ]
# gregr: goal scoring rank in reg. season
roster.master$gregr[order(-roster.master$greg)] <- 1:nrow(roster.master)
# gplayr: goal scoring rank in playoffs
roster.master$gplayr[order(-roster.master$gplay)] <- 1:nrow(roster.master)

# Top Regular Season and Playoff Goal Scorers in the 13/14 season
head(roster.master[order(-roster.master$greg),
                   c("gregr", "player.id", "pos", "last", "first", "greg")],
     n=20)
head(roster.master[order(-roster.master$gplay),
                   c("gplayr", "player.id", "pos", "last", "first", "gplay")],
     n=20)


# THE WEIGHTED GOAL ------------------------------------------------------------
# Idea: Some goals are more important than others. New measure: Multiplying each 
# goal scored with its respective marginal probability to win the game
# Var manipulation to start
games$awayscore2 <- as.numeric(games$awayscore)
games$homescore2 <- as.numeric(games$homescore)
games$awaypoints <- 0  # points won in a game: win=2, loss after S/O=1, loss=0
games$homepoints <- 0
games$awaypoints[games$session=="Regular" &
                   games$awayscore2>games$homescore2] <- 2
games$awaypoints[games$session=="Regular" & games$periods>3 &
                   games$awayscore2<games$homescore2] <- 1
games$homepoints[games$session=="Regular" &
                   games$awayscore2<games$homescore2] <- 2
games$homepoints[games$session=="Regular" & games$periods>3 &
                   games$awayscore2>games$homescore2] <- 1
# In playoffs there are only wins and losses, i.e. 2 points or zero
games$awaypoints[games$session=="Playoffs" &
                   games$awayscore2>games$homescore2] <- 2
games$homepoints[games$session=="Playoffs" &
                   games$awayscore2<games$homescore2] <- 2

games$awaypoints <- factor(games$awaypoints,ordered=T)
games$homepoints <- factor(games$homepoints,ordered=T)

# logit estimation between points won and goals scored at home and on road
p_sfor.away.logit <- polr(awaypoints ~ awayscore2, data=games,
                          method="logistic")
p_sfor.home.logit <- polr(homepoints ~ homescore2, data=games,
                          method="logistic")

# DESCRIPTIVE TABLES (0/1,1/2,2/3,....approx integral)
p_s.descr <- data.frame(cbind(awayscore2 = 0:10, homescore2=0:10))
p_s.descr <- cbind(p_s.descr, predict(p_sfor.away.logit,p_s.descr,type="probs"),
                   predict(p_sfor.home.logit,p_s.descr, type="probs"))
p_s.descr <- p_s.descr[-c(3,4,6,7)]
p_s.descr <- rename(p_s.descr, c("2"="P2away", "2.1"="P2home"))
p_s.descr$dP2away <- 0
p_s.descr$dP2away[p_s.descr$awayscore2==0] <- p_s.descr$P2away
p_s.descr$dP2away[p_s.descr$awayscore2>0] <- diff(p_s.descr$P2away)
p_s.descr$dP2home <- 0
p_s.descr$dP2home[p_s.descr$homescore2==0] <- p_s.descr$P2home
p_s.descr$dP2home[p_s.descr$homescore2>0] <- diff(p_s.descr$P2home)
p_s.away <- p_s.descr[c(1,5)]
p_s.home <- p_s.descr[c(2,6)]
rm(p_s.descr)
# Creating a dummy for each goal scored, indicating that it was the n'th goal
# scored for the respective team in the respective game
grand.data$away1 <- ifelse(grand.data$awayscore2==1, 1, 0)
grand.data$away2 <- ifelse(grand.data$awayscore2==2, 1, 0)
grand.data$away3 <- ifelse(grand.data$awayscore2==3, 1, 0)
grand.data$away4 <- ifelse(grand.data$awayscore2==4, 1, 0)
grand.data$away5 <- ifelse(grand.data$awayscore2==5, 1, 0)
grand.data$away6 <- ifelse(grand.data$awayscore2==6, 1, 0)
grand.data$away7 <- ifelse(grand.data$awayscore2==7, 1, 0)
grand.data$away8 <- ifelse(grand.data$awayscore2==8, 1, 0)
grand.data$away9 <- ifelse(grand.data$awayscore2==9, 1, 0)
grand.data$away10 <- ifelse(grand.data$awayscore2==10, 1, 0)
grand.data$home1 <- ifelse(grand.data$homescore2==1, 1, 0)
grand.data$home2 <- ifelse(grand.data$homescore2==2, 1, 0)
grand.data$home3 <- ifelse(grand.data$homescore2==3, 1, 0)
grand.data$home4 <- ifelse(grand.data$homescore2==4, 1, 0)
grand.data$home5 <- ifelse(grand.data$homescore2==5, 1, 0)
grand.data$home6 <- ifelse(grand.data$homescore2==6, 1, 0)
grand.data$home7 <- ifelse(grand.data$homescore2==7, 1, 0)
grand.data$home8 <- ifelse(grand.data$homescore2==8, 1, 0)
grand.data$home9 <- ifelse(grand.data$homescore2==9, 1, 0)
grand.data$home10 <- ifelse(grand.data$homescore2==10, 1, 0)

# greg: a df containing the player IDs, the total of goals scored (regular
# season) and the total of i'th goals scored at home and on road
greg <- ddply(grand.data, .(greg), summarize,
            goals=length(greg),
            away1=sum(awayscore2==1, na.rm=T),
            away2=sum(awayscore2==2, na.rm=T),
            away3=sum(awayscore2==3, na.rm=T),
            away4=sum(awayscore2==4, na.rm=T),
            away5=sum(awayscore2==5, na.rm=T),
            away6=sum(awayscore2==6, na.rm=T),
            away7=sum(awayscore2==7, na.rm=T),
            away8=sum(awayscore2==8, na.rm=T),
            away9=sum(awayscore2==9, na.rm=T),
            away10=sum(awayscore2==10, na.rm=T),
            home1=sum(homescore2==1, na.rm=T),
            home2=sum(homescore2==2, na.rm=T),
            home3=sum(homescore2==3, na.rm=T),
            home4=sum(homescore2==4, na.rm=T),
            home5=sum(homescore2==5, na.rm=T),
            home6=sum(homescore2==6, na.rm=T),
            home7=sum(homescore2==7, na.rm=T),
            home8=sum(homescore2==8, na.rm=T),
            home9=sum(homescore2==9, na.rm=T),
            home10=sum(homescore2==10, na.rm=T))
# multiplying the total of i'th goals scored with its marginal probabilities
greg$a1 <- greg$away1 * p_s.away[2, 2]
greg$a2 <- greg$away2 * p_s.away[3, 2]
greg$a3 <- greg$away3 * p_s.away[4, 2]
greg$a4 <- greg$away4 * p_s.away[5, 2]
greg$a5 <- greg$away5 * p_s.away[6, 2]
greg$a6 <- greg$away6 * p_s.away[7, 2]
greg$a7 <- greg$away7 * p_s.away[8, 2]
greg$a8 <- greg$away8 * p_s.away[9, 2]
greg$a9 <- greg$away9 * p_s.away[10, 2]
greg$a10 <- greg$away10 * p_s.away[11, 2]
greg$h1 <- greg$home1 * p_s.home[2, 2]
greg$h2 <- greg$home2 * p_s.home[3, 2]
greg$h3 <- greg$home3 * p_s.home[4, 2]
greg$h4 <- greg$home4 * p_s.home[5, 2]
greg$h5 <- greg$home5 * p_s.home[6, 2]
greg$h6 <- greg$home6 * p_s.home[7, 2]
greg$h7 <- greg$home7 * p_s.home[8, 2]
greg$h8 <- greg$home8 * p_s.home[9, 2]
greg$h9 <- greg$home9 * p_s.home[10, 2]
greg$h10 <- greg$home10 * p_s.home[11, 2]
# Summing up a single player's contribution: our new measure, P2Preg
greg$P2Preg <- round(greg$a1 + greg$a2 + greg$a3 + greg$a4 + greg$a5 + greg$a6 +
                       greg$a7 + greg$a8 + greg$a9 + greg$a10 + greg$h1 +
                       greg$h2 + greg$h3 + greg$h4 + greg$h5 + greg$h6 +
                       greg$h7 + greg$h8 + greg$h9 + greg$h10, 4)
greg <- greg[c(1, 43)]
greg <- rename(greg, c("greg"="player.id"))
# adding new measure to roster.master: merge()
roster.master <- merge(roster.master, greg,by="player.id", all=T)
roster.master$P2Preg[is.na(roster.master$P2Preg)] <- 0

# gplay: Creating a df containing the player IDs, the total of
# goals scored (playoffs) and the total of i'th goals scored at home and 
# on road
gplay <- ddply(grand.data, .(gplay), summarize,
            goals=length(gplay),
            away1=sum(awayscore2==1,na.rm=T),
            away2=sum(awayscore2==2,na.rm=T),
            away3=sum(awayscore2==3,na.rm=T),
            away4=sum(awayscore2==4,na.rm=T),
            away5=sum(awayscore2==5,na.rm=T),
            away6=sum(awayscore2==6,na.rm=T),
            away7=sum(awayscore2==7,na.rm=T),
            away8=sum(awayscore2==8,na.rm=T),
            away9=sum(awayscore2==9,na.rm=T),
            away10=sum(awayscore2==10,na.rm=T),
            home1=sum(homescore2==1,na.rm=T),
            home2=sum(homescore2==2,na.rm=T),
            home3=sum(homescore2==3,na.rm=T),
            home4=sum(homescore2==4,na.rm=T),
            home5=sum(homescore2==5,na.rm=T),
            home6=sum(homescore2==6,na.rm=T),
            home7=sum(homescore2==7,na.rm=T),
            home8=sum(homescore2==8,na.rm=T),
            home9=sum(homescore2==9,na.rm=T),
            home10=sum(homescore2==10,na.rm=T))
# multiplying the total of i'th goals scored with its marginal probabilities
gplay$a1 <- gplay$away1 * p_s.away[2, 2]
gplay$a2 <- gplay$away2 * p_s.away[3, 2]
gplay$a3 <- gplay$away3 * p_s.away[4, 2]
gplay$a4 <- gplay$away4 * p_s.away[5, 2]
gplay$a5 <- gplay$away5 * p_s.away[6, 2]
gplay$a6 <- gplay$away6 * p_s.away[7, 2]
gplay$a7 <- gplay$away7 * p_s.away[8, 2]
gplay$a8 <- gplay$away8 * p_s.away[9, 2]
gplay$a9 <- gplay$away9 * p_s.away[10, 2]
gplay$a10 <- gplay$away10 * p_s.away[11, 2]
gplay$h1 <- gplay$home1 * p_s.home[2, 2]
gplay$h2 <- gplay$home2 * p_s.home[3, 2]
gplay$h3 <- gplay$home3 * p_s.home[4, 2]
gplay$h4 <- gplay$home4 * p_s.home[5, 2]
gplay$h5 <- gplay$home5 * p_s.home[6, 2]
gplay$h6 <- gplay$home6 * p_s.home[7, 2]
gplay$h7 <- gplay$home7 * p_s.home[8, 2]
gplay$h8 <- gplay$home8 * p_s.home[9, 2]
gplay$h9 <- gplay$home9 * p_s.home[10, 2]
gplay$h10 <- gplay$home10 * p_s.home[11, 2]
# Summing up a single player's contribution: our new measure, P2Pplay
gplay$P2Pplay <- round(gplay$a1 + gplay$a2 + gplay$a3 + gplay$a4 + gplay$a5 +
                         gplay$a6 + gplay$a7 + gplay$a8 + gplay$a9 + gplay$a10 +
                         gplay$h1 + gplay$h2 + gplay$h3 + gplay$h4 + gplay$h5 +
                         gplay$h6 + gplay$h7 + gplay$h8 + gplay$h9 +
                         gplay$h10,4)
gplay <- gplay[c(1, 43)]
gplay <- rename(gplay, c("gplay"="player.id"))
# adding new measure to roster.master: merge()
roster.master <- merge(roster.master, gplay, by="player.id", all=TRUE)
roster.master$P2Pplay[is.na(roster.master$P2Pplay)] <- 0

# Cleaning up unnecessary vars
rm(p_s.home, p_s.away, greg, gplay)

# omit one row of NAs
roster.master <- na.omit(roster.master)
# P2Pplayr: marginal goal scoring rank playoffs
roster.master$P2Pplayr[order(-roster.master$P2Pplay)] <- 1:nrow(roster.master)
# P2Pregr: marginal goal scoring rank reg. season
roster.master$P2Pregr[order(-roster.master$P2Preg)] <- 1:nrow(roster.master)

# dregr: rank won(+) or lost (-) with the new measurement, reg. season
roster.master$dregr <- roster.master$gregr - roster.master$P2Pregr
# dplayr: rank won(+) or lost (-) with the new measurement, playoffs
roster.master$dplayr <- roster.master$gplayr - roster.master$P2Pplayr



# PLOTS: Marginal Probs of Scored Goals-------------------------------
p_s.for.dat <- data.frame(cbind(  # cont. var from zero to ten
  awayscore2 = seq(from=0, to=10,length.out=500),
  homescore2 = seq(from=0,to=10, length.out=500)))
p_s.for.dat <- cbind(  # predictions from logit regressions
  p_s.for.dat,
  predict(p_sfor.away.logit,p_s.for.dat,type="probs"),
  predict(p_sfor.home.logit,p_s.for.dat,type="probs"))
p_s.for.dat <- p_s.for.dat[-c(3,4,6,7)]
p_s.for.dat <- rename(p_s.for.dat,c("2"="P2away","2.1"="P2home"))
p_s.for.dat.diff <- data.frame(cbind(  # MARGINAL probabilities
  dP2away = diff(p_s.for.dat$P2away)/diff(p_s.for.dat$awayscore2),
  dP2home = diff(p_s.for.dat$P2home)/diff(p_s.for.dat$homescore2),
  Score = p_s.for.dat$awayscore[-500]))
p_s.for.dat <- rename(p_s.for.dat,c("P2away"="Away","P2home"="Home"))
p_s.for.dat.diff <- rename(p_s.for.dat.diff,
                           c("dP2away"="Away","dP2home"="Home"))
# TIDY dataframes!!
p_s.for.dat  <- melt(p_s.for.dat,id.vars=c("awayscore2","homescore2"),
                     variable.name="Location",value.name="Probability")
p_s.for.dat.diff <- melt(p_s.for.dat.diff,id.vars=c("Score"),
                         variable.name="Location",value.name="mProbability")
# ggplot(p_s.for.dat, aes(x=awayscore2, y=Probability, colour=Location)) +
#   geom_line() +
#   xlab("Goals scored") +
#   ylab("Probability") +
#   ggtitle("Probability to win 2 points versus Goals Scored") +
#   scale_x_continuous(breaks=seq(0, 10, 1))  # Ticks from 0-10, every 1
ggplot(p_s.for.dat.diff, aes(x=Score, y=mProbability, colour=Location)) +
  geom_line() +
  xlab("Goals scored") +
  ylab("Marginal Probability") +
  ggtitle("Marginal Probability to win 2 points versus Goals Scored") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  scale_y_continuous(breaks=seq(0, 1, 0.05))
rm(p_sfor.home.logit, p_sfor.away.logit, p_s.for.dat, p_s.for.dat.diff)




# TOP SCORERS LISTS -----------------------------------------------
# Top 20 NHL goal scorers of the 13/14 regular season, new measure included
head(roster.master[order(-roster.master$greg),
                   c("gregr", "player.id", "pos", "last", "first", "greg",
                     "P2Preg")],
     n=20)
# Top 20 NHL goal scorers of the 13/14 playoffs, new measure included
head(roster.master[order(-roster.master$gplay),
                   c("gplayr", "player.id", "pos", "last", "first", "gplay",
                     "P2Pplay")],
     n=20)
# Top 20 NHL weighted goal scorers of the 13/14 regular season
head(roster.master[order(-roster.master$P2Preg),
                   c("P2Pregr", "player.id", "pos", "last", "first", "P2Preg",
                     "greg", "dregr")],
     n=20)
# Top 20 NHL weighted goal scorers of the 13/14 playoffs
head(roster.master[order(-roster.master$P2Pplay),
                   c("P2Pplayr", "player.id", "pos", "last", "first", "gplay",
                     "P2Pplay", "dplayr")],
     n=20)

# Most underrated Scorers regular season with at least n goals:
# These sets of players did much better under the new measurement because they 
# scored a lot of "important" goals with a high marginal probability to help the
# team win.
head(subset(roster.master[order(-roster.master$dregr),
                          c("gregr", "P2Pregr", "dregr", "player.id", "pos",
                            "last", "first", "P2Preg", "greg")],
            greg >= 20),
     n=20)
# Most underrated Scorers playoffs
head(subset(roster.master[order(-roster.master$dplayr),
                          c("gplayr", "P2Pplayr", "dplayr", "player.id", "pos",
                            "last", "first", "P2Preg", "gplay")],
            gplay >= 1),
     n=20)


# Overrated Scorers regular season with at least n goals:
# Players on these lists did worse under the new measurement because they scored 
# relatively many "unimportant" goals
head(subset(roster.master[order(roster.master$dregr),
                          c("gregr", "P2Pregr", "dregr", "player.id", "pos",
                            "last", "first", "P2Preg", "greg")],
            greg >= 10),
     n=20)
# Overrated Scorers playoffs
head(subset(roster.master[order(roster.master$dplayr),
                          c("gplayr", "P2Pplayr", "dplayr", "player.id", "pos",
                            "last", "first", "P2Preg", "gplay")],
            gplay >= 1),
     n=20)


# EVEN STRENGTH GOALS ONLY ------------------------------------------------
grand.data$gregev <- ifelse(
  grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a6==1 & grand.data$a5!=1 & grand.data$h6==1 & grand.data$h5!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a5==1 & grand.data$a4!=1 & grand.data$h5==1 & grand.data$h4!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a4==1 & grand.data$a3!=1 & grand.data$h4==1 & grand.data$h3!=1,
  grand.data$eplayer1,
  NA)
grand.data$ass1regev <- ifelse(
  grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a6==1 & grand.data$a5!=1 & grand.data$h6==1 & grand.data$h5!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a5==1 & grand.data$a4!=1 & grand.data$h5==1 & grand.data$h4!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a4==1 & grand.data$a3!=1 & grand.data$h4==1 & grand.data$h3!=1,
  grand.data$eplayer2,
  NA)
grand.data$ass2regev <- ifelse(
  grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a6==1 & grand.data$a5!=1 & grand.data$h6==1 & grand.data$h5!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a5==1 & grand.data$a4!=1 & grand.data$h5==1 & grand.data$h4!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode<=1230 & grand.data$period<5 &
    grand.data$a4==1 & grand.data$a3!=1 & grand.data$h4==1 & grand.data$h3!=1,
  grand.data$eplayer3,
  NA)
grand.data$gplayev <- ifelse(
  grand.data$etype=="GOAL" & grand.data$gcode>1230 &grand.data$a6==1 & 
    grand.data$a5!=1 & grand.data$h6==1 & grand.data$h5!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a5==1 &
    grand.data$a4!=1 & grand.data$h5==1 & grand.data$h4!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a4==1 &
    grand.data$a3!=1 & grand.data$h4==1 & grand.data$h3!=1,
  grand.data$eplayer1,
  NA)
grand.data$ass1playev <- ifelse(
  grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a6==1 &
    grand.data$a5!=1 & grand.data$h6==1 & grand.data$h5!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a5==1 &
    grand.data$a4!=1 & grand.data$h5==1 & grand.data$h4!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a4==1 &
    grand.data$a3!=1 & grand.data$h4==1 & grand.data$h3!=1,
  grand.data$eplayer2,
  NA)
grand.data$ass2playev <- ifelse(
  grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a6==1 & 
    grand.data$a5!=1 & grand.data$h6==1 & grand.data$h5!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a5==1 & 
    grand.data$a4!=1 & grand.data$h5==1 & grand.data$h4!=1 |
    grand.data$etype=="GOAL" & grand.data$gcode>1230 & grand.data$a4==1 & 
    grand.data$a3!=1 & grand.data$h4==1 & grand.data$h3!=1,
  grand.data$eplayer3,
  NA)

gregev <- as.data.frame(table(grand.data$gregev, useNA=NULL))
gregev <- rename(gregev, c("Var1"="player.id", "Freq"="gregev"))
ass1regev <- as.data.frame(table(grand.data$ass1regev, useNA=NULL))
ass1regev <- rename(ass1regev, c("Var1"="player.id", "Freq"="ass1regev"))
ass2regev <- as.data.frame(table(grand.data$ass2regev, useNA=NULL))
ass2regev <- rename(ass2regev, c("Var1"="player.id", "Freq"="ass2regev"))
reg <- merge(gregev, ass1regev, by="player.id", all=T)
reg <- merge(reg, ass2regev, by="player.id", all=T)
reg[is.na(reg)] <- 0
# reg <- reg[with(reg, order(player.id)), ]
rm(ass1regev, ass2regev, gregev)

gplayev <- as.data.frame(table(grand.data$gplayev, useNA=NULL))
gplayev <- rename(gplayev, c("Var1"="player.id", "Freq"="gplayev"))
ass1playev <- as.data.frame(table(grand.data$ass1playev, useNA=NULL))
ass1playev <- rename(ass1playev, c("Var1"="player.id", "Freq"="ass1playev"))
ass2playev <- as.data.frame(table(grand.data$ass2playev, useNA=NULL))
ass2playev <- rename(ass2playev, c("Var1"="player.id", "Freq"="ass2playev"))
play <- merge(gplayev, ass1playev, by="player.id", all=TRUE)
play <- merge(play, ass2playev, by="player.id", all=TRUE)
play[is.na(play)] <- 0
# play <- play[with(play, order(player.id)), ]
rm(ass1playev, ass2playev, gplayev)

roster.master <- merge(roster.master, reg, by="player.id", all=TRUE)
roster.master <- merge(roster.master, play, by="player.id", all=TRUE)
roster.master[is.na(roster.master)] <- 0
rm(reg, play)

# gregevr: even strenght goal scoring rank in reg. season
roster.master$gregevr[order(-roster.master$gregev)] <- 1:nrow(roster.master)
roster.master$dregevr <- roster.master$gregr - roster.master$gregevr
# gplayevr: even strength goal scoring rank in playoffs
roster.master$gplayevr[order(-roster.master$gplayev)] <- 1:nrow(roster.master)
roster.master$dplayevr <- roster.master$gplayr - roster.master$gplayevr




# TOP SCORERS LISTS, AGAIN ---------------------------------------
# Top Scorers Even Strength, Regular season
head(roster.master[order(-roster.master$gregev),
                   c("gregr", "gregevr", "dregevr", "player.id", "pos", "last",
                     "first", "greg", "gregev")],
     n=20)
# Top Goal Scorers Even Strength, Playoffs
head(roster.master[order(-roster.master$gplayev),
                   c("gplayr", "gplayevr", "dplayevr", "player.id", "pos",
                     "last", "first", "gplay", "gplayev")],
     n=20)

# Underrated players regular season with at least 10 goals:
# Players on this list scored the bulk of their goals in even strength
# situations. This implies dominating opponents 5-on-5 with puck possession and 
# shot creation.And also a lack of PP ice time (hey Kyle Palmieri!)
head(subset(roster.master[order(-roster.master$dregevr),
                          c("gregr", "gregevr", "dregevr", "player.id", "pos",
                            "last", "first", "greg", "gregev")],
            greg >= 10),
     n=20)
# PP-monsters: Overrated Scorers regular season with at least 10 goals:
# Players on this list scored most of their goals on the power play.
head(subset(roster.master[order(roster.master$dregevr),
                          c("gregr", "gregevr", "dregevr", "player.id", "pos",
                            "last", "first", "greg", "gregev")],
            greg >= 10),
     n=20)







