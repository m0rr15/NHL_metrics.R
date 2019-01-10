
# compile_season.R
# Morris Trachsler, 2014

# Scrapes NHL data from the NHL website using the nhlscrapr package.
# Downloading all data at once didn't work and scraping in chunks 
# does the trick

#Load Packages
library("biglm", lib.loc="~/R/win-library/3.1")
library("bitops", lib.loc="~/R/win-library/3.1")
library("nhlscrapr", lib.loc="~/R/win-library/3.1")
library("RCurl", lib.loc="~/R/win-library/3.1")
library("rjson", lib.loc="~/R/win-library/3.1")

#Set working Dir
setwd("C:/Users/morris/Desktop/RR")

# GAME TABLES FOR EVERY SEASON
game.table0203=full.game.database()[1:1335,]
game.table0304=full.game.database()[1336:2670,]
game.table0506=full.game.database()[2671:4005,]
game.table0607=full.game.database()[4006:5340,]
game.table0708=full.game.database()[5341:6675,]
game.table0809=full.game.database()[6676:8010,]
game.table0910=full.game.database()[8011:9345,]
game.table1011=full.game.database()[9346:10680,]
game.table1112=full.game.database()[10681:12015,]
game.table1213=full.game.database()[12016:12840,]
game.table1314=full.game.database()[12841:14175,]
game.table0914=full.game.database()[8011:14175,]
game.table1014=full.game.database()[9346:14175,]

# COMPILE.ALL, EACH SEASON IND.
compile.all.games(output.file="Season0203.RData",
                  new.game.table=game.table0203)
compile.all.games(output.file="Season0304.RData",
                  new.game.table=game.table0304)
compile.all.games(output.file="Season0506.RData",
                  new.game.table=game.table0506)
compile.all.games(output.file="Season0607.RData",
                  new.game.table=game.table0607)
compile.all.games(output.file="Season0708.RData",
                  new.game.table=game.table0708)
compile.all.games(output.file="Season0809.RData",
                  new.game.table=game.table0809)
compile.all.games(output.file="Season0910.RData",
                  new.game.table=game.table0910)
compile.all.games(output.file="Season1011.RData",
                  new.game.table=game.table1011)
compile.all.games(output.file="Season1112.RData",
                  new.game.table=game.table1112)
compile.all.games(output.file="Season1213.RData",
                  new.game.table=game.table1213)
compile.all.games(output.file="Season1314.RData",
                  new.game.table=game.table1314)
compile.all.games(output.file="Season0914.RData",
                  new.game.table=game.table0914)
compile.all.games(output.file="Season1014.RData",
                  new.game.table=game.table1014)


