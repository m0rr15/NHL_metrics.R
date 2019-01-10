
#Load Packages
library("biglm", lib.loc="~/R/win-library/3.1")
library("bitops", lib.loc="~/R/win-library/3.1")
library("nhlscrapr", lib.loc="~/R/win-library/3.1")
library("RCurl", lib.loc="~/R/win-library/3.1")
library("rjson", lib.loc="~/R/win-library/3.1")

#Set working Dir
setwd("C:/Users/morris/Desktop/RR")



game.table=full.game.database()


compile.all.games()
