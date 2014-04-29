## Läs in data
require(data.table)
goaldata <- fread("Our data/all goals - allsvenskan.csv")
setkey(goaldata, "gameId", "minute")

# Vissa matcher har färre antal rader än de borde ha, dvs. alla mål är inte med
# som egna rader. De måste tas bort från datat.

## MUNGE
# Räkna antal rader och antal gjorda mål per match
goaldata[,nrows:=.N,by="gameId"]
goaldata[,ngoals:=homeTeamScore+visitingTeamScore,by="gameId"]

# Ta bort de matcher där antal rader ≠ antal gjorda mål
cleangoals <- goaldata[nrows == ngoals & minute <= 120]


## Aggregera till matchnivå
endresults <- cleangoals[J(unique(gameId)), mult="last"]

## Hämta halvtidsdata
halftime <- cleangoals[minute<=45, mult="last"]
halftime <- halftime[J(unique(gameId)), mult="last"]

# ta ut halvtidsresultatet
halftime_goals <- halftime[, list(gameId, homeGoals, visitingGoals)]
setnames(halftime_goals, c("homeGoals","visitingGoals"), c("htHomeScore","htVisScore"))

# Lägg till halvtidsresultatet till slutresultatsdata
endr2 <- merge(endresults, halftime_goals, by="gameId", all.x=TRUE)
suppressWarnings(endr2[is.na(htHomeScore)]$htHomeScore <- 0)
suppressWarnings(endr2[is.na(htVisScore)]$htVisScore <- 0)


## Målskillnader
endr2[,htDiff := htHomeScore - htVisScore, by="gameId"]
endr2[,endDiff := homeTeamScore - visitingTeamScore, by="gameId"]

