# Halvtidsdiffens påverkan på slutdiffen
cor(endr2$htDiff, endr2$endDiff)
htmod <- lm(endDiff ~ htDiff + homePos*visitingPos, data=endr2)
summary(htmod)


endmod <- lm(homeTeamScore ~ htDiff*htHomeScore + homePos*visitingPos, data=endr2)
