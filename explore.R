# Tidsfördelningen för antal gjorda mål
ggplot(cleangoals,aes(x=minute)) + geom_density()

# Halvtidsdiff mot slutdiff
ggplot(endr2, aes(x=htDiff, y=endDiff)) + geom_bin2d()
ggplot(endr2, aes(x=htDiff, y=endDiff, alpha=0.3)) + geom_point()
endr2[,htDiffChar := as.character(htDiff),by="gameId"]
ggplot(endr2, aes(x=htDiffChar, y=endDiff, fill=htDiffChar)) + geom_boxplot()

# Halvtidsresultat mot slutligt antal gjorda mål för hemma- resp. bortalaget
ggplot(endr2[htHomeScore < 4 & htVisScore < 4], aes(x=homeTeamScore)) + 
	geom_histogram(binwidth=1,color="black",fill="red",alpha=0.4) + 
	geom_histogram(binwidth=1, color="black", fill="blue", alpha=0.4, aes(x=visitingTeamScore)) +
	facet_grid(htHomeScore ~ htVisScore)

