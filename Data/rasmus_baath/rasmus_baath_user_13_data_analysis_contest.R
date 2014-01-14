
## @knitr include=FALSE
opts_chunk$set(cache=TRUE, message=FALSE, warning=FALSE, fig.width=9, fig.height=5, dpi=90)


## @knitr cache=FALSE
library(rjags)
library(coda)
library(mcmcplots)
library(stringr)
library(plyr)
library(xtable)
source("plotPost.R")
set.seed(12345)

load("laliga.RData")

# -1 Away win, 0 Draw, 1 Home win
laliga$MatchResult <- sign(laliga$HomeGoals - laliga$AwayGoals) 

# Creating a data frame d with only the complete match results
d <- na.omit(laliga)

teams <- unique(c(d$HomeTeam, d$AwayTeam))
seasons <- unique(d$Season)

# A list for JAGS with the data from d where the strings are coded as integers
data_list <- list(HomeGoals = d$HomeGoals, AwayGoals = d$AwayGoals, 
                  HomeTeam = as.numeric(factor(d$HomeTeam, levels=teams)),
                  AwayTeam = as.numeric(factor(d$AwayTeam, levels=teams)),
                  Season = as.numeric(factor(d$Season, levels=seasons)),
                  n_teams = length(teams), n_games = nrow(d), 
                  n_seasons = length(seasons))

# Convenience function to generate the type of column names Jags outputs.
col_name <- function(name, ...) {
  paste0(name, "[", paste(..., sep=",") , "]")
}



## @knitr fig.height=5, fig.width=5
old_par <- par(mfcol=c(2,1), mar=rep(2.2, 4))
hist(c(d$AwayGoals, d$HomeGoals), xlim=c(-0.5, 8), breaks = -1:9 + 0.5)
mean_goals <- mean(c(d$AwayGoals, d$HomeGoals))
hist(rpois(9999, mean_goals), xlim=c(-0.5, 8), breaks = -1:9 + 0.5)
par(old_par)


## @knitr tidy=FALSE
m1_string <- "model {
for(i in 1:n_games) {
  HomeGoals[i] ~ dpois(lambda_home[HomeTeam[i],AwayTeam[i]])
  AwayGoals[i] ~ dpois(lambda_away[HomeTeam[i],AwayTeam[i]])
}

for(home_i in 1:n_teams) {
  for(away_i in 1:n_teams) {
    lambda_home[home_i, away_i] <- exp(baseline + skill[home_i] - skill[away_i])
    lambda_away[home_i, away_i] <- exp(baseline + skill[away_i] - skill[home_i])
  }
}

skill[1] <- 0
for(j in 2:n_teams) {
  skill[j] ~ dnorm(group_skill, group_tau)
}  

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1 / pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)
baseline ~ dnorm(0, 0.0625)
}"



## @knitr results='hide'
# Compiling model 1
m1 <- jags.model(textConnection(m1_string), data=data_list, n.chains=3, n.adapt=5000)
# Burning some samples on the altar of the MCMC god
update(m1, 5000)
# Generating MCMC samples
s1 <- coda.samples(m1, variable.names=c("baseline", "skill", "group_skill", "group_sigma"), n.iter=10000, thin=2)
# Merging the three MCMC chains into one matrix
ms1 <- as.matrix(s1)


## @knitr fig.height=3
plot(s1[,col_name("skill", which(teams == "FC Sevilla"))])
plot(s1[,col_name("skill", which(teams == "FC Valencia"))])


## @knitr fig.height=5
# Plots histograms over home_goals, away_goals, the difference in goals and a barplot over the credible match results.
plot_goals <- function(home_goals, away_goals) {
  n_matches <- length(home_goals)
  goal_diff <- home_goals - away_goals
  match_result <- ifelse(goal_diff < 0, "away_win", ifelse(goal_diff > 0, "home_win", "equal"))
  hist(home_goals, xlim=c(-0.5, 10), breaks=(0:100) - 0.5)
  hist(away_goals, xlim=c(-0.5, 10), breaks=(0:100) - 0.5)
  hist(goal_diff, xlim=c(-6, 6), breaks=(-100:100) - 0.5 )
  barplot(table(match_result) / n_matches , ylim=c(0, 1))
}

# Simulates game goals scores using the MCMC samples from the m1 model.
plot_pred_comp1 <- function(home_team, away_team, ms) {
  old_par <- par(mfrow = c(2, 4))
  baseline <- ms[, "baseline"]
  home_skill <- ms[, col_name("skill", which(teams == home_team))]
  away_skill <- ms[, col_name("skill", which(teams == away_team))]
  home_goals <- rpois(nrow(ms),  exp(baseline +  home_skill - away_skill))
  away_goals <- rpois(nrow(ms),  exp(baseline +  away_skill - home_skill))
  plot_goals(home_goals, away_goals)
  home_goals <- d$HomeGoals[ d$HomeTeam == home_team & d$AwayTeam == away_team]
  away_goals <- d$AwayGoals[ d$HomeTeam == home_team & d$AwayTeam == away_team]
  plot_goals(home_goals, away_goals)
  par(old_par)
}

plot_pred_comp1("FC Valencia", "FC Sevilla", ms1)



## @knitr fig.height=5
plot_pred_comp1("FC Sevilla", "FC Valencia",ms1)



## @knitr tidy=FALSE
# model 2
m2_string <- "model {
for(i in 1:n_games) {
  HomeGoals[i] ~ dpois(lambda_home[HomeTeam[i],AwayTeam[i]])
  AwayGoals[i] ~ dpois(lambda_away[HomeTeam[i],AwayTeam[i]])
}

for(home_i in 1:n_teams) {
  for(away_i in 1:n_teams) {
    lambda_home[home_i, away_i] <- exp( home_baseline + skill[home_i] - skill[away_i])
    lambda_away[home_i, away_i] <- exp( away_baseline + skill[away_i] - skill[home_i])
  }
}

skill[1] <- 0 
for(j in 2:n_teams) {
  skill[j] ~ dnorm(group_skill, group_tau)
}

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)

home_baseline ~ dnorm(0, 0.0625)
away_baseline ~ dnorm(0, 0.0625)
}"


## @knitr results='hide'
m2 <- jags.model(textConnection(m2_string), data=data_list, n.chains=3, n.adapt=5000)
update(m2, 5000)
s2 <- coda.samples(m2, variable.names=c("home_baseline", "away_baseline","skill", "group_sigma", "group_skill"), n.iter=10000, thin=2)
ms2 <- as.matrix(s2)


## @knitr fig.height=3
plot(s2[,"home_baseline"])
plot(s2[,"away_baseline"])


## @knitr fig.height=4, fig.width=7, results='hide'
plotPost(exp(ms2[,"home_baseline"]) - exp(ms2[,"away_baseline"]), compVal=0, 
        xlab="Home advantage in number of goals")


## @knitr 
dic_m1 <- dic.samples(m1, 10000, "pD")
dic_m2 <- dic.samples(m2, 10000, "pD")
diffdic(dic_m1, dic_m2)


## @knitr fig.height=5
plot_pred_comp2 <- function(home_team, away_team, ms) {
  old_par <- par(mfrow = c(2, 4))
  home_baseline <- ms[, "home_baseline"]
  away_baseline <- ms[, "away_baseline"]
  home_skill <- ms[, col_name("skill", which(teams == home_team))]
  away_skill <- ms[, col_name("skill", which(teams == away_team))]
  home_goals <- rpois(nrow(ms),  exp(home_baseline +  home_skill - away_skill))
  away_goals <- rpois(nrow(ms),  exp(away_baseline +  away_skill - home_skill))
  plot_goals(home_goals, away_goals)
  home_goals <- d$HomeGoals[ d$HomeTeam == home_team & d$AwayTeam == away_team]
  away_goals <- d$AwayGoals[ d$HomeTeam == home_team & d$AwayTeam == away_team]
  plot_goals(home_goals, away_goals)
  par(old_par)
}

plot_pred_comp2("FC Valencia", "FC Sevilla", ms2)


## @knitr fig.height=5
plot_pred_comp2("FC Sevilla", "FC Valencia",ms2)


## @knitr fig.height=5, fig.width=7
qplot(Season, HomeTeam, data=d, ylab="Team", xlab = "Particicipation by Season")


## @knitr tidy=FALSE
m3_string <- "model {
for(i in 1:n_games) {
  HomeGoals[i] ~ dpois(lambda_home[Season[i], HomeTeam[i],AwayTeam[i]])
  AwayGoals[i] ~ dpois(lambda_away[Season[i], HomeTeam[i],AwayTeam[i]])
}

for(season_i in 1:n_seasons) {
  for(home_i in 1:n_teams) {
    for(away_i in 1:n_teams) {
      lambda_home[season_i, home_i, away_i] <- exp( home_baseline[season_i] + skill[season_i, home_i] - skill[season_i, away_i])
      lambda_away[season_i, home_i, away_i] <- exp( away_baseline[season_i] + skill[season_i, away_i] - skill[season_i, home_i])
    }
  }
}

skill[1, 1] <- 0 
for(j in 2:n_teams) {
  skill[1, j] ~ dnorm(group_skill, group_tau)
}

group_skill ~ dnorm(0, 0.0625)
group_tau <- 1/pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)

home_baseline[1] ~ dnorm(0, 0.0625)
away_baseline[1] ~ dnorm(0, 0.0625)

for(season_i in 2:n_seasons) {
  skill[season_i, 1] <- 0 
  for(j in 2:n_teams) {
    skill[season_i, j] ~ dnorm(skill[season_i - 1, j], season_tau)
  }
  home_baseline[season_i] ~ dnorm(home_baseline[season_i - 1], season_tau)
  away_baseline[season_i] ~ dnorm(away_baseline[season_i - 1], season_tau)
}

season_tau <- 1/pow(season_sigma, 2) 
season_sigma ~ dunif(0, 3) 
}"


## @knitr results='hide'
m3 <- jags.model(textConnection(m3_string), data=data_list, n.chains=3, n.adapt=10000)
update(m3, 10000)
s3 <- coda.samples(m3, variable.names=c("home_baseline", "away_baseline","skill", "season_sigma", "group_sigma", "group_skill"), n.iter=40000, thin=8)
ms3 <- as.matrix(s3)


## @knitr fig.height=3
plot(s3[,"season_sigma"])


## @knitr 
dic_m3 <- dic.samples(m3, 40000, "pD")
diffdic(dic_m2, dic_m3)


## @knitr fig.height=8, dpi=90
# The ranking of the teams for the 2012/13 season.
team_skill <- ms3[, str_detect(string=colnames(ms3), "skill\\[5,")]
team_skill <- (team_skill - rowMeans(team_skill)) + ms3[, "home_baseline[5]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[,order(colMeans(team_skill), decreasing=T)]
old_par <- par(mar=c(2,0.7,0.7,0.7), xaxs='i')
caterplot(team_skill, labels.loc="above", val.lim=c(0.7, 3.8))
par(old_par)


## @knitr fig.height=3, fig.width=7, results='hide'
plotPost(team_skill[, "FC Barcelona"] - team_skill[, "Real Madrid CF"], compVal = 0, xlab = "← Real Madrid     vs     Barcelona →")


## @knitr tidy=FALSE
n <- nrow(ms3)
m3_pred <- sapply(1:nrow(laliga), function(i) {
  home_team <- which(teams == laliga$HomeTeam[i])
  away_team <- which(teams == laliga$AwayTeam[i])
  season <- which(seasons == laliga$Season[i])
  home_skill <- ms3[, col_name("skill", season, home_team)] 
  away_skill <- ms3[, col_name("skill", season, away_team)]
  home_baseline <- ms3[, col_name("home_baseline", season)]
  away_baseline <- ms3[, col_name("away_baseline", season)]

  home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
  away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))
  home_goals_table <- table(home_goals)
  away_goals_table <- table(away_goals)
  match_results <- sign(home_goals - away_goals)
  match_results_table <- table(match_results)
  
  mode_home_goal <- as.numeric(names(home_goals_table)[ which.max(home_goals_table)])
  mode_away_goal <- as.numeric(names(away_goals_table)[ which.max(away_goals_table)])
  match_result <-  as.numeric(names(match_results_table)[which.max(match_results_table)])
  rand_i <- sample(seq_along(home_goals), 1)
  
  c(mode_home_goal = mode_home_goal, mode_away_goal = mode_away_goal, match_result = match_result,
    mean_home_goal = mean(home_goals), mean_away_goal = mean(away_goals),
    rand_home_goal = home_goals[rand_i], rand_away_goal = away_goals[rand_i],
    rand_match_result = match_results[rand_i])
})
m3_pred <- t(m3_pred)


## @knitr fig.height=3, fig.width=4
hist(laliga$HomeGoals, breaks= (-1:10) + 0.5, xlim=c(-0.5, 10))


## @knitr fig.height=3, fig.width=4
hist(m3_pred[ , "mode_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10))


## @knitr fig.height=3, fig.width=4
hist(m3_pred[ , "mean_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10))


## @knitr fig.height=3, fig.width=4
hist(m3_pred[ , "rand_home_goal"], breaks= (-1:10) + 0.5, xlim=c(-0.5, 10))


## @knitr 
mean(laliga$HomeGoals == m3_pred[ , "mode_home_goal"], na.rm=T)
mean((laliga$HomeGoals - m3_pred[ , "mean_home_goal"])^2, na.rm=T)


## @knitr fig.height=3, fig.width=4
hist(laliga$MatchResult, breaks= (-2:1) + 0.5)


## @knitr fig.height=3, fig.width=4
hist(m3_pred[ , "match_result"], breaks= (-2:1) + 0.5)


## @knitr fig.height=3, fig.width=4
hist(m3_pred[ , "rand_match_result"], breaks= (-2:1) + 0.5)


## @knitr 
mean(laliga$MatchResult == m3_pred[ , "match_result"], na.rm=T)


## @knitr results='asis'
laliga_forecast <- laliga[is.na(laliga$HomeGoals), c("Season", "Week", "HomeTeam", "AwayTeam")]
m3_forecast <- m3_pred[is.na(laliga$HomeGoals),] 
laliga_forecast$mean_home_goals <- round(m3_forecast[,"mean_home_goal"], 1) 
laliga_forecast$mean_away_goals <- round(m3_forecast[,"mean_away_goal"], 1)
laliga_forecast$mode_home_goals <- m3_forecast[,"mode_home_goal"] 
laliga_forecast$mode_away_goals <- m3_forecast[,"mode_away_goal"]
laliga_forecast$predicted_winner <- ifelse(m3_forecast[ , "match_result"] == 1, laliga_forecast$HomeTeam, 
                                           ifelse(m3_forecast[ , "match_result"] == -1, laliga_forecast$AwayTeam, "Draw"))

rownames(laliga_forecast) <- NULL
print(xtable(laliga_forecast, align="cccccccccc"), type="html")


## @knitr results='asis'
laliga_sim <- laliga[is.na(laliga$HomeGoals), c("Season", "Week", "HomeTeam", "AwayTeam")]
laliga_sim$home_goals <- m3_forecast[,"rand_home_goal"] 
laliga_sim$away_goals <- m3_forecast[,"rand_away_goal"]
laliga_sim$winner <- ifelse(m3_forecast[ , "rand_match_result"] == 1, laliga_forecast$HomeTeam, 
                            ifelse(m3_forecast[ , "rand_match_result"] == -1, laliga_forecast$AwayTeam, "Draw"))

rownames(laliga_sim) <- NULL
print(xtable(laliga_sim, align="cccccccc"), type="html")


## @knitr 
n <- nrow(ms3)
home_team <- which(teams == "FC Sevilla")
away_team <- which(teams == "FC Valencia")
season <- which(seasons == "2012/13")
home_skill <- ms3[, col_name("skill", season, home_team)] 
away_skill <- ms3[, col_name("skill", season, away_team)]
home_baseline <- ms3[, col_name("home_baseline", season)]
away_baseline <- ms3[, col_name("away_baseline", season)]

home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))


## @knitr fig.height=5
old_par <- par(mfrow = c(2, 2), mar=rep(2.2, 4))
plot_goals(home_goals, away_goals)
par(old_par)


## @knitr 
1 / c(Sevilla =  mean(home_goals > away_goals), Draw = mean(home_goals == away_goals), Valencia = mean(home_goals < away_goals))


## @knitr results='asis'
goals_payout <- laply(0:6, function(home_goal) {
  laply(0:6, function(away_goal) {
    1 / mean(home_goals == home_goal & away_goals  == away_goal)
  })
})

colnames(goals_payout) <- paste("Valencia", 0:6, sep=" - ")
rownames(goals_payout) <- paste("Sevilla", 0:6, sep=" - ")
goals_payout <- round(goals_payout, 1)
print(xtable(goals_payout, align="cccccccc"), type="html")


