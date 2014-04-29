## PREDICTION OF FOOTBALL RESULTS IN SWEDISH LEAGUE 'ALLSVENSKAN'
#' This is the main script for initializing and running basic predictive modelling
#' of football results in Allsvenskan, based on a data set containing all games
#' during the six seasons of 2008 to 2013.
#' 
#' The goal is to build a Bayesian model for predicting the number of goals made by
#' the home and visiting teams in an arbitrary football game. This also implies
#' determining the winner and loser of the game.
#' 
#' The script is structured in the following way:
#' - Libraries and initialization
#' - Data loading and basic munging
#' - Convenience functions needed to plot and evaluate models
#' - Models
#' 	- Model 1. Incredibly naïve baseline model
#' 	- Model 2. Consider home advantage
#' 	- Model 3. Consider between-season differences
#'
#' 
#' Copyleft by Love Hansson and Jens Finnäs, 2014.
#' 
#' Based on a model inspired by Baio och Blangiardo, ch.2 (cf. the 'Theory' folder)
#' and Bååth (cf. http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-one/)



## Libraries and initialization ----
require(dplyr)
require(rjags)
require(lubridate)
require(mcmcplots)
require(coda)
require(ggplot2)
require(stringr)

set.seed(12345)  # for reproducibility



## Data ----
matchdata <- tbl_df(read.csv("Our data//matchdata - allsvenskan 2008-2013 + form.csv", fileEncoding = "UTF-8")) 
matchdata <- matchdata %.%
	mutate(
		resultat = sign(hl_slutmal - bl_slutmal),
		sasong = year(sasong)
	)

# Initial values
T = 16 # Antal lag i Allsvenskan (sedan säsongen 2008/2009 är det 16 lag i Allsvenskan per säsong)
gps = 240 # Antal matcher per säsong = sum(1:15*2)
teams <- levels(matchdata$hl_namn)
seasons = unique(matchdata$sasong)

# A list for JAGS with the data from matchdata where the strings are coded as
# integers
data_list <- list(
	HomeGoals = matchdata$hl_slutmal,
	AwayGoals = matchdata$bl_slutmal,
	HomeTeam = as.integer(matchdata$hl_namn),
	AwayTeam = as.integer(matchdata$bl_namn),
	HomeForm5 = matchdata$hl_form5,
	AwayForm5 = matchdata$bl_form5,
	Season = as.factor(matchdata$sasong),
	n_teams = length(teams),
	n_games = nrow(matchdata),
	n_seasons = length(seasons)
)

# Explore the data
par(mfcol = c(2, 1), mar = rep(2.2, 4))
hist(c(data_list$AwayGoals, data_list$HomeGoals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5, main = "Distribution of the number of goals\nscored by a team in a match.")
mean_goals <- mean(c(data_list$AwayGoals, data_list$HomeGoals))
hist(rpois(9999, mean_goals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5, main = "Random draw from a Poisson distribution with\nthe same mean as the distribution above.")



## Functions ----

# PlotPost by John Kruschke
source("ProgramsDoingBayesianDataAnalysis/plotPost.R")

# Convenience function to generate the type of column names Jags outputs.
col_name <- function(name, ...) {
	paste0(name, "[", paste(..., sep = ","), "]")
}

# Plots histograms over home_goals, away_goals, the difference in goals
# and a barplot over match results.
plot_goals <- function(home_goals, away_goals) {
	n_matches <- length(home_goals)
	goal_diff <- home_goals - away_goals
	match_result <- ifelse(goal_diff < 0, "away_win", ifelse(goal_diff > 0,
															 "home_win", "equal"))
	hist(home_goals, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
	hist(away_goals, xlim = c(-0.5, 10), breaks = (0:100) - 0.5)
	hist(goal_diff, xlim = c(-6, 6), breaks = (-100:100) - 0.5)
	barplot(table(match_result)/n_matches, ylim = c(0, 1))
}

# Plot function for the first model
plot_pred_comp1 <- function(home_team, away_team, ms) {
	# Simulates and plots game goals scores using the MCMC samples from the m1
	# model.
	par(mfrow = c(2, 4))
	baseline <- ms[, "baseline"]
	home_skill <- ms[, col_name("skill", which(teams == home_team))]
	away_skill <- ms[, col_name("skill", which(teams == away_team))]
	home_goals <- rpois(nrow(ms), exp(baseline + home_skill - away_skill))
	away_goals <- rpois(nrow(ms), exp(baseline + away_skill - home_skill))
	plot_goals(home_goals, away_goals)
	# Plots the actual distribution of goals between the two teams
	home_goals <- matchdata$hl_slutmal[matchdata$hl_namn == home_team & matchdata$bl_namn == away_team]
	away_goals <- matchdata$bl_slutmal[matchdata$hl_namn == home_team & matchdata$bl_namn == away_team]
	plot_goals(home_goals, away_goals)
}

# Plot function for the second model
plot_pred_comp2 <- function(home_team, away_team, ms) {
	par(mfrow = c(2, 4))
	home_baseline <- ms[, "home_baseline"]
	away_baseline <- ms[, "away_baseline"]
	home_skill <- ms[, col_name("skill", which(teams == home_team))]
	away_skill <- ms[, col_name("skill", which(teams == away_team))]
	home_goals <- rpois(nrow(ms), exp(home_baseline + home_skill - away_skill))
	away_goals <- rpois(nrow(ms), exp(away_baseline + away_skill - home_skill))
	plot_goals(home_goals, away_goals)
	home_goals <- matchdata$hl_slutmal[matchdata$hl_namn == home_team & matchdata$bl_namn == away_team]
	away_goals <- matchdata$bl_slutmal[matchdata$hl_namn == home_team & matchdata$bl_namn == away_team]
	plot_goals(home_goals, away_goals)
}

## MODEL EVALUATION ----

## Model 1: Incredibly naïve model ----

# Compiling model 1
m1 <- jags.model("Models/m1-naive.bug", data = data_list, n.chains = 3,
				 n.adapt = 5000)
# Burning some samples on the altar of the MCMC god
update(m1, 5000)
# Generating MCMC samples
s1 <- coda.samples(m1, variable.names = c("baseline", "skill", "group_skill",
										  "group_sigma"), n.iter = 10000, thin = 2)
# Merging the three MCMC chains into one matrix
ms1 <- as.matrix(s1)


# Simulate 15000 matches with AIK as home team and Göteborg as away team
plot_pred_comp1("AIK", "IFK Göteborg", ms1)
# Simulate 15000 matches with Göteborg as home team and AIK as away team.
# Note that in the naïve model, predicted results are exactly the inverse
# for this simulation whereas the real results differ markedly.
plot_pred_comp1("IFK Göteborg", "AIK", ms1)



## Model 2: Consider home team advantage ----

# Compile and run the model
m2 <- jags.model("Models/m2-home_adv.bug", data = data_list, n.chains = 3,
				 n.adapt = 5000)
update(m2, 5000)
s2 <- coda.samples(m2, variable.names = c(
	"home_baseline", "away_baseline",
	"skill", "group_sigma", "group_skill"),
	n.iter = 10000, thin = 2)
ms2 <- as.matrix(s2)

# Plot home and away baseline to illustrate home advantage
plot(s2[, "home_baseline"])
plot(s2[, "away_baseline"])
plotPost(exp(ms2[, "home_baseline"]) - exp(ms2[, "away_baseline"]), compVal = 0,
		 xlab = "Home advantage in number of goals")


# Compare the DIC of the baseline model (m1) with the home/away model (m2)
dic_m1 <- dic.samples(m1, 10000, "pD")
dic_m2 <- dic.samples(m2, 10000, "pD")
diffdic(dic_m1, dic_m2)

# Compare home/away matches for AIK and Göteborg
# (note the significant difference from plots from the first model)
plot_pred_comp2("AIK", "IFK Göteborg", ms2)
plot_pred_comp2("IFK Göteborg", "AIK", ms2)



## Model 3: Consider inter-seasonal skill differences ----
# Not all 23 teams are in Allsvenskan at once (d'oh!).
# The following plot illustrates which teams were in the league nay given season
qplot(sasong, hl_namn, data = matchdata, ylab = "Team", xlab = "Particicipation by Season")

# Compile and run the model (we need to increase the amount of samples and the amount of
# thinning to handle increased autocorrelation)
m3 <- jags.model("Models/m3-seasonal_diff.bug", data = data_list, n.chains = 3,
				 n.adapt = 10000)
update(m3, 10000)
s3 <- coda.samples(m3, variable.names = c(
	"home_baseline", "away_baseline",
	"skill", "season_sigma", "group_sigma", "group_skill"),
	n.iter = 40000,
	thin = 8)
ms3 <- as.matrix(s3)

# Look into the newly introduced season_sigma parameter
plot(s3[, "season_sigma"])

# Examine whether Model 3 predicts better than Model 2
dic_m3 <- dic.samples(m3, 40000, "pD")
diffdic(dic_m2, dic_m3)


## Ranking teams and making predictions ----
# The ranking of the teams for the 2012/13 season.
team_skill <- ms3[, str_detect(string = colnames(ms3), "skill\\[5,")]
team_skill <- (team_skill - rowMeans(team_skill)) + ms3[, "home_baseline[5]"]
team_skill <- exp(team_skill)
colnames(team_skill) <- teams
team_skill <- team_skill[, order(colMeans(team_skill), decreasing = T)]
par(mar = c(2, 0.7, 0.7, 0.7), xaxs = "i")

# Plot the rankings. The differences to top-notch leagues like la liga is
# astonishing: no small group of teams clearly singles out from the rest.
# This small difference in skill between teams probably puts an upper constraint
# on the predictive strength of this current model.
caterplot(team_skill, labels.loc = "above", val.lim = c(0.7, 3.8))

# Jämför skill mellan två av de topprankade lagen
plotPost(team_skill[, "Gais"] - team_skill[, "Helsingborg"], compVal = 0,
		 xlab = "← Gais    vs     Helsingborg →")


# Model assessment
n <- nrow(ms3)
m3_pred <- sapply(1:nrow(matchdata), function(i) {
	home_team <- which(teams == matchdata$hl_namn[i])
	away_team <- which(teams == matchdata$bl_namn[i])
	season <- which(seasons == matchdata$sasong[i])
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


# How often does the model predict the right number of home goals?
mean(matchdata$hl_slutmal == m3_pred[, "mode_home_goal"], na.rm = T)
# The mean squared error of predicted number of home goals
mean((matchdata$hl_slutmal - m3_pred[, "mean_home_goal"])^2, na.rm = T)

# How often does the model predict the correct outcome (home win/draw/away win)?
mean(matchdata$resultat == m3_pred[, "match_result"], na.rm = T)

# Results:
# Right number of home goals: ~32.6%
# Correct outcome: ~51% of games (random would have been approx. 33%?)


# Simulate n games between AIK (home) and Gais (away) in the season of 2013
n <- nrow(ms3)
home_team <- which(teams == "Gais")
away_team <- which(teams == "AIK")
season <- which(seasons == "2013")
home_skill <- ms3[, col_name("skill", season, home_team)]
away_skill <- ms3[, col_name("skill", season, away_team)]
home_baseline <- ms3[, col_name("home_baseline", season)]
away_baseline <- ms3[, col_name("away_baseline", season)]
home_goals <- rpois(n, exp(home_baseline + home_skill - away_skill))
away_goals <- rpois(n, exp(away_baseline + away_skill - home_skill))

# Plot the results
par(mfrow = c(2, 2), mar = rep(2.2, 4))
plot_goals(home_goals, away_goals)

# Look at the results of the m first matches
m <- 100; result <- 0
for (i in 1:m) {
	cat("Game:\t\tHome:\t\tAway:\t\tResult:\n")
	cat("\t", i, "\t\t\t", home_goals[i], "\t\t\t\t", away_goals[i], "\t\t\t\t", sign(home_goals[i]-away_goals[i]), "\n")
	result <- result + sign(home_goals[i]-away_goals[i])
}
cat("Sum of Result:", result, "\n")


## Model 4: Consider team form ----
# Compile and run the model (we need to increase the amount of samples and the amount of
# thinning to handle increased autocorrelation)
m4 <- jags.model("Models/m4-team_form.bug", data = data_list, n.chains = 3,
				 n.adapt = 10000)
update(m3, 10000)
s3 <- coda.samples(m3, variable.names = c(
	"home_baseline", "away_baseline",
	"skill", "season_sigma", "group_sigma", "group_skill"),
	n.iter = 40000,
	thin = 8)
ms3 <- as.matrix(s3)
