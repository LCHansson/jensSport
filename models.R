## Modell efter Baio och Blangiardo, ch.2 (se 'Theory'-mappen)
## samt Bååth (se: http://www.sumsar.net/blog/2013/07/modeling-match-results-in-la-liga-part-one/)

# Libraries
require(dplyr)
require(rjags)
require(lubridate)

# Data
matchdata <- tbl_df(read.csv("Our data//matchdata - allsvenskan 2008-2013.csv")) 
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
	Season = matchdata$sasong,
	n_teams = length(teams),
	n_games = nrow(matchdata),
	n_seasons = length(seasons)
)

# Convenience function to generate the type of column names Jags outputs.
col_name <- function(name, ...) {
	paste0(name, "[", paste(..., sep = ","), "]")
}


# Explore the data
par(mfcol = c(2, 1), mar = rep(2.2, 4))
hist(c(data_list$AwayGoals, data_list$HomeGoals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5, main = "Distribution of the number of goals\nscored by a team in a match.")
mean_goals <- mean(c(data_list$AwayGoals, data_list$HomeGoals))
hist(rpois(9999, mean_goals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5, main = "Random draw from a Poisson distribution with\nthe same mean as the distribution above.")




# Model
m1_txt <-
	"model {
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

# Compiling model 1
m1 <- jags.model(textConnection(m1_txt), data = data_list, n.chains = 3,
				 n.adapt = 5000)
# Burning some samples on the altar of the MCMC god
update(m1, 5000)
# Generating MCMC samples
s1 <- coda.samples(m1, variable.names = c("baseline", "skill", "group_skill",
										  "group_sigma"), n.iter = 10000, thin = 2)
# Merging the three MCMC chains into one matrix
ms1 <- as.matrix(s1)


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

plot_pred_comp1("AIK", "IFK Göteborg", ms1)
plot_pred_comp1("IFK Göteborg", "AIK", ms1)
