
source('load.R')

#
# simulate each game
#

# off rating = 

simulate.from.lm <- function(n.sim, x.bar, std.error, df)
{
	(rt(n.sim, df) * std.error) + x.bar	
}

predict.game <- function(team.coefficients, opp_or, opp_dr, loc, n.sim = 10000)
{
	sim <- list()
	
	sim[['intercept']] <- simulate.from.lm(n.sim, team.coefficients$int.x, team.coefficients$int.se, team.coefficients$df)
	sim[['opp_or']] <- simulate.from.lm(n.sim, team.coefficients$opp_or.x, team.coefficients$opp_or.se, team.coefficients$df)
	sim[['opp_dr']] <- simulate.from.lm(n.sim, team.coefficients$opp_dr.x, team.coefficients$opp_dr.se, team.coefficients$df)


	if( !is.na(team.coefficients[[ paste(loc, '.x', sep = '') ]] ) )
	{
		sim[['location']] <- simulate.from.lm( n.sim, team.coefficients[[ paste(loc, '.x', sep = '') ]], team.coefficients[[ paste(loc, '.se', sep = '') ]], team.coefficients$df )
	}
	else
	{
		sim[['location']] <- 0
	}

	points <- sim[['intercept']] + (opp_or * sim[['opp_or']]) + (opp_dr * sim[['opp_dr']]) + sim[['location']]

	return(points)
}

results$prob <- 0

n.sim <- 10000
for(i in 1:nrow(results))
{
	print(i)
	game <- results[i, ]


	ratings.team1 <- ratings.spread[ ratings.spread$season == game$season & ratings.spread$teamid == game$wteam, ]
	ratings.team2 <- ratings.spread[ ratings.spread$season == game$season & ratings.spread$teamid == game$lteam, ]

	coefficients.team1 <- coefficients[ coefficients$season == game$season & coefficients$teamid == game$wteam, ]
	coefficients.team2 <- coefficients[ coefficients$season == game$season & coefficients$teamid == game$lteam, ]



	#
	# team1
	#
	

	loc <- 'homeN'
	if( game$wloc == 'H' ) {
		loc <- 'homeH'
	} else if( game$wloc == 'A' ) {
		loc <- 'homeA'
	}

	team1 <- predict.game(coefficients.team1, ratings.team2$off, ratings.team2$def, loc, n.sim)


	#
	# team2
	#


	loc <- 'homeN'
	if( game$wloc == 'H' ) {
		loc <- 'homeA'
	} else if( game$wloc == 'A' ) {
		loc <- 'homeH'
	}

	team2 <- predict.game(team.coefficients, ratings.team2$off, ratings.team2$def, loc, n.sim)


	results$prob[i] <- mean(team1 > team2)
}



write.csv(results, "data/output/regular_season_predicted.csv", row.names = FALSE)






# for season n
n <- results[results$season == 'N',]
log.loss( as.numeric(n$prob >= 0.5), n$prob, nrow(n) )


mean(n$prob > 0.5)

plot(density(n$prob))