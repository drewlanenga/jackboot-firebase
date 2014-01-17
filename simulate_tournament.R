
source('load.R')

#
#  functions to help with the 'simulation'
#

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




#
#
#  find the probability that team1 beats team2 for each entry in the sample submission
#
#


submission <- sample.submission

for(i in 1:nrow(sample.submission))
{
	print(i)
	ids <- strsplit(sample.submission$id[i], '_')

	season.letter <- ids[[1]][1]
	team.ids <- as.numeric(ids[[1]][2:3])


	ratings.team1 <- ratings.spread[ ratings.spread$season == season.letter & ratings.spread$teamid == team.ids[1], ]
	ratings.team2 <- ratings.spread[ ratings.spread$season == season.letter & ratings.spread$teamid == team.ids[1], ]

	coefficients.team1 <- coefficients[ coefficients$season == season.letter & coefficients$teamid == team.ids[2], ]
	coefficients.team2 <- coefficients[ coefficients$season == season.letter & coefficients$teamid == team.ids[2], ]


	#
	# team1
	#

	team1 <- predict.game(coefficients.team1, ratings.team2$off, ratings.team2$def, 'homeN', n.sim)


	#
	# team2
	#

	team2 <- predict.game(team.coefficients, ratings.team2$off, ratings.team2$def, 'homeN', n.sim)


	# add to output matrix
	submission[i, 2] <-  mean(team1 > team2)
}

write.csv(submission, "data/output/submission.csv", row.names = FALSE, col.names = TRUE)

