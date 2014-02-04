
source('load.R')

library(bayesm)
col.boxscores <- function(x) { colSums( x[ , 5:ncol(x)], na.rm = TRUE ) }


espn.schedule <- dbGetData( "SELECT * FROM `schedule`" )
ratings <- dbGetData("SELECT * FROM `ratings`")

strength <- dbGetData("SELECT * FROM `strength`")
for(i in 3:ncol(strength))
{
	strength[ , i ] <- as.numeric( strength[ , i ] )
}



# whether or not to skip
overwrite.output <- FALSE



n.mcmc <- 10000

# columns in the boxscore -- scale these by minute later
boxscores.index <- 6:19

# the only ones worth predicting (the ones that contribute to the actual score)
boxscores.predict <- list('ftm' = 1, 'fgm' = 2, 'threem' = 3)

# espn.seasons <- 2009:2013
espn.seasons <- c(2009)

for( espn.season in espn.seasons )
{
	cat( paste("\n\n NEW SEASON: ", espn.season, sep = "") )

	season.schedule <- espn.schedule[ espn.schedule$year == espn.season, ]
	season.strength <- strength[ strength$season == espn.season, ]

	season.teams <- sort( unique( c(season.schedule$homeid, season.schedule$visitid) ) )

	for( teamid in season.teams )
	{
		cat( paste("\n\t", teamid, sep = "") )


		team.schedule <- season.schedule[ season.schedule$homeid == teamid | season.schedule$visitid == teamid, ]
		home <- team.schedule$homeid == teamid

		# extract the opponents from the schedule
		opp.ids <- team.schedule$homeid
		opp.ids[ home ] <- team.schedule$visitid[ home ]


		# get team's own boxscores
		team.boxscores <- dbGetData( paste( "SELECT * FROM `boxscores` WHERE `gameid` != 0 AND `teamid` = ", teamid, " AND `playerid` != 0 AND `year` = ", espn.season, sep = "" ) )
		
		# skip if there aren't any boxscores
		if( nrow(team.boxscores) == 0 )
			next

		# scale per minute of play
		team.boxscores[ , boxscores.index ] <- team.boxscores[ , boxscores.index ] / team.boxscores$min

		# get rid of 0 minute contributors
		team.boxscores <- team.boxscores[ team.boxscores$min > 0, ]
		
		# append strength stats to boxscores
		matched <- match( team.boxscores$gameid, team.schedule$gameid )
		team.boxscores$oppid <- opp.ids[ matched ]

		matched <- match( team.boxscores$oppid, season.strength$teamid )
		team.boxscores$opp.strength_off <- season.strength$strength_off[ matched ]
		team.boxscores$opp.strength_def <- season.strength$strength_def[ matched ]


		playerids <- sort(unique( team.boxscores$playerid ))


		for( boxscore.predict in names(boxscores.predict) )
		{
			cat( paste("\n\t\t", boxscore.predict, "\n\n", sep = "") )

			boxscore.number <- boxscores.predict[[ boxscore.predict ]]

			try
			({
				# see if there are any files in the output directory.  if so, skip it
				if( !overwrite.output )
				{
					already.exists <- dbGetData( paste( "SELECT COUNT(*) AS `count` FROM `mcmc_densities` WHERE `season` = ", espn.season, " AND `teamid` = ", teamid, " AND `boxscore` = ", boxscore.number, sep = "") )
					
					if( already.exists$count[1] > 0 )
					{
						next
					}
				}

				i <- 1
				reg.data <- list()

				for( playerid in playerids )
				{
					player.boxscores <- team.boxscores[ team.boxscores$playerid == playerid, ]

					# see if the player actually contributed to that boxscore
					if( sum( player.boxscores[[ boxscore.predict ]] ) > 0 )
					{
						y <- player.boxscores[[ boxscore.predict ]]
						X <- as.matrix(cbind( 1, player.boxscores[ , c('opp.strength_def', 'opp.strength_off') ] ))

						reg.data[[ i ]] <- list( y = y, X = X, playerid = playerid )

						i <- i + 1
					}
				}

				# some stupid tiny edge teams require checking to see if there's any data for the model
				if( length( reg.data ) > 0 )
				{
					# run it!  (read up on Z stuff later)
					mcmc <- list( R = n.mcmc, keep = 1 )
					out <- rhierLinearModel( Data = list(regdata = reg.data), Mcmc = mcmc )

					#colnames(out$betadraw) <- c('int', 'opp.strength_def', 'opp.strength_off')
					model.params <- c('int', 'def', 'off')
					# write the coefficients out
					for( i in 1:length(reg.data) )
					{
						for( j in 1:length(model.params) )
						{
							model.param <- model.params[j]

							approx <- density.approximate( out$betadraw[i,j,] )

							insert <- list()
							insert[[ 'season' ]] <- espn.season
							insert[[ 'teamid' ]] <- teamid
							insert[[ 'boxscore' ]] <- boxscore.number
							insert[[ 'playerid' ]] <- reg.data[[ i ]]$playerid
							insert[[ 'model' ]] <- paste("'", model.param, "'", sep = "")
							insert[[ 'density' ]] <- paste("'", approx$dist, "'", sep = "")
							insert[[ 'loss' ]] <- approx$loss
							insert[[ 'params' ]] <-  paste("'", serialize.strip(toJSON(approx$params)), "'", sep = "")

							sql <- paste( "REPLACE INTO `mcmc_densities` (`", paste(names(insert), collapse = "`,`"), "`) VALUES (", paste(insert, collapse = ","), ")", sep = "")
							print(sql)
							dbSendQuery( con, sql )
						}
					}
				}
			})

		}
	}
}

