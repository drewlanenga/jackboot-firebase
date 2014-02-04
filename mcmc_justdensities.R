
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
espn.seasons <- c(2010)


for( espn.season in espn.seasons )
{
	cat( paste("\n\n NEW SEASON: ", espn.season, sep = "") )

	season.teams <- list.files( paste( "data/output/mcmc", espn.season, sep = "/" ) )

	for( teamid in season.teams )
	{
		cat( paste("\n\t", teamid, sep = "") )


		for( boxscore.predict in names(boxscores.predict) )
		{
			playerids <- list.files( paste( "data/output/mcmc", espn.season, teamid, boxscore.predict, sep = "/" ) )

			cat( paste("\n\t\t", boxscore.predict, "\n\n", sep = "") )

			boxscore.number <- boxscores.predict[[ boxscore.predict ]]

			for( playerid in playerids )
			{
				try
				({
					#colnames(out$betadraw) <- c('int', 'opp.strength_def', 'opp.strength_off')
					model.params <- c('int', 'def', 'off')

					player.model <- read.csv( paste( "data/output/mcmc/", espn.season, teamid, boxscore.predict, playerid, sep = "/") )
					
					# write the coefficients out
					for( j in 1:length(model.params) )
					{
						try({
							model.param <- model.params[j]

							approx <- density.approximate( player.model[,j] )

							insert <- list()
							insert[[ 'season' ]] <- espn.season
							insert[[ 'teamid' ]] <- teamid
							insert[[ 'boxscore' ]] <- boxscore.number
							insert[[ 'playerid' ]] <- playerid
							insert[[ 'model' ]] <- paste("'", model.param, "'", sep = "")
							insert[[ 'density' ]] <- paste("'", approx$dist, "'", sep = "")
							insert[[ 'loss' ]] <- approx$loss
							insert[[ 'params' ]] <-  paste("'", serialize.strip(toJSON(approx$params)), "'", sep = "")

							sql <- paste( "REPLACE INTO `mcmc_densities` (`", paste(names(insert), collapse = "`,`"), "`) VALUES (", paste(insert, collapse = ","), ")", sep = "")
							dbSendQuery( con, sql )
						})
					}
				})
			}
		}
	}
}

