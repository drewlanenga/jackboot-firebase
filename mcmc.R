
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
boxscores.predict <- c('ftm', 'fgm', 'threem')

# espn.seasons <- 2009:2013
espn.seasons <- c(2009)

for( espn.season in espn.seasons )
{
	cat( paste("\n\n NEW SEASON: ", espn.season, sep = "") )
	# create output directory
	dir.create( paste("data/output/mcmc", espn.season, sep =  "/"), showWarnings = FALSE)

	season.schedule <- espn.schedule[ espn.schedule$year == espn.season, ]
	season.strength <- strength[ strength$season == espn.season, ]

	season.teams <- sort( unique( c(season.schedule$homeid, season.schedule$visitid) ) )

	for( teamid in season.teams )
	{
		cat( paste("\n\t", teamid, sep = "") )

		# create output directory
		dir.create( paste("data/output/mcmc", espn.season, teamid, sep =  "/"), showWarnings = FALSE)


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


		for( boxscore.predict in boxscores.predict )
		{
			cat( paste("\n\t\t", boxscore.predict, "\n\n", sep = "") )

			try
			({
				# create output directory
				dir.create( paste("data/output/mcmc", espn.season, teamid, boxscore.predict, sep =  "/"), showWarnings = FALSE)

				# see if there are any files in the output directory.  if so, skip it
				if( !overwrite.output )
				{
					files <- list.files( paste("data/output/mcmc", espn.season, teamid, boxscore.predict, sep =  "/") )
					if( length(files) > 0 )
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

					colnames(out$betadraw) <- c('int', 'opp.strength_def', 'opp.strength_off')
					out$betadraw <- round( out$betadraw, 8 )

					# write the coefficients out
					for( i in 1:length(reg.data) )
					{
						file.name <- paste("data/output/mcmc", espn.season, teamid, boxscore.predict, reg.data[[i]]$playerid , sep = "/")
						print(file.name)

						write.csv( t(out$betadraw[i,,]), file.name, row.names = FALSE )
					}
				}
			})

		}
	}
}

