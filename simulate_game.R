
mean.min <- function(x) { mean( x$min, na.rm = TRUE ) }

get.draws <- function( player.models, n.draws = 10000 )
{
	draws <- list()

	for( i in 1:nrow(player.models) )
	{
		model <- player.models$model[ i ]
		density <- player.models$density[ i ]
		params <- as.list( fromJSON( player.models$params[ i ] ) )
		
		if( density == 'normal' )
		{
			draws[[ model ]] <- rnorm( n.draws, params$mean, sqrt(params$var) )
		}
		else if( density == 't' )
		{
			draws[[ model ]] <- ( rt( n.draws, params$df ) * sqrt(params$var) ) + params$mean
		}
		else if( density == 'gamma' )
		{
			draws[[ model ]] <- rgamma( n.draws, shape = params$a, rate = params$b )
		}
	}

	return(draws)
}


# note that the simulated teams require ESPN ids

simulate.game <- function( simulate.season, simulate.teams, n.sim = 10000 )
{
	simulate.boxscores <- c('ftm', 'fgm', 'threem')
	simulate.model <- c('int', 'opp.strength_def', 'opp.strength_off')

	# uf -- 57, byu -- 252
	#simulate.teams <- c( 57, 252)



	points <- list()
	for( simulate.team in simulate.teams )
	{
		# find the average mins by player, and scale out of 200 minutes
		
		team.mins <- dbGetData( paste("SELECT `gameid`,`teamid`,`playerid`,`year`,`min` FROM `boxscores` WHERE `gameid` != 0 AND `teamid` = ", simulate.team, " AND `playerid` != 0 AND `year` = ", simulate.season, sep = "") )

		player.mins <- ddply( team.mins, .(playerid), mean.min )
		player.mins$V1 <- 200 * player.mins$V1 / sum(player.mins$V1)


		# get the opponents strengths
		opp.id <- simulate.teams[ simulate.teams != simulate.team ]
		opp.strength <- dbGetData( paste("SELECT `strength_off`,`strength_def` FROM `strength` WHERE `season` = ", simulate.season, " AND `teamid` = ", opp.id, sep = "") )



		# find each players contribution to the point components

		point.components <- list()
		point.components[[ 'ftm' ]] <- rep( 0, n.sim )
		point.components[[ 'fgm' ]] <- rep( 0, n.sim )
		point.components[[ 'threem' ]] <- rep( 0, n.sim )


		for( simulate.boxscore in simulate.boxscores )
		{

			for( i in 1:nrow( player.mins ) )
			{
				playerid <- player.mins$playerid[ i ]

				# see if the player contributes to that boxscore at all
				file.name <- paste( "data/output/mcmc", simulate.season, simulate.team, simulate.boxscore, playerid, sep = "/" )
				if( file.exists( file.name ) )
				{
					# read in the players posterior draws (output from mcmc.R)
					player.posterior.draws <- read.csv( file.name )

					# sample (with replacement) a random number (n.sim) of rows from player.posterior draws
					sample.index <- sample( nrow(player.posterior.draws), n.sim, replace = TRUE )

					# how much the player will contribute (per minute) to the given box score
					predicted.contribution <- player.posterior.draws$int[ sample.index ] + ( player.posterior.draws$opp.strength_def[ sample.index ] * opp.strength$strength_def ) + ( player.posterior.draws$opp.strength_off[ sample.index ] * opp.strength$strength_off )

					# add the current player's contribution to the running team total
					point.components[[ simulate.boxscore ]] <- point.components[[ simulate.boxscore ]] + ( player.mins$V1[ i ] *  predicted.contribution )
				}
			}
		}


		# TODO: figure out best rounding method
		points[[ as.character(simulate.team) ]] <- round( point.components[[ 'ftm' ]] + point.components[[ 'fgm' ]] * 2 + point.components[[ 'threem' ]] * 3 )
	}

	# game outcomes being the difference in poitns between the two teams
	games <- points[[ as.character( simulate.teams[1] ) ]] - points[[ as.character( simulate.teams[2] ) ]]

	return( games )
}


simulate.game.from.density <- function( simulate.season, simulate.teams, n.sim = 10000 )
{
	simulate.boxscores <- c('ftm', 'fgm', 'threem')
	simulate.model <- c('int', 'opp.strength_def', 'opp.strength_off')
	simulate.model <- c('int', 'def', 'off')

	# uf -- 57, byu -- 252
	#simulate.teams <- c( 57, 252)



	points <- list()
	for( simulate.team in simulate.teams )
	{
		# find the average mins by player, and scale out of 200 minutes
		
		team.mins <- dbGetData( paste("SELECT `gameid`,`teamid`,`playerid`,`year`,`min` FROM `boxscores` WHERE `gameid` != 0 AND `teamid` = ", simulate.team, " AND `playerid` != 0 AND `year` = ", simulate.season, sep = "") )

		player.mins <- ddply( team.mins, .(playerid), mean.min )
		player.mins$V1 <- 200 * player.mins$V1 / sum(player.mins$V1)


		# get the opponents strengths
		opp.id <- simulate.teams[ simulate.teams != simulate.team ]
		opp.strength <- dbGetData( paste("SELECT `strength_off`,`strength_def` FROM `strength` WHERE `season` = ", simulate.season, " AND `teamid` = ", opp.id, sep = "") )



		# find each players contribution to the point components

		point.components <- list()
		point.components[[ 'ftm' ]] <- rep( 0, n.sim )
		point.components[[ 'fgm' ]] <- rep( 0, n.sim )
		point.components[[ 'threem' ]] <- rep( 0, n.sim )


		for( h in 1:length(simulate.boxscores) )
		{
			simulate.boxscore <- simulate.boxscores[ h ]
			for( i in 1:nrow( player.mins ) )
			{
				playerid <- player.mins$playerid[ i ]

				# see if the player contributes to that boxscore at all
				# season, teamid, boxscore, playerid, model, density, loss, params
				player.models <- dbGetData( paste( "SELECT `model`,`density`,`params` FROM `mcmc_densities` WHERE `season` = ", simulate.season, " AND `teamid` = ", simulate.team, " AND `boxscore` = ", h, " AND `playerid` = ", playerid, sep = "" ) )
				if( nrow(player.models) > 0 )
				{
					#cat(paste("\n\t\t", playerid))

					for( j in 1:nrow(player.models) )
					{
						player.posterior.draws <- get.draws( player.models, n.sim )

						# how much the player will contribute (per minute) to the given box score
						predicted.contribution <- player.posterior.draws$int + ( player.posterior.draws$def * opp.strength$strength_def ) + ( player.posterior.draws$off * opp.strength$strength_off )

						#print(head(predicted.contribution * player.mins$V1[i]))

						# add the current player's contribution to the running team total
						point.components[[ simulate.boxscore ]] <- point.components[[ simulate.boxscore ]] + ( player.mins$V1[ i ] *  predicted.contribution )
					}
				}
			}
		}


		# TODO: figure out best rounding method
		points[[ as.character(simulate.team) ]] <- round( point.components[[ 'ftm' ]] + point.components[[ 'fgm' ]] * 2 + point.components[[ 'threem' ]] * 3 )
	}

	# game outcomes being the difference in poitns between the two teams
	games <- points[[ as.character( simulate.teams[1] ) ]] - points[[ as.character( simulate.teams[2] ) ]]

	return( games )
}


#games <- simulate.game( 2009, c(2006, 2011), 10000 )

#
# run one for kicks
#

run.one <- FALSE

if ( run.one )
{
	simulate.season <- 2009
	simulate.teams <- c( 57, 252 )

	games <- simulate.game( simulate.season, simulate.teams, 10000 )



	# probability team 1 won
	mean( games > 0 )

	# probability of a tie
	mean( games == 0 )

	# probability team 1 lost
	mean( games < 0 )


	#
	# plots
	#

	# create a new graphics device
	dev.new()

	# adjust device paramters -- two plot rows, one plot column
	par(mfrow = c(2,1))


	# plot that makes you feel good
	team1.density <- density( points[[ as.character( simulate.teams[1] ) ]] )
	team2.density <- density( points[[ as.character( simulate.teams[2] ) ]] )


	x.min <- min( min(team1.density$x), min(team2.density$x) )
	x.max <- max( max(team1.density$x), max(team2.density$x) )

	y.min <- min( min(team1.density$y), min(team2.density$y) )
	y.max <- max( max(team1.density$y), max(team2.density$y) )

	plot( team1.density, xlim = c(x.min, x.max), ylim = c(y.min, y.max), main = paste("Points for Team", simulate.teams[1], "vs. Team", simulate.teams[2]) )
	lines( team2.density, col = 'red' )
	legend( 'topright', legend = c( paste('Team ', simulate.teams[1]), paste('Team ', simulate.teams[2]) ), col = c('black', 'red'), lty = 1, lwd = 3 )


	# histogram of distribution of game differences
	hist( games, breaks = length(unique(games)), main = paste('Margin for Team', simulate.teams[1], 'vs. Team', simulate.teams[2]), xlab = 'Point Margin' )
}
