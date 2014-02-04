
source('load.R')

library(plyr)

# from http://en.wikipedia.org/wiki/Offensive_rating
# 100 x Pts / (Tm FGA + .40 x Tm FTA - 1.07 x (Tm ORB / (Tm ORB + Tm DRB)) x (Tm FGA - Tm FG) + Tm TO)

colSums.boxscores <- function(x) { colSums( x[ , 5:ncol(x)], na.rm = TRUE ) }

calculate.ratings <- function( team.boxscores )
{
	# get sums across players
	x <- ddply(team.boxscores, .(gameid), colSums.boxscores)

	# oliver's rating / 100
	oliver <- x$pts / ( x$fga + (0.4 * x$fta) - (1.07 * (x$oreb / (x$oreb + x$dreb)) * (x$fga - x$fgm) ) + x$to )
	oliver[ !is.finite(oliver) ] <- 0

	# points per possession
	ppp <- x$pts / ( (x$fga + x$threea) )
	ppp[ !is.finite(ppp) ] <- 0

	# winning margin
	#margin <- 

	names(oliver) <- names(ppp) <- x$gameid


	# reduce by game id (this method deflates ratings with players on the bench)
	#oliver.gameid <- tapply(oliver, team.boxscores$gameid, mean, na.rm = TRUE)
	#ppp.gameid <- tapply(ppp, team.boxscores$gameid, mean, na.rm = TRUE)

	return( list(oliver = oliver, ppp = ppp) )
}



# get the schedule from espn

espn.schedule <- dbGetData( "SELECT * FROM `schedule`" )


# each season

for( espn.season in 2009:2013 )
{
	cat(espn.season)

	season.schedule <- espn.schedule[ espn.schedule$year == espn.season, ]
	season.teams <- sort( unique( c(season.schedule$homeid, season.schedule$visitid) ) )

	# each team

	for( teamid in season.teams )
	{
		cat( paste("\n\t", teamid) )

		team.boxscores <- dbGetData( paste("SELECT * FROM `boxscores` WHERE `gameid` != 0 AND `teamid` = ", teamid, " AND `playerid` != 0 AND `year` = ", espn.season, sep = "" ) )
		team.boxscores$gameid <- as.character(team.boxscores$gameid)


		team.ratings <- calculate.ratings(team.boxscores)

		for( gameid in names(team.ratings$oliver) )
		{
			dbSendQuery( con, paste("INSERT IGNORE INTO `ratings` (`gameid`,`teamid`,`oliver`,`ppp`, `margin`) VALUES (", gameid, ",", teamid, ",'", team.ratings$oliver[[ gameid ]], "','", team.ratings$ppp[[ gameid ]], "') ") )
		}
	}

	cat("\n")
}




ratings.ppp <- dbGetData( "SELECT `ppp` FROM `ratings` ORDER BY `gameid`, `teamid`" )
ratings.ppp_sum <- dbGetData( "SELECT `ppp` FROM `ratings_sum` ORDER BY `gameid`, `teamid`" )

