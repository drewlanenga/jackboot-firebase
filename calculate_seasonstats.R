
source('load.R')


espn.schedule <- dbGetData( "SELECT * FROM `schedule`" )



espn.seasons <- 2009:2013

# for each team
for( espn.season in espn.seasons )
{
	cat( paste("\n", espn.season) )

	season.schedule <- espn.schedule[ espn.schedule$year == espn.season, ]
	
	season.teams <- sort( unique( c(season.schedule$homeid, season.schedule$visitid) ) )

	# own teams stuff
	cat( paste("\n\tDEFAULT") )
	for( teamid in season.teams )
	{
		cat( paste("\n\t\t", teamid) )

		team.schedule <- season.schedule[ season.schedule$homeid == teamid | season.schedule$visitid == teamid, ]
		
		home <- team.schedule$homeid == teamid


		points.earned <- team.schedule$visitscore
		points.earned[ home ] <- team.schedule$homescore[ home ]

		points.allowed <- team.schedule$homescore
		points.allowed[ home] <- team.schedule$visitscore[ home ]

		points.margin <- points.earned - points.allowed

		dbSendQuery( con, paste("INSERT IGNORE INTO `strength` (`season`,`teamid`,`points_earned`,`points_allowed`,`points_margin`) VALUES (", espn.season, ",", teamid,",", median(points.earned), ",", median(points.allowed),",", median(points.margin), ")") )
	}

	strength <- dbGetData( paste( "SELECT `season`,`teamid`,`points_earned`,`points_allowed`,`points_margin` FROM `strength` WHERE `season` = ", espn.season, sep = "" ) )

	# opponents stuff
	cat( paste("\n\tOPPONENTS") )
	for( teamid in season.teams )
	{
		cat( paste("\n\t\t", teamid) )

		team.schedule <- season.schedule[ season.schedule$homeid == teamid | season.schedule$visitid == teamid, ]
		
		home <- team.schedule$homeid == teamid

		opp.ids <- team.schedule$homeid
		opp.ids[ home ] <- team.schedule$visitid[ home ]

		opp.strength <- strength[ strength$teamid %in% opp.ids, ]


		dbSendQuery( con, paste("UPDATE `strength` SET `o_points_earned` = ", median(opp.strength$points_earned),", `o_points_allowed` = ", median(opp.strength$points_allowed), ", `o_points_margin` = ", median(opp.strength$points_margin), " WHERE `season` = ", espn.season, " AND `teamid` = ", teamid, sep = "") )

		# opponents opponents stuff -- maybe later
	}

	cat("\n")
}

# create the strength scores -- higher values -> higher strength   (both offensively and defensively)
dbSendQuery( con, "UPDATE `strength` SET `strength_off` = `points_earned` - `o_points_allowed`, `strength_def` = `o_points_earned` - `points_allowed`" )
