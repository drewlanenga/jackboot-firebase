
source('load.R')
source('simulate_game.R')


seasons <- list('N' = 2009, 'O' = 2010, 'P' = 2011,'Q' = 2012,'R' = 2013)



submission.table <- "submission"

done <- FALSE
while( !done )
{
	# find one to do
	to.do <- dbGetData( paste("SELECT `id` FROM `", submission.table, "` WHERE `pred` = '0' ORDER BY RAND() LIMIT 1", sep = "") )

	if( nrow(to.do) == 0 )
	{
		done <- TRUE
	}
	else
	{
		id <- to.do$id[1]
		print(id)

		dbSendQuery( con, paste("UPDATE `", submission.table, "` SET `pred` = -1 WHERE `id` = '", id, "'", sep = "" ) )
	
		ids <- strsplit(id, '_')

		season.letter <- ids[[1]][1]
		kaggle.ids <- as.numeric(ids[[1]][2:3])


		team.ids <- dbGetData( paste("SELECT `teamid`, `kaggleid` FROM `teams_matched` WHERE `kaggleid` IN (", paste( kaggle.ids, collapse = ","), ")", sep = "") )

		if( nrow(team.ids) < 2 )
		{
			print( paste("NOT ENOUGH DATA: ", id) )

			# mark it as bad
			dbSendQuery( con, paste("UPDATE `", submission.table, "` SET `pred` = -99 WHERE `id` = '", id, "'", sep = "" ) )
			next
		}



		# sort them in the right order (kaggle ascending)
		simulate.teamids <- team.ids$teamid[ order(team.ids$kaggleid) ]
		games <- simulate.game( seasons[[ season.letter ]], simulate.teamids, n.sim = 10000 )

		# update db
		result <- mean(games > 0)
		if(result == 0)
			result <- '0.0000'
		dbSendQuery( con, paste("UPDATE `", submission.table, "` SET `pred` = '", result, "' WHERE `id` = '", id, "'", sep = "" ) )

	}
}


# write.csv(submission, "data/output/submission_mcmc.csv", row.names = FALSE, col.names = TRUE)
