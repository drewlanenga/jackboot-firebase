
source('load.R')
source('simulate_game.R')


check.lock <- function( lock.file )
{
	return( scan( lock.file, quiet = TRUE ) == 1 )
}

write.lock <- function( lock.file, locked = 0 )
{
	write( locked, lock.file )
}

write.csv.lock <- function(data, filename)
{
	lock.file <- paste(filename, '.lock', sep = "")

	if( !file.exists(lock.file) )
		write(0, lock.file)

	locked <- check.lock( lock.file )

	# if it is locked, wait for it to unlock
	if( locked )
	{
		while( locked )
		{
			Sys.sleep( 0.1 )

			locked <- check.lock( lock.file )
		}
	}

	# lock that beast up!
	write.lock( lock.file, 1 )

	# write it
	write.csv( data, filename, row.names = FALSE)

	# unlock it
	write.lock( lock.file, 0 )
}

read.csv.lock <- function( filename )
{
	lock.file <- paste(filename, '.lock', sep = "")

	locked <- check.lock( lock.file )

	# if it is locked, wait for it to unlock
	if( locked )
	{
		while( locked )
		{
			Sys.sleep( 0.1 )

			locked <- check.lock( lock.file )
		}
	}

	# lock it up
	write.lock( lock.file, 1 )

	# read it
	data <- read.csv( filename, as.is = TRUE )

	# unlock it
	write.lock( lock.file, 0 )

	return(data)
}






seasons <- list('N' = 2009, 'O' = 2010, 'P' = 2011,'Q' = 2012,'R' = 2013)

submission.file <- "data/output/submission_mcmc.csv"
submission <- read.csv.lock( submission.file )



i <- 1
while( i <= nrow(submission) )
{
	# re-read it in to check status of other processes
	submission <- read.csv.lock( submission.file )

	# which rows still need to be predicted
	to.do <- which( submission[, 2] == 0 )

	if( length( to.do ) == 0 )
	{
		i <- nrow(submission + 5)
	}
	else
	{
		i <- sample( to.do, 1)
		
		# mark it as taken
		print( i )
		submission[i, 2] <- -1
		write.csv.lock(submission, submission.file)


		ids <- strsplit(submission$id[i], '_')

		season.letter <- ids[[1]][1]
		kaggle.ids <- as.numeric(ids[[1]][2:3])


		team.ids <- dbGetData( paste("SELECT `teamid`, `kaggleid` FROM `teams_matched` WHERE `kaggleid` IN (", paste( kaggle.ids, collapse = ","), ")", sep = "") )

		if( nrow(team.ids) < 2 )
		{
			print( paste("NOT ENOUGH DATA: ", submission$id[i]) )

			# mark it as bad
			submission[i, 2] <- NA
			write.csv.lock( submission, submission.file )

			i <- i + 1

			next
		}



		# sort them in the right order (kaggle ascending)
		simulate.teamids <- team.ids$teamid[ order(team.ids$kaggleid) ]
		games <- simulate.game( seasons[[ season.letter ]], simulate.teamids, n.sim = 10000 )


		# add to output matrix ( re-read it in to check status of other processes )
		submission <- read.csv.lock( submission.file )
		submission[i, 2] <-  mean(games > 0)

		# write it out every time
		write.csv.lock( submission, submission.file )

	}

	#i <- i + 1
}


# write.csv(submission, "data/output/submission_mcmc.csv", row.names = FALSE, col.names = TRUE)
