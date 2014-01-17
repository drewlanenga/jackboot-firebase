
source('load.R')

library(plyr)


#
# get the overall off/def ratings for each team
#

#
#
#
#	point spread method
#
#
#

results$gameid <- 1:nrow(results)
results$spread <- results$wscore - results$lscore

win <- results[, c('season', 'gameid', 'daynum', 'wteam', 'wscore', 'spread')]
lose <- results[, c('season', 'gameid', 'daynum', 'lteam', 'lscore', 'spread')]

lose$spread <- -lose$spread

names(win) <- names(lose) <- c('season', 'gameid', 'daynum', 'teamid', 'points', 'spread')
transformed <- rbind.data.frame( win, lose )

transformed$win <- transformed$spread > 0


f <- function(x)
{
	offensive <- mean(x$spread > 0) * mean(x$spread)
	defensive <- mean(x$spread < 0) * mean(-x$spread)
	
	return( c(offensive, defensive) )
}


# ddply generates aggregate stats, applies function `f` to groups of season and teamid

ratings <- ddply(transformed, .(season, teamid), f)
names(ratings) <- c('season', 'teamid', 'off', 'def')

write.csv(ratings, file = "data/output/ratings_spread.csv", row.names = FALSE)



