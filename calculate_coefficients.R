
source('load.R')




#
# get the model coefficients for each team
#



n.rows <- length(unique(results$season)) + length(unique(c( results$wteam, results$lteam )))
models <- as.data.frame( matrix( NA, nrow = n.rows, ncol = 15 ) )
names(models) <- c('season', 'teamid', 'df', 'int.x', 'int.se', 'opp_or.x', 'opp_or.se', 'opp_dr.x', 'opp_dr.se', 'homeH.x', 'homeH.se', 'homeA.x', 'homeA.se', 'homeN.x', 'homeN.se' )


# each season
i <- 1
for(season.letter in unique(results$season))
{
	print(i)

	ratings <- results.ratings[ results.ratings$season == season.letter, ]

	season <- results[ results$season == season.letter, ]

	# each team in the season
	for(  team in unique( c(season$wteam, season$lteam) )    )
	{
		schedule <- season[ which(season$wteam == team | season$lteam == team) , ]



		# daynum, oppid, opp_or, opp_dr, points, loc, numot
		games <- schedule[, c('daynum', 'wteam', 'wscore', 'wscore', 'wscore', 'wloc', 'numot')]
		names(games) <- c('daynum', 'opp_id', 'opp_dr', 'opp_or', 'points', 'home', 'numot')

		won <- which( schedule$wteam == team )
		lost <- which( schedule$wteam != team )

		games$opp_id[ won ] <- schedule$lteam[ won ]
		games$points[ lost ] <- schedule$lscore[ lost ]


		# match up off/def ratings

		matched <- match( games$opp_id, ratings$teamid )

		games$opp_or <- ratings$off[ matched ]
		games$opp_dr <- ratings$def[ matched ]


		#
		#
		#  generate the model
		#
		#

		mod <- lm( points ~ opp_or + opp_dr + home, data = games)

		model.coefficients <- coefficients(summary(mod))

		row <- rep(NA, ncol(models))

		row[1:3] <- c( season.letter, team, nrow(games) - 1 )
		row[4:5] <- model.coefficients[1, 1:2]

		type.map <- list('opp_or' = 6:7, 'opp_dr' = 8:9, 'homeH' = 10:11, 'homeA' = 12:13, 'homeN' = 14:15)
		j <- 1
		for(type in names(type.map))
		{
			which.type <- which(type == rownames(model.coefficients))

			if( length(which.type) )
			{
				row[ type.map[[ type ]] ] <- model.coefficients[which.type, 1:2]
			}

			j <- j + 1
		}

		models[i, ] <- row

		i <- i + 1
	}
}

# make the columns numeric
for(i in 2:ncol(models))
{
	models[, i] <- as.numeric( models[, i] )
}


# write the coefficients out
write.csv(models, "data/output/coefficients.csv", row.names = FALSE)
