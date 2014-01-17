
#
#  from kaggle
#

teams <- read.csv("data/kaggle/teams.csv")
seasons <- read.csv("data/kaggle/seasons.csv")
results <- read.csv("data/kaggle/regular_season_results.csv")

tourney.results <- read.csv("data/kaggle/tourney_results.csv")
tourney.seeds <- read.csv("data/kaggle/tourney_seeds.csv")
tourney.slots <- read.csv("data/kaggle/tourney_slots.csv")

# as.is prevents R from making the id field a factor -- you can't strsplit factors
sample.submission <- read.csv("data/kaggle/sample_submission.csv", as.is = TRUE)


#
# from espn
#

espn.boxscores <- read.csv("data/espn/boxscores.csv")
espn.schedule <- read.csv("data/espn/schedule.csv")
espn.teams <- read.csv("data/espn/teams.csv")



#
#  from drew
#

ratings.spread <- read.csv("data/output/ratings_spread.csv")
coefficients <- read.csv("data/output/coefficients.csv")
