# Jackboot Firebase

##  UPDATE

I never learned how to read.

Apparently, you can (and are encouraged) to supplement [data sources](http://www.kaggle.com/c/march-machine-learning-madness/data), but you are [discouraged](http://www.kaggle.com/c/march-machine-learning-madness/rules) from sharing private data outside of teams.

So I scraped ESPN for all regular season data for the past five years, which can be found in `data/espn/`.


### Loading Data

Source the `load.R` script.


### Current Model

1. Generate the offensive and defensive rating for each team in each season.  (`calculate_ratings.R`)
2. Generate a linear model (currently via least squares, in `calculate_coefficients.R`) as defined by:
``points ~ offensive rating + defensive rating + location``
3. Simulate each game by selecting 10,000 random draws from the distributions of the model coefficients, and compare pairwise points as games.
