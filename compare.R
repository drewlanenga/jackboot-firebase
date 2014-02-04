sub.mcmc <- dbGetData( "SELECT `id`,`pred` FROM `submission_mcmc`" )
sub.dens <- dbGetData( "SELECT `id`,`pred` FROM `submission_densities`" )
sub.nona <- dbGetData( "SELECT `id`,`pred` FROM `submission_densities_nonapprox`" )

combined <- cbind.data.frame(sub.mcmc, sub.dens$pred, sub.nona$pred)
names(combined) <- c('id', 'mcmc', 'dens', 'nona')


combined$diff <- combined[, 2] - combined[, 4]
combined$wrong <- (combined[, 2] > 0.5) != (combined[, 4] > 0.5)

sub.mcmc$pred <- as.numeric(sub.mcmc$pred)
sub.dens$pred <- as.numeric(sub.dens$pred)
sub.nona$pred <- as.numeric(sub.nona$pred)

sum( abs(sub.mcmc$pred - sub.dens$pred ) > 0.17)

which(abs(sub.mcmc$pred - sub.dens$pred) > 0.17)


sub.mcmc.density <- density(sub.mcmc$pred, from = 0, to = 1)
sub.dens.density <- density(sub.dens$pred, from = 0, to = 1)
sub.nona.density <- density(sub.nona$pred, from = 0, to = 1)

plot(sub.mcmc.density, ylim = c(0,2))
lines(sub.dens.density, col = 'red')
lines(sub.nona.density, col = 'blue')


hist(sub.nona$pred, breaks = 101)

sub.mcmc.norm <- abs( sub.mcmc$pred - 0.5 )
sub.dens.norm <- abs( sub.mcmc$pred - 0.5 )
sub.nona.norm <- abs( sub.mcmc$pred - 0.5 )

sub.dens$pred[1846]

