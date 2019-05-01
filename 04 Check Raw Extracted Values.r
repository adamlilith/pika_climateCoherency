### Ochotona princeps - Calculate derived climate variables
### Adam B. Smith | 2016-08
###
### Calculate climate variables for use in the main analysis of pika range limits.  Climate variables come from the Workbook listing alternative mechanisms of range limitation.

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/04 Check Raw Extracted Values.r')

rm(list=ls())

source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')

### CONTENTS ###

x <- readRDS('')

vars <- c('cellArea', 'elev', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax', 'dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0')

### cell area
#############

par(mfrow=c(1, 3))

hist(x$cellAreaPrism, main='Cell Area PRISM')
hist(x$cellAreaGmted2010, main='Cell Area GMTED2000')
hist(x$cellAreaDayMet, main='Cell Area DayMet')

### elevation
#############

par(mfrow=c(1, 2))

hist(x$elevPrism, main='Elevation PRISM')
hist(x$elevGmted2010, main='Elevation GMTED2000')

### annual values by year
#########################

sink(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/Raw Variables - Across Usable Presences 1990-2015 - Climate - Annual.txt'))

	for (v in c('ppt', 'tmin', 'tmax', 'vpdmin', 'vpdmax', 'tdmean', 'dobrowskiSwe')) {

		columns <- paste0(v, 'Annual_yearMinus00')

		years <- sort(unique(x$obsYear))
		years <- years[years >= 1980]
		
		for (oy in years) {

			y <- x[which(x$obsYear %in% oy), columns]
			
			say(v, ' ', oy,
				'  min=', suffix(prefix(round(min(y, na.rm=T), 1), 5, pad=' '), 5, pad=' '),
				'  mean=', suffix(prefix(round(mean(y, na.rm=T), 1), 5, pad=' '), 5, pad=' '),
				'  max=', suffix(prefix(round(max(y, na.rm=T), 1), 5, pad=' '), 5, pad=' ')
			)
			
		}
		say('', post=2)
		
	}

sink()

### monthly values by month
###########################
sink(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/Raw Variables - Across Usable Presences 1990-2015 - Climate - Monthly.txt'))

	for (v in c('ppt', 'tmin', 'tmax', 'vpdmin', 'vpdmax', 'tdmean', 'dayMetSwe', 'dobrowskiSwe')) {

		columns <- paste0(v, 'Monthly_yearMinus00_month', prefix(1:12, 2))

		years <- sort(unique(x$obsYear))
		years <- years[years >= 1990]
		
		for (oy in years) {

			for (mo in 1:12) {
				
				y <- x[x$obsYear==oy, paste0(v, 'Monthly_yearMinus00_month', prefix(mo, 2))]
				
				if (!is.null(y)) {
					
					say(v, ' ', oy, ' ', mo, ' ', ' n=', sum(!is.na(y)),
						'  min=', suffix(prefix(round(min(y, na.rm=T), 1), 5, pad=' '), 5, pad=' '),
						'  mean=', suffix(prefix(round(mean(y, na.rm=T), 1), 5, pad=' '), 5, pad=' '),
						'  max=', suffix(prefix(round(max(y, na.rm=T), 1), 5, pad=' '), 5, pad=' ')
					)
					
				}
			
			}
			
		}
		say('', post=2)
		
	}
	
sink()

### daily values by month
#########################
sink(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/Raw Variables - Across Usable Presences 1990-2015 - Climate - Daily.txt'))

	for (v in c('ppt', 'tmin', 'tmax', 'dayMetSwe', 'dobrowskiET0')) {

		columns <- paste0(v, 'Daily_yearMinus00_doy', prefix(1:366, 3))

		years <- sort(unique(x$obsYear))
		years <- years[years >= 1990]
		
		for (yr in years) {
		
			y <- x[x$obsYear == oy, ]
		
			minVal <- min(y[ , columns], na.rm=T)
			maxVal <- max(y[ , columns], na.rm=T)
			
			plot(1:366, 1:366, col='white', xlab='DOY', ylim=c(minVal, maxVal))
			for (i in 1:nrow(y)) lines(1:366, y[i, columns])
		
		}

		
		for (oy in years) {

			for (mo in 1:12) {
				
				y <- x[x$obsYear==oy, paste0(v, 'Monthly_yearMinus00_doy', prefix(mo, 3))]
				
				if (!is.null(y)) {
					
					say(v, ' ', oy, ' ', mo, ' ', ' n=', sum(!is.na(y)),
						'  min=', suffix(prefix(round(min(y, na.rm=T), 1), 5, pad=' '), 5, pad=' '),
						'  mean=', suffix(prefix(round(mean(y, na.rm=T), 1), 5, pad=' '), 5, pad=' '),
						'  max=', suffix(prefix(round(max(y, na.rm=T), 1), 5, pad=' '), 5, pad=' ')
					)
					
				}
			
			}
			
		}
		say('', post=2)
		
	}
	
sink()


### precip: annual
##################

columns <- paste0('pptAnnual_yearMinus', prefix(9:0, 2))
par(mfrow=c(2, 5))
for (i in seq_along(columns)) hist(x[ , columns[i]], main=columns[i], breaks=seq(0, 6000, length.out=20))

### tmin: annual
##################

columns <- paste0('tminAnnual_yearMinus', prefix(9:0, 2))
par(mfrow=c(2, 5))
for (i in seq_along(columns)) hist(x[ , columns[i]], main=columns[i])

### tmax: annual
##################

columns <- paste0('tmaxAnnual_yearMinus', prefix(9:0, 2))
par(mfrow=c(2, 5))
for (i in seq_along(columns)) hist(x[ , columns[i]], main=columns[i])

### monthly
###########
for (v in c('ppt', 'tmin', 'tmax', 'vpdmin', 'vpdmax', 'tdmean')) {
	
	columns <- paste0(v, 'Monthly_yearMinus', rep(prefix(10:0, 2), each=12), '_month', prefix(1:12, 2))
	
	low <- apply(x[ , columns], 2, min, na.rm=T)
	mean <- apply(x[ , columns], 2, mean, na.rm=T)
	high <- apply(x[ , columns], 2, max, na.rm=T)
	
	windows(11, 9)
	par(mfrow=c(1, 1))
	all <- c(low, mean, high)
	all <- all[-which(is.na(all))]
	all <- all[-which(is.infinite(all))]

	plot(seq_along(low), low, type='l', main=v, ylab=v, ylim=c(min(all), max(all))); text(1:(11*12), low, labels=apply(x[ , columns], 2, FUN=function(u) sum(!is.na(u))), col='blue')
	
	lines(seq_along(mean), mean, type='l', main=v); text(seq_along(mean), mean, labels=apply(x[ , columns], 2, FUN=function(u) sum(!is.na(u))), col='green')
	lines(seq_along(high), high, type='l', main=v); text(seq_along(high), high, labels=apply(x[ , columns], 2, FUN=function(u) sum(!is.na(u))), col='red')

}

