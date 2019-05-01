calcDerivedVars <- function(x, window=10, rawCol=NULL, verbose=TRUE) {
# calcDerivedVars 
#
# ARGUMENTS
# x			Data frame with raw climate variables
# window	Integer >= 0, number of years prior to observation over which to calculate climate variables
# rawCol	Either:
#				Integer(s) indicating column names of raw climate data. If supplied then these columns will be removed from the output.
#				NULL. Do not remove raw data columns (include them in the output).
# verbose	Logical, if TRUE then displays progress
#
# VALUES
# 
# 
# REQUIRED DEPENDANCIES
#
#
# OPTIONAL DEPENDANCIES
#
#
# BAUHAUS
# 
#
# EXAMPLE
# FUNCTION()
#
# SOURCE	source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/Function - Calculate Derived Climate Variables.r')
#
# TESTING
#
#
# LICENSE
# This document is copyright ©2014 by Adam B. Smith.  This document is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.  This document is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. Copies of the GNU General Public License versions are available at http://www.R-project.org/Licenses/.
#
# AUTHOR	Adam B. Smith | Missouri Botanical Garden, St. Louis, Missouri | adamDOTsmithATmobotDOTorg
# DATE		
# REVISIONS 

#################################
### FUNCTIONS AND DEFINITIONS ###
#################################

source('C:/ecology/Drive/R/!Omnibus.r')

cv <- function(x) sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE)

out <- if (is.null(rawCol)) {
	x
} else {
	x[ , -rawCol]
}

library(weathermetrics)

############
### MAIN ###
############

	### acute heat
	##############

	if (verbose) say('Calculating acute heat (runs of 5 days or more with maximum temperature >=25.5 deg C)... [3 variables]')

		# function to calculate these metrics for a single time series of temperatures
		acuteHeat <- function(x, crit=25.5, runLength=5) {
		
			# x		list of temperature values across <window> yr prior to record observation
			# crit	critical value of temperature
			# runLenth	number of days temperature must be >= to count as a "run"
			
			x <- x[-which(is.na(x))]
			hotDays <- x >= crit
			
			# total number of hot days
			numHeatDays <- sum(hotDays)
			
			# number of runs of hot days
			runs <- rle(hotDays)
			numHeatRuns <- sum(runs$lengths[runs$values==1] >= runLength)
			
			# length of longest run
			heatRunLongest <- longRun(hotDays, val=1)
			
			out <- c(
				numHeatDays,
				numHeatRuns=numHeatRuns,
				heatRunLongest=heatRunLongest
			)
			
			names(out) <- c('numHeatDays', 'numHeatRuns', 'heatRunLongest')
			return(out)
			
		}
		
		# get appropriate columns
		colNames <- paste0('tmaxDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))
		
		# calculate acute heat indices (columns are records, rows variables)
		heat <- apply(X=x[ , colNames], MARGIN=1, FUN=acuteHeat)
		
		out$acuteHeat_days <- heat[rownames(heat)=='numHeatDays', ] # number of days in last 10 yr with maximum temperature >= 25pt5C deg C
		out$acuteHeat_runs <- heat[rownames(heat)=='numHeatRuns', ] # number of runs >= 10 days long in last 10 yr in which maximum temperature >=25pt5C deg C
		out$acuteHeat_longestRun <- heat[rownames(heat)=='heatRunLongest', ] # length of days of longest run with maximum temperature >= 25pt5C deg C in last 10 yr
		
	### chronic heat
	################
		
	if (verbose) say('Calculating chronic heat (mean daily temperature from June through August)... [1 variable]')

		# get column names for raw climate data... note getting from (non-leap year) June 1 to September 1... will remove days at start/end of period depending if leap/non-leap year, respesectively
		days <- 152:244
		colNamesMaxTemp <- paste0('tmaxDaily_yearMinus', rep(prefix(window:0, 2), each=length(days)), '_doy', prefix(days, 3))
		colNamesMinTemp <- paste0('tminDaily_yearMinus', rep(prefix(window:0, 2), each=length(days)), '_doy', prefix(days, 3))

		maxTemp <- x[ , colNamesMaxTemp]
		minTemp <- x[ , colNamesMinTemp]
		
		meanTemp <- (maxTemp + minTemp) / 2
		names(meanTemp) <- paste0('tmeanDaily_yearMinus', rep(prefix(window:0, 2), each=length(days)), '_doy', prefix(days, 3))
		
		rm(maxTemp, minTemp)
		
		# truncate days based on leap year/non-leap year
		for (i in 1:nrow(x)) {
		
			for (priorYear in window:0) {
			
				if ((x$obsYear[i] - priorYear) %% 4 == 0) { # leap year
					meanTemp[i, paste0('tmeanDaily_yearMinus', prefix(priorYear, 2), '_doy152')] <- NA
				} else { # non-leap year
					meanTemp[i, paste0('tmeanDaily_yearMinus', prefix(priorYear, 2), '_doy244')] <- NA
				}
				
			}
		
		}
		
		# calculate chronic heat stress (mean daily temperature from June through August)
		out$chronicHeat_deg <- rowMeans(meanTemp, na.rm=T)
		
	### acute cold
	##############
	
	if (verbose) say('Calculating acute cold stress (days with no snow pack in which minimum daily temperature was <=0 C or -10C)... [2 variables]')

		colNamesMinTemp <- paste0('tminDaily_yearMinus', prefix(rep(window:0, each=366), 2), '_doy', prefix(1:366, 3))
		colNamesSnow <- paste0('dayMetSweDaily_yearMinus', prefix(rep(window:0, each=366), 2), '_doy', prefix(1:366, 3))
		
		minTemp <- x[ , colNamesMinTemp]
		snow <- x[ , colNamesSnow]
		snow <- snow < .Machine$double.eps

		minTempLtThold <- minTemp <= 0
		out$acuteColdLte0C_days <- rowSums(minTempLtThold * snow, na.rm=TRUE)
	
		minTempLtThold <- minTemp <= -5
		out$acuteColdLteNeg5C_days <- rowSums(minTempLtThold * snow, na.rm=TRUE)
	
		minTempLtThold <- minTemp <= -10
		out$acuteColdLteNeg10C_days <- rowSums(minTempLtThold * snow, na.rm=TRUE)
	
	### chronic cold
	################
	
	if (verbose) say('Calculating chronic cold stress (mean of daily mean temperature from December through February)... [1 variable]')
		
		# get column names for raw climate data... note getting from (non-leap year) December 1 to February 29... will remove last day if non-leap year
		days <- c(336:366, 1:60) 
		
		colNamesMaxTemp <- paste0('tmaxDaily_yearMinus', rep(prefix(window:0, 2), each=length(days)), '_doy', prefix(days, 3))
		colNamesMinTemp <- paste0('tminDaily_yearMinus', rep(prefix(window:0, 2), each=length(days)), '_doy', prefix(days, 3))
		
		maxTemp <- x[ , c(colNamesMaxTemp)]
		minTemp <- x[ , c(colNamesMinTemp)]
		meanTemp <- (minTemp + maxTemp) / 2
		names(meanTemp) <- paste0('tmeanDaily_yearMinus', rep(prefix(window:0, 2), each=length(days)), '_doy', prefix(days, 3))
		rm(minTemp, maxTemp)
		
		# truncate days based on leap year/non-leap year
		for (i in 1:nrow(x)) {
		
			for (priorYear in window:0) {
			
				if ((x$obsYear[i] - priorYear) %% 4 == 0) { # leap year
					meanTemp[i, paste0('tmeanDaily_yearMinus', prefix(priorYear, 2), '_doy336')] <- NA
				} else { # non-leap year
					meanTemp[i, paste0('tmeanDaily_yearMinus', prefix(priorYear, 2), '_doy060')] <- NA
				}
				
			}
		
		}
		
		out$chronicCold_deg <- rowMeans(meanTemp, na.rm=TRUE)
		
	### melt/refreeze cycles
	########################
		
	if (verbose) say('Calculating melt/re-freeze cycles (number of runs 5 d or longer with minimum temperature > 0 from December through February bracketed by min temp <= 0)... [1 variable]')

		out$thaws_num <- 0

		# get column names for raw climate data... note getting from (non-leap year) December 1 to February 29... will remove last day if non-leap year
		for (i in 1:nrow(x)) {
		
			if (verbose & i == 1) say('      record: ', i, post=0)
			if (verbose & i %% 1000 == 0) say(i, post=0)
			
			runs <- 0 # counter of number of runs across years for this record
			
			for (yearPrior in window:0) {
			
				# get min temps for this year's winter (last days of this year plus first days of next year)

				# winter days of this year plus next year
				daysWinterStart <- if ((x$obsYear[i] - yearPrior) %% 4 == 0) { # leap year
					336:366
				} else {
					335:365
				}
				
				daysWinterEnd <- if (yearPrior == 0) {
					NULL
				} else if ((x$obsYear[i] - (yearPrior - 1)) %% 4 == 0) { # leap year
					1:60
				} else {
					1:59
				}
				
				days <- c(daysWinterStart, daysWinterEnd)

				colNamesStart <- paste0('tminDaily_yearMinus', rep(prefix(yearPrior, 2), each=length(daysWinterStart)), '_doy', prefix(daysWinterStart, 3))
				colNamesEnd <- if (yearPrior > 0) {
					paste0('tminDaily_yearMinus', rep(prefix(yearPrior - 1, 2), each=length(daysWinterEnd)), '_doy', prefix(daysWinterEnd, 3))
				} else {
					NULL
				}

				colNames <- c(colNamesStart, colNamesEnd)
				thisYearMinTemp <- as.numeric(x[i, colNames])
				
				freeze <- thisYearMinTemp <= 0
				freezeRle <- rle(freeze)
				
				# if there was at least one thaw period (at least one TRUE --> FALSE)
				if (!any(is.na(freezeRle$values)) & length(freezeRle$values) > 1 & freezeRle$values[1]) {
					
					thaws <- freezeRle$lengths[!freezeRle$values]
					
				} else if (!any(is.na(freezeRle$values)) & length(freezeRle$values) > 2 & !freezeRle$values[1]) {
				
					thaws <- freezeRle$lengths[!freezeRle$values]
					thaws <- thaws[2:length(thaws)]
				
				} else {
					
					thaws <- -Inf
				
				}
				
				runs <- runs + sum(thaws >= 5)
					
			} # next prior year
		
			out$thaws_num[i] <- runs
		
		} # next record
	
	# # # ### shoulder season cold stress
	# # # ###############################
		
	# # # if (verbose) say('Calculating shoulder season cold stress (days with minimum temperature <=-10 or -5 or 0 C with no snow cover)... [3 variables]', pre=1 )

		# # # colNamesMinTemp <- paste0('tminDaily_yearMinus', prefix(rep(window:0, each=366), 2), '_doy', prefix(1:366, 3))
		# # # colNamesSnow <- paste0('dayMetSweDaily_yearMinus', prefix(rep(window:0, each=366), 2), '_doy', prefix(1:366, 3))
		
		# # # minTemp <- x[ , colNamesMinTemp]
		# # # snow <- x[ , colNamesSnow]
		# # # snow <- snow < .Machine$double.eps

		# # # minTempLtThold <- minTemp <= 0
		# # # out$acuteColdStressLte0C_days <- minTempLtThold * snow
			
	### growing season precip
	#########################
			
	if (verbose) say('Calculating growing season precipitation (mean precip across years of precip from June through September)... [1 variable]')

		# get column names for raw climate data
		months <- 6:9
		colNamesPpt <- paste0('pptMonthly_yearMinus', prefix(rep(window:0, each=length(months)), 2), '_month', prefix(months, 2))

		precip <- apply(x[ , colNamesPpt], 1, sum, na.rm=T)
		numMonths <- apply(x[ , colNamesPpt], 1, FUN=function(x) sum(!is.na(x)))
		out$growingSeasonPrecip_mm <- precip / (numMonths / 3)
		
	### growing season precip variability
	#####################################
		
	if (verbose) say('Calculating growing season precipitation variability (June through September)... [1 variables]')

		# get column names for raw climate data
		months <- 6:9
		colNamesPpt <- paste0('pptMonthly_yearMinus', prefix(rep(window:0, each=length(months)), 2), '_month', prefix(months, 2))
		
		precipMonthly <- x[ , colNamesPpt]
		
		# condense monthly values into into growing seasons
		precipGrowSeason <- cbind(rowSums(precipMonthly[ , 1:4], na.rm=FALSE))
		
		for (i in 2:(window + 1)) precipGrowSeason <- cbind(precipGrowSeason, rowSums(precipMonthly[ , (i - 1) * 4 + (1:4)], na.rm=TRUE))
		
		out$growSeasonPrecipCV <- apply(precipGrowSeason, 1, cv)
		
	### snow pack duration
	######################
		
	if (verbose) say('Calculating duration of snow pack... [1 variable]')

		# get column names for raw climate data
		colNamesSnow <- paste0('dayMetSweDaily_yearMinus', prefix(rep(window:0, each=366), 2), '_doy', prefix(1:366, 3))
		out$snowPack_days <- apply(X=x[ , colNamesSnow], MARGIN=1, FUN=function(x) sum(x > 0, na.rm=T))

	### growing season atmospheric water balance
	############################################
		
	if (verbose) say('Calculating growing season atmospheric water balance (sum across all days of daily precip minus ET0 during summer months of June through September)... [1 variables]')

		days <- 152:244
		colNamesPpt <- paste0('pptDaily_yearMinus', prefix(rep(window:0, each=length(days)), 2), '_doy', prefix(days, 3))
		colNamesEt0 <- paste0('dobrowskiET0Daily_yearMinus', prefix(rep(window:0, each=length(days)), 2), '_doy', prefix(days, 3))

		ppt <- x[ , colNamesPpt]
		et0 <- x[ , colNamesEt0]
		waterStress <- ppt - et0
		rm(ppt, et0)
		names(waterStress) <- paste0('waterStressDaily_yearMinus', prefix(rep(window:0, each=length(days)), 2), '_doy', prefix(days, 3))
		
		# truncate days based on leap year/non-leap year
		for (i in 1:nrow(x)) {
		
			for (priorYear in window:0) {
			
				if ((x$obsYear[i] - priorYear) %% 4 == 0) { # leap year
					waterStress[i, paste0('waterStressDaily_yearMinus', prefix(priorYear, 2), '_doy152')] <- NA
				} else { # non-leap year
					waterStress[i, paste0('waterStressDaily_yearMinus', prefix(priorYear, 2), '_doy244')] <- NA
				}
				
			}
		
		}
		
		out$growSeasonWaterBal_mm <- rowSums(waterStress, na.rm=TRUE)
		out$growSeasonWaterBalSD <- apply(waterStress, 1, sd, na.rm=TRUE)
		
	### relative humidity
	#####################
		
	if (verbose) say('Calculating relative humidity (mean, high, and low, amd variability)... [1 variable]')

		months <- 6:9
		colNamesMinTemp <- paste0('tminMonthly_yearMinus', prefix(rep(window:0, each=length(months)), 2), '_month', prefix(months, 2))
		colNamesMaxTemp <- paste0('tmaxMonthly_yearMinus', prefix(rep(window:0, each=length(months)), 2), '_month', prefix(months, 2))
		colNamesDewpointTemp <- paste0('tdmeanMonthly_yearMinus', prefix(rep(window:0, each=length(months)), 2), '_month', prefix(months, 2))
		
		minTemp <- x[ , colNamesMinTemp]
		maxTemp <- x[ , colNamesMaxTemp]
		meanTemp <- (minTemp + maxTemp) / 2
		rm(minTemp, maxTemp)
		
		dewPointTemp <- x[ , colNamesDewpointTemp]
		
		tooHigh <- dewPointTemp > meanTemp
		
		# force dep point temp to be equal to mean temp if it's over mean temp
		if (any(rowSums(tooHigh, na.rm=TRUE) > 0)) {
		
			for (i in 1:nrow(dewPointTemp)) {
			
				indexTooHigh <- which(c(tooHigh[i, ]))
				dewPointTemp[i, indexTooHigh] <- meanTemp[i, indexTooHigh]
			
			}
		
		}
		
		# for (i in 1:nrow(dewPointTemp)) {
			# for (j in 1:ncol(dewPointTemp)) {
				# if (!is.na(dewPointTemp[i, j]) & dewPointTemp[i, j] > meanTemp[i, j]) dewPointTemp[i, j] <- meanTemp[i, j]
			# }
		# }
		
		humid <- dewpoint.to.humidity(dp=dewPointTemp, t=meanTemp, temperature.metric='celsius') / 100

		out$relHumidMean <- apply(humid, 1, mean, na.rm=TRUE)
		out$relHumidHighFreq <- apply(humid >= 0.75, 1, sum, na.rm=TRUE)
		out$relHumidLowFreq <- apply(humid <= 0.25, 1, sum, na.rm=TRUE)
		out$relHumidCv <- apply(humid, 1, cv)
	
		rm(humid)
	
		# # # # function for E or Es
		# # # eFunct <- function(T, E0=NULL, T0=273.15, LdivRv=5423) {
		
			# # # # calculates E or Es for use in calculating relative humidity
			# # # # see https://iridl.ldeo.columbia.edu/dochelp/QA/Basic/dewpoint.html
			
			# # # # T			temperature
			# # # # E0		0.611 kPa
			# # # # LdivRv	L/Rv = 5423 K
			# # # # T0		273.15 K

			# # # # # Clausius-Clapeyron equation
			# # # # if (is.null(E0)) E0 <- 0.611
			# # # # return(E0 * exp(LdivRv * ((1 / T0) - ( 1 / T))))
			
			# # # # August-Roche-Magnus formula
			# # # if (is.null(E0)) E0 <- 6.1094
			# # # return(E0 * exp((17.625 * T) / (T + 243.04)))
		
		# # # }
		
		# # # # get column names for raw climate data non-leap year
		# # # colNamesTempMax <- colNamesTempMin <- character()
		# # # for (yr in 10:0) {
		
			# # # colNamesTempMax <- c(colNamesTempMax, paste0('tmaxDaily_yearMinus', prefix(yr, 2), '_doy', prefix(1:366, 3)))
			# # # colNamesTempMin <- c(colNamesTempMin, paste0('tminDaily_yearMinus', prefix(yr, 2), '_doy', prefix(1:366, 3)))
			
		# # # }
		
		# # # x$relativeHumidityMedianDaily <- x$relativeHumidityLowerDaily05perc <- x$relativeHumidityUpperDaily95perc <- NA
		
		# # # for (i in 1:nrow(x)) {
			
			# # # minTemp <- unlist(x[i, colNamesTempMin])
			# # # maxTemp <- unlist(x[i, colNamesTempMax])
			
			# # # # assume dewpoint is equal to minimum tempertaure (from p. 49 of Introduction to Environmental Biophysics, 2nd ed)
			# # # dp <- minTemp

			# # # # saturation vapor pressure and ambient vapor pressure (from Dobrowski et al 2013 SI)
			# # # es <- 0.6108 * exp(minTemp * 17.27 / (minTemp + 237.3)) / 2 + 0.6108 * exp(maxTemp * 17.27 / (maxTemp + 237.3)) / 2     
			# # # ea <- 0.6108 * exp(dp * 17.27 / (dp + 237.3))

			# # # rh <- ea / es
			
			# # # # ## VERSION 1 Assume dewpoint temp = min temp (from p. 49 of Introduction to Environmental Biophysics, 2nd ed), then calculate RH using weathermetrics package. Predicts higher RH than Dobrowski method but nontheles linear with Dobrowski.
			# # # # meanTemp <- (minTemp + maxTemp) / 2

			# # # # # calculate relative humidity from dewpoint and mean temperature
			# # # # rh2 <- dewpoint.to.humidity(dp=dp, t=meanTemp, temperature.metric='celsius') / 100
			
			# # # ## VERSION 2 Use Clausius-Clapeyron equation or August-Roche-Magnus formula to calculate RH
			# # # # minTemp <- unlist(x[i, colNamesTempMin]) + 273.15
			# # # # maxTemp <- unlist(x[i, colNamesTempMax]) + 273.15
			
			# # # # minTemp <- unlist(x[i, colNamesTempMin])
			# # # # maxTemp <- unlist(x[i, colNamesTempMax])
			
			# # # # meanTemp <- (minTemp + maxTemp) / 2

			# # # # EsTemp <- eFunct(T=meanTemp)
			
			# # # # ETemp <- eFunct(T=meanTemp, E0=EsTemp)
			
			# # # # rh <- ETemp / EsTemp
		
			# # # x$relativeHumidityMedianDaily[i] <- median(rh, na.rm=T)
			# # # x$relativeHumidityLowerDaily05perc[i] <- quantile(rh, 0.05, na.rm=T)
			# # # x$relativeHumidityUpperDaily95perc[i] <- quantile(rh, 0.95, na.rm=T)

		# # # }
		
	### vapor pressure deficit
	##########################
	
	if (verbose) say('Calculating mean and extreme of vapor pressure deficit... [2 variables]')
	
	colNamesVaporMin <- paste0('vpdminMonthly_yearMinus', prefix(rep(window:0, each=12), 2), '_month', prefix(1:12, 2))
	colNamesVaporMax <- paste0('vpdmaxMonthly_yearMinus', prefix(rep(window:0, each=12), 2), '_month', prefix(1:12, 2))
	
	vaporMin <- x[ , colNamesVaporMin]
	vaporMax <- x[ , colNamesVaporMax]

	vaporMean <- (vaporMin + vaporMax) / 2
	
	out$vpdMean <- rowMeans(vaporMean, na.rm=TRUE)
	out$vpdHigh <- apply(vaporMax, 1, quantile, probs=0.9, na.rm=TRUE)
	
	### snow pack variability
	#########################
		
	if (verbose) say('Calculating variability of snow pack through time... [1 variable]')

		# get column names for raw climate data
		colNamesSnow <- paste0('dobrowskiSweMonthly_yearMinus', prefix(rep(window:0, each=12), 2), '_month', prefix(1:12, 2))
		snow <- x [ , colNamesSnow]
		
		out$snowPackStDev <- apply(X=snow, MARGIN=1, FUN=function(x) sd(x, na.rm=T))

	### summer temperature variability
	##################################
		
	if (verbose) say('Calculating variability of summer temperature... [1 variable]')

		# get column names for raw climate data... note getting from (non-leap year) June 1 to September 1... will remove last day if non-leap year
		days <- 152:244
		colNamesMax <- paste0('tmaxDaily_yearMinus', prefix(rep(window:0, each=length(days)), 2), '_doy', prefix(days, 3))

		maxTemp <- x[ , colNamesMax]
		
		# truncate days based on leap year/non-leap year
		for (i in 1:nrow(x)) {
		
			for (priorYear in window:0) {
			
				if ((x$obsYear[i] - priorYear) %% 4 == 0) { # leap year
					maxTemp[i, paste0('tmaxDaily_yearMinus', prefix(priorYear, 2), '_doy152')] <- NA
				} else { # non-leap year
					maxTemp[i, paste0('tmaxDaily_yearMinus', prefix(priorYear, 2), '_doy244')] <- NA
				}
				
			}
		
		}
		
		out$summerTempSD <- apply(X=maxTemp, MARGIN=1, FUN=function(x) sd(x, na.rm=T))

	return(out)
		
}
