chooseBgWithWeighting <- function(
	bg,
	pres,
	presenceWeights,
	multiplier=1,
	window=10,
	earliestYear=1981,
	totalBg=10000,
	mutableColumns=c(
		'tminAnnual', 'tmaxAnnual', 'pptAnnual', 'snowPackSweAnnualApril1',
		'tminMonthly', 'tmaxMonthly', 'pptMonthly', 'snowPackSweMonthly',
		'tminDaily', 'tmaxDaily', 'pptDaily', 'snowPackSweDaily'
	),
	immutableColumns=c('bgIndex', 'longWgs84', 'latWgs84', 'obsYear', 'obsMonth', 'obsDayOfMonth', 'obsDayOfYear', 'elev_gmted2010_m', 'elev_prism_m'),
	verbose=TRUE
) {
# chooseBgWithWeighting Draws background sites from a supplied data frame such that the background sites are in proportion to the weighting assigned to each presence site. The output will match the format of the presences data frame (i.e., will have the same column names for the same raw climate variables). Upon being chosen the set of raw climate variables matching the temporal window of the presences will be taken with the background site and renamed using the convention for column naming of raw climate data for presences.  For example, if a presence observed on 2014-04-22 has a weight of 0.5, then 2 background sites will be chosen and the climate data ending starting 2004-04-23 extending through 2014-04-22 will be chosen from the background set and renamed to match the column names for raw climate data for that presence. The script will rescale weights such that there is a user-specified total number of background sites.
#
# ARGUMENTS
# bg		Data frame with background sites
# pres		Data frame with presences
# presenceWeights	Vector of site weights for presences
# multiplier Numeric > 0; for each presence a number of background sites equal to min(1, round(multiplier / (the site's weight)) are chosen. Depending on relative magnitude of sites' weights, some may get the same number of background sites despite having different weights. For example, a preence with a weight of 0.2 will have 5 background sites chosen while another with a weight of 0.22 will also have 5 chosen. The multiplier increases the number of background sites chosen for each presence so that there is better differentiation between presences with slightly different weights.  For example, a multiplier of 2 means that twice as many background sites are chosen. So in the same example the presence with a weight of 0.2 will be associated with 10 background sites and the presence with a weight of 0.22 will be associated with 9 background sites.
# window	Positive integer, number of years before a presence was observed to consider it's climate data
# earliestYear First year in which there is DAILY climate data available.
# totalBg	Positive integer, total number of backgroud sites to select.  Script rescales temporal weights such to match this number.
# mutableColumns	Character vector of prefix of names of mutableColumns to transfer from background sites. Column names will be changed to match the names in pres.  For example, in the background set column names for daily precipitation are as "pptDaily_yearMinus00_day266" whereas in the background set they are as "pptDaily_year1980_doy_226".
# immutableColumns Character list of names of columns that will *always* appear in the background and presence set and will not change name by, for example, year.
# verbose	Logical, if TRUE then displays progress
#
# VALUES
# data frame of background sites
# 
# REQUIRED DEPENDANCIES
# raster
# !Omnibus functions
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
# SOURCE	source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/Function to Choose Background Sites with Weighting.r')
#
# TESTING
#
#
# LICENSE
# This document is copyright ©2014 by Adam B. Smith.  This document is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.  This document is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. Copies of the GNU General Public License versions are available at http://www.R-project.org/Licenses/.
#
# AUTHOR	Adam B. Smith | Missouri Botanical Garden, St. Louis, Missouri | adamDOTsmithATmobotDOTorg
# DATE		2016-09
# REVISIONS 

############################
## FUNCTIONS AND PACKAGES ##
############################

source('C:/ecology/Drive/R/!Omnibus.r')

####################
## PRE-PROCESSING ##
####################

## calculate number of background sites chosen per presence... rescale to get desired number of background points
recip <- multiplier / presenceWeights
sumRecip <- sum(recip, na.rm=TRUE)

nBg <- pmax(rep(1, nrow(pres)), round((totalBg * recip) / sumRecip))

# subtract or add additional background sites in proportion to number already assigned so total = desired number
if (sum(nBg, na.rm=TRUE) < totalBg) { # add

	i <- sample(1:length(nBg), totalBg - sum(nBg), prob=nBg)
	nBg[i] <- nBg[i] + 1
	
}

if (sum(nBg, na.rm=TRUE) > totalBg) { # subtract

	i <- sample(1:length(nBg), sum(nBg) - totalBg, prob=nBg)
	nBg[i] <- nBg[i] - 1
	
}

## add column to presences to denote index number of background site (ie, rownumber of original background site).  Will be NA for presences, of course, but useful if needing to recalculate background climate from pre-existing/added columns.
bg <- cbind(data.frame(bgIndex=1:nrow(bg), obsYear=rep(NA, nrow(bg)), obsMonth=rep(NA, nrow(bg)), obsDayOfMonth=rep(NA, nrow(bg)), obsDayOfYear=rep(NA, nrow(bg))), bg)
pres <- cbind(data.frame(bgIndex=rep(NA, nrow(pres))), pres)

##########
## MAIN ##
##########

### create data frame in same format as presences with just one row but with all values equal to NA
empty <- pres[1, ]
empty[ , ] <- NA
empty <- cbind(
	data.frame(obsYear=rep(NA, nrow(empty)), obsMonth=rep(NA, nrow(empty)), obsDayOfMonth=rep(NA, nrow(empty)), obsDayOfYear=rep(NA, nrow(empty))),
	empty
)
gc()

# data frame to store selected background sites
bgChosen <- empty[rep(1, totalBg), ]

### select background sites
###########################

dates <- as.character(as.Date(paste0(pres$obsYear, '-', pres$obsMonth, '-', pres$obsDayOfMonth)))
uniqueDates <- sort(unique(dates))

# counter for row number of selected background data frame on which to place newly selected background sites
selectedBgIndex <- 0

for (thisDate in uniqueDates) {

	if (verbose) say(thisDate, round(100 * which(uniqueDates %in% thisDate) / length(uniqueDates), 1), '%')

	presIndex <- which(dates %in% thisDate)

	# total number of background sites needed
	totalBgNeeded <- sum(nBg[presIndex], na.rm=TRUE)
	
	# if needing any background sites (can be 0 because some points lie outside PRISM)
	if (totalBgNeeded > 0) {
		
		# get index of chosen background sites
		bgIndex <- sample(1:nrow(bg), sum(nBg[presIndex], na.rm=TRUE))

		obsYear <- pres$obsYear[presIndex[1]]
		obsMonth <- pres$obsMonth[presIndex[1]]
		obsDayOfMonth <- pres$obsDayOfMonth[presIndex[1]]
		obsDoy <- pres$obsDayOfYear[presIndex[1]]
		
		bgColumnNames <- presColumnNames <- immutableColumns
		
		# create columns names for these presence sites and matching columns for background sites
		for (colName in mutableColumns) {
		
			# annual variables
			if (colName=='tminAnnual' | colName=='tmaxAnnual' | colName=='pptAnnual' | colName=='snowPackSweAnnualApril1') {
			
				bgColumnNames <- c(bgColumnNames, paste0(colName, '_year', max(earliestYear, obsYear - window):obsYear, ifelse(colName=='snowPackSweAnnualApril1', '_kgPerM2', '')))
				presColumnNames <- c(presColumnNames, paste0(colName, '_yearMinus', prefix(min(obsYear - max(earliestYear, obsYear - window), window):0, 2), ifelse(colName=='snowPackSweAnnualApril1', '_kgPerM2', '')))
				
			}
			
			# monthly variables
			if (colName=='tminMonthly' | colName=='tmaxMonthly' | colName=='pptMonthly' | colName=='snowPackSweMonthly') {
			
				for (yearPrior in min(obsYear - earliestYear, window):0) {
			
					# startMonth <- if (yearPrior == window) {
						# obsMonth + 1
					# } else {
						# 1
					# }
			
					# endMonth <- if (yearPrior == 0) {
						# obsMonth
					# } else {
						# 12
					# }
			
					bgColumnNames <- c(bgColumnNames, paste0(colName, '_year', obsYear - yearPrior, '_month', prefix(1:12, 2), ifelse(colName=='snowPackSweMonthly', '_kgPerM2', '')))
					presColumnNames <- c(presColumnNames, paste0(colName, '_yearMinus', prefix(yearPrior, 2), '_month', prefix(1:12, 2), ifelse(colName=='snowPackSweMonthly', '_kgPerM2', '')))
					
				}
				
			}
			
			# daily variables
			if (colName=='tminDaily' | colName=='tmaxDaily' | colName=='pptDaily' | colName=='snowPackSweDaily') {
			
				for (yearPrior in min(obsYear - earliestYear, window):0) {
			
					# startDoy <- if (yearPrior == window) {
						# obsDoy + 1
					# } else {
						# 1
					# }
			
					# endDoy <- if (yearPrior == 0) {
						# obsDoy
					# } else {
						# 366
					# }
			
					endDoy <- if ((obsYear - yearPrior) %% 4 != 0) {
						365
					} else {
						366
					}
			
					bgColumnNames <- c(bgColumnNames, paste0(colName, '_year', obsYear - yearPrior, '_doy', prefix(1:endDoy, 3), ifelse(colName=='snowPackSweDaily', '_kgPerM2', '')))
					presColumnNames <- c(presColumnNames, paste0(colName, '_yearMinus', prefix(yearPrior, 2), '_doy', prefix(1:endDoy, 3), ifelse(colName=='snowPackSweDaily', '_kgPerM2', '')))
					
				}
				
			}
			
		} # next column name

		# select and rename background site columns to match those of presence columns
		thisBgChosenSelectedColumns <- bg[bgIndex, bgColumnNames]
		names(thisBgChosenSelectedColumns) <- presColumnNames
		
		thisBgChosenSelectedColumns$bgIndex <- bgIndex
		
		# remember date for which this BG site pertains
		thisBgChosenSelectedColumns$obsYear <- obsYear
		thisBgChosenSelectedColumns$obsMonth <- obsMonth
		thisBgChosenSelectedColumns$obsDayOfMonth <- obsDayOfMonth
		thisBgChosenSelectedColumns$obsDayOfYear <- obsDoy
		
		# get index of presence data frame column that matches each column of selected background sites
		colIndex <- match(names(thisBgChosenSelectedColumns), names(pres))
		
		# get row number(s) of data frame of pre-defined background sites (initially all NAs) to which to assign these particular background sites
		selectedBgIndex <- max(selectedBgIndex) + (1:length(bgIndex))

		# remember these background sites
		bgChosen[selectedBgIndex, colIndex] <- thisBgChosenSelectedColumns
		
		rm(thisBgChosenSelectedColumns)
		
	} # if any background sites needed
	
}

#####################
## POST-PROCESSING ##
#####################

return(bgChosen)

}
