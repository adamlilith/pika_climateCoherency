extractBasedOnDate <- function(
	x,
	obsYearField='obsYear',
	obsMonthField='obsMonth',
	obsDayOfYearField='obsDayOfYear',
	longLatFields=c('longWgs84', 'latWgs84'),
	CRS='+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs',
	window=10,
	truncateAnnualWindow=TRUE,
	climYears=1981:2015,
	vars=c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax', 'dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0'),
	annual=c(NA, 	    NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     FALSE,      TRUE,           TRUE),
	monthly=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     TRUE,       TRUE,           TRUE),
	daily=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   FALSE,   FALSE,     FALSE,    TRUE,      FALSE,          TRUE),
	savePartial=NULL,
	censureDates=TRUE,
	drive='G:/',
	dayMetDir='ecology/Climate/DayMet/V3/',
	dobrowskiDailyET0Dir='ecology/Climate/PRISM/ET0 - Daily - Dobrowski et al 2013',
	dobrowskiMonthlyET0Dir='ecology/Climate/PRISM/ET0 - Monthly - Dobrowski et al 2013',
	dobrowskiAnnualET0Dir='ecology/Climate/PRISM/ET0 - Annual - Sum - Dobrowski et al 2013',
	dobrowskiMonthlySweDir='ecology/Climate/PRISM/Snow Water Equivalent - Monthly - Dobrowski et al 2013',
	dobrowskiAnnualSweDir='ecology/Climate/PRISM/Snow Water Equivalent - Annual - Mean - Dobrowski et al 2013',
	prismAN81mDir='ecology/Climate/PRISM/AN81m 1981-2015',
	prismAN81dDir='ecology/Climate/PRISM/AN81d 1981-2015',
	verbose=2
) {
# extractBasedOnDate Extract elevation, PRISM, and DayMet climate data based on date of record. For each point in a data frame, extracts annual/monthly/daily climate dat (if available) for a period up to X years prior to the record's date of observation.  X must be such that it does not exceed the temporal bounds of PRISM data (1895 for monthly and 1981 for daily) or DayMet data (1980). Returns a (possibly huge) data frame with the original data plus extracted climate data.
#
# For PRISM, annual data will be obtained from the annual rasters in the AN81m data set.
# For PRISM, monthly data wil be from AN81m monthly rasters.
# For PRISM, daily data will be from AN81d daily rasters.
#
# ARGUMENTS
# x			SpatialPointsDataFrame object in NAD83 CRS (PRISM)
# obsYearField, obsMonthField, obsDayOfYearField	Character, names of fields in x that list year, month, and day of year of observation.
# longLatFields  2-element character list of longitude and latitude column names in x.
# CRS			String, CRS of x using columns named in longLatFields
# window	Integer >= 0, number of years prior to each observation for which to extract data.
# truncateAnnualWindow   Logical, if TRUE then will use (window - 1):0 as the window for ANNUAL data
# climYears	Integer, for PRISM monthly data this can be (at most) 1895:2015 but for PRISM daily data 1981:2015 and DayMet 1980:2015
# vars		Character, name(s) of types of variables to extract:
#				topo			topography (elevation, slope, aspect, topographic position index, topographic roughness index)
#				ppt				precipitation (PRISM: annual, monthly, daily)
#				tmin, tmax 		min/max temperature (PRISM: annual, monthly, daily)
#				tdeman			mean dew point temnperture (PRISM: annual, monthly)
#				vpdmin, vpdmax 	min/max vapor pressure deficit (PRISM: annual, monthly)
#				dayMetSwe	    snow water equivalent (DayMet: annual, monthly, daily)
#				dobrowskiSwe	snow water equivalent (as per Dobrowski et al. 2013: monthly, annual)
#				dobrowskiET0	potential evapotranspiration (as per Dobrowski et al. 2013: annual, monthly, daily)
#
# periods	Character list, periods over which to extract each climate variable (if available). Default: annual, monthly, and daily.  If a particular period variant does not exist for a climate variable it will be ignored if the available periods are longer than the desired period (e.g., Dobroski monthly SWE is available but not daily SWE) or calculated from the next-shorter period variant (e.g., DayMet SWE is on a daily time step, so monthly and yearly values can be calcualted from these).  Calculations are as:
#				elev			value (period is ignored)
# 				ppt				annual sum, monthly sum, daily sum
# 				tmin, tmax		annual mean, monthly mean, daily value
#				tdmean			annual mean, monthly mean
#				vpdmin, vpdmax	annual mean, monthly mean
#				dayMetSwe		annual mean, SWE on last day of month
#				dobrowskiSwe	annual mean, SWE on last day of month
#				dobrowskiET0	annual sum, monthly sum, daily value
#
#	Note that all daily data contains a day of year 366 to accomodate leap years (even if the year isn't a leap year--in which case the column contains NA for that record),
#
# savePartial	NULL or characater string. If not NULL then this should be a character string specifying a path and file name (with no extension--".rds" will be added to it).  This will save a version of the data frame with data extracted up to the *last* variable for which all data was fully extracted. For example, if the progress messages indicate that elev, ppt, and tmin have been extracted but extraction is currently bring done for tmax, then the data frame with elev, ppt, and tmin wil be saved.  Good for guarding against interrupted extractions!  If NULL then no partial files will be saved.
#
# censureDates	Logical, if TRUE then forces all data prior to date of observation minus <window> to NA and all data after date of observation to NA
#
# drive			Letter of drive on which climate data is stored plus ":/" (e.g., "J:/")
# dayMetDir		Character, path of DayMet Data (contains folders for annual, monthly, daily data)
# prismAN81mDir Character, path of PRISM AN81m data
# prismAN81dDir Character, path of PRISM AN81d data
#
# VALUES
# data frame with original data plus additional columns with names as:
#
# ANNUAL:
# <variable code><Variable period>_yearMinus<years prior to observation>
# examples: `tminAnnual_yearMinus09`, `pptAnnual_yearMinus00`
#
# MONTHLY:
# <variable code>_<variable period>_yearMinus<years prior to observation>_month<month of year>
# examples: `tminMonthly_yearMinus09_month12`, `pptMonthly_yearMinus00_month01`
#
# DAILY:
# <variable code>_<variable period>_yearMinus<years prior to observation>_doy<Julian day of year>
# examples: `tminDaily_yearMinus09_doy005`, `pptDaily_yearMinus00_doy266`
#
# where:
# <variable code> is one of vars (see arguments; example: "ppt")
# <Variable period> is either "Annual", "Monthly" or "Daily"
# "yearMinus"<years prior to observation> number of years prior to the record to which the climate data pertain (example: climate data from 3 years prior to the date of record observation would be "yearMinus03" while climate data in the same year would be "yearMinus00")
# "month<month of year>" is the month of the year of the climate data (example: climate data from April 5 years prior to the date of observation would be "yearMinius05_month04".
# "doy<Julian day of year>" day of year in which climate data was obtained (example: climate data from February 1st of the 10th year prior to observation woudk be "yearMinus10_doy032"
#
# verbose
# 0 ==> no display of progress, 1 ==> some display of progress, >=2 ==> display all progress
#
# Note that all numbers in columns names are padded by one leading zeros (so years has 2 digits, months 2 digits, and DOY 3 digits).
# 
# REQUIRED DEPENDANCIES
# raster, sp
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
# SOURCE	source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/Function - Extract Environmental Data Based on Date.rast')
#
# TESTING
#
#
# LICENSE
# This document is copyright ©2014 by Adam B. Smith.  This document is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3, or (at your option) any later version.  This document is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. Copies of the GNU General Public License versions are available at http://www.R-project.org/Licenses/.
#
# AUTHOR	Adam B. Smith | Missouri Botanical Garden, St. Louis, Missouri | adamDOTsmithATmobotDOTorg
# DATE		2016-12
# REVISIONS 

############################
## FUNCTIONS AND PACKAGES ##
############################

library(raster)

# function to rescale extracted climate data into standard units
# function to rescale extracted climate data into standard units
rescale <- function(y, climVar) {

	if (climVar %in% c('ppt', 'tmin', 'tmax', 'tdmean', 'dobrowskiET0', 'dobrowskiSwe')) y <- y / 100000
	if (climVar %in% c('vpdmin', 'vpdmax')) y <- y / 1000000
	return(y)
	
}

####################
## PRE-PROCESSING ##
####################

xyDayMet <- SpatialPoints(x[ , c(longLatFields)], CRS(CRS))
xyDayMet <- sp::spTransform(xyDayMet, CRS('+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +a=6378137 +rf=298.257223563 +lat_1=25 +lat_2=60'))
	
##########
## MAIN ##
##########

	# for each environmental variable
	for (countClimVar in seq_along(vars)) {

		if (verbose > 0) say(toupper(vars[countClimVar]), post=0)

		# make copy of data frame for which to extract data
		xyThis <- x[ , c(longLatFields, obsYearField, obsMonthField, obsDayOfYearField)]
		
		# cell area
		if (vars[countClimVar] == 'cellArea') {
		
			template <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m.tif'))
			template <- raster::area(template)
			xyThis$cellAreaPrism_km2 <- raster::extract(template, xyThis[ , 1:2])
			rm(template)
		
			template <- raster(paste0(drive, 'ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075.tif'))
			template <- raster::area(template)
			xyThis$cellAreaGmted2010_km2 <- raster::extract(template, xyThis[ , 1:2])
			rm(template)
		
			xyThis$cellAreaDayMet_km2 <- 1
		
		# topography data
		} else if (vars[countClimVar] == 'topo') {
		
			# elevation
			rast <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m.tif'))
			xyThis$elevPrism_m <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			rast <- raster(paste0(drive, 'ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075.tif'))
			xyThis$elevGmted2010_m <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			# slope
			rast <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m_slope_degrees.tif'))
			xyThis$slopePrism_deg <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			rast <- raster(paste0(drive, 'ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_slope_degrees.tif'))
			xyThis$slopeGmted2010_deg <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			# aspect
			rast <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m_aspect_degrees.tif'))
			xyThis$aspectPrism_deg <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			rast <- raster(paste0(drive, 'ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_aspect_degrees.tif'))
			xyThis$aspectGmted2010_deg <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			# TPI
			rast <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m_topoPositionIndex_m.tif'))
			xyThis$tpiPrism_m <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			rast <- raster(paste0(drive, 'ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_topoPositionIndex_m.tif'))
			xyThis$tpiGmted2010_m <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			# TRI
			rast <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m_topoRuggednessIndex_m.tif'))
			xyThis$triPrism_m <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
			rast <- raster(paste0(drive, 'ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_topoRuggednessIndex_m.tif'))
			xyThis$triGmted2010_m <- raster::extract(rast, xyThis[ , 1:2])
			rm(rast)
		
		# climate data
		} else {

			### create empty columns
			########################

			if (annual[countClimVar]) {
				
				# for each year prior to observation year
				for (yearPrior in -(window - truncateAnnualWindow):0) {

					xyThis$NEW <- NA
					names(xyThis)[ncol(xyThis)] <- paste0(vars[countClimVar], 'Annual_yearMinus', prefix(abs(yearPrior), 2))
					  
				}
				
			}
			
			if (monthly[countClimVar]) {
					
				# for each year/month prior to observation year
				for (yearPrior in -(window):0) {

					# for each month
					for (i in 1:12) {
						
						xyThis$NEW <- NA
						names(xyThis)[ncol(xyThis)] <- paste0(vars[countClimVar], 'Monthly_yearMinus', prefix(abs(yearPrior), 2), '_month', prefix(i, 2))
					  
					}
					
				}
				
			}

			if (daily[countClimVar]) {
					
				# for each year/month/day prior to observation year
				for (yearPrior in -(window):0) {

					new <- as.data.frame(matrix(rep(NA, 366 * nrow(xyThis)), nrow=nrow(xyThis)))
					names(new) <- paste0(vars[countClimVar], 'Daily_yearMinus', prefix(abs(yearPrior), 2), '_doy', prefix(1:366, 3))
					xyThis <- cbind(xyThis, new)
					
				}
					
			}

			gc()
				
			# for each year for which there is climate data
			for (climYear in climYears) {

				if (verbose >= 2) say(vars[countClimVar], ' ', climYear, ' --------------------------------------------------', pre=1, post=1)
				
				### extract DAILY data
				######################
				
				if (daily[countClimVar]) {
					
					# get index of records from this year or any year in window prior to year of recording
					theseRecords <- which(climYear >= (x[ , obsYearField] - window) & climYear <= x[ , obsYearField])

					if (climYear >= 1981 & length(theseRecords) > 0) {
						
						if (verbose >= 2) say('   Extracting DAILY climate for ', climYear, ' ', vars[countClimVar])
						
						if (vars[countClimVar] == 'dayMetSwe') {
						
							climStack <- stack(listFiles(paste0(drive, dayMetDir, '/SWE - Daily'), pattern=paste0('year', climYear)))
							climExtracted <- raster::extract(climStack, xyDayMet[theseRecords, ])
						
						} else if (vars[countClimVar] == 'dobrowskiET0') {
						
							climStack <- stack(listFiles(paste0(drive, dobrowskiDailyET0Dir), pattern=paste0('_year', climYear)))
							climExtracted <- raster::extract(climStack, xyThis[theseRecords, longLatFields])
						
						} else {
						
							climStack <- stack(listFiles(paste0(drive, prismAN81dDir, '/', vars[countClimVar], '/', climYear), pattern='.tif'))
							climExtracted <- raster::extract(climStack, xyThis[theseRecords, longLatFields])
							
						}
						
						rm(climStack); gc()
						climExtracted <- rescale(climExtracted, vars[countClimVar])

						# add extra day in non-leap years
						if (ncol(climExtracted)==365) climExtracted <- cbind(climExtracted, matrix(rep(NA, length(theseRecords), ncol=1)))
						
						# remember (and censure data before day of record in year <date of observation> - <window> AND after day of observatoin in <year of observation>)
						for (i in seq_along(theseRecords)) {
							
							colName <- paste0(vars[countClimVar], 'Daily_yearMinus', prefix(abs(xyThis$obsYear[theseRecords[i]] - climYear), 2), '_doy', prefix(1:366, 3))

							keep <- if (censureDates & xyThis[theseRecords[i], obsYearField] - window == climYear) {
								xyThis[theseRecords[i], obsDayOfYearField] < {1:366}
							} else if (censureDates & xyThis[theseRecords[i], obsYearField] == climYear) {
								xyThis[theseRecords[i], obsDayOfYearField] >= {1:366}
							} else {
								rep(TRUE, 366)
							}
							
							keep <- ifelse(keep, 1, NA)

							xyThis[theseRecords[i], colName] <- climExtracted[i, ] * keep
							
						} # next record
						
						rm(climExtracted)
						   
						gc()
						
					} # if climate year is >=1981
					
				} # if extracting daily
				
				### extract MONTHLY data
				########################
					
				if (monthly[countClimVar]) {
					
					# get index of records from this year or any year in window prior to year of recording
					theseRecords <- which(climYear >= (xyThis[ , obsYearField] - window) & climYear <= xyThis[ , obsYearField])

					# if any records needing this data
					if (length(theseRecords) > 0) {
						
						# for each month for which there is climate data
						for (climMonth in 1:12) {
						
							if (verbose >= 2) {
								if (climMonth == 1) say('   Extracting MONTHLY climate for ', vars[countClimVar], ' ', climYear, post=0)
								say(climMonth, post=ifelse(climMonth==12, 1, 0))
							}
							
							if (vars[countClimVar] == 'dayMetSwe') {
							
								climRast <- raster(paste0(drive, dayMetDir, '/SWE - Monthly - Mean/meanSwe_year', climYear, '_month', prefix(climMonth, 2), '.tif'))
								climExtracted <- c(raster::extract(climRast, xyDayMet[theseRecords, ]))
							
							} else if (vars[countClimVar] == 'dobrowskiSwe') {
								
								climRast <- raster(paste0(drive, dobrowskiMonthlySweDir, '/snowWaterEquivalent_year', climYear, '_month', prefix(climMonth, 2), '_mm.tif'))
								climExtracted <- c(raster::extract(climRast, xyThis[theseRecords, longLatFields]))
								
							} else if (vars[countClimVar] == 'dobrowskiET0') {
							
								climRast <- raster(paste0(drive, dobrowskiMonthlyET0Dir, '/et0_year', climYear, '_month', prefix(climMonth, 2), '.tif'))
								climExtracted <- c(raster::extract(climRast, xyThis[theseRecords, longLatFields]))

							} else {
							
								climRast <- raster(paste0(drive, prismAN81mDir, '/', vars[countClimVar], '/', climYear, '/', 'prism_', vars[countClimVar], '_us_30s_', climYear, prefix(climMonth, 2), '.tif'))
								climExtracted <- c(raster::extract(climRast, xyThis[theseRecords, longLatFields]))
								
							}
						 
							rm(climRast)
							climExtracted <- rescale(climExtracted, vars[countClimVar])
						 
							# remember (and censure data before month of record in year <date of observation> - <window> AND after month of observatoin in <year of observation>)
							for (i in seq_along(theseRecords)) {
								
								colName <- paste0(vars[countClimVar], 'Monthly_yearMinus', prefix(xyThis$obsYear[theseRecords[i]] - climYear, 2), '_month', prefix(climMonth, 2))
								
								keep <- if (censureDates & xyThis$obsYear[theseRecords[i]] - window == climYear) {
									xyThis$obsMonth[theseRecords[i]] < climMonth
								} else if (censureDates & xyThis$obsYear[theseRecords[i]] == climYear) {
									xyThis$obsMonth[theseRecords[i]] >= climMonth
								} else {
									TRUE
								}
								
								keep <- ifelse(keep, 1, NA)
								
								xyThis[theseRecords[i], colName] <- climExtracted[i] * keep
								
							} # next record
							
						} # next month
						
					} # if any records needing data
					
				} # if extracting monthly
					
				### extract ANNUAL data
				#######################

				if (annual[countClimVar]) {
					
					if (verbose >= 2) say('annual', post=0)
					
					# get index of records from this year or any year in window prior to year of recording
					theseRecords <- which(climYear >= (x[ , obsYearField] - (window - truncateAnnualWindow)) & climYear <= x[ , obsYearField])

					# if any records need data
					if (length(theseRecords) > 0) {
						
						if (vars[countClimVar] == 'dayMetSwe') {

							climRast <- raster(paste0(drive, dayMetDir, '/SWE - Annual - Mean/meanSwe_year', climYear, '.tif'))
							climExtracted <- c(raster::extract(climRast, xyDayMet[theseRecords, ]))
							
						} else if (vars[countClimVar] == 'dobrowskiSwe') {

							climRast <- raster(paste0(drive, dobrowskiAnnualSweDir, '/snowWaterEquivalent_year', climYear, '_mm.tif'))
							climExtracted <- c(raster::extract(climRast, xyThis[theseRecords, longLatFields]))
						
						} else if (vars[countClimVar] == 'dobrowskiET0') {
							
							climRast <- raster(paste0(drive, dobrowskiAnnualET0Dir, '/et0_year', climYear, '.tif'))
							climExtracted <- c(raster::extract(climRast, xyThis[theseRecords, longLatFields]))

						} else {
						 
							climRast <- raster(paste0(drive, prismAN81mDir, '/', vars[countClimVar], '/', climYear, '/', 'prism_', vars[countClimVar], '_us_30s_', climYear, '.tif'))
							climExtracted <- c(raster::extract(climRast, xyThis[theseRecords, longLatFields]))
							
						}
						 
						rm(climRast)
						climExtracted <- rescale(climExtracted, vars[countClimVar])

						# remember
						for (i in seq_along(theseRecords)) {
							
							colName <- paste0(vars[countClimVar], 'Annual_yearMinus', prefix(xyThis$obsYear[theseRecords[i]] - climYear, 2))
							xyThis[theseRecords[i], colName] <- climExtracted[i]
							
						} # next record
					
						gc()
						
					}

				} # if needing daily data

			} # next climate year
			
		} # if climate data
		
		x <- cbind(x, xyThis[ , 6:ncol(xyThis)])
		rm(xyThis)
		gc()

		if (!is.null(savePartial)) {
			
			saveRDS(x, file=paste0(savePartial, ' - PARTIAL ', vars[countClimVar], '.rds'))
			if (verbose > 0) say('Note: Saved file when all data for ', vars[countClimVar], ' was extracted... so if interrupted the file may not have data for all climate variables.', pre=1, post=1)
			
		}
		
	} # next variable

#####################
## POST-PROCESSING ##
#####################

return(x)

}
