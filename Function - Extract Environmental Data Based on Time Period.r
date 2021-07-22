extractTimePeriod <- function(
	x,
	periodStartEnd=2010:2015,
	vars=c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax', 'dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0'),
	periods=c('annual', 'monthly', 'daily'),
	savePartial=NULL,
	drive='G:/',
	dayMetDir='ecology/Climate/DayMet/V3/',
	dobrowskiDailyET0Dir='ecology/Climate/PRISM/ET0 - Daily - Dobrowski et al 2013',
	dobrowskiMonthlyET0Dir='ecology/Climate/PRISM/ET0 - Monthly - Dobrowski et al 2013',
	dobrowskiAnnualET0Dir='ecology/Climate/PRISM/ET0 - Annual - Sum - Dobrowski et al 2013',
	dobrowskiMonthlySweDir='ecology/Climate/PRISM/Snow Water Equivalent - Monthly - Dobrowski et al 2013',
	dobrowskiAnnualSweDir='ecology/Climate/PRISM/Snow Water Equivalent - Annual - Mean - Dobrowski et al 2013',
	prismAN81mDir='ecology/Climate/PRISM/AN81m 1981-2015',
	prismAN81dDir='ecology/Climate/PRISM/AN81d 1981-2015'	
) {
# extractTimePeriod Extract elevation, PRISM, and DayMet climate data based on date of record. For each point in a data frame, extracts annual/monthly/daily climate data (if available) for a period up to X years prior to the record's date of observation.  X must be such that it does not exceed the temporal bounds of PRISM data (1895 for monthly and 1981 for daily) or DayMet data (1980). Returns a (possibly huge) data frame with the original data plus extracted climate data.
#
# For PRISM, annual data will be obtained from the annual rasters in the AN81m data set.
# For PRISM, monthly data wil be from AN81m monthly rasters.
# For PRISM, daily data will be from AN81d daily rasters.
#
# ARGUMENTS
# x			SpatialPointsDataFrame object in PRISM projection (NAD83)
# obsYearField, obsMonthField, obsDayOfYearField	Character, names of fields in x that list year, month, and day of year of observation.
# periodStartEnd	Integer(s) >= 0, years in which to extract climate data
# vars		Character, name(s) of types of variables to extract:
#				cellArea		cell area
#				topo			topography (elevation, slope in degrees, aspect in degrees, topographic position index in m, topographic roughness index in m
#				ppt				precipitation (PRISM: annual, monthly, daily)
#				tmin, tmax 		min/max temperature (PRISM: annual, monthly, daily)
#				tdeman			mean dew point temnperture (PRISM: annual, monthly)
#				vpdmin, vpdmax 	min/max vapor pressure deficit (PRISM: annual, monthly)
#				dayMetSwe	    snow water equivalent (DayMet: annual, monthly, daily)
#				dobrowskiSwe	snow water equivalent (as per Dobrowski et al. 2013: monthly, annual)
#				dobrowskiET0	potential evapotranspiration (as per Dobrowski et al. 2013: annual, monthly, daily)
#
# periods	Character list, periods over which to extract each climate variable (if available). Default: annual, monthly, and daily.  If a particular period variant does not exist for a climate variable it will be ignored if the available periods are longer than the desired period (e.g., Dobroski monthly SWE is available but not daily SWE) or calculated from the next-shorter period variant (e.g., DayMet SWE is on a daily time step, so monthly and yearly values can be calcualted from these).  Calculations are as:
#				cellArea		value (period is ignored)
#				topo			value (period is ignored)
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
# savePartial	NULL or character string. If not NULL then this should be a character string specifying a path and file name (with no extension--".rda" will be added to it).  This will save a version of the data frame with data extracted up to the *last* variable for which all data was fully extracted. For example, if the progress messages indicate that elev, ppt, and tmin have been extracted but extraction is currently bring done for tmax, then the data frame with elev, ppt, and tmin wil be saved.  Good for guarding against interrupted extractions!  If NULL then no partial files will be saved.
#
# dayMetDir		Character, path of DayMet Data (contains folders for annual, monthly, daily data)
# prismAN81mDir Character, path of PRISM AN81m data
# prismAN81dDir Character, path of PRISM AN81d data
#
# VALUES
# data frame with original data plus additional columns with names as:
#
# ANNUAL:
# <variable code><Variable period>_year<year>
# examples: `tminAnnual_year1998`, `pptAnnual_year2000`
#
# MONTHLY:
# <variable code>_<variable period>_year<year>_month<month of year>
# examples: `tminMonthly_year1997_month12`, `pptMonthly_year2014_month01`
#
# DAILY:
# <variable code>_<variable period>_year<year>_doy<Julian day of year>
# examples: `tminDaily_yearMinus1981_doy005`, `pptDaily_year2015_doy266`
#
# where:
# <variable code> is one of vars (see arguments; example: "ppt")
# <Variable period> is either "Annual", "Monthly" or "Daily"
# "year"<year> year of climate data
# "month<month of year>" is the month of the year of the climate data (example: climate data from April 5 years prior to the date of observation would be "yearMinius05_month04".
# "doy<Julian day of year>" day of year in which climate data was obtained (example: climate data from February 1st of the 10th year prior to observation woudk be "yearMinus10_doy032"
#
# Note that all numbers in columns names are padded by one leading zeros (so months have 2 digits, and DOY 3 digits).
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
# SOURCE	source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/Function - Extract Environmental Data Based on Time Period.r')
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
rescale <- function(y, climVar) {

	if (climVar %in% c('ppt', 'tmin', 'tmax', 'tdmean', 'dobrowskiET0', 'dobrowskiSwe')) y <- y / 100000
	if (climVar %in% c('vpdmin', 'vpdmax')) y <- y / 1000000
	return(y)
	
}

# add drive to directories
dobrowskiDailyET0Dir <- paste0(drive, 'ecology/Climate/PRISM/ET0 - Daily - Dobrowski et al 2013')
dobrowskiMonthlyET0Dir <- paste0(drive, 'ecology/Climate/PRISM/ET0 - Monthly - Dobrowski et al 2013')
dobrowskiAnnualET0Dir <- paste0(drive, 'ecology/Climate/PRISM/ET0 - Annual - Sum - Dobrowski et al 2013')
dobrowskiMonthlySweDir <- paste0(drive, 'ecology/Climate/PRISM/Snow Water Equivalent - Monthly - Dobrowski et al 2013')
dobrowskiAnnualSweDir <- paste0(drive, 'ecology/Climate/PRISM/Snow Water Equivalent - Annual - Mean - Dobrowski et al 2013')
prismAN81mDir <- paste0(drive, 'ecology/Climate/PRISM/AN81m 1981-2015')
prismAN81dDir <- paste0(drive, 'ecology/Climate/PRISM/AN81d 1981-2015')

####################
## PRE-PROCESSING ##
####################

xExtracted <- as.data.frame(x) # will get climate columns added to it
xExtracted$coords.x1 <- xExtracted$coords.x2 <- NULL

# xExtracted <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika - Will Thompson & Aaron Johnston for Climate Extraction/Will Thompson/02b !TEMP Site locations_ Will Thompson - Daily - Extracted up to dayMetSwe.rds')

xyDayMet <- sp::spTransform(x, CRS(projection(raster('G:/ecology/Climate/DayMet/V3/GeoTIFF/swe_year1980_doy001.tif'))))

##########
## MAIN ##
##########

	# for each environmental variable
	for (climVar in vars) {

		say(climVar, ' ====================================================================================================', pre=1)

		# make copy of data frame for which to extract data
		xThis <- as.data.frame(x)
		
		# cell area
		if (climVar == 'cellArea') {
		
			template <- raster('G:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
			template <- raster::area(template)
			xThis$cellAreaPrism <- raster::extract(template, cbind(x$coords.x1, x$coords.x2))
			rm(template)
		
			template <- raster('C:/ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075.tif')
			template <- raster::area(template)
			xThis$cellAreaGmted <- raster::extract(elevation, cbind(x$coords.x1, x$coords.x2))
			rm(template)
		
			template <- raster(paste0(dayMetDir, '/GeoTIFF/swe_year1980_doy001.tif'))
			template <- raster::area(template)
			xThis$cellAreaDayMet <- raster::extract(elevation, xyDayMet)
			rm(template)
		
		# PRISM elevation data
		} else if (climVar == 'topo') {

			# elev
			rast <- raster('G:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
			xThis$elevPrism <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
			# rast <- raster('C:/ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075.tif')
			# xThis$elevGmted2010 <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
			# slope
			rast <- raster('G:/ecology/Climate/PRISM/PRISM_us_dem_800m_slope_degrees.tif')
			xThis$slopePrism <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
			# rast <- raster('C:/ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_slope_degrees.tif')
			# xThis$slopeGmted2010 <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
			# aspect
			rast <- raster('G:/ecology/Climate/PRISM/PRISM_us_dem_800m_aspect_degrees.tif')
			xThis$aspectPrism <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
			# rast <- raster('C:/ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_aspect_degrees.tif')
			# xThis$aspectGmted2010 <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))

			# TPI
			rast <- raster('G:/ecology/Climate/PRISM/PRISM_us_dem_800m_topoPositionIndex_m.tif')
			xThis$topoPositionIndexPrism <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
			# rast <- raster('C:/ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_topoPositionIndex_m.tif')
			# xThis$topoPositionIndexGmted2010 <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))

			# TRI
			rast <- raster('G:/ecology/Climate/PRISM/PRISM_us_dem_800m_topoRuggednessIndex_m.tif')
			xThis$topoRuggednessIndexPrism <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
			# rast <- raster('C:/ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075_topoRuggednessIndex_m.tif')
			# xThis$topoRuggednessIndexGmted2010 <- raster::extract(rast, cbind(x$coords.x1, x$coords.x2))
		
		# climate data
		} else {

			### create empty columns
			########################
		
			if ('annual' %in% periods) {
				
				# for each year prior to observation year
				for (year in periodStartEnd) {

					xThis$NEW <- NA
					names(xThis)[ncol(xThis)] <- paste0(climVar, 'Annual_year', year)
					  
				}
				
			}
			
			if ('monthly' %in% periods) {
					
				# for each year/month prior to observation year
				for (year in periodStartEnd) {

					# for each month
					for (i in 1:12) {
						
						xThis$NEW <- NA
						names(xThis)[ncol(xThis)] <- paste0(climVar, 'Monthly_year', year, '_month', prefix(i, 2))
					  
					}
					
				}
				
			}

			if ('daily' %in% periods & (climVar == 'ppt' | climVar=='tmin' | climVar=='tmax' | climVar=='dayMetSwe' | climVar=='dobrowskiET0')) {
					
				# for each year/month/day prior to observation year
				for (year in periodStartEnd) {

					new <- as.data.frame(matrix(rep(NA, 366 * nrow(xThis)), nrow=nrow(xThis)))
					names(new) <- paste0(climVar, 'Daily_year', year, '_doy', prefix(1:366, 3))
					xThis <- cbind(xThis, new)
					
				}
					
			}

			gc()
				
			### for each year for which there is climate data
			#################################################
			
			for (climYear in periodStartEnd) {

				say(climVar, ' ', climYear, ' --------------------------------------------------', pre=1, post=1)
				
				### extract DAILY data
				######################
				
				if ('daily' %in% periods) {
					
					if (climYear >= 1981) {
						
						say('   Extracting DAILY climate for ', climYear, ' ', climVar)
						
						if (climVar == 'dayMetSwe') {
						
							climStack <- stack(listFiles(paste0(dayMetDir, '/SWE - Daily'), pattern=paste0('year', climYear)))
							climExtracted <- raster::extract(climStack, xyDayMet)
						
						} else if (climVar == 'dobrowskiET0') {
						
							climStack <- stack(listFiles(dobrowskiDailyET0Dir, pattern=paste0('_year', climYear)))
							climExtracted <- raster::extract(climStack, x)
						
						} else {
						
							climStack <- stack(listFiles(paste0(prismAN81dDir, '/', climVar, '/', climYear), pattern='.tif'))
							climExtracted <- raster::extract(climStack, x)
							
						}
						
						rm(climStack); gc()
						climExtracted <- rescale(climExtracted, climVar)
						
						# add extra day in non-leap years
						if (ncol(climExtracted)==365) climExtracted <- cbind(climExtracted, matrix(rep(NA, nrow(xThis), ncol=1)))
						
						# remember
						for (i in 1:nrow(xThis)) {
							
							colName <- paste0(climVar, 'Daily_year', climYear, '_doy', prefix(1:366, 3))
							xThis[i, colName] <- climExtracted[i, ]
							
						} # next record
						
						rm(climExtracted)
						   
						gc()
						
					} # if climate year is >=1981
					
				} # if extracting daily
				
				### extract MONTHLY data
				########################
					
				if ('monthly' %in% periods) {
					
					# for each month for which there is climate data
					for (climMonth in 1:12) {

						if (climMonth == 1) say('   Extracting MONTHLY climate for ', climVar, ' ', climYear, post=0)
						say(climMonth, post=ifelse(climMonth==12, 1, 0))
						
						if (climVar == 'dayMetSwe') {
						
							climRast <- raster(paste0(dayMetDir, '/SWE - Monthly - Mean/meanSwe_year', climYear, '_month', prefix(climMonth, 2), '.tif'))
							climExtracted <- c(raster::extract(climRast, xyDayMet))
						
						} else if (climVar == 'dobrowskiSwe') {
							
							climRast <- raster(paste0(dobrowskiMonthlySweDir, '/snowWaterEquivalent_year', climYear, '_month', prefix(climMonth, 2), '_mm.tif'))
							climExtracted <- raster::extract(climRast, x)
							
						} else if (climVar == 'dobrowskiET0') {
						
							climRast <- raster(paste0(dobrowskiMonthlyET0Dir, '/et0_year', climYear, '_month', prefix(climMonth, 2), '.tif'))
							climExtracted <- raster::extract(climRast, x)
						
						} else {
						
							climRast <- raster(paste0(prismAN81mDir, '/', climVar, '/', climYear, '/', 'prism_', climVar, '_us_30s_', climYear, prefix(climMonth, 2), '.tif'))
							climExtracted <- c(raster::extract(climRast, x))
							
						}
							
						rm(climRast)
						climExtracted <- rescale(climExtracted, climVar)
					 
						# remember (and censure data before month of record in year <date of observation> - <periodStartEnd> AND after month of observatoin in <year of observation>)
						for (i in 1:nrow(xThis)) {
							
							colName <- paste0(climVar, 'Monthly_year', climYear, '_month', prefix(climMonth, 2))
							xThis[i, colName] <- climExtracted[i]
							
						} # next record
						
					} # next month
					
				} # if extracting monthly
					
				### extract ANNUAL data
				#######################

				if ('annual' %in% periods) {
					
					say('   Extracting ANNUAL climate for ', climYear, ' ', climVar)
					
					if (climVar == 'dayMetSwe') {

						climRast <- raster(paste0(dayMetDir, '/SWE - Annual - Mean/meanSwe_year', climYear, '.tif'))
						climExtracted <- c(raster::extract(climRast, xyDayMet))
						
					} else if (climVar == 'dobrowskiSwe') {

						climRast <- raster(paste0(dobrowskiAnnualSweDir, '/snowWaterEquivalent_year', climYear, '_mm.tif'))
						climExtracted <- c(raster::extract(climRast, x))
					
					} else if (climVar == 'dobrowskiET0') {

						climRast <- raster(paste0(dobrowskiAnnualET0Dir, '/et0_year', climYear, '.tif'))
						climExtracted <- c(raster::extract(climRast, x))
						
					} else {
					 
						climRast <- raster(paste0(prismAN81mDir, '/', climVar, '/', climYear, '/', 'prism_', climVar, '_us_30s_', climYear, '.tif'))
						climExtracted <- c(raster::extract(climRast, x))
						
					}
					
					rm(climRast)
					climExtracted <- rescale(climExtracted, climVar)

					# remember
					for (i in 1:nrow(xThis)) {
						
						colName <- paste0(climVar, 'Annual_year', climYear)
						xThis[i, colName] <- climExtracted[i]
						
					} # next record
				
					gc()
					
				}

			} # next climate year
			
		} # climate data
			
		xThis$coords.x1 <- xThis$coords.x2 <- NULL
		xExtracted <- cbind(xExtracted, xThis[ , names(xThis)[-which(names(xThis) %in% names(xExtracted))]])
		names(xExtracted)[which(!(names(xThis) %in% names(xExtracted)))] <- names(xThis)[-which(names(xThis) %in% names(xExtracted))]
		rm(xThis)
		gc()

		if (!is.null(savePartial)) {
			
			saveRDS(xExtracted, file=paste0(savePartial, ' - Extracted up to ', climVar, '.rds'))
			say('Note: Saved file when all data for ', climVar, ' was extracted... so if interrupted the file may not have data for all climate variables.', pre=1, post=1)
			
		}
		
	} # next variable

#####################
## POST-PROCESSING ##
#####################

return(xExtracted)

}
