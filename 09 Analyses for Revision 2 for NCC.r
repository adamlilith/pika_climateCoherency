### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2019-04

# source('C:/Ecology/Drive/Research/Iconic Species/pika_climateCoherency/09 Analyses for Revision 2 for NCC.r')

rm(list=ls())

	drive <- 'C:/'
	# drive <- 'D:/'
	# drive <- 'E:/'

### CONTENTS ###
### libraries, variables, and functions ###
### analysis of spatial redundancy between division schemes ###
### correlations between variables calculated using temporal windows of different sizes ###

###########################################
### libraries, variables, and functions ###
###########################################

	source(paste0(drive, 'Ecology/Drive/Research/Iconic Species/pika_climateCoherency/!Omnibus Variables for Pika Non-Stationarity Analysis.r'))

	schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')
	pmes <- c('pmeNone', 'pmeMin')
	
# say('###############################################################')
# say('### analysis of spatial redundancy between division schemes ###')
# say('###############################################################', post=2)

	# say('This routine calculates an index of similarity between division schemes based on the probability that for each unit in a division, two randomly located points in the unit of a first division fall in the same unit in the second division. As the divisions become increasingly similar this probability approaches 1.')

	# # PRISM raster for masking (DEM is larger than climate extent)
	# prismDem <- raster('F:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
	# prismMask <- prismDem * 0 + 1

	# # PME raster
	# pmeMin <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/minHabitable.tif'))
	# projection(pmeMin) <- getCRS('prism')
	
	# # crop DEM and PME rasters to US only
	# load('D:/Ecology/Political Geography/GADM/ver3pt6/Countries WGS84/USA Admin Level 1.Rdata')
	# projection(usaAdmin1) <- getCRS('prism')
	# usaAdmin1 <- usaAdmin1[!(usaAdmin1@data$NAME_1 %in% c('Hawaii', 'Alaska')), ]

	# prismDem <- crop(prismDem, usaAdmin1)
	# pmeMin <- crop(pmeMin, usaAdmin1)
	
	# # stores similarity between divisions
	# similarity <- data.frame()
	
	# # by SCHEME #1
	# for (scheme1 in schemes) {
	
		# out <- schemeInfo(scheme1, poly=TRUE)
		# schemeNice1 <- out$schemeNice
		# divisionFieldPres1 <- out$divisionFieldPres
		# divisionFieldPoly1 <- out$divisionFieldPoly
		# divisionPoly1 <- out$divisionPoly
		# rm(out); gc()

		# # project to PRISM CRS
		# if (projection(divisionPoly1) == getCRS('wgs84')) {
			# projection(divisionPoly1) <- getCRS('prism')
		# } else if (projection(divisionPoly1) != getCRS('prism')) {
			# divisionPoly1 <- sp::spTransform(divisionPoly1, getCRS('prism', TRUE))
		# }
		
		# # get names of focal units in scheme #1
		# units1 <- getUnits(scheme=scheme1, incAll=FALSE)

		# # by SCHEME #2
		# for (scheme2 in schemes[!(schemes %in% scheme1)]) {
		
			# out <- schemeInfo(scheme2, poly=TRUE)
			# schemeNice2 <- out$schemeNice
			# divisionFieldPres2 <- out$divisionFieldPres
			# divisionFieldPoly2 <- out$divisionFieldPoly
			# divisionPoly2 <- out$divisionPoly
			# rm(out); gc()

			# # project to PRISM CRS
			# if (projection(divisionPoly2) == getCRS('wgs84')) {
				# projection(divisionPoly2) <- getCRS('prism')
			# } else if (projection(divisionPoly2) != getCRS('prism')) {
				# divisionPoly2 <- sp::spTransform(divisionPoly2, getCRS('prism', TRUE))
			# }

			# # by PME
			# for (pme1 in pmes) {

				# # by UNIT in SCHEME #1
				# for (unit1 in units1) {
				
					# unitPoly1 <- divisionPoly1[divisionPoly1@data[ , divisionFieldPoly1] == unit1, ]
				
					# # create mask raster for unit in scheme #1
					# maskRectExtent <- if (pme1 == 'pmeNone') {
						# crop(prismDem, unitPoly1)
					# } else if (pme1 == 'pmeMin') {
						# crop(pmeMin, unitPoly1)
					# }
					
					# maskPolyExtent <- raster::rasterize(unitPoly1, maskRectExtent)
					# mask <- maskRectExtent * maskPolyExtent
					
					# # get pairs of random points
					# randPoints1a <- randomPoints(mask, 10000)
					# randPoints1b <- randomPoints(mask, 10000)
					
					# # by PME of scheme #2
					# for (pme2 in pmes) {
					
						# say('Similarity between ', scheme1, ' PME ', pme1, ' unit ', unit1, ' and ', scheme2, ' PME ', pme2, ':', post=0)
					
						# # what unit(s) are do they reside in for scheme #2?
						# inUnits2a <- raster::extract(divisionPoly2, randPoints1a)
						# inUnits2b <- raster::extract(divisionPoly2, randPoints1b)
						
						# if (pme2 == 'pmeNone') {
							# inPme2a <- raster::extract(prismDem, randPoints1a)
							# inPme2b <- raster::extract(prismDem, randPoints1b)
						# } else if (pme2 == 'pmeMin') {
							# inPme2a <- raster::extract(pmeMin, randPoints1a)
							# inPme2b <- raster::extract(pmeMin, randPoints1b)
						# }
						
						# # calculate similarity (frequency with which pairs of points fall in same unit in second scheme)
						# inSameUnit2 <- inUnits2a[ , divisionFieldPoly2] == inUnits2b[ , divisionFieldPoly2]
						# inPme2 <- inPme2a & inPme2b
						# inSameUnit2 <- inSameUnit2 & inPme2
						# unitSimilarity <- sum(inSameUnit2, na.rm=TRUE) / 10000
						
						# # area
						# unitPoly1Ea <- sp::spTransform(unitPoly1, getCRS('albersNA', TRUE))
						# unitArea1 <- gArea(unitPoly1Ea) / 1000^2
						
						# # remember
						# similarity <- rbind(
							# similarity,
							# data.frame(
								# scheme1 = scheme1,
								# unit1 = unit1,
								# pme1 = pme1,
								# scheme2 = scheme2,
								# pme2 = pme2,
								# areaUnitScheme1_km2 = unitArea1,
								# similarity=unitSimilarity
							# )
						# )
						
						# say(round(unitSimilarity, 3))
						
					# } # next PME for SCHEME #2
					
				# } # next UNIT in SCHEME #1
				
			# } # next PME in SCHEME #1
			
		# } # next SCHEME #2
		
	# } # next SCHEME #1
	# say('')
	
	# write.csv(similarity, paste0(workDir, '/Analysis - Non-stationarity/Similarity Test between Divisions and PMEs.csv'), row.names=FALSE)
	
say('###########################################################################################')
say('### correlations between variables calculated using temporal windows of different sizes ###')
say('###########################################################################################')
	
	op <- readRDS(paste0(workDir, '/Species Records - Pika/!Collated Data 2016-06-30 1256/03 Ochotona princeps - Usable - Presences 1990-2015 - PRISM & DayMet Climate Data Extracted.rds'))
	
	op <- op[SUBSETXXXXXXXXXXXXXXX ]
	
	# calculate derived variables for all intervals <= 10 years
	derived <- list()
	for (interval in 1:10) {
	
		say(interval)
	
		vars <- calcDerivedVars(op, window=interval)
		vars <- vars[ , names(vars) %in% predictorsToUse]
		vars <- vars[ , predictorsToUse]
		derived[[interval]] <- vars
	
	}
	
	# calculate correlations
	cors <- data.frame()
	for (interval in 9:1) {
	
		theseCors <- cor(vars[[10]], cor[[interval]])
		names(theseCors) <- predictorsToUse
		cors <- rbind(cors, theseCors)
		
	}
	
	cors <- t(cors)
	rownames(cors) <- predictorsToUse
	
	names(cors) <- paste0('interval', 9:1)
	write.csv(cors, paste0(workDir, './Analysis - Non-stationarity/Correlations between Predictors Calculated Using Different Temporal Windows.csv'), row.names=FALSE)
	
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
