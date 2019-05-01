### Ochotona princeps - Extract Raw Climate Data for Ochotona princeps
### Adam B. Smith | 2017-04

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/02 Pika Minimum Elevation.r')

rm(list=ls())

drive <- 'C:/'
# drive <- 'D:/'
# drive <- 'E:/'

### CONTENTS ###
### libraries, variables, and functions ###
### calculate pika minimum elevation models ###
### calculate PME raster ###

######### NOT USED #########


###########################################
### libraries, variables, and functions ###
###########################################

source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')

# # # say('###############################################')
# # # say('### calculate pika minimum elevation models ###')
# # # say('###############################################')

# # # !!! NOTE !!! This portion of this script was not used in the final analysis.  We used Mimi's PME calculator instead.

# # # numCircs <- 5000 # number of circles with valid records needed
# # # buffer <- 35000 # circle radius
# # # lowestQuant <- 0.05 # lowest quantile across which to calculate minimum elevation
# # # minPresLowestQuant <- 3 # minimum number of points needed in lowest quantile
# # # minSd <- 0.01 # largest standard deviation (as proportion of total elevational range) to keep a circle

# # # sampleSize <- 1000 # number of circles to try each attempt

# # # say('Extracting data for calculation of pika minimum elevation using BRTs, GAMs, and LMs.')
# # # say('Elevation used as the response variable will be the average of all sites within a ', buffer / 1000, '-km radius circle.')
# # # say('Locating ', numCircs, ' circles that meet these criteria:')
# # # say('   * >= ', minPresLowestQuant, ' records that have fall within the lowest ', 100 * lowestQuant, 'th percentile of elevations in this circle.')
# # # say('   * These sites have a sd in elevation > 0 and <= ', 100 * minSd, '% of total elevational range of pikas.')

# # # # get presences and create buffered area from which to draw circles (extent of presences plus buffer plus a little more)
# # # pres <- getPres(canada=TRUE)
# # # presSp <- SpatialPoints(cbind(pres$longWgs84, pres$latWgs84), CRS(crsNad83))
# # # presSpEa <- sp::spTransform(presSp, CRS(crsClimateNA))
# # # presSpExtentEa <- extent(presSpEa)
# # # presSpExtentEa <- as(presSpExtentEa, 'SpatialPolygons')
# # # projection(presSpExtentEa) <- crsClimateNA
# # # presSpExtentEa <- gBuffer(presSpExtentEa, width=2.1 * buffer)
# # # presSpExtent <- sp::spTransform(presSpExtentEa, CRS(crsNad83))

# # # demWC <- raster('C:/ecology/Climate/WORLDCLIM Ver1pt4 Rel 3/30 arcsec/Elevation - 30 arcsec/elevation.tif')
# # # demWC <- crop(demWC, presSpExtent)

# # # areaWC <- raster::area(demWC)
# # # areaWC <- as.matrix(areaWC)
# # # areaWC <- as.matrix(areaWC, byrow=TRUE)

# # # # get elevation for each record... use GMTED2010 if available, then PRISM if available, then WORLDCLIM
# # # pres$elevWorldclim_m <- raster::extract(demWC, presSp)

# # # pres$elevation <- pres$elevGmted2010_m
# # # pres$elevation[is.na(pres$elevation)] <- pres$elevPrism_m[is.na(pres$elevation)]
# # # pres$elevation[is.na(pres$elevation)] <- pres$elevWorldclim_m[is.na(pres$elevation)]

# # # # minimum elevation of records in lowest x% quantile in each circle... NA will mean circle is invalid
# # # minElev <- rep(NA, numCircs)

# # # # matrix, one column per circle, one row per presence... if a given presence is in a given circle, that [col, row] will turn to 1
# # # isInCirc <- matrix(0, ncol=numCircs, nrow=nrow(pres))
	
# # # say('PME | ', date(), ' | number of valid circles:')
# # # validCircs <- 0

# # # # for keeping circle centroids
# # # keepCenters <- data.frame(longitude=rep(NA, numCircs), latitude=rep(NA, numCircs))

# # # # find circles that meet criteria
# # # while (validCircs < numCircs) {

	# # # ### locate centers and create buffers around them

	# # # centerCells <- sample(1:length(areaWC), sampleSize, prob=areaWC, replace=TRUE)

	# # # i <- 1
	
	# # # while (validCircs < numCircs & i <= sampleSize) {

		# # # centerCell <- centerCells[i]
		
		# # # # centers <- randomPoints(demWC, 1)
		# # # centers <- xyFromCell(demWC, centerCell)
		# # # centers <- SpatialPoints(centers, CRS(crsNad83))
		# # # centers <- sp::spTransform(centers, CRS(crsClimateNA))
		# # # circs <- gBuffer(centers, width=buffer, byid=TRUE)
		# # # circs <- sp::spTransform(circs, CRS(crsNad83))
		# # # centers <- sp::spTransform(centers, CRS(crsNad83))
		
		# # # ###  find how many records are in each circle, select circles with >x records in lowest z% quantile and with <=y% of total elevational range among records among these lowest z% of records

		# # # # index of presences in circle
		# # # inCirc <- which(!is.na(over(presSp, circs)))
		
		# # # # records in circle
		# # # if (length(inCirc) > 0) {
			
			# # # # lowest elevations
			# # # inCircLowElev <- inCirc[which(pres$elevation[inCirc] <= quantile(pres$elevation[inCirc], lowestQuant, na.rm=TRUE))]
			# # # lowElevs <- pres$elevation[inCircLowElev]

			# # # # remove NAs
			# # # naElev <- which(is.na(lowElevs))
			# # # if (length(naElev) > 0) {
				# # # lowElevs <- lowElevs[-naElev]
				# # # inCircLowElev <- inCircLowElev[-naElev]
			# # # }
			
			# # # # if circle meets criteria, remember stats
			# # # if (sum(!is.na(lowElevs)) >= minPresLowestQuant & sd(lowElevs, na.rm=TRUE) > 0 & sd(lowElevs, na.rm=TRUE) <= minSd * diff(range(pres$elevation, na.rm=TRUE))) {

				# # # validCircs <- validCircs + 1
				# # # say(validCircs, post=ifelse(validCircs %% 250 == 0, 1, 0))
				
				# # # minElev[validCircs] <- mean(lowElevs)
				# # # isInCirc[inCircLowElev, validCircs] <- 1
				# # # keepCenters[validCircs, ] <- coordinates(centers)

			# # # }
			
		# # # }
		
		# # # i <- i + 1
		
	# # # } # next candidate circle

# # # } # try to find next valid circle
	
# # # ### calculate weights of each circle... weight of circle in which all used records are used in no other circle is 1...
# # # ### elsewise, weight for a circle:
# # # ### 1 assign each used record a value equal to 1 / number of times used in any circle
# # # ### 2 sum these values for records used in a circle
# # # ### 3 divide by total number of records used in that circle... values range in (0, 1]
# # # presWeights <- 1 / rowSums(isInCirc)
# # # presWeights[is.infinite(presWeights)] <- 0
# # # circWeights <- c(presWeights %*% isInCirc / colSums(isInCirc))
# # # circWeights <- circWeights / max(circWeights)

# # # say('Modeling PME', pre=1)
	
	# # # data <- data.frame(minElev=minElev, longitude=keepCenters[ , 1], latitude=keepCenters[ , 2])
	
	# # # ### brt
	# # # modelBrt <- gbm.step(
		# # # data=data,
		# # # gbm.x=c('longitude', 'latitude'),
		# # # gbm.y='minElev',
		# # # family='gaussian',
		# # # tree.complexity=12,
		# # # learning.rate=0.0001,
		# # # bag.fraction=0.7,
		# # # n.trees=4000,
		# # # max.trees=4000,
		# # # site.weights=circWeights,
		# # # plot.main=FALSE,
		# # # plot.folds=FALSE,
		# # # silent=TRUE,
		# # # verbose=TRUE,
	# # # )

	# # # ### gam
	# # # modelGam <- gam(
		# # # formula=minElev ~ s(longitude) + s(latitude) + s(longitude, latitude),
		# # # family='gaussian',
		# # # data=data,
		# # # scale=-1,
		# # # select=TRUE,
		# # # weights=circWeights
	# # # )

	# # # ### lm
	# # # modelLm <- lm(
		# # # formula=minElev ~ longitude * latitude,
		# # # data=data,
		# # # weights=circWeights
	# # # )

	# # # ### remember models
	# # # minElevModels <- list()

	# # # minElevModels$numCircs <- numCircs
	# # # minElevModels$buffer <- buffer
	# # # minElevModels$lowestQuant <- lowestQuant
	# # # minElevModels$minPresLowestQuant <- minPresLowestQuant
	# # # minElevModels$minSd <- minSd

	# # # minElevModels$circles <- data.frame(longitude=keepCenters[ , 1], latitude=keepCenters[ , 2], minElev=minElev, circWeight=circWeights)

	# # # minElevModels$models <- list()
	# # # minElevModels$models$brt <- modelBrt
	# # # minElevModels$models$gam <- modelGam
	# # # minElevModels$models$lm <- modelLm
	
	# # # gc()

	# # # # save
	# # # dirCreate(workDir, 'Pika Minimum Elevation')
	# # # saveRDS(minElevModels, paste0(workDir, 'Pika Minimum Elevation/Pika Minimum Elevation Models.rds'))

	# # # ### plot
	# # # gadm <- shapefile('C:/ecology/Political Geography/GADM/ver2/gadm2_northAmericaAndCentralAmerica_sansAlaska_dissolvedToLevel1')

	# # # par(mfrow=c(1, 2))
	
	# # # plot(demWC, main='Circles with Presences')
	# # # plot(gadm, add=TRUE)
	# # # pts <- SpatialPoints(minElevModels$circles[ , 1:2], CRS(crsWgs84))
	# # # pts <- sp::spTransform(pts, CRS(crsClimateNA))
	# # # buffs <- gBuffer(pts, width=35000, byid=TRUE)
	# # # buffs <- sp::spTransform(buffs, CRS(crsWgs84))
	# # # plot(buffs, border='red', add=TRUE)
	# # # points(presSp, pch=16, cex=0.4)
	# # # legend('bottomleft', inset=0.01, legend=c('presence', 'circle'), col=c('black', 'red'), pch=c(16, NA), lwd=c(NA, 1))
	
	# # # plot(demWC, main='Circle centroids scaled to PME')
	# # # plot(gadm, add=TRUE)
	# # # points(minElevModels$circles[ , 1:2], cex=minElevModels$circles$minElev / 2000)
	
	# # # hist(minElevModels$circles$minElev)
	
	
# # # say('############################')
# # # say('### calculate PME raster ###')
# # # say('############################')

	# # # !!! NOTE !!! This portion of this script was not used in the final analysis.  We used Mimi's PME calculator instead.

	# # # say('PME raster is min/mean/max across the three algorithms.', post=1)
	# # # minElevModels <- readRDS(paste0(workDir, 'Pika Minimum Elevation/Pika Minimum Elevation Models.rds'))

	# # # # get presences and create buffered area from which to draw circles (extent of presences plus buffer plus a little more)
	# # # pres <- getPres(canada=TRUE)
	# # # presSp <- SpatialPoints(cbind(pres$longWgs84, pres$latWgs84), CRS(crsNad83))
	# # # presSpEa <- sp::spTransform(presSp, CRS(crsClimateNA))
	# # # presSpExtentEa <- extent(presSpEa)
	# # # presSpExtentEa <- as(presSpExtentEa, 'SpatialPolygons')
	# # # projection(presSpExtentEa) <- crsClimateNA
	# # # presSpExtentEa <- gBuffer(presSpExtentEa, width=10 * minElevModels$buffer)
	# # # presSpExtent <- sp::spTransform(presSpExtentEa, CRS(crsNad83))

	# # # demWC <- raster('C:/ecology/Climate/WORLDCLIM Ver1pt4 Rel 3/30 arcsec/Elevation - 30 arcsec/elevation.tif')
	# # # demWC <- crop(demWC, presSpExtent)

	# # # demPrism <- raster('G:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
	# # # demPrism <- crop(demPrism, presSpExtent)

	# # # glims <- shapefile(paste0(workDir, 'Extents_Masks_Maps/PikaMinElevation/!ModelDocumentation/GlacierDatasets/GLIMS_NorthAmerica/glims_polygons'))
	
	# # # # predict PME raster for each algorithm
	# # # for (longLat in c('prism', 'worldclim')) {
	
		# # # say('Calculating PME rasters for ', longLat, post=0)

		# # # if (longLat == 'worldclim') { 
			# # # longLatRasts <- longLatRasters(template=demWC, mask=demWC)
			# # # dem <- demWC
		# # # } else {
			# # # longLatRasts <- longLatRasters(template=demPrism, mask=demPrism)
			# # # dem <- demPrism
		# # # }
		
		# # # say('| BRT', post=0)
		
			# # # brtRast <- predict(
				# # # longLatRasts,
				# # # minElevModels$models$brt,
				# # # n.trees=minElevModels$models$brt$gbm.call$best.trees,
				# # # na.rm=TRUE,
				# # # type='response'
			# # # )
		
		# # # say('| GAM', post=0)
		
			# # # gamRast <- predict(
				# # # longLatRasts,
				# # # minElevModels$models$gam,
				# # # na.rm=TRUE,
				# # # type='response'
			# # # )
			
		# # # say('| LM', post=0)
		
			# # # lmRast <- predict(
				# # # longLatRasts,
				# # # minElevModels$models$lm,
				# # # na.rm=TRUE
			# # # )
			
		# # # par(mfrow=c(1, 3))
		# # # plot(brtRast, main='BRT PME')
		# # # plot(gamRast, main='GAM PME')
		# # # plot(lmRast, main='LM PME')

		# # # say('| min/mean/max across algorithms', post=0)

		# # # pme <- stack(brtRast, gamRast, lmRast)

		# # # pmeMin <- min(pme)
		# # # pmeMean <- mean(pme)
		# # # pmeMax <- max(pme)
		
		# # # pmeMin <- setMinMax(pmeMin)
		# # # pmeMean <- setMinMax(pmeMean)
		# # # pmeMax <- setMinMax(pmeMax)
		
		# # # names(pmeMin) <- 'pmeMin'
		# # # names(pmeMean) <- 'pmeMean'
		# # # names(pmeMax) <- 'pmeMax'

		# # # dirCreate(workDir, 'Pika Minimum Elevation/Rasters - ', toupper(longLat), ' Elevation Basemap')
		
		# # # writeRaster(pmeMin, paste0(workDir, 'Pika Minimum Elevation/Rasters - ', toupper(longLat), ' Elevation Basemap/pme_ensembleMin'), format='GTiff', overwrite=TRUE)
		# # # writeRaster(pmeMean, paste0(workDir, 'Pika Minimum Elevation/Rasters - ', toupper(longLat), ' Elevation Basemap/pme_ensembleMean'), format='GTiff', overwrite=TRUE)
		# # # writeRaster(pmeMax, paste0(workDir, 'Pika Minimum Elevation/Rasters - ', toupper(longLat), ' Elevation Basemap/pme_ensembleMax'), format='GTiff', overwrite=TRUE)

		# # # say('| create mask (area above PME, +3-km buffer, 3-km buffer a/r records still outside, add cells removed with records)', post=1)
		
		# # # # area above PME
		# # # pmeMinMask <- pmeMin <= dem
		# # # pmeMeanMask <- pmeMean <= dem
		# # # pmeMaxMask <- pmeMax <= dem
		
		# # # pmeMinMask <- calc(pmeMinMask, fun=function(x) ifelse(x == 0, NA, 1))
		# # # pmeMeanMask <- calc(pmeMeanMask, fun=function(x) ifelse(x == 0, NA, 1))
		# # # pmeMaxMask <- calc(pmeMaxMask, fun=function(x) ifelse(x == 0, NA, 1))
		
		# # # ### +3-km buffer
		# # # pmeMinMask <- raster::buffer(pmeMinMask, width=3000)
		# # # pmeMeanMask <- raster::buffer(pmeMeanMask, width=3000)
		# # # pmeMaxMask <- raster::buffer(pmeMaxMask, width=3000)

		# # # ### add 3-km buffer a/r records still outside
		
		# # # # find records outside
		# # # insideMin <- extract(pmeMinMask, presSp)
		# # # insideMean <- extract(pmeMeanMask, presSp)
		# # # insideMax <- extract(pmeMaxMask, presSp)

		# # # # get records outside
		# # # presOutMin <- presSp[is.na(insideMin)]
		# # # presOutMean <- presSp[is.na(insideMean)]
		# # # presOutMax <- presSp[is.na(insideMax)]
		
		# # # # create 3-km buffer around them
		# # # presOutMin <- sp::spTransform(presOutMin, CRS(crsClimateNA))
		# # # presOutMean <- sp::spTransform(presOutMean, CRS(crsClimateNA))
		# # # presOutMax <- sp::spTransform(presOutMax, CRS(crsClimateNA))

		# # # presOutBuffMin <- gBuffer(presOutMin, width=3000)
		# # # presOutBuffMean <- gBuffer(presOutMean, width=3000)
		# # # presOutBuffMax <- gBuffer(presOutMax, width=3000)
		
		# # # presOutBuffMin <- sp::spTransform(presOutBuffMin, CRS(projection(dem)))
		# # # presOutBuffMean <- sp::spTransform(presOutBuffMean, CRS(projection(dem)))
		# # # presOutBuffMax <- sp::spTransform(presOutBuffMax, CRS(projection(dem)))
		
		# # # # create mask using buffers
		# # # presOutMaskMin <- mask(pmeMin, presOutBuffMin)
		# # # presOutMaskMean <- mask(pmeMean, presOutBuffMean)
		# # # presOutMaskMax <- mask(pmeMax, presOutBuffMax)
		
		# # # # combine masks
		# # # pmeMinMask <- calc(stack(pmeMinMask, presOutMaskMin), fun=function(x) max(x, na.rm=TRUE))
		# # # pmeMeanMask <- calc(stack(pmeMeanMask, presOutMaskMean), fun=function(x) max(x, na.rm=TRUE))
		# # # pmeMaxMask <- calc(stack(pmeMaxMask, presOutMaskMax), fun=function(x) max(x, na.rm=TRUE))
		
		# # # ### remove ice/snow
		# # # glims <- sp::spTransform(glims, CRS(projection(pmeMin)))
		
		# # # pmeMinMask <- mask(pmeMinMask, glims, inverse=TRUE)
		# # # pmeMeanMask <- mask(pmeMeanMask, glims, inverse=TRUE)
		# # # pmeMaxMask <- mask(pmeMaxMask, glims, inverse=TRUE)
		
		# # # ### add cells of records now outside (in GLIMS area)
		
		# # # # find records outside
		# # # insideMin <- extract(pmeMinMask, presSp)
		# # # insideMean <- extract(pmeMeanMask, presSp)
		# # # insideMax <- extract(pmeMaxMask, presSp)

		# # # # get records outside
		# # # presOutMin <- presSp[is.na(insideMin)]
		# # # presOutMean <- presSp[is.na(insideMean)]
		# # # presOutMax <- presSp[is.na(insideMax)]
		
		# # # # get cell numbers of presences outside
		# # # cellNumOutMin <- cellFromXY(dem, presOutMin)
		# # # cellNumOutMean <- cellFromXY(dem, presOutMean)
		# # # cellNumOutMax <- cellFromXY(dem, presOutMax)
		
		# # # # unmask these cells
		# # # pmeMinMask[cellNumOutMin] <- 1
		# # # pmeMeanMask[cellNumOutMean] <- 1
		# # # pmeMaxMask[cellNumOutMax] <- 1
		
		# # # pmeMin <- pmeMin * pmeMinMask
		# # # pmeMean <- pmeMean * pmeMeanMask
		# # # pmeMax <- pmeMax * pmeMaxMask
		
		# # # ### save!
		# # # writeRaster(pmeMin, paste0(workDir, 'Pika Minimum Elevation/Rasters - ', toupper(longLat), ' Elevation Basemap/pme_ensembleMin_bufferedAndMasked'), format='GTiff', overwrite=TRUE)
		# # # writeRaster(pmeMean, paste0(workDir, 'Pika Minimum Elevation/Rasters - ', toupper(longLat), ' Elevation Basemap/pme_ensembleMean_bufferedAndMasked'), format='GTiff', overwrite=TRUE)
		# # # writeRaster(pmeMax, paste0(workDir, 'Pika Minimum Elevation/Rasters - ', toupper(longLat), ' Elevation Basemap/pme_ensembleMax_bufferedAndMasked'), format='GTiff', overwrite=TRUE)
		
		# # # gc()
		
	# # # } # next WC/PRISM

say('########################################')
say('### nice map of Mimis version of PME ###')
say('########################################')
	
	pme <- raster('C:/ecology/Drive/Research/Iconic Species/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/!minPME.tif')
	elev <- raster('C:/ecology/Drive/Research/Iconic Species/Environmental Data/PRISM/PRISM_us_dem_800m.tif')
	iucn <- shapefile('C:/ecology/Drive/Research/Iconic Species/Extents_Masks_Maps/Range Maps/iucnMammals_version4_ochotonaPrinceps')
	canada <- shapefile('C:/ecology/Political Geography/GADM/ver2pt8/WGS84/CAN_adm1')
	usa <- shapefile('C:/ecology/Political Geography/GADM/ver2pt8/WGS84/USA_adm1')
	mexico <- shapefile('C:/ecology/Political Geography/GADM/ver2pt8/WGS84/MEX_adm1')
	
	nam <- rbind(canada, usa, mexico)
	
	iucn <- sp::spTransform(iucn, getCRS('albersNA', TRUE))
	iucnBuff <- rgeos::gBuffer(iucn, width=1000000)
	iucnBuff <- sp::spTransform(iucnBuff, getCRS('wgs84', TRUE))
	
	westNam <- crop(nam, iucnBuff)
	
	pme <- crop(pme, westNam)
	westNamRast <- rasterize(westNam, pme)
	pme <- mask(pme, westNamRast)
	
	plot(pme)
	plot(westNam, add=TRUE)
	
	
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=2)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
