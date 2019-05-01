### Ochotona princeps - Extract Raw Climate Data for Ochotona princeps
### Adam B. Smith | 2016-08

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/02 Generate and Extract Raw Climate Data for Background Sites.r')

rm(list=ls())
	
	drive <- 'C:/'
	# drive <- 'D:/'
	# drive <- 'E:/'

	source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')

	set <- 1
	# set <- 2

	# totalBg <- 10000
	totalBg <- nrow(getPres()) - 10000

	# do <- c('locate bg', 'extract', 'derived')
	# do <- c('extract', 'derived')
	# do <- 'locate bg'
	do <- 'illustrate bg'
	# do <- 'extract'
		# extractOption <- 'all' # extract all climate data
		# extractOption <- 'onlyprism' # extract non-PRISM data
		# extractOption <- 'nonprism' # extract non-PRISM data
	# do <- 'derived'
	
	pmes <- c('pmeMin', 'pmeNone')
	# pmes <- 'pmeMin'
	# pmes <- 'pmeNone'
	
	# valances <- c('including', 'excluding')
	valances <- 'including'
	# valances <- 'excluding'

	# schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPmeMin', 'physioFenneman')

	schemes <- 'cladeNonOverlap'
		# forceUnit <- NULL
		# forceUnit <- 'all'
		# forceUnit <- 'fenisex'
		# forceUnit <- 'princeps'
		# forceUnit <- 'saxatilis'
		forceUnit <- 'schisticeps'
		# forceUnit <- 'uinta'
	
	# schemes <- 'ecoregionEpa3Modified'

		# forceUnit <- c('all', 'Blue Mtns & Idaho Batholith', 'Canadian Rockies', 'Cascades', 'Cent Basin & Range', 'Colo Plats & Wasatch-Uinta Mtns', 'E Cascades Slopes & Foothills', 'N Basin & Range', 'N Basin & Range', 'N Cascades', 'N Rockies', 'S Rockies', 'Sierra Nevada', 'Snake R Plain & Mid-Rockies')
		# forceUnit <- c('Sierra Nevada', 'Snake R Plain & Mid-Rockies')

		# forceUnit <- NULL
		# forceUnit <- 'all'
		# forceUnit <- 'Blue Mtns & Idaho Batholith'
		# forceUnit <- 'Canadian Rockies'
		# forceUnit <- 'Cascades'
		# forceUnit <- 'Cent Basin & Range'
		# forceUnit <- 'Colo Plats & Wasatch-Uinta Mtns'
		# forceUnit <- 'E Cascades Slopes & Foothills'
		# forceUnit <- 'N Basin & Range'
		# forceUnit <- 'N Cascades'
		# forceUnit <- 'N Rockies'
		# forceUnit <- 'S Rockies'
		# forceUnit <- 'Sierra Nevada'
		# forceUnit <- 'Snake R Plain & Mid-Rockies'
		
	# schemes <- 'elevQuantWrtPaeMin'

		# forceUnit <- NULL
		# forceUnit <- c('Lowest', 'Low', 'Middle', 'High', 'Highest')
	
		# forceUnit <- 'all'
		# forceUnit <- 'Lowest'
		# forceUnit <- 'Low'
		# forceUnit <- 'Middle'
		# forceUnit <- 'High'
		# forceUnit <- 'Highest'
	
	# schemes <- 'physioFenneman'
		# forceUnit <- NULL
		# forceUnit <- 'all'
		# forceUnit <- 'Cascades'
		# forceUnit <- 'Sierra Nevada'
		# forceUnit <- 'Northern Rockies'
		# forceUnit <- 'Southern Rockies'
		# forceUnit <- 'Great Basin'
	


### CONTENTS ###
### libraries, variables, and functions ###
### draw SPATIOTEMPORALLY-WEIGHTED background sites and extract raw climate data ###
### illustrate drawing of SPATIOTEMPORALLY-WEIGHTED background sites ###
### calculate derived climate variables for SPATIOTEMPORALLY-WEIGHTED background sites ###
### draw RANDOM background sites and from across western US extract raw climate data ###
### draw REPRESENTATIVE sites from each division unit plus random background for all units and extract raw climate data ###


###########################################
### libraries, variables, and functions ###
###########################################

# years of presence data to consider
years <- 1990:2015

####################################################################################
### draw SPATIOTEMPORALLY-WEIGHTED background sites and extract raw climate data ###
####################################################################################

if ('locate bg' %in% do) {

	say('draw SPATIOTEMPORALLY-WEIGHTED background sites and extract raw climate data', level=1)

	## BAUHAUS
	# 1 calculate weight of all presences using temporal overlap with other presences in same cell
	# 2 calculate annual KDE from presences in each year using PRISM DEM as base raster (which is larger in extent than PRISM climate rasters)
	# 3 crop annual KDE to PRISM climate raster extent
	# 4 rescale annual KDE so that the sum of cell values is equal to number of background sites to be drawn from it (=number of presences in that year)
	# 5 sum all annual KDE rasters then draw background sites in proportion to cell weights

	# presences
	pres <- getPres()
	# PRISM raster for masking (DEM is larger than climate extent)
	prismDem <- raster('F:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
	prismMask <- prismDem * 0 + 1

	presSp <- SpatialPointsDataFrame(cbind(pres$longWgs84, pres$latWgs84), data=pres, proj4string=getCRS('nad83', TRUE))

	# PME rasters
	pmeMin <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/minHabitable.tif'))
	pmeMean <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/meanHabitable.tif'))
	pmeMax <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/maxHabitable.tif'))
	pmeNone <- pmeMin
	pmeNone[] <- 1

	projection(pmeMin) <- projection(getCRS('nad83'))
	projection(pmeMean) <- projection(getCRS('nad83'))
	projection(pmeMax) <- projection(getCRS('nad83'))
	projection(pmeNone) <- projection(getCRS('nad83'))

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=TRUE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		divisionPoly <- out$divisionPoly
		rm(out); gc()

		# project division polygon
		divisionPoly <- sp::spTransform(divisionPoly, CRS(getCRS('nad83')))

		# by VALANCE
		for (unitValance in valances) {
		
			# by PME
			for (pmeVariant in pmes) {
			
				pmeNice <- pmeNiceName(pmeVariant)

				# get names of focal units to include/exclude
				units <- if (is.null(forceUnit)) {
					getUnits(scheme=scheme, incAll=(unitValance == 'including'))
				} else {
					forceUnit
				}
				
				say('UNITS: ', paste(units, collapse=' | '))

				# by UNIT
				for (thisUnit in units) {
			
					say('===============================================================', pre=2)
					say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', unitValance, ' | UNIT ', thisUnit, breaks=NULL)
					
					if (exists('bg')) rm(bg)
					
					# get this unit's polygon... defined as all polygons containing at least one presence assigned to this thisUnit
					thisUnitPres <- if (thisUnit=='all') {
						pres
					} else if (unitValance == 'including') {
						pres[which(pres[ , divisionFieldPres] == thisUnit), ]
					} else if (unitValance == 'excluding') {
						pres[which(pres[ , divisionFieldPres] != thisUnit), ]
					}
					
					thisUnitPresSp <- SpatialPoints(cbind(thisUnitPres$longWgs84, thisUnitPres$latWgs84), CRS(getCRS('nad83')))
				
					# which unit(s) has these points?
					polyContainsUnitPoints <- over(divisionPoly, thisUnitPresSp, returnList=TRUE)
					polyContainsUnitPoints <- unlist(lapply(polyContainsUnitPoints, length)) > 0
				
					thisUnitPoly <- divisionPoly[polyContainsUnitPoints, ]
					thisUnitPoly <- gUnaryUnion(thisUnitPoly)

					dirCreate(workDir, 'Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice,  ' PME 00 Sites')
					dirCreate(workDir, 'Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice,  ' PME 01 Raw Climate')
		
					pdf(paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 00 Sites/!BG Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME.pdf'), paper='letter', width=6.5, height=7.5)

						# # dissolve thisUnit polygon and add buffer if it's not to be masked by PME
						# if (pmeVariant == 'pmeNone') {
						
							# origCrs <- projection(thisUnitPoly)
							# thisUnitPoly <- sp::spTransform(thisUnitPoly, getCRS('climateNA', TRUE))
							# thisUnitPoly <- gBuffer(thisUnitPoly, width=67000)
							# thisUnitPoly <- sp::spTransform(thisUnitPoly, CRS(origCrs))
						
						# }
						
						# create base mask from which to create KDE (but NOT from which to draw background sites)
						prismMaskThisUnit <- crop(prismMask, thisUnitPoly)
				
						pme <- get(pmeVariant)
						pme <- crop(pme, thisUnitPoly)
						
						thisUnitRast <- rasterize(thisUnitPoly, pme)
						
						# clip to PME-adjusted available habitat
						availHabitat <- thisUnitRast * pme * 0 + 1
						availHabitat <- crop(availHabitat, prismMaskThisUnit)
						availHabitatEa <- projectRaster(availHabitat, crs=getCRS('climateNA'))
						availHabitatEa <- availHabitatEa * 0 + 1

						# create 2D cell array for binning presences
						yCells <- seq(raster::extent(availHabitatEa)@ymin, raster::extent(availHabitatEa)@ymax, length.out=round(nrow(availHabitatEa) / resMult))
						xCells <- seq(raster::extent(availHabitatEa)@xmin, raster::extent(availHabitatEa)@xmax, length.out=round((extent(availHabitatEa)@xmax - extent(availHabitatEa)@xmin) / (abs(yCells[1] - yCells[2]))))

						grid <- SpatialPixels(
							SpatialPoints(
								coords=cbind(rep(xCells, each=length(yCells)), rep(yCells, length(xCells))),
								proj4string=getCRS('climateNA', TRUE)
							)
						)						

						# # # # calculate weight of each year's KDE raster relative to others
						# # # samples <- hist(thesePres$obsYear, breaks=(min(years) - 1):max(years), plot=FALSE)

						# calculate weights (number of background sites) to assign to each presence
						nBg <- raster::extract(prismMask, cbind(thisUnitPres$longWgs84, thisUnitPres$latWgs84))
						nBgNotNA <- which(!is.na(nBg))
						nBgIsNa <- which(is.na(nBg))
						
						# subtract or add additional background sites in proportion to number already assigned so total = desired number
						if (sum(nBg, na.rm=TRUE) < totalBg) { # add

							i <- sample(nBgNotNA, totalBg - sum(nBg, na.rm=TRUE), replace=TRUE, prob=nBg[nBgNotNA])
							for (j in i) nBg[j] <- nBg[j] + 1
							
						} else if (sum(nBg, na.rm=TRUE) > totalBg) { # subtract

							i <- sample(nBgNotNA, sum(nBg, na.rm=TRUE) - totalBg, prob=nBg[nBgNotNA])
							nBg[i] <- nBg[i] - 1
							
						}
						
						# for each year train KDE and draw background sites in proportion to density of presence sites in that year
						for (countYear in seq_along(years)) {
						
							# if any records this year and any existing records need matching background sites
							if (years[countYear] %in% thisUnitPres$obsYear & sum(nBg[thisUnitPres$obsYear==years[countYear]], na.rm=TRUE) > 0) {

								say(years[countYear], post=0)
							
								# get presences from this year
								thisPresIndex <- which(thisUnitPres$obsYear==years[countYear]) # index of this year's presences

								thisPres <- thisUnitPres[thisPresIndex, ]
								thisPres <- SpatialPointsDataFrame(coords=cbind(thisPres$longWgs84, thisPres$latWgs84), data=thisPres, proj4string=CRS(getCRS('nad83')))
								thisPresEa <- SpatialPoints(coords=cbind(thisPres$longWgs84, thisPres$latWgs84), proj4string=CRS(getCRS('nad83')))
								thisPresEa <- spTransform(thisPresEa, CRSobj=getCRS('climateNA', TRUE))
								
								sumKde <- 0 # dummy value for sum of KDE raster cells
								
								# while the KDE has no cells with density > 0 from which to pick (0 densities occur if presences are outside PME raster area)
								while (sumKde == 0) {
								
									# if too few presences (<5) to train KDE, then artifically augment with same presences and add slight scatter so there's enough to train KDE
									if (length(thisPresEa) < 5) {
										
										newPres <- as.data.frame(thisPresEa)

										while (length(thisPresEa) < 5) {
											
											scatteredPres <- newPres + cbind(rnorm(nrow(newPres), 0, 1.96 * 3000), rnorm(nrow(newPres), 0, 1.96 * 3000))
											thisPresEa <- rbind(thisPresEa, SpatialPoints(coords=scatteredPres, proj4string=CRS(projection(thisPresEa))))
											
										}
										
									}
										
									# Epanechnikov estimator for bandwidth
									h <- 0.5 * sum(apply(coordinates(thisPresEa), 2, sd, na.rm=T)) * length(thisPresEa)^(-1/6) * 1.77

									# train kernel density estimator
									kde <- kernelUDAdapt(
										xy=thisPresEa,
										h=h,
										kern='epa',
										grid=grid,
										extent=1,
										epsilon=20
									)

									kde <- raster(kde)
									
									kde <- projectRaster(kde, availHabitat)
									kde <- kde * availHabitat
									kde <- projectRaster(kde, prismMaskThisUnit)
									kde <- kde * prismMaskThisUnit
									names(kde) <- paste0('kde', years[countYear])

									sumKde <- cellStats(kde, 'sum')

									# if KDE has all zero densities (ie, if no presences fall within its mask), add random jitter to presences and try again... presumably eventually enough will fall in the mask to generate a non-zero KDE
									if (sumKde == 0) {
									
										thisPresEa <- coordinates(thisPresEa) + cbind(rnorm(length(thisPresEa), 0, 1.96 * 3000), rnorm(length(thisPresEa), 0, 1.96 * 3000))
										thisPresEa <- SpatialPoints(coords=thisPresEa, proj4string=getCRS('climateNA', TRUE))
									
									}
									
								} # while KDE has all 0 densities
								
								kde <- setMinMax(kde)
								kde <- kde + minValue(kde)
								kde <- stretch(kde, 0, maxValue(kde))
								
								thisBg <- as.data.frame(sampleRast(kde, max(1, sum(nBg[thisPresIndex])), replace=TRUE, prob=TRUE))
								
								names(thisBg) <- c('longWgs84', 'latWgs84')
								
								# add year/month/doy of matching presence for each BG site... if number of presences = number of background sites, then each presence is matched ONCE by a background site or if >, then AT LEAST ONCE by a background site and possibly more
								obsYear <- rep(years[countYear], nrow(thisBg))
								matchingPres <- if (nrow(thisPres) > nrow(thisBg)) {
									sample(1:nrow(thisPres), nrow(thisBg), replace=FALSE)
								} else if (nrow(thisPres) == nrow(thisBg)) {
									1:nrow(thisPres)
								} else if (nrow(thisPres) < nrow(thisBg)) {
									c(1:nrow(thisPres), sample(1:nrow(thisPres), nrow(thisBg) - nrow(thisPres), replace=TRUE))
								}
									
								obsMonth <- thisPres$obsMonth[matchingPres]
								obsDayOfYear <- thisPres$obsDayOfYear[matchingPres]
								num <- as.character(thisPres$num[matchingPres])
								
								thisBg <- cbind(thisBg, data.frame(matchingPresNum=num, obsYear=obsYear, obsMonth=obsMonth, obsDayOfYear=obsDayOfYear))
								
								bg <- if (exists('bg', inherits=FALSE)) {
									rbind(bg, thisBg)
								} else {
									thisBg
								}
							
								# plot this year's BG
								main <- paste0('Spatiotemporally Weighted Background for ', years[countYear], '\n', nrow(thisPres), ' Presence(s) | ', nrow(thisBg), '  Background Site(s)')
								
								par(mar=c(0, 0, 0, 0) + 0.1, fg='white', col.main='black')
								plot(kde, legend=FALSE)
								plot(thisUnitPoly, add=TRUE, border=alpha('blue', 0.5))
								points(thisBg$longWgs84, thisBg$latWgs84, pch=16, cex=0.5, col=alpha('red', 0.1), xpd=NA)
								points(thisPres, pch=1, col=alpha('black', 0.5), xpd=NA)
								legend('bottomright', legend=c('presence', 'background'), pch=c(1, 16), pt.cex=c(1, 0.5), col=c('black', 'red'), box.col='black', text.col='black', xpd=NA)
								title(main=main, sub=base::date(), outer=TRUE, line=-3, xpd=NA)
							
							} # if this year had any records and if existing records need matching background sites

						} # next year
						
						# plot all year's BG
						main <- paste0('Spatiotemporally Weighted Background for All Years\nUsing ', schemeNice, ' Division Scheme\nfor ', toupper(unitValance), ' ', toupper(thisUnit), ' using ', pmeNice, ' PME\n', nrow(thisUnitPres), ' Presences | ', nrow(bg), ' Background Site(s)')
						
						par(mar=c(0, 0, 0, 0) + 0.1, fg='white', col.main='black')
						plot(kde, col='white', legend=FALSE)
						plot(thisUnitPoly, add=TRUE, border=alpha('blue', 0.5))
						points(bg$longWgs84, bg$latWgs84, pch=16, cex=0.5, col=alpha('red', 0.5))
						points(thisUnitPres$longWgs84, thisUnitPres$latWgs84, pch=1, col=alpha('black', 0.1))
						legend('bottomright', legend=c('presence', 'background'), pch=c(1, 16), pt.cex=c(1, 0.5), col=c('black', 'red'), box.col='black', text.col='black')
						title(main=main, sub=base::date(), outer=TRUE, line=-3, xpd=NA)

					dev.off()

					bg <- bg[sample(1:nrow(bg), nrow(bg)), ]
					
					## save
					saveRDS(bg, paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 00 Sites/BG Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME.rds'))

				} # next unit
				
			} # next PME
			
		} # next valance
	
	} # next scheme
	
}

########################################################################
### illustrate drawing of SPATIOTEMPORALLY-WEIGHTED background sites ###
########################################################################

if ('illustrate bg' %in% do) {

	say('illustrate drawing of SPATIOTEMPORALLY-WEIGHTED background sites', level=1)

	years <- 2008
	# years <- 2009
	
	## BAUHAUS
	# 1 calculate weight of all presences using temporal overlap with other presences in same cell
	# 2 calculate annual KDE from presences in each year using PRISM DEM as base raster (which is larger in extent than PRISM climate rasters)
	# 3 crop annual KDE to PRISM climate raster extent
	# 4 rescale annual KDE so that the sum of cell values is equal to number of background sites to be drawn from it (=number of presences in that year)
	# 5 sum all annual KDE rasters then draw background sites in proportion to cell weights

	# presences
	pres <- getPres()
	# PRISM raster for masking (DEM is larger than climate extent)
	prismDem <- raster('F:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
	prismMask <- prismDem * 0 + 1

	presSp <- SpatialPointsDataFrame(cbind(pres$longWgs84, pres$latWgs84), data=pres, proj4string=getCRS('nad83', TRUE))

	# PME rasters
	pmeMin <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/minHabitable.tif'))
	pmeMean <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/meanHabitable.tif'))
	pmeMax <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/maxHabitable.tif'))
	pmeNone <- pmeMin
	pmeNone[] <- 1

	projection(pmeMin) <- projection(getCRS('nad83'))
	projection(pmeMean) <- projection(getCRS('nad83'))
	projection(pmeMax) <- projection(getCRS('nad83'))
	projection(pmeNone) <- projection(getCRS('nad83'))

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=TRUE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		divisionPoly <- out$divisionPoly
		rm(out); gc()

		# project division polygon
		divisionPoly <- sp::spTransform(divisionPoly, CRS(getCRS('nad83')))

		# by VALANCE
		for (unitValance in valances) {
		
			# by PME
			for (pmeVariant in pmes) {
			
				pmeNice <- pmeNiceName(pmeVariant)

				# get names of focal units to include/exclude
				units <- if (is.null(forceUnit)) {
					getUnits(scheme=scheme, incAll=(unitValance == 'including'))
				} else {
					forceUnit
				}
				
				say('UNITS: ', paste(units, collapse=' | '))

				# by UNIT
				for (thisUnit in units) {
			
					say('===============================================================', pre=2)
					say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', unitValance, ' | UNIT ', thisUnit, breaks=NULL)
					
					if (exists('bg')) rm(bg)
					
					# get this unit's polygon... defined as all polygons containing at least one presence assigned to this thisUnit
					thisUnitPres <- if (thisUnit=='all') {
						pres
					} else if (unitValance == 'including') {
						pres[which(pres[ , divisionFieldPres] == thisUnit), ]
					} else if (unitValance == 'excluding') {
						pres[which(pres[ , divisionFieldPres] != thisUnit), ]
					}
					
					thisUnitPresSp <- SpatialPoints(cbind(thisUnitPres$longWgs84, thisUnitPres$latWgs84), CRS(getCRS('nad83')))
				
					# which unit(s) has these points?
					polyContainsUnitPoints <- over(divisionPoly, thisUnitPresSp, returnList=TRUE)
					polyContainsUnitPoints <- unlist(lapply(polyContainsUnitPoints, length)) > 0
				
					thisUnitPoly <- divisionPoly[polyContainsUnitPoints, ]
					thisUnitPoly <- gUnaryUnion(thisUnitPoly)

					dirCreate(workDir, 'Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice,  ' PME 00 Sites')
		
					png(paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 00 Sites/!BG Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME ILLUSTRATION for Year ', years, '.png'), width=1200, height=1200, res=450)

						# create base mask from which to create KDE (but NOT from which to draw background sites)
						prismMaskThisUnit <- crop(prismMask, thisUnitPoly)
				
						pme <- get(pmeVariant)
						pme <- crop(pme, thisUnitPoly)
						
						thisUnitRast <- rasterize(thisUnitPoly, pme)
						
						# clip to PME-adjusted available habitat
						availHabitat <- thisUnitRast * pme * 0 + 1
						availHabitat <- crop(availHabitat, prismMaskThisUnit)
						availHabitatEa <- projectRaster(availHabitat, crs=getCRS('climateNA'))
						availHabitatEa <- availHabitatEa * 0 + 1

						# create 2D cell array for binning presences
						yCells <- seq(raster::extent(availHabitatEa)@ymin, raster::extent(availHabitatEa)@ymax, length.out=round(nrow(availHabitatEa) / resMult))
						xCells <- seq(raster::extent(availHabitatEa)@xmin, raster::extent(availHabitatEa)@xmax, length.out=round((extent(availHabitatEa)@xmax - extent(availHabitatEa)@xmin) / (abs(yCells[1] - yCells[2]))))

						grid <- SpatialPixels(
							SpatialPoints(
								coords=cbind(rep(xCells, each=length(yCells)), rep(yCells, length(xCells))),
								proj4string=getCRS('climateNA', TRUE)
							)
						)						

						# calculate weights (number of background sites) to assign to each presence
						nBg <- raster::extract(prismMask, cbind(thisUnitPres$longWgs84, thisUnitPres$latWgs84))
						nBgNotNA <- which(!is.na(nBg))
						nBgIsNa <- which(is.na(nBg))
						
						# subtract or add additional background sites in proportion to number already assigned so total = desired number
						if (sum(nBg, na.rm=TRUE) < totalBg) { # add

							i <- sample(nBgNotNA, totalBg - sum(nBg, na.rm=TRUE), replace=TRUE, prob=nBg[nBgNotNA])
							for (j in i) nBg[j] <- nBg[j] + 1
							
						} else if (sum(nBg, na.rm=TRUE) > totalBg) { # subtract

							i <- sample(nBgNotNA, sum(nBg, na.rm=TRUE) - totalBg, prob=nBg[nBgNotNA])
							nBg[i] <- nBg[i] - 1
							
						}
						
						# for each year train KDE and draw background sites in proportion to density of presence sites in that year
						for (countYear in seq_along(years)) {
						
							# if any records this year and any existing records need matching background sites
							if (years[countYear] %in% thisUnitPres$obsYear & sum(nBg[thisUnitPres$obsYear==years[countYear]], na.rm=TRUE) > 0) {

								say(years[countYear], post=0)
							
								# get presences from this year
								thisPresIndex <- which(thisUnitPres$obsYear==years[countYear]) # index of this year's presences

								thisPres <- thisUnitPres[thisPresIndex, ]
								thisPres <- SpatialPointsDataFrame(coords=cbind(thisPres$longWgs84, thisPres$latWgs84), data=thisPres, proj4string=CRS(getCRS('nad83')))
								thisPresEa <- SpatialPoints(coords=cbind(thisPres$longWgs84, thisPres$latWgs84), proj4string=CRS(getCRS('nad83')))
								thisPresEa <- spTransform(thisPresEa, CRSobj=getCRS('climateNA', TRUE))
								
								sumKde <- 0 # dummy value for sum of KDE raster cells
								
								# while the KDE has no cells with density > 0 from which to pick (0 densities occur if presences are outside PME raster area)
								while (sumKde == 0) {
								
									# if too few presences (<5) to train KDE, then artifically augment with same presences and add slight scatter so there's enough to train KDE
									if (length(thisPresEa) < 5) {
										
										newPres <- as.data.frame(thisPresEa)

										while (length(thisPresEa) < 5) {
											
											scatteredPres <- newPres + cbind(rnorm(nrow(newPres), 0, 1.96 * 3000), rnorm(nrow(newPres), 0, 1.96 * 3000))
											thisPresEa <- rbind(thisPresEa, SpatialPoints(coords=scatteredPres, proj4string=CRS(projection(thisPresEa))))
											
										}
										
									}
										
									# Epanechnikov estimator for bandwidth
									h <- 0.5 * sum(apply(coordinates(thisPresEa), 2, sd, na.rm=T)) * length(thisPresEa)^(-1/6) * 1.77

									# train kernel density estimator
									kde <- kernelUDAdapt(
										xy=thisPresEa,
										h=h,
										kern='epa',
										grid=grid,
										extent=1,
										epsilon=20
									)

									kde <- raster(kde)
									
									kde <- projectRaster(kde, availHabitat)
									kde <- kde * availHabitat
									kde <- projectRaster(kde, prismMaskThisUnit)
									kde <- kde * prismMaskThisUnit
									names(kde) <- paste0('kde', years[countYear])

									sumKde <- cellStats(kde, 'sum')

									# if KDE has all zero densities (ie, if no presences fall within its mask), add random jitter to presences and try again... presumably eventually enough will fall in the mask to generate a non-zero KDE
									if (sumKde == 0) {
									
										thisPresEa <- coordinates(thisPresEa) + cbind(rnorm(length(thisPresEa), 0, 1.96 * 3000), rnorm(length(thisPresEa), 0, 1.96 * 3000))
										thisPresEa <- SpatialPoints(coords=thisPresEa, proj4string=getCRS('climateNA', TRUE))
									
									}
									
								} # while KDE has all 0 densities
								
								kde <- setMinMax(kde)
								kde <- kde + minValue(kde)
								kde <- stretch(kde, 0, maxValue(kde))
								
								thisBg <- as.data.frame(sampleRast(kde, max(1, sum(nBg[thisPresIndex])), replace=TRUE, prob=TRUE))
								
								names(thisBg) <- c('longWgs84', 'latWgs84')
								
								# add year/month/doy of matching presence for each BG site... if number of presences = number of background sites, then each presence is matched ONCE by a background site or if >, then AT LEAST ONCE by a background site and possibly more
								obsYear <- rep(years[countYear], nrow(thisBg))
								matchingPres <- if (nrow(thisPres) > nrow(thisBg)) {
									sample(1:nrow(thisPres), nrow(thisBg), replace=FALSE)
								} else if (nrow(thisPres) == nrow(thisBg)) {
									1:nrow(thisPres)
								} else if (nrow(thisPres) < nrow(thisBg)) {
									c(1:nrow(thisPres), sample(1:nrow(thisPres), nrow(thisBg) - nrow(thisPres), replace=TRUE))
								}
									
								obsMonth <- thisPres$obsMonth[matchingPres]
								obsDayOfYear <- thisPres$obsDayOfYear[matchingPres]
								num <- as.character(thisPres$num[matchingPres])
								
								thisBg <- cbind(thisBg, data.frame(matchingPresNum=num, obsYear=obsYear, obsMonth=obsMonth, obsDayOfYear=obsDayOfYear))
								
								bg <- if (exists('bg', inherits=FALSE)) {
									rbind(bg, thisBg)
								} else {
									thisBg
								}
							
								# plot this year's BG
								main <- paste0('Spatiotemporally Weighted Background for ', years[countYear], '\n', nrow(thisPres), ' Presence(s) | ', nrow(thisBg), '  Background Site(s)')
								
								par(mar=c(0, 0, 0, 0) + 0.1, fg='white', col.main='black', cex=0.4)
								plot(kde, legend=FALSE)
								plot(thisUnitPoly, add=TRUE, border=alpha('blue', 0.5))
								points(thisBg$longWgs84, thisBg$latWgs84, pch=16, cex=0.5, col=alpha('red', 0.1), xpd=NA)
								points(thisPres, pch=1, col=alpha('black', 0.5), xpd=NA)
								legend('bottomright', legend=c('presence', 'background'), pch=c(1, 16), pt.cex=c(1, 0.5), col=c('black', 'red'), box.col='black', text.col='black', xpd=NA)
								title(main=main, sub=base::date(), outer=TRUE, line=-3, xpd=NA)
							
								writeRaster(kde, paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 00 Sites/!BG_Sites_00_Set_', prefix(set, 2), '_', schemeNice, '_-_', toupper(unitValance), '_', toupper(thisUnit), '_-_', pmeNice, '_PME_KDE_for_Year ', years), format='GTiff', overwrite=TRUE)
							
							} # if this year had any records and if existing records need matching background sites

						} # next year
						
					dev.off()
					
				} # next unit
				
			} # next PME
			
		} # next valance
	
	} # next scheme
	
}

################################
### extract raw climate data ###
################################

if ('extract' %in% do) {

	sayHead('extract raw climate data')

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by VALANCE
		for (unitValance in valances) {
		
			# by PME
			for (pmeVariant in pmes) {
			
				pmeNice <- pmeNiceName(pmeVariant)

				# get names of focal units to include/exclude
				units <- if (is.null(forceUnit)) {
					getUnits(scheme=scheme, incAll=(unitValance == 'including'))
				} else {
					forceUnit
				}
				
				say('UNITS: ', paste(units, collapse=' | '))

				# by UNIT
				for (thisUnit in units) {
			
					say('===============================================================', pre=2)
					say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', unitValance, ' | UNIT ', thisUnit, breaks=NULL)

					if (extractOption == 'all') {
					
						inPathName <- paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 00 Sites/BG Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME.rds')
					
						vars <- c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax', 'dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0')
						annual <- c(NA, 	    NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     FALSE,      TRUE,           TRUE)
						monthly <- c(NA, 		NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     TRUE,       TRUE,           TRUE)
						daily <- c(NA, 		NA,     	TRUE,  TRUE,   TRUE,   FALSE,   FALSE,     FALSE,    TRUE,      FALSE,          TRUE)
					
					} else if (extractOption == 'onlyprism') {
					
						inPathName <- paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 00 Sites/BG Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME.rds')

						vars <- c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax', 'dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0')
						annual <- c(NA, 	    NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     FALSE,      FALSE,           FALSE)
						monthly <- c(NA, 		NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     FALSE,      FALSE,           FALSE)
						daily <- c(NA, 		NA,     	TRUE,  TRUE,   TRUE,   FALSE,   FALSE,     FALSE,    FALSE,      FALSE,           FALSE)

					} else if (extractOption == 'nonprism') {
					
						inPathName <- paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 01 Raw Climate/BG Sites 01 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME - BASIC PRISM.rds')
						
						vars <- c('dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0')
						annual <- c(FALSE,      TRUE,           TRUE)
						monthly <- c(TRUE,       TRUE,           TRUE)
						daily <- c(TRUE,      FALSE,          TRUE)
						
					}
					
					bg <- readRDS(inPathName)
						
					outPathName <- paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 01 Raw Climate/BG Sites 01 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME - Raw Climate Data.rds')
					
					bg <- extractBasedOnDate(
						x=bg,
						obsYearField='obsYear',
						obsMonthField='obsMonth',
						obsDayOfYearField='obsDayOfYear',
						window=10,
						climYears=1981:2015,
						truncateAnnualWindow=TRUE,

						vars=vars,
						annual=annual,
						monthly=monthly,
						daily=daily,

						# savePartial=sub(outPathName, pattern='.rds', replacement=''),
						savePartial=NULL,
						censureDates=TRUE,
						drive='F:/',
						verbose=1
					)
						
					## save
					say('Saving background sites...', post=1)

					saveRDS(bg, outPathName)
				
					rm(bg); gc()
					
				} # next PME variant
				
			} # next division thisUnit
			
		} # next thisUnit valance
		
	} # next division

}

##########################################################################################
### calculate derived climate variables for SPATIOTEMPORALLY-WEIGHTED background sites ###
##########################################################################################

if ('derived' %in% do) {

	sayHead('calculate derived climate variables for SPATIOTEMPORALLY-WEIGHTED background sites')

	pres <- getPres()

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=FALSE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		rm(out); gc()

		# by VALANCE
		for (unitValance in valances) {
		
			# get names of focal units to include/exclude
			units <- if (is.null(forceUnit)) {
				getUnits(scheme=scheme, incAll=(unitValance == 'including'))
			} else {
				forceUnit
			}
			
			say('UNITS: ', paste(units, collapse=' | '))

			# by UNIT
			for (thisUnit in units) {
			
				# by PME
				for (pmeVariant in pmes) {
				
					pmeNice <- pmeNiceName(pmeVariant)

					say('===============================================================', pre=1)
					say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', unitValance, ' | UNIT ', thisUnit)
					
					dirCreate(workDir, 'Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice,  ' PME 02 Derived Climate')

					inPathName <- paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 01 Raw Climate/BG Sites 01 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME - Raw Climate Data.rds')
						
					bg <- readRDS(inPathName)
					
					rawCol <- which(grepl(names(bg), pattern='pptAnnual_yearMinus09')):ncol(bg)
						
					bg <- calcDerivedVars(x=bg, window=10, rawCol=rawCol, verbose=TRUE)
						
					## save
					say('Saving background sites...', post=2)

					outPathName <- paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 02 Derived Climate/BG Sites 02 Set ', prefix(set, 2), ' ', schemeNice, ' - ', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME - Derived Variables.rds')
						
					saveRDS(bg, outPathName)
				
					rm(bg); gc()
					
				} # next PME variant
				
			} # next division thisUnit
			
		} # next thisUnit valance
		
	} # next division
	
}

# say('########################################################################################')
# say('### draw RANDOM background sites and from across western US extract raw climate data ###')
# say('#########################################################################################')

# # date for extracting climate data
# obsYear <- 2015
# obsMonth <- 10
# obsDayOfMonth <- 7

# # buffer around range map to demarcate western US
# buffer <- 800

# # starting number of BG sites... rows with derived variables that are NA will be removed
# nStart <- 11000

# say('User-specified date: ', obsYear, '-', obsMonth, '-', obsDayOfMonth)
# say('User-specified buffer around IUCN range map to define "western US": ', buffer, ' km')

# #### crop DEM to area with buffer around IUCN range map 

	# dem <- raster('F:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')

	# rangeMap <- readOGR(paste0(workDir, '/Extents_Masks_Maps/Range Maps'), 'iucnMammals_version4_ochotonaPrinceps')
	# rangeMap <- sp::spTransform(rangeMap, getCRS('climateNA', TRUE))
	# rangeBuff <- gBuffer(rangeMap, width=buffer * 1000)
	# rangeBuff <- sp::spTransform(rangeBuff, CRS(projection(dem)))
	# dem <- crop(dem, rangeBuff)
	# mask <- rasterize(rangeBuff, dem)
	# mask <- crop(mask, dem)

# ### get points and extract raw climate data

	# bg <- as.data.frame(randomPoints(mask, 2 * nStart))
	# names(bg) <- c('longWgs84', 'latWgs84')
	# inDem <- raster::extract(dem, bg)
	# if (any(is.na(inDem))) bg <- bg[!is.na(inDem), ]
	# bg <- bg[1:nStart, ]

	# dirCreate(workDir, 'Background Sites/Random - Western USA')

	# say('Saving background sites...', post=1)
	# outPathName <- paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 00 Set ', prefix(set, 2), ' Selected from IUCN Range Map + ', buffer, '-km Buffer.rds')
	# saveRDS(bg, outPathName)
	
	# bg$obsYear <- obsYear
	# bg$obsMonth <- obsMonth
	# bg$obsDayOfMonth <- obsDayOfMonth
	# bg$obsDayOfYear <- doy(2015, 10, 7)

	# bg <- extractBasedOnDate(
		# x=bg,
		# obsYearField='obsYear',
		# obsMonthField='obsMonth',
		# obsDayOfYearField='obsDayOfYear',
		# window=10,
		# climYears=1981:2015,
		# truncateAnnualWindow=TRUE,

		# vars=c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax', 'dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0'),
		# annual=c(NA, 	    NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     FALSE,      TRUE,           TRUE),
		# monthly=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     TRUE,       TRUE,           TRUE),
		# daily=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   FALSE,   FALSE,     FALSE,    TRUE,      FALSE,          TRUE),

		# # vars=c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax'),
		# # annual=c(NA, 	    NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE),
		# # monthly=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE),
		# # daily=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   FALSE,   FALSE,     FALSE),

		# # vars=c('dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0'),
		# # annual=c(FALSE,      TRUE,           TRUE),
		# # monthly=c(TRUE,       TRUE,           TRUE),
		# # daily=c(TRUE,      FALSE,          TRUE),

		# savePartial=NULL,
		# censureDates=TRUE,
		# drive='F:/',
		# verbose=1
	# )
	
	# ## save
	# say('Saving background sites...', post=1)
	# outPathName <- paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 01 Set ', prefix(set, 2), ' Selected from IUCN Range Map + ', buffer, '-km Buffer - ', nStart, ' Sites - Raw Climate Data for ', obsYear, '-', prefix(obsMonth, 2), '-', prefix(obsDayOfMonth, 2), '.rds')
	# saveRDS(bg, outPathName)

	# say('Calculate derived climate variables...')
	# bg <- calcDerivedVars(x=bg, window=10, rawCol=(which(names(bg) %in% 'triGmted2010_m') + 1):ncol(bg), verbose=TRUE)
	
	# outPathName <- paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 02 Set ', prefix(set, 2), ' Selected from IUCN Range Map + ', buffer, '-km Buffer - ', nStart , ' Sites - Derived Variables for ', obsYear, '-', prefix(obsMonth, 2), '-', prefix(obsDayOfMonth, 2), '.rds')
	# saveRDS(bg, outPathName)

	# # remove rows with NAs
	# bg$snowPackStDev[is.na(bg$snowPackStDev)] <- 0
	# bg <- bg[-which(is.na(rowSums(bg[ , predictorsToUse]))), ]
	# bg <- bg[1:10000, ]
	
	# outPathName <- paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 03 Set ', prefix(set, 2), ' Selected from IUCN Range Map + ', buffer, '-km Buffer - 10000 Sites - Derived Variables for ', obsYear, '-', prefix(obsMonth, 2), '-', prefix(obsDayOfMonth, 2), ' - 10000 sites with no NAs.rds')
	# saveRDS(bg, outPathName)

	# ## calculate PCA
	# pca <- princomp(bg[ , predictorsToUse], cor=TRUE)
	# pcNames <- paste0('PC', prefix(seq_along(predictorsToUse), 2))
	# saveRDS(pca, paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set ', prefix(set, 2), ' Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for ', obsYear, '-', prefix(obsMonth, 2), '-', prefix(obsDayOfMonth, 2), '.rds'))
	
# say('###########################################################################################################################')
# say('### draw REPRESENTATIVE sites from each division unit plus random background for all units and extract raw climate data ###')
# say('###########################################################################################################################')

# say('I need to characterize the differences between division units apart from the presences they possess. Thus, I will train ENMs on each unit (as if it were a species) using 1) a set of background sites drawn from each unit in a spatially random manner ("representative sites") contrasted with 2) a set of background sites drawn from across all occupied units also drawn randomly.')
# say('There is a choice to be made between using an equal number of represenative sites per unit (e.g., 1000) versus using a number equal to the number of presences in each unit. However, I am using a weighted performance metric when calculating ENM skill for the presence-ENMS which is intended to account for differences in prevalance of presences in each unit. Hence, I should select representative sites in number equal to the area of the unit relative to the area of all units.')

# ## BAUHAUS
# # 1 select two sets of sites, both from all units with presences
# # 2 choose one set and assign each site to a unit... these are the "representative" sites
# # 3 extract

# # presences
# pres <- getPres
# # PRISM raster for masking (DEM is larger than climate extent)
# presSp <- SpatialPoints(cbind(pres$longWgs84, pres$latWgs84), CRS(getCRS('nad83')))
# prismDem <- raster('F:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
# prismMask <- prismDem * 0 + 1

# # remove presence sites outside PRISM DEM domain
# inPrismMask <- raster::extract(prismMask, cbind(pres$longWgs84, pres$latWgs84))
# if (sum(inPrismMask, na.rm=TRUE) < nrow(pres)) pres <- pres[-which(is.na(inPrismMask)), ]

# presSp <- SpatialPointsDataFrame(cbind(pres$longWgs84, pres$latWgs84), data=pres, proj4string=CRS(getCRS('nad83')))

# totalBg <- 10000 # number of background sites to sample

# # PME rasters
# pmeMin <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/minHabitable.tif'))
# pmeMean <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/meanHabitable.tif'))
# pmeMax <- raster(paste0(workDir, '/Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/maxHabitable.tif'))
# pmeNone <- pmeMin
# pmeNone[] <- 1

# projection(pmeMin) <- projection(getCRS('nad83'))
# projection(pmeMean) <- projection(getCRS('nad83'))
# projection(pmeMax) <- projection(getCRS('nad83'))
# projection(pmeNone) <- projection(getCRS('nad83'))

# # for (scheme in c('cladeNonOverlap', 'physioFennemanL2', 'physioHammond', 'ecoregionEpa3Modified', 'gens')) {
# # for (scheme in c('cladeNonOverlap')) {
# # for (scheme in c('physioFennemanL2')) {
# # for (scheme in c('physioHammond')) {
# for (scheme in c('ecoregionEpa3Modified')) {
# # for (scheme in c('gens')) {

	# out <- schemeInfo(scheme, poly=TRUE)
	# schemeNice <- out$schemeNice
	# divisionFieldPres <- out$divisionFieldPres
	# divisionFieldPoly <- out$divisionFieldPoly
	# divisionPoly <- out$divisionPoly
	# rm(out); gc()

	# dirCreate(workDir, 'Representative Unit Sites/', schemeNice)
	
	# # project division polygon
	# divisionPoly <- sp::spTransform(divisionPoly, CRS(getCRS('nad83')))

	# # get names of focal units to include/exclude
	# units <- getUnits(scheme=scheme, pres=pres, incAll=FALSE)

	# say('UNITS: ', paste(units, collapse=' | '))

	# divisionPolyDf <- as.data.frame(divisionPoly)
	# thisUnitPoly <- divisionPoly[divisionPolyDf[ , divisionFieldPoly] %in% units, ]
	# thisUnitPoly <- gUnaryUnion(thisUnitPoly)

	# # for (pmeVariant in c('pmeMin', 'pmeNone')) {
	# # for (pmeVariant in c('pmeMin')) {
	# for (pmeVariant in c('pmeNone')) {
	
		# pmeNice <- pmeNiceName(pmeVariant)

		# say('===============================================================', pre=2)
		# say('SCHEME ', schemeNice, ' | PME ', pmeNice, breaks=NULL)
		# say(base::date(), ' :: Generating background sites')
		
		# if (exists('bg')) rm(bg)
		
		# ### create base mask from which to drawn sites
		# prismMaskThisUnit <- crop(prismMask, thisUnitPoly)

		# pme <- get(pmeVariant)
		# pme <- crop(pme, thisUnitPoly)
		
		# thisUnitRast <- rasterize(thisUnitPoly, pme)
		
		# # clip to PME-adjusted available habitat
		# availHabitat <- thisUnitRast * pme * 0 + 1
		# availHabitat <- crop(availHabitat, prismMaskThisUnit)

		# ### draw representative sites
		# sites <- randomPoints(availHabitat, 30000, tryf=5)
		# inDem <- raster::extract(prismMask, sites)
		
		# sites <- sites[which(!is.na(inDem)), ]
		# sites <- sites[1:20000, ]
		# sitesSp <- SpatialPoints(sites, CRS(getCRS('nad83')))
		# sites <- as.data.frame(sites)
		# names(sites) <- c('longWgs84', 'latWgs84')
		
		# ### assign each site to a date based on dates of presences in unit
		# unitsSitesIn <- over(divisionPoly, sitesSp, returnList=TRUE)
		# sites$matchingPresNum <- obsDayOfYear <- sites$obsDayOfYear <- sites$obsDayOfMonth <- sites$obsMonth <- sites$obsYear <- sites$unit <- NA
		
		# for (i in seq_along(unitsSitesIn)) {
		
			# # assign each site to a unit
			# thisUnit <- divisionPolyDf[i, divisionFieldPoly]
			# theseUnitsInSites <- unitsSitesIn[[i]]
			# sites$unit[theseUnitsInSites] <- thisUnit
			
			# # assign each site a date by sampling from presences in this unit
			# presInUnit <- which(pres[ , divisionFieldPres] %in% thisUnit)
			
			# samples <- sample(presInUnit, length(theseUnitsInSites), replace=TRUE)
			# sites$matchingPresNum[theseUnitsInSites] <- as.character(pres$num)[samples]
			# sites$obsDayOfYear[theseUnitsInSites] <- pres$obsDayOfYear[samples]
			# sites$obsDayOfMonth[theseUnitsInSites] <- pres$obsDayOfMonth[samples]
			# sites$obsMonth[theseUnitsInSites] <- pres$obsMonth[samples]
			# sites$obsYear[theseUnitsInSites] <-pres$obsYear[samples]
		
		# } # next unit
		
		# ###############
		# ### extract ###
		# ###############
		
		# say(base::date(), ' :: Extracting raw climate data: ', pre=1, post=0)
		
		# sites <- extractBasedOnDate(
			# x=sites,
			# obsYearField='obsYear',
			# obsMonthField='obsMonth',
			# obsDayOfYearField='obsDayOfYear',
			# window=10,
			# climYears=1981:2015,
			# truncateAnnualWindow=TRUE,

			# # vars=c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax', 'dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0'),
			# # annual=c(NA, 	    NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     FALSE,      TRUE,           TRUE),
			# # monthly=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE,     TRUE,       TRUE,           TRUE),
			# # daily=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   FALSE,   FALSE,     FALSE,    TRUE,      FALSE,          TRUE),

			# vars=c('cellArea', 'topo', 'ppt', 'tmin', 'tmax', 'tdmean', 'vpdmin', 'vpdmax'),
			# annual=c(NA, 	    NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE),
			# monthly=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   TRUE,    TRUE,      TRUE),
			# daily=c(NA, 		NA,     TRUE,  TRUE,   TRUE,   FALSE,   FALSE,     FALSE),

			# # vars=c('dayMetSwe', 'dobrowskiSwe', 'dobrowskiET0'),
			# # annual=c(FALSE,      TRUE,           TRUE),
			# # monthly=c(TRUE,       TRUE,           TRUE),
			# # daily=c(TRUE,      FALSE,          TRUE),

			# savePartial=NULL,
			# censureDates=TRUE,
			# drive='F:/',
			# verbose=1
		# )
			
		# ## save
		# say('Saving representative sites...', post=1)

		# ### divide sites into "background" and "representative"
		# bgSites <- sites[1:10000, ]
		# repSites <- sites[10001:20000, ]
		
		# pdf(paste0(workDir, '/Representative Unit Sites/', schemeNice, '/!Representative Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', pmeNice, ' PME.pdf'), paper='letter', width=6.5, height=7.5)

			# # plot
			# main <- paste0('Representative & Background Sites\nUsing ', schemeNice, ' Division Scheme\nusing ', pmeNice, ' PME\n', nrow(repSites), ' Representative Sites | ', nrow(bg), ' Background Sites')
			
			# par(mar=c(0, 0, 0, 0) + 0.1, fg='white', col.main='black')
			# plot(thisUnitPoly, col='white', legend=FALSE)
			# plot(divisionPoly, add=TRUE, border=alpha('blue', 0.5))
			# points(bgSites$longWgs84, bgSites$latWgs84, pch='.', col=alpha('black', 0.5))
			# points(repSites$longWgs84, repSites$latWgs84, pch=1, cex=0.3, col=alpha('red', 0.6))
			# legend('bottomright', legend=c('representative', 'background'), pch=c(1, 16), pt.cex=c(1, 0.5), col=c('red', 'gray'), box.col='black', text.col='black')
			# title(main=main, sub=base::date(), outer=TRUE, line=-3, xpd=NA)

		# dev.off()

		# saveRDS(bgSites, paste0(workDir, '/Representative Unit Sites/', schemeNice, '/Representative Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', pmeNice, ' PME - BACKGROUND.rds'))
		# saveRDS(repSites, paste0(workDir, '/Representative Unit Sites/', schemeNice, '/Representative Sites 00 Set ', prefix(set, 2), ' ', schemeNice, ' - ', pmeNice, ' PME - UNITS.rds'))
		
		# # calculate derived climate variables
		# bgSites <- calcDerivedVars(x=bgSites, window=10, rawCol=(which(names(bgSites) %in% 'triGmted2010_m') + 1):ncol(bgSites), verbose=TRUE)
		# repSites <- calcDerivedVars(x=repSites, window=10, rawCol=(which(names(repSites) %in% 'triGmted2010_m') + 1):ncol(repSites), verbose=TRUE)

		# # set snow pack SD values equal to NA to 0
		# bgSites$snowPackStDev[is.na(bgSites$snowPackStDev)] <- 0
		# repSites$snowPackStDev[is.na(repSites$snowPackStDev)] <- 0
		
		# saveRDS(bgSites, paste0(workDir, '/Representative Unit Sites/', schemeNice, '/Representative Sites 02 Set ', prefix(set, 2), ' ', schemeNice, ' - ', pmeNice, ' PME - BACKGROUND - Derived Variables.rds'))
		# saveRDS(repSites, paste0(workDir, '/Representative Unit Sites/', schemeNice, '/Representative Sites 02 Set ', prefix(set, 2), ' ', schemeNice, ' - ', pmeNice, ' PME - UNITS - Derived Variables.rds'))

	# } # next PME variant
	
# } # next division



say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=2)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
