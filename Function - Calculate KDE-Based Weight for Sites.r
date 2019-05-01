kdeWeighting <- function(scheme, valance, unit, sites, mask, verbose=FALSE) {
# kdeWeighting  Caculates the KDE-based weight for a site based on density of presences in the unit

# scheme		Name of scheme
# unit			Name of focal unit
# sites			Data frame of sites at which to calculate weights. Must have thee fields: longWgs84, latWgs84, obsYear
# mask			Raster to use as template for KDE
# verbose		TRUE ==> show progress

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/Function - Calculate KDE-Based Weight for Sites.r')

	weighting <- rep(NA, nrow(sites))

	# presences
	pres <- getPres()

	# mask
	mask <- 0 * mask + 1

	# get scheme/poly info
	out <- schemeInfo(scheme, poly=TRUE)
	divisionFieldPres <- out$divisionFieldPres
	divisionFieldPoly <- out$divisionFieldPoly
	divisionPoly <- out$divisionPoly
	rm(out); gc()
	divisionPolyDf <- as.data.frame(divisionPoly)

	mask <- crop(mask, divisionPoly)
	
	### train KDE for each year with presences in this unit, extract KDE prediction to use as weight in evaluation

	# get this "to" unit's unit polygon and all of its presences
	if (unit == 'all') {
		unitPres <- pres
		unitPoly <- divisionPoly[which(divisionPolyDf[ , divisionFieldPoly] %in% sort(unique(pres[ , divisionFieldPres]))), ]
	} else if (valance == 'including') {
		unitPres <- pres[which(pres[ , divisionFieldPres] %in% unit), ]
		unitPoly <- divisionPoly[which(divisionPolyDf[ , divisionFieldPoly] %in% unit), ]
	} else if (valance == 'excluding') {
		unitPres <- pres[-which(pres[ , divisionFieldPres] %in% unit), ]
		unitPoly <- divisionPoly[which(!(divisionPolyDf[ , divisionFieldPoly] %in% unit)), ]
	}

	unitPolyEa <- sp::spTransform(unitPoly, CRS(crsClimateNA))

	# create 2D cell array for binning presences
	yCells <- seq(raster::extent(unitPolyEa)@ymin, raster::extent(unitPolyEa)@ymax, length.out=round(nrow(mask) / resMult))
	xCells <- seq(raster::extent(unitPolyEa)@xmin, raster::extent(unitPolyEa)@xmax, length.out=round((extent(unitPolyEa)@xmax - extent(unitPolyEa)@xmin) / (abs(yCells[1] - yCells[2]))))

	grid <- SpatialPixels(
		SpatialPoints(
			coords=cbind(rep(xCells, each=length(yCells)), rep(yCells, length(xCells))),
			proj4string=CRS(crsClimateNA)
		)
	)						

	years <- sort(unique(unitPres$obsYear))

	# calculate KDE for each year, extract prdiction (to use as weights in evaluation)
	for (thisYear in years) {
	
		if (verbose) say('Calculating weight for ', thisYear)
		
		unitPresThisYear <- unitPres[which(unitPres$obsYear == thisYear), ]
		unitPresThisYearSp <- SpatialPoints(cbind(unitPresThisYear$longWgs84, unitPresThisYear$latWgs84), getCRS('crsNad83', TRUE))
		unitPresThisYearEa <- sp::spTransform(unitPresThisYearSp, CRS(crsClimateNA))
		
		## calculate test site weights using value from KDE trained on each year's presences in the test unit
		sumKde <- 0 # dummy value for sum of KDE raster cells
		
		# while the KDE has no cells with density > 0 from which to pick (0 densities occur if presences are outside PME raster area)
		while (sumKde == 0) {
		
			# if too few presences (<5) to train KDE, then artificially augment with same presences and add slight scatter so there's enough to train KDE
			if (length(unitPresThisYearEa) < 5) {
				
				newPres <- as.data.frame(unitPresThisYearEa)

				while (length(unitPresThisYearEa) < 5) {
					
					scatteredPres <- newPres + cbind(rnorm(nrow(newPres), 0, 1.96 * 3000), rnorm(nrow(newPres), 0, 1.96 * 3000))
					unitPresThisYearEa <- rbind(unitPresThisYearEa, SpatialPoints(coords=scatteredPres, proj4string=CRS(projection(unitPresThisYearEa))))
					
				}
				
			}
				
			# Epanechnikov estimator for bandwidth
			h <- 0.5 * sum(apply(coordinates(unitPresThisYearEa), 2, sd, na.rm=T)) * length(unitPresThisYearEa)^(-1/6) * 1.77

			# train kernel density estimator
			kde <- kernelUDAdapt(
				xy=unitPresThisYearEa,
				h=h,
				kern='epa',
				grid=grid,
				extent=1,
				epsilon=20
			)
			
			kde <- raster(kde)
			sumKde <- cellStats(kde, 'sum')

			# if KDE has all zero densities (ie, if no presences fall within its mask), add random jitter to presences and try again... presumably eventually enough will fall in the mask to generate a non-zero KDE
			if (sumKde == 0) {
			
				unitPresThisYearEa <- coordinates(unitPresThisYearEa) + cbind(rnorm(length(unitPresThisYearEa), 0, 1.96 * 3000), rnorm(length(unitPresThisYearEa), 0, 1.96 * 3000))
				unitPresThisYearEa <- SpatialPoints(coords=unitPresThisYearEa, proj4string=CRS(crsClimateNA))
			
			}
		
		} # while KDE has 0 densities
			
		if (minValue(kde) < 0) kde <- kde + minValue(kde)

		### extract KDE values as weights
		if (thisYear %in% sites$obsYear) {

			inThisYear <- which(sites$obsYear == thisYear)
			theseWeights <- raster::extract(kde, sp::spTransform(SpatialPoints(sites[inThisYear, c('longWgs84', 'latWgs84')], CRS(crsWgs84)), CRS(crsClimateNA)))
			
			# if (stand) theseWeights <- theseWeights / maxValue(kde)
			# if (subtract) theseWeights <- maxValue(kde) - theseWeights
		
			weighting[inThisYear] <- theseWeights
		
		}
		
	} # next "to" year

	weighting

}
