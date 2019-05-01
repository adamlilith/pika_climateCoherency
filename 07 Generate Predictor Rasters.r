### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2017-04

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/07 Generate Predictor Rasters.r')

rm(list=ls())

### CONTENTS ###
### libraries, variables, and functions ###
### generate stratified sample of sites ###
### calculate distance from coast raster ###
### calculate longitude and latitude rasters ###
###

###########################################
### libraries, variables, and functions ###
###########################################

source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')

dirCreate(workDir, 'Analysis - Non-stationarity/Predictor Rasters')

say('###########################################')
say('### generate stratified sample of sites ###')
say('###########################################')

say('Sampling by stratifying by elevation.')

# process DEM
dem <- raster('C:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')

rangeMap <- readOGR(paste0(workDir, '/Extents_Masks_Maps/Range Maps'), 'iucnMammals_version4_ochotonaPrinceps')
rangeMap <- sp::spTransform(rangeMap, CRS(crsClimateNA))
rangeMapBuff <- gBuffer(rangeMap, width=600000)
rangeMapBuff <- sp::spTransform(rangeMapBuff, CRS(projection(dem)))
dem <- crop(dem, rangeMapBuff)

sites <- data.frame(x=numeric(), y=numeric())

say('Sample from quantiles of elevation')
fromQuant <- c(0,     0.005, seq(0.01, 0.09, by=0.01), seq(0.10, 0.85, by=0.05), seq(0.90, 0.98, by=0.01), 0.99, 0.995)
toQuant <-   c(0.005, 0.01,  seq(0.02, 0.10, by=0.01), seq(0.15, 0.90, by=0.05), seq(0.91, 0.99, by=0.01), 0.995, 1)

for (i in seq_along(fromQuant)) {

	say(fromQuant[i], ' ~ ', toQuant[i])

	below <- quantile(dem, fromQuant[i])
	above <- quantile(dem, toQuant[i])
	
	masked <- dem >= below & dem <= above
	masked <- mask(masked, masked, maskvalue=0)

	samples <- randomPoints(masked, 1000)
	sites <- rbind(sites, samples)
	
}

sites <- sites[sample(1:nrow(sites), 20000), ]
sites <- as.data.frame(sites)
names(sites) <- c('longitude', 'latitude')
saveRDS(sites, paste0(workDir, 'Analysis - Non-stationarity/Predictor Rasters/00 Sites Stratified by Elevation.rds'))

plot(dem)
points(sites, col=alpha('blue', 0.1), pch=16)

say('Extract environmental data at points: long/lat')

	source('C:/ecology/Drive/r/Geography/Rasters - Calculate Latitude and Longitude Rasters.r')
	x <- longLatRasters(template=dem)
	x <- x * (dem * 0 + 1)
	names(x) <- c('longitude', 'latitude')
	env <- as.data.frame(extract(sites, x))

say('Extract distance to coast')

	# use long/lat rasters
	long <- as.matrix(subset(x, 'longitude'))
	lat <- as.matrix(subset(x, 'latitude'))

	### get coordinates of coastal cells (excluding islands)
	
	# get row/rol indices of coastal cells
	coastRow <- coastCol <- integer()

	for (row in 1:nrow(dem)) {
	
		notNa <- !is.na(long[row, ])
		
		# if any NA's in row
		if (sum(notNa) != 0) {
		
			# see if there's an island
			firstLandCol <- which.max(notNa)
			lastLandCol <- ncol(dem) - which.max(rev(notNa)) + 1
			
			allLandRow <- firstLandCol:lastLandCol
			
			# island exists (starting at first non-NA cell, there are NAs remaining the at least one column going east)
			if (length(allLandRow) > sum(notNa)) {
			
				# find continental coastal cell column
				justLandRemaining <- FALSE
				col <- firstLandCol
				while (!justLandRemaining) {
					if (sum(!notNa[(col + 1):lastLandCol]) > 0) {
						col <- col + 1
					} else {
						justLandRemaining <- TRUE
					}
				}
				
				col <- col + 1
				
			# no islands (starting from first non-NA cell, remaining cells in row groing east are also not NAs)
			} else {

				col <- firstLandCol
			
			}

		# if no NA's in row (all land)
		} else if (sum(notNa) > 0) {
			col <- 1
		}
		
		coastRow <- c(coastRow, row)
		coastCol <- c(coastCol, col)
		
	}

	coastLong <- long[coastRow, coastCol]
	coastLat <- lat[coastRow, coastCol]

	plot(dem)
	for (i in seq_along(coastRow)) {
		if (i == 1) {
			xy <- cbind(long[coastRow[i], coastCol[i]], lat[coastRow[i], coastCol[i]])
		} else {
			xy <- rbind(xy, cbind(long[coastRow[i], coastCol[i]], lat[coastRow[i], coastCol[i]]))
		}
	}
	points(xy, pch='.')
	



	### distance to coast
	
	# identify coastal cells
	
	mat <- as.matrix(dem)
	matNum <- matrix(1:ncell(dem), byrow=TRUE, nrow=nrow(dem))
	
	cellIsCoast <- integer()
	
	for (row in 1:nrow(dem)) {
	
		thisRow <- mat[row, ]
	
		# if any non-NA cells
		if (sum(is.na(thisRow)) < ncol(dem)) {
		
			land <- which(!is.na(thisRow))
			
			# for each land cell test if cells in Queen's neighborhood are NAs
			for (i in seq_along(land)) {
			
				coast <- FALSE
			
				# W
				if (land[i] > 1) if (is.na(mat[row, land[i] - 1])) coast <- TRUE
			
				# NW
				if (land[i] > 1 & row > 1) if (is.na(mat[row - 1, land[i] - 1])) coast <- TRUE
			
				# N
				if (row > 1) if (is.na(mat[row - 1, land])) coast <- TRUE
			
				# NE
				if (row > 1 & land[i] < ncol(dem)) if (is.na(mat[row - 1, land[i] + 1])) coast <- TRUE
			
				# E
				if (land[i] < ncol(dem)) if (is.na(mat[row, land[i] + 1])) coast <- TRUE
			
				# SE
				if (row < nrow(dem) & land[i] < ncol(dem)) if (is.na(mat[row + 1, land[i] + 1])) coast <- TRUE
			
				# S
				if (row < nrow(dem)) if (is.na(mat[row + 1, land[i]])) coast <- TRUE
			
				# SW
				if (row < nrow(dem) & land[i] > 1) if (is.na(mat[row + 1, land[i] - 1])) coast <- TRUE
			
				if (coast) cellIsCoast <- c(cellIsCoast, matNum[row, land[i])
			
			}
		
		}
	
	}
	
	dists <- long * NA
	for (row in 1:nrow(dem)) {
	
		notNaCol <- which(!is.na(long[row, ]))
		if (length(notNaCol) > 0) {
			
			# get cell column of coast... remove islands
			firstNotNaCol <- notNaCol[1]
			
			
			
			if (!is.na(firstNotNaCol)) {
				dists[row, notNaCol] <- distCosine(cbind(long[row, firstNotNaCol], lat[row, firstNotNaCol]), cbind(long[row, notNaCol], lat[row, notNaCol])) / 1000
			}
		}
	
	}
	
	distToCoast <- dem
	distToCoast[] <- dists
	rm(long, lat, dists); gc()
	
	thisEnv <- as.data.frame(extract(sites, distToCoast))
	env <- cbind(env, thisEnv)

say('Extract environmental data at points: topography')

	x <- stack(
		raster('C:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif'),
		raster('E:/ecology/Climate/PRISM/PRISM_us_dem_800m_aspect_eastness.tif'),
		raster('E:/ecology/Climate/PRISM/PRISM_us_dem_800m_aspect_northness.tif'),
		raster('E:/ecology/Climate/PRISM/PRISM_us_dem_800m_slope_degrees.tif'),
		raster('E:/ecology/Climate/PRISM/PRISM_us_dem_800m_topoPositionIndex_m.tif')
	)

	thisEnv <- as.data.frame(extract(sites, x))
	env <- cbind(env, thisEnv)

say('Extract environmental data at points: climate')

	x <- stack(listFiles('E:/ecology/Climate/PRISM/AN81m - Monthly Means', pattern='.tif'))
	thisEnv <- as.data.frame(extract(sites, x))
	env <- cbind(env, thisEnv)

