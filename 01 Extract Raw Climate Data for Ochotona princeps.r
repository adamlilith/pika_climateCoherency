### Ochotona princeps - Extract Raw Climate Data for Ochotona princeps
### Adam B. Smith | 2016-07

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/01 Extract Raw Climate Data for Ochotona princeps.r')

rm(list=ls())

### CONTENTS ###
### libraries, variables, and functions ###
### create study region masks and extents ###
### extract clade identity and elevation for each record ###
### extract climate data for pika sites ###
### calculate derived climate variables for presences ###
### assign scheme division names to presences: physiography (Fenneman provinces) ###
### assign scheme division names to presences: ecoregions (EPA Modified Ecoregions) ###
### assign scheme division names to presences: climate (GEnS) ###
### assign scheme division names to presences: climate (Koppen) ###
### assign scheme division names to presences: ecoregion (Bailey) ###
### assign scheme division names to presences: elevational bands above PME ###
### assign scheme division names to presences: physiography (Hammond) ###
### RE-assign scheme division names to presences: elevational bands above PME ###
### RE-assign scheme division names to presences: physiography (Fennemann modified provinces) ###
### calculate similarity between division schemes ###
### analyze sample intensity in time ###

###########################################
### libraries, variables, and functions ###
###########################################

# PRISM AN81d raster directory
prismAN81dDir <- 'F:/Climate/PRISM/AN81d 1981-2015/'

# PRISM AN81m raster directory
prismAN81mDir <- 'F:/Climate/PRISM/AN81m 1895-2015/'

source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')

# say('#############################################')
# say('### create study region masks and extents ###')
# say('#############################################')

	# say('Using PRISM raster as basemap. Study region is Octotonoa princeps IUCN range map plus 600-km buffer.')

	# dirCreate(workDir, 'Iconic Species/Extents_Masks_Maps/Masks & Extents/Extents')
	# dirCreate(workDir, 'Extents_Masks_Maps/Masks & Extents/Masks')
	
	# demPrism <- raster('E:/Climate/PRISM/PRISM_us_dem_800m.tif')
	# demGmted2010 <- raster('C:/ecology/Topography/GMTED2010/7pt5_arcsec/conus_20101117_gmted_mea075.tif')

	# ### calculate extent
	# ####################
	
	# iucn <- readOGR('C:/ecology/Drive/Research/Iconic Species/Extents_Masks_Maps/Range Maps', 'iucnMammals_version4_ochotonaPrinceps', verbose=FALSE)
	
	# iucn <- spTransform(iucn, climateNA)
	
	# studyRegionClimateNA <- gBuffer(iucn, width=600000)

	# studyRegionClimateNA <- as(studyRegionClimateNA, 'SpatialPolygonsDataFrame')
	
	# writeOGR(studyRegionClimateNA, paste0(workDir, '/Extents_Masks_Maps/Masks & Extents/Extents'), 'ochotonaPrinceps_iucnRangePlus600kmBuffer_union_presenceRecordsPlus600kmBuffer_climateNA', driver='ESRI Shapefile', verbose=FALSE, overwrite_layer=TRUE)
	
	# studyRegionWgs84 <- spTransform(studyRegionClimateNA, CRS(wgs84))
	# studyRegionNad83 <- spTransform(studyRegionClimateNA, CRS(nad83))
	
	# writeOGR(studyRegionWgs84, paste0(workDir, '/Extents_Masks_Maps/Masks & Extents/Extents'), 'ochotonaPrinceps_iucnRangePlus600kmBuffer_union_presenceRecordsPlus600kmBuffer_wgs84', driver='ESRI Shapefile', verbose=FALSE, overwrite_layer=TRUE)
	# writeOGR(studyRegionNad83, paste0(workDir, '/Extents_Masks_Maps/Masks & Extents/Extents'), 'ochotonaPrinceps_iucnRangePlus600kmBuffer_union_presenceRecordsPlus600kmBuffer_nad83', driver='ESRI Shapefile', verbose=FALSE, overwrite_layer=TRUE)
	
	# ### crop GMTED2010 DEM to study region
	# ######################################
	
	# demGmted2010 <- crop(demGmted2010, studyRegionWgs84)
	
	# writeRaster(demGmted2010, paste0(workDir, '/Environmental Data/Elevation - GMTED2010/conus_20101117_gmted_mea075_croppedByIucnRangePlus600km'), format='GTiff', overwrite=TRUE)
	
	# ### crop PRISM DEM to study region
	# ##################################
	
	# demPrism <- crop(demPrism, studyRegionNad83)
	# writeRaster(demPrism, paste0(workDir, '/Environmental Data/Elevation - GMTED2010/PRISM_us_dem_800m'), format='GTiff', overwrite=TRUE)
	
	# ### create mask raster
	# ######################
	
	# maskPrism <- demPrism * 0 + 1
	# names(maskPrism) <- 'mask'
	# writeRaster(maskPrism, paste0(workDir, '/Extents_Masks_Maps/Masks & Extents/Masks/ochotonaPrinceps_mask_prism_croppedByIucnRangePlus600km.tif'), format='GTiff', overwrite=TRUE)

# say('############################################################')
# say('### extract clade identity and elevation for each record ###')
# say('############################################################')	

	# pikaDataUsable <- readRDS(paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/00 Pika - Cleaned Using R - Usable Records for All Ochotona.rds'))
	# head(pikaDataUsable)
	
	# # from all pika data, select: **Ochotona princeps**, **present** or **absent** (not `NA`), **year >=1895** & **year <=2015**.

	# good <- which(pikaDataUsable$species=='Ochotona princeps' & !is.na(pikaDataUsable$origRecentPikaOrSignsObserved) & pikaDataUsable$obsYear >= 1895 & pikaDataUsable$obsYear <= 2015 & !is.na(pikaDataUsable$longWgs84) & !is.na(pikaDataUsable$latWgs84) & !is.na(pikaDataUsable$obsYear) & !is.na(pikaDataUsable$obsMonth) & !is.na(pikaDataUsable$obsDayOfMonth))

	# op <- pikaDataUsable[good, ]
	# nrow(pikaDataUsable)
	# nrow(op)

	# # extract clade
	
	# cladePolyNonoverlap <- readOGR(paste0(workDir, '/Extents_Masks_Maps/GeneticClades_GIS/Method2_IUCNBuffer/NonOverlapClade'), 'IUCN-47kmBuff_NonOverlap', verbose=FALSE)
	
	# opSpatial <- SpatialPoints(coords=cbind(op$longWgs84, op$latWgs84), proj4string=CRS(projection(cladePolyNonoverlap)))
	# cladeId <- over(opSpatial, cladePolyNonoverlap)
	
	# op$clade <- cladeId$subspecies
	
	# # a few points don't fall within clades but very nearly do... assign these to the closest clade
	# nonClade <- op[is.na(op$clade) & op$origRecentPikaOrSignsObserved, ]
	
	# if (nrow(nonClade) > 0) {
	
		# nonCladeSp <- SpatialPoints(coords=cbind(nonClade$longWgs84, nonClade$latWgs84), proj4string=CRS(crsWgs84))
		# nonCladeSp <- spTransform(nonCladeSp, CRS(crsClimateNA))
		
		# cladePolyNonoverlapEa <- spTransform(cladePolyNonoverlap, CRS(crsClimateNA))
		
		# for (i in 1:length(nonCladeSp)) {
		
			# dists <- gDistance(nonCladeSp[i, ], cladePolyNonoverlapEa, byid=TRUE)
			# op$clade[which(nonClade$num[i] == op$num)] <- cladePolyNonoverlapEa$subspecies[which.min(dists)]
			
		# }
		
	# }
	
	# # extract elevation
	# gmted2010 <- raster(paste0(workDir, '/Environmental Data/Elevation - GMTED2010/conus_20101117_gmted_mea075_croppedByIucnRangePlus600km.tif'))
	# prism <- raster(paste0(workDir, '/Environmental Data/PRISM/PRISM_us_dem_800m.tif'))
	
	# op$elev_gmted2010_m <- raster::extract(gmted2010, opSpatial)
	# op$elev_prism_m <- raster::extract(prism, opSpatial)

	# saveRDS(op, file=paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/01 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - Clade Identity and Elevation Extracted.rds'))
	
# say('###########################################')
# say('### extract climate data for pika sites ###')
# say('###########################################')

	# op <- readRDS(paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/01 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - Clade Identity and Elevation Extracted.rds'))
	
	# ## Climate data will be extracted for the date of observation plus up to 10 yr prior to the data of observation, the exact coverage depending on the temporal grain of the climate data.

	# op <- extractBasedOnDate(
		# x=op,
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
		# savePartial=paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/02 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - PRISM & DayMet Climate Data Extracted'),
		# censureDates=TRUE,
		# drive='F:/'
	# )

	# op$elev_prism_m <- NULL
	# op$elev_gmted2010_m <- NULL
	
	# saveRDS(op, paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/02 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - PRISM & DayMet Climate Data Extracted.rds'))

	# say('### cull to just presences between 1990 and 2015 ###')
	# op$origUsable <- as.logical(op$origUsable)
	# op <- op[op$obsYear >= 1990 & op$origRecentPikaOrSignsObserved & op$origUsable & op$coordsOk, ]

	# saveRDS(op, paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/03 Ochotona princeps - Usable - Presences 1990-2015 - PRISM & DayMet Climate Data Extracted.rds'))
	
# say('#########################################################')
# say('### calculate derived climate variables for presences ###')
# say('#########################################################')

# op <- readRDS(paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/03 Ochotona princeps - Usable - Presences 1990-2015 - PRISM & DayMet Climate Data Extracted.rds'))

# rawCol <- which(grepl(names(op), pattern='pptAnnual_yearMinus09')):ncol(op)

# op <- calcDerivedVars(x=op, window=10, rawCol=rawCol, verbose=TRUE)

# saveRDS(op, paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/04 Ochotona princeps - Usable - Presences 1990-2015 - Derived Climate Variables.rds'))

# say('####################################################################################')
# say('### assign scheme division names to presences: physiography (Fenneman provinces) ###')
# say('####################################################################################')

# op <- readRDS(paste0(workDir, '/Species Records - Pika/', recordsVersionSubDir, '/04 Ochotona princeps - Usable - Presences 1990-2015 - Derived Climate Variables.rds'))

# say('### fenneman physiographic provinces ###')
# shape <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/Fenneman_Physio'), 'physio', verbose=FALSE)

# opSp <- SpatialPoints(cbind(op$longWgs84, op$latWgs84), proj4string=CRS(projection(shape)))
# opSpEa <- sp::spTransform(opSp, CRS(crsClimateNA))
# op$physioFennemanL1 <- raster::extract(shape, opSp)$DIVISION
# op$origPhysioFennemanL2 <- op$physioFennemanL2 <- raster::extract(shape, opSp)$PROVINCE
# op$physioFennemanL3 <- raster::extract(shape, opSp)$SECTION

# units <- sort(unique(op$physioFennemanL2))
# possibleCols <- c(8, 12, 17, 24, 26, 32, 37, 42, 47, 75, 79, 82, 84, 101, 116, 142, 258, 610, 614, 'yellow', 'red')
# unitsCol <- possibleCols[seq_along(units)]

# # tally and plot
# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/Fenneman_Physio/!Division of Presence Records by Fenneman Provinces.pdf'), height=8.5, width=11)

	# par(mfrow=c(1, 2), mar=0.2 * c(5, 4, 4, 2) + 0.1)

	# # list of presences per unit
	# plot(0, 0, fg='white', bg='white', col='white', col.lab='white', col.main='white', col.axis='white')
	# tally <- 'Presences per Unit (Assigned As-is)'
	# for (prov in units) tally <- c(tally, paste(prov, ' ', nrow(subset(op, physioFennemanL2==prov)), collapse=' '))
	# text(-1, 0, paste(tally, collapse='\n'), adj=0, xpd=NA)

	# plot(opSp, pch=1)
	# plot(shape, add=TRUE, col='white')
	# for (i in seq_along(units)) plot(subset(shape, PROVINCE==units[i]), col=alpha(unitsCol[i], 0.3), add=TRUE)
	# points(opSp, pch=seq_along(units)[match(op$physioFennemanL2, units)], col=alpha(unitsCol[match(op$physioFennemanL2, units)], 0.5))
	# cents <- gCentroid(shape, byid=TRUE)
	# text(cents, shape$PROVINCE, cex=0.6, col=unitsCol[match(shape$PROVINCE, units)])

	# # re-assign unit of presences that fall in units that have very few presences (<60) to closest unit with >60 presences
	# units <- sort(unique(na.omit(op$physioFennemanL2)))
	
	# shapeEa <- sp::spTransform(shape, CRS(crsClimateNA))
	
	# # re-allocate points from this unit?
	# for (unitFrom in units) {
	
		# opThisUnitIndex <- which(op$physioFennemanL2 == unitFrom)
		# opThisUnit <- op[opThisUnitIndex, ]
		
		# if (nrow(opThisUnit) < 60) {
		
			# # distance matrix
			# distMatrix <- matrix(NA, ncol=length(units), nrow=nrow(opThisUnit))
			
			# # points to be re-assigned
			# thisOpSpEa <- opSpEa[which(op$physioFennemanL2 == unitFrom), ]
		
			# # calculate distance from each point to each division unit
			# for (countUnitTo in seq_along(units)) {
			
				# if (unitFrom != units[countUnitTo]) {

					# unitToPoly <- shapeEa[which(shapeEa$PROVINCE == units[countUnitTo]), ]
					# unitToPoly <- gUnaryUnion(unitToPoly)
					# distMatrix[ , countUnitTo] <- gDistance(unitToPoly, thisOpSpEa, byid=TRUE)
					
				# }
			
			# }
			
			# # re-assign
			# op$physioFennemanL2[opThisUnitIndex] <- units[apply(distMatrix, 1, which.min)]
		
		# }
		
	# }
	
	# title(main='Physiographic Regions - Fennemann Provinces - Raw Assignation', outer=TRUE, line=-2, sub=date())

	# par(mfrow=c(1, 2), mar=0.2 * c(5, 4, 4, 2) + 0.1)

	# # list of presences per unit
	# plot(0, 0, fg='white', bg='white', col='white', col.lab='white', col.main='white', col.axis='white')
	# tally <- 'Presences per Unit (Assigned As-is)'
	# for (prov in units) tally <- c(tally, paste(prov, ' ', nrow(subset(op, physioFennemanL2==prov)), collapse=' '))
	# text(-1, 0, paste(tally, collapse='\n'), adj=0, xpd=NA)

	# plot(opSp, pch=1)
	# plot(shape, add=TRUE, col='white')
	# for (i in seq_along(units)) plot(subset(shape, PROVINCE==units[i]), col=alpha(unitsCol[i], 0.3), add=TRUE)
	# points(opSp, pch=seq_along(units)[match(op$physioFennemanL2, units)], col=alpha(unitsCol[match(op$physioFennemanL2, units)], 0.5))
	# cents <- gCentroid(shape, byid=TRUE)
	# text(cents, shape$PROVINCE, cex=0.6, col=unitsCol[match(shape$PROVINCE, units)])

	# title(main='Physiographic Regions - Fennemann Provinces - Re-assignation', outer=TRUE, line=-2, sub=date())

# dev.off()

# saveRDS(op, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/05 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Provinces (Fenneman).rds'))

# say('#######################################################################################')
# say('### assign scheme division names to presences: ecoregions (EPA Modified Ecoregions) ###')
# say('#######################################################################################')

# say('### Ecoregions: EPA Level III Modified ###')
# say('Note: This script not only assigns and allocates records to ecoregions but also joins ecoregions (by renaming) IN THE SHAPEFILE and saves the result.')
# epaLevel2 <- readOGR('F:/ecology/Ecoregionalizations/US EPA - Ecoregions of North America/Level II', 'NA_CEC_Eco_Level2', verbose=FALSE)
# epaLevel3 <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!OmernikEcoregions'), 'us_eco_l3SarrREV', verbose=FALSE)

# epaLevel3 <- epaRename(epa=epaLevel3)
# op <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/05 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Provinces (Fenneman).rds'))
# opSp <- SpatialPoints(cbind(op$longWgs84, op$latWgs84), proj4string=CRS(crsNad83))
# opSp <- sp::spTransform(opSp, CRS(projection(epaLevel3)))
# opSpEa <- sp::spTransform(opSp, CRS(crsClimateNA))

# # extract divisions
# epaLevel2 <- sp::spTransform(epaLevel2, CRS(projection(epaLevel3)))
# op$origEcoregionEpaLevel1 <- raster::extract(epaLevel2, opSp)$NA_L1NAME
# op$ecoregionEpaLevel2 <- raster::extract(epaLevel2, opSp)$NA_L2NAME
# op$origEcoregionEpaLevel3Modified <- raster::extract(epaLevel3, opSp)$L3_KEY

# presFieldOrigName <- 'origEcoregionEpaLevel3Modified'
# presFieldNewName <- 'ecoregionEpaLevel3Modified'
# shapeFieldName <- 'L3_KEY'

# units <- sort(unique(op[ , presFieldOrigName]))
# possibleCols <- rev(c(8, 12, 17, 24, 26, 32, 37, 42, 47, 75, 79, 82, 84, 101, 116, 142, 258, 610, 614, 552, 652))
# unitsCol <- possibleCols[seq_along(units)]

# say('UNITS: ', paste(units, collapse=' '))

# gadm <- readOGR('C:/ecology/Political Geography/GADM/ver2pt8/WGS84', 'USA_adm2', verbose=FALSE)
# gadm <- sp::spTransform(gadm, CRS(projection(epaLevel3)))

# # tally and plot
# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!OmernikEcoregions/!Division of Presence Records by EPA Level III Modified Ecoregions.pdf'), height=8.5, width=11)

	# par(layout(matrix(c(1, 2, 2, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)
	
	# ## BEFORE re-assignation: Level III
	
	# # list of presences per unit
	# plot(0, 0, fg='white', bg='white', col='white', col.lab='white', col.main='white', col.axis='white')
	# tally <- 'Presences per Unit'
	# for (prov in units) tally <- c(tally, paste(prov, ' ', nrow(subset(op, origEcoregionEpaLevel3Modified==prov)), collapse=' '))
	# text(-1, 0, paste(tally, collapse='\n'), xpd=NA, adj=0)

	# # EPA Level 3
	# plot(opSp, pch=1, main=NA)
	# plot(epaLevel3, add=TRUE, col='white')
	# for (i in seq_along(units)) plot(subset(epaLevel3, L3_KEY==units[i]), col=alpha(unitsCol[i], 0.4), add=TRUE)
	# points(opSp, pch=seq_along(units)[match(op$origEcoregionEpaLevel3Modified, units)], col=alpha(unitsCol[match(op$origEcoregionEpaLevel3Modified, units)], 0.7))
	# plot(epaLevel3, add=TRUE, border=alpha('black', 0.3))
	# cents <- gCentroid(epaLevel3, byid=TRUE)
	# text(cents, epaLevel3$L3_KEY, cex=0.8)

	# title(main='Assigned by Underlying Level III Ecoregion (Modified)', outer=TRUE, line=-1)
	
	# par(mfrow=c(1, 1), mar=0.2 * c(5, 4, 4, 2) + 0.1)

	# ## BEFORE re-assignation: Level II
	
	# # EPA level II
	# plot(opSp, pch=1, col='white', main='Level II Ecoregions')
	# n <- nrow(epaLevel2)
	# plot(epaLevel2, add=TRUE, border=rgb(runif(n), runif(n), runif(n), alpha=0.5), lwd=3)
	# points(opSp, pch=seq_along(units)[match(op$origEcoregionEpaLevel3Modified, units)], col=alpha(unitsCol[match(op$origEcoregionEpaLevel3Modified, units)], 0.7))
	# # cents <- gCentroid(epaLevel2, byid=TRUE)
	# # text(cents, epaLevel2$NA_L2NAME, cex=0.6)

	# ## RE-ASSIGN unit of presences that fall in units that have very few presences (<125) or have highly restricted spatial sampling
	# epaLevel3Df <- as.data.frame(epaLevel3)

	# epaLevel3[epaLevel3Df$L3_KEY %in% 'Idaho Batholith' | epaLevel3Df$L3_KEY %in% 'Blue Mtns'] <- 'Blue Mtns & Idaho Batholith'
	# epaLevel3[epaLevel3Df$L3_KEY %in% 'Colo Plats' | epaLevel3Df$L3_KEY %in% 'Wasatch-Uinta Mtns'] <- 'Colo Plats & Wasatch-Uinta Mtns'
	# epaLevel3[epaLevel3Df$L3_KEY %in% 'Mid-Rockies' | epaLevel3Df$L3_KEY %in% 'Snake R Plain'] <- 'Snake R Plain & Mid-Rockies'
	
	# op$ecoregionEpaLevel3Modified <- raster::extract(epaLevel3, opSp)$L3_KEY

	# # re-assign presence(s) very close to outpost of Southern Rockies but in the Colorado Plateau to the Southern Rockies
	# op$ecoregionEpaLevel3Modified[
		# op$ecoregionEpaLevel3Modified == 'Colo Plats & Wasatch-Uinta Mtns' &
		# op$longWgs84 > -109.6594 & op$latWgs84 < 38.77877] <- 'S Rockies'
	
	# units <- sort(unique(na.omit(op$ecoregionEpaLevel3Modified)))
	
	# par(layout(matrix(c(1, 2, 2, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# # list of presences per province
	# plot(0, 0, fg='white', bg='white', col='white', col.lab='white', col.main='white', col.axis='white')
	# tally <- 'Presences per Unit (Re-Assigned)'
	# for (prov in units) tally <- c(tally, paste(prov, ' ', nrow(subset(op, ecoregionEpaLevel3Modified==prov)), collapse=' '))
	# text(-1, 0, paste(tally, collapse='\n'), xpd=NA, adj=0)

	# # EPA Level III
	# units <- sort(unique(op[ , presFieldNewName]))
	# possibleCols <- c(8, 12, 17, 24, 26, 32, 37, 42, 47, 75, 79, 82, 84, 101, 116, 142, 258, 610, 614, 'yellow', 'red')
	# unitsCol <- possibleCols[seq_along(units)]

	# plot(opSp, pch=1)
	# plot(epaLevel3, add=TRUE, col='white')
	# for (i in seq_along(units)) plot(subset(epaLevel3, L3_KEY==units[i]), col=alpha(unitsCol[i], 0.2), add=TRUE)
	# points(opSp, pch=seq_along(units)[match(op$ecoregionEpaLevel3Modified, units)], col=alpha(unitsCol[match(op$ecoregionEpaLevel3Modified, units)], 0.7))
	# plot(epaLevel3, add=TRUE, border=alpha('black', 0.3))
	# cents <- gCentroid(epaLevel3, byid=TRUE)
	# text(cents, epaLevel3$L3_KEY, cex=0.8)

	# title(main='Assigned by Underlying Level III Ecoregion (Modified) - Re-assigned', outer=TRUE, line=-1)
	
# dev.off()

# writeOGR(epaLevel3, paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!OmernikEcoregions'), 'us_eco_l3SarrREV_joinedUndersampledEcoregions', driver='ESRI Shapefile', overwrite_layer=TRUE)

# saveRDS(op, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/06 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (EPA Level III Modified).rds'))

	
# say('#################################################################')
# say('### assign scheme division names to presences: climate (GEnS) ###')
# say('#################################################################')

# shape <- readRDS('F:/ecology/Climate/GEnS/GEnSv3_11012012/wgs84/GEnS_v3_wgs84_conus_dissolvedToGEnSname.rds')
# shape <- sp::spTransform(shape, CRS(projection(crsNad83)))

# shape <- gensRename(gens=shape)

# op <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/06 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (EPA Level III Modified).rds'))
# opSp <- SpatialPoints(cbind(op$longWgs84, op$latWgs84), proj4string=CRS(crsNad83))
# opSpEa <- sp::spTransform(opSp, CRS(crsClimateNA))

# # extract divisions
# GEnS <- raster::extract(shape, opSp)
# GEnS <- GEnS[!duplicated(GEnS[ , 1]), ]
# op$climateGEnSZone <- op$origClimateGEnSZone <- GEnS$GEnZname
# gc()

# presFieldOrigName <- 'origClimateGEnSZone'
# presFieldNewName <- 'climateGEnSZone'
# shapeFieldName <- 'GEnZname'

# units <- as.character(sort(unique(op[ , presFieldOrigName])))

# qualCols <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#b15928')
# polyCol <- colorRampPalette(colors=qualCols)
# polyCol <- polyCol(n=nrow(shape))

# unitsCol <- colorRampPalette(colors=c('gray0', 'gray100'))
# unitsCol <- unitsCol(n=nrow(shape))

# say('UNITS: ', paste(units, collapse=' '))

# # tally and plot
# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!Metzger_GEnS/!Division of Presence Records by GEnS.pdf'), height=6.5, width=13)

	# par(layout(matrix(c(1, 2, 2, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# ## plot before assigned
	
	# # list of presences per unit
	# plot(0, 0, fg='white', bg='white', col='white', col.lab='white', col.main='white', col.axis='white')
	# tally <- 'Presences per Unit'
	# for (prov in units) tally <- c(tally, paste(prov, ' ', sum(op[ , presFieldNewName]==prov, na.rm=TRUE)), collapse=' ')
	# text(-1, 0, paste(tally, collapse='\n'), xpd=NA, adj=0)

	# # map all units
	# plot(extent(opSp), main=NA)
	# plot(shape, add=TRUE, col=polyCol, border=NA)
	# points(opSp, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=alpha(unitsCol[match(op[, presFieldNewName], units)], 0.9))
	# cents <- gCentroid(shape, byid=TRUE)
	# text(cents, labels=as.data.frame(shape)[ , shapeFieldName], cex=0.8)

	# title(main='Assigned by Underlying GEnS Zone', outer=TRUE, line=-1)

	# ### re-assign
	# op[which(op[ , presFieldOrigName] == 'Extremely cold & wet (D)'), presFieldNewName] <- 'Cold & wet (E)'
	# op[which(op[ , presFieldOrigName] == 'Cool temperate & xeric (I)'), presFieldNewName] <- 'Cool temperate & dry (H)'
	
	# par(layout(matrix(c(1, 2, 2, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# # list of presences per unit
	# plot(0, 0, fg='white', bg='white', col='white', col.lab='white', col.main='white', col.axis='white')
	# tally <- 'Presences per Unit'
	# for (prov in units) tally <- c(tally, paste(prov, ' ', sum(op[ , presFieldNewName]==prov, na.rm=TRUE)), collapse=' ')
	# text(-1, 0, paste(tally, collapse='\n'), xpd=NA, adj=0)

	# # map all units
	# plot(extent(opSp), main=NA)
	# plot(shape, add=TRUE, col=polyCol, border=NA)
	# points(opSp, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=alpha(unitsCol[match(op[, presFieldNewName], units)], 0.9))
	# cents <- gCentroid(shape, byid=TRUE)
	# text(cents, labels=as.data.frame(shape)[ , shapeFieldName], cex=0.8)

	# title(main='Assigned by Underlying GEnS Zone (Re-assigned)', outer=TRUE, line=-1)

# dev.off()

# saveRDS(op, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/07 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (GEnS).rds'))

# say('###################################################################')
# say('### assign scheme division names to presences: climate (Koppen) ###')
# say('###################################################################')

# shape <- readOGR('F:/ecology/Ecoregionalizations/Koppen Climate Classification/koppenShapefile', 'koppenshape', verbose=FALSE)

# shape$zone <- koppenRename(x=shape$GRIDCODE)

# tallyPointsInZone <- function(shape, pres, presFieldName) {

	# tally <- data.frame(zone=sort(unique(shape$zone)), n=0)
	# for (i in 1:nrow(pres)) {
		# whichZone <- which(tally$zone %in% pres[i, presFieldName])
		# tally$n[whichZone] <- tally$n[whichZone] + 1
	# }
	
	# return(tally)
	
# }

# op <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/07 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (GEnS).rds'))
# opSp <- SpatialPointsDataFrame(cbind(op$longWgs84, op$latWgs84), data=op, proj4string=CRS(crsWgs84))
# opSp <- sp::spTransform(opSp, CRS(projection(shape)))

# # extract divisions
# op$origClimateKoppen <- op$climateKoppen <- raster::extract(shape, opSp)$zone

# presFieldOrigName <- 'origClimateKoppen'
# presFieldNewName <- 'climateKoppen'
# shapeFieldName <- 'zone'

# opSpCropped <- opSp[which(!is.na(op[ , presFieldOrigName])), ]

# units <- as.character(sort(unique(op[ , presFieldOrigName])))

# qualCols <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#b15928')
# cols <- colorRampPalette(colors=qualCols)
# polyCol <- cols(n=length(unique(shape$zone)))

# unitsCols <- rev(c('red', 'orange', 'yellow', 'green', 'blue', 'purple', 'darkred', 'black'))
# unitsCols <- unitsCols[1:length(units)]

# say('UNITS: ', paste(units, collapse=' '))

# say('')
# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName)
# print(tally)
# say('')

# # tally and plot
# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!KoppenFromPRISM/!Division of Presence Records by Koppen from PRISM.pdf'), height=8.5, width=11)

	# par(layout(matrix(c(1, 1, 1, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# ## plot before assigned
	
	# # map all units
	# plot(opSpCropped, col='white', main=NA)
	# for (i in 1:nrow(shape)) plot(shape[i, ], add=TRUE, col=polyCol[which(units %in% as.data.frame(shape[i, ])[ , shapeFieldName])], border=NA)
	# plot(shape, add=TRUE, col=alpha('white', 0.1), border=NA)
	# points(opSp, cex=2, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=unitsCols[match(op[, presFieldNewName], units)])

	# # list of presences per unit
	# frame()
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName)
	# tally <- tally[tally$n > 0, ]
	# tally <- apply(tally, 1, paste, collapse=' ')
	# legend('left', inset=0, xpd=NA, cex=2, legend=tally, pch=1:length(units), title='Presences / Unit', col=unitsCols)
	
	# title(main='Assigned by Underlying Koppen Zone', outer=TRUE, line=-1, sub=date())

	# ## RE-ASSIGN points zone with <125 records to nearest zone with >=125 original presences')
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName)
	# tally <- tally[order(tally$n, decreasing=TRUE), ]

	# fromZones <- tally$zone[tally$n < 125 & tally$n > 0]
	
	# for (fromZone in fromZones) {
	
		# from <- which(op$origClimateKoppen %in% fromZone)
		
		# par(mfrow=c(1, 2), mar=0.2 * c(2, 2, 2, 2) + 0.1)
		# for (thisFrom in from) {
		
			# buff <- gBuffer(opSp[thisFrom, ], width=60000)

			# # BEFORE RE-ASSIGNATION
			# plot(buff, border=NA, main='')
			# for (i in 1:nrow(shape)) plot(shape[i, ], add=TRUE, col=polyCol[which(units %in% as.data.frame(shape[i, ])[ , shapeFieldName])], border=NA)
			# points(opSp, cex=2, pch=seq_along(units)[match(op[ , presFieldNewName], units)], col=unitsCols[match(op[ , presFieldNewName], units)])
			# points(opSp[thisFrom, ], cex=2, pch=16, col='black')
			# legend('bottomleft', legend=units, cex=2, fill=polyCol, title='Polygon zone')
			
			# title(main=paste0('BEFORE: Presence with ', fromZone, ' index #', thisFrom, ' assigned to ', op[thisFrom, presFieldNewName], '.'), line=-1, sub=date())

			# # RE-ASSIGN
			# thisPoint <- opSp[thisFrom, ]
			# dists <- c(gDistance(shape, thisPoint, byid=TRUE))
			# dists[which(dists < eps)] <- NA
			# dists[which(shape$zone %in% fromZone)] <- NA
			
			# dists[tally$n[match(shape$zone, tally$zone)] < 125] <- NA
			
			# op[thisFrom, presFieldNewName] <- as.data.frame(shape)[which.min(dists), shapeFieldName]
			
			# # AFTER RE-ASSIGNATION
			# plot(buff, border=NA, main='')
			# for (i in 1:nrow(shape)) plot(shape[i, ], add=TRUE, col=polyCol[which(units %in% as.data.frame(shape[i, ])[ , shapeFieldName])], border=NA)
			# points(opSp, cex=2, pch=seq_along(units)[match(op[ , presFieldNewName], units)], col=unitsCols[match(op[ , presFieldNewName], units)])
			# points(opSp[thisFrom, ], cex=2, pch=16, col='black')
			# legend('bottomleft', legend=units, cex=2, fill=polyCol, title='Polygon zone')

			# title(main=paste0('AFTER: Presence with ', fromZone, ' index #', thisFrom, ' assigned to ', op[thisFrom, presFieldNewName], '.'), line=-1, sub=date())

		# }
		
	# } # next zone to re-assign from

	# par(layout(matrix(c(1, 1, 1, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# ## plot AFTER re-assigned
	
	# # map all units
	# plot(opSpCropped, col='white', main=NA)
	# for (i in 1:nrow(shape)) plot(shape[i, ], add=TRUE, col=polyCol[which(units %in% as.data.frame(shape[i, ])[ , shapeFieldName])], border=NA)
	# plot(shape, add=TRUE, col=alpha('white', 0.1), border=NA)
	# points(opSp, cex=2, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=unitsCols[match(op[, presFieldNewName], units)])
	# points(opSp[which(op$climateKoppen=='BSk'), ], pch=21, col='black', bg='red', cex=1.4)

	# # list of presences per unit
	# frame()
	# tallyNew <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldNewName)
	# tallyOld <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName)
	# tallyNew <- tallyNew[tallyOld$n > 0, ]
	# tally <- apply(tallyNew, 1, paste, collapse=' ')
	# legend('left', inset=0, xpd=NA, cex=2, legend=tally, pch=1:length(units), title='Presences / Unit', col=unitsCols)
	
	# title(main='Assigned by Underlying Koppen Zone (Re-assigned)', outer=TRUE, line=-1, sub=date())

# dev.off()

# saveRDS(op, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/08 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (Koppen).rds'))


# say('#####################################################################')
# say('### assign scheme division names to presences: ecoregion (Bailey) ###')
# say('#####################################################################')

# shape <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!BaileyEcoregions'), 'eco_us', verbose=FALSE)

# shape$provinceShort <- shape$PROVINCE
# shape$provinceShort <- baileyRename(shape$provinceShort)

# tallyPointsInZone <- function(shape, pres, presFieldName, shapeFieldName) {

	# tally <- data.frame(zone=sort(unique(as.data.frame(shape)[ , shapeFieldName])), n=0)
	# for (i in 1:nrow(pres)) {
		# whichZone <- which(tally$zone %in% pres[i, presFieldName])
		# tally$n[whichZone] <- tally$n[whichZone] + 1
	# }
	
	# return(tally)
	
# }

# op <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/08 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (Koppen).rds'))
# opSp <- SpatialPointsDataFrame(cbind(op$longWgs84, op$latWgs84), data=op, proj4string=CRS(crsWgs84))
# opSp <- sp::spTransform(opSp, CRS(projection(shape)))

# # extract divisions
# op$origEcoregionBaileyProvince <- op$ecoregionBaileyProvince <- raster::extract(shape, opSp)$provinceShort

# presFieldOrigName <- 'origEcoregionBaileyProvince'
# presFieldNewName <- 'ecoregionBaileyProvince'
# shapeFieldName <- 'provinceShort'

# opSpCropped <- opSp[which(!is.na(op[ , presFieldOrigName])), ]

# units <- as.character(sort(unique(op[ , presFieldOrigName])))

# qualCols <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#b15928')
# cols <- colorRampPalette(colors=qualCols)
# polyCol <- cols(n=length(units))

# unitsCols <- rev(c('red', 'orange', 'yellow', 'green', 'blue', 'purple', 'darkred', 'black', 'darkgreen', 'darkblue'))
# unitsCols <- unitsCols[1:length(units)]

# say('UNITS: ', paste(units, collapse=' '))

# # tally and plot
# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!BaileyEcoregions/!Division of Presence Records by Bailey Ecoregions.pdf'), height=8.5, width=11)

	# ## ORIGINAL ASSIGNATION
	
	# par(layout(matrix(c(1, 1, 1, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# # map all units
	# plot(opSpCropped, col='white', main=NA)
	# plot(shape, add=TRUE)
	# for (prov in units) plot(shape[as.data.frame(shape)[ , shapeFieldName] == prov, ], add=TRUE, border=polyCol[which(units %in% prov)], lwd=5, col=alpha(polyCol[which(units %in% prov)], 0.5))
	# points(opSp, cex=2, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=unitsCols[match(op[, presFieldNewName], units)])

	# # list of presences per unit
	# frame()
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName, shapeFieldName=shapeFieldName)
	# tally <- tally[tally$n > 0, ]
	# tally <- apply(tally, 1, paste, collapse=' ')
	# legend('left', inset=0, xpd=NA, cex=1, legend=tally, pch=1:length(units), title='Presences / Unit', col=unitsCols)
	
	# title(main='Assigned by Original Bailey Ecoregion', outer=TRUE, line=-1, sub=date())

	# say('## re-assign points in regions with <125 presences to nearest region with >=125 original presences')
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName, shapeFieldName=shapeFieldName)
	# tallyCulled <- tally[tally$n > 0, ]
	
	# unitsTooFew <- tallyCulled$zone[tallyCulled$n < 125]

	# par(mfrow=c(1, 2), mar=0.4 * c(2, 2, 2, 2) + 0.1)

	# for (thisUnit in unitsTooFew) {
	
		# pointsTooFew <- which(op[ , presFieldOrigName] %in% thisUnit)
		# buff <- gBuffer(opSp[pointsTooFew, ], width=60000)

		
		# plot(buff, main=paste('Before Re-assignation from', thisUnit), col='white')
		# plot(shape, add=TRUE)
		# for (prov in units) plot(shape[as.data.frame(shape)[ , shapeFieldName] == prov, ], add=TRUE, border=polyCol[which(units %in% prov)], lwd=5, col=alpha(polyCol[which(units %in% prov)], 0.5))
		# plot(shape[as.data.frame(shape)[ , shapeFieldName] == thisUnit, ], col='darkred', border=NA, add=T)
		# points(opSp, cex=2, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=unitsCols[match(op[, presFieldNewName], units)])
		# points(opSp[pointsTooFew, ], pch=21, cex=2.2, bg='red', col='black')
		
		# for (i in pointsTooFew) {
		
			# dists <- c(gDistance(opSp[i, ], shape, byid=TRUE))
			# for (thisThisUnit in units) if (tally$n[tally$zone == thisThisUnit] < 125) dists[as.data.frame(shape)[ , shapeFieldName]==thisThisUnit] <- NA
			
			# op[i , presFieldNewName] <- as.data.frame(shape)[which.min(dists), shapeFieldName]
			
		# }
		
	# }
	
	# ## AFTYER RE-ASSIGNATION
	
	# par(layout(matrix(c(1, 1, 1, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# # map all units
	# plot(opSpCropped, col='white', main=NA)
	# plot(shape, add=TRUE)
	# for (prov in units) plot(shape[as.data.frame(shape)[ , shapeFieldName] == prov, ], add=TRUE, border=polyCol[which(units %in% prov)], lwd=5, col=alpha(polyCol[which(units %in% prov)], 0.5))
	# points(opSp, cex=2, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=unitsCols[match(op[, presFieldNewName], units)])

	# # list of presences per unit
	# frame()
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldNewName, shapeFieldName=shapeFieldName)
	# tallyOrig <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName, shapeFieldName=shapeFieldName)
	# tally <- tally[tallyOrig$n > 0, ]
	# tally <- apply(tally, 1, paste, collapse=' ')
	# legend('left', inset=0, xpd=NA, cex=1, legend=tally, pch=1:length(units), title='Presences / Unit', col=unitsCols)
	
	# title(main='Re-assigned by Bailey Ecoregion', outer=TRUE, line=-1, sub=date())
		
# dev.off()

# saveRDS(op, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/09 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (Bailey).rds'))

# say('##############################################################################')
# say('### assign scheme division names to presences: elevational bands above PME ###')
# say('##############################################################################')

# shape <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!ElevationQuintiles/!PME-based_Elevation'), '!MAbove_MinHab_Quintile', verbose=FALSE)

# shape$elevZoneAboveMinPme <- pmeRename(shape$GRIDCODE)

# tallyPointsInZone <- function(shape, pres, presFieldName, shapeFieldName) {

	# tally <- data.frame(zone=sort(unique(as.data.frame(shape)[ , shapeFieldName])), n=0)
	# for (i in 1:nrow(pres)) {
		# whichZone <- which(tally$zone %in% pres[i, presFieldName])
		# tally$n[whichZone] <- tally$n[whichZone] + 1
	# }
	
	# return(tally)
	
# }

# op <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/09 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (Bailey).rds'))
# opSp <- SpatialPointsDataFrame(cbind(op$longWgs84, op$latWgs84), data=op, proj4string=CRS(crsWgs84))
# opSp <- sp::spTransform(opSp, CRS(projection(shape)))

# # extract divisions
# op$origElevZoneAboveMinPme <- op$elevZoneAboveMinPme <- raster::extract(shape, opSp)$elevZoneAboveMinPme

# presFieldOrigName <- 'origElevZoneAboveMinPme'
# presFieldNewName <- 'elevZoneAboveMinPme'
# shapeFieldName <- 'elevZoneAboveMinPme'

# opSpCropped <- opSp[which(!is.na(op[ , presFieldOrigName])), ]

# units <- as.character(sort(unique(op[ , presFieldOrigName])))

# qualCols <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#b15928')
# cols <- colorRampPalette(colors=qualCols)
# polyCol <- cols(n=length(units))

# unitsCols <- c('red', 'black', 'orange', 'darkgreen', 'darkblue', 'purple')
# unitsCols <- unitsCols[1:length(units)]

# say('UNITS: ', paste(units, collapse=' | '))

# # tally and plot
# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!ElevationQuintiles/!PME-based_Elevation/!Division of Presence Records by Elevation Quantiles Above Min PME.pdf'), height=8.5, width=11)

	# ## ORIGINAL ASSIGNATION
	
	# par(layout(matrix(c(1, 1, 1, 2), nrow=1)), mar=0.2 * c(2, 2, 2, 2) + 0.1)

	# # map all units
	# plot(opSpCropped, col='white', main=NA)
	# for (prov in units) plot(shape[as.data.frame(shape)[ , shapeFieldName] == prov, ], add=TRUE, border=NA, col=alpha(polyCol[which(units %in% prov)], 0.5))
	# points(opSp, cex=1.2, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=unitsCols[match(op[, presFieldNewName], units)])
	# legend('bottomleft', legend=units, fill=polyCol)

	# # list of presences per unit
	# frame()
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName, shapeFieldName=shapeFieldName)
	# tally <- tally[tally$n > 0, ]
	# tally <- apply(tally, 1, paste, collapse=' ')
	# legend('left', inset=0, xpd=NA, cex=1, legend=tally, pch=1:length(units), title='Presences / Unit', col=unitsCols)
	
	# ### RE-ASSIGNATION
	# say('No presences need re-assigned, but some still fall outside quintiles. Assigning these manually according to Mimi\'s documentation in "C:/ecology/Drive/Research/Iconic Species/Extents_Masks_Maps/EcoRegions/!ElevationQuintiles/Assignation of Presence Records Falling Outside Elevation Quintiles.csv".')
	
	# reAssign <- read.csv(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!ElevationQuintiles/Assignation of Presence Records Falling Outside Elevation Quintiles.csv'))
	
	# for (i in 1:nrow(reAssign)) op$elevZoneAboveMinPme[op$num == reAssign$num[i]] <- reAssign$QuintileNice[i]

	# ### RE-ASSIGNED
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName, shapeFieldName=shapeFieldName)
	# tallyCulled <- tally[tally$n > 0, ]
	# print(tallyCulled)
	
	# # map each units
	# par(mfrow=c(1, 1), mar=0.2 * c(2, 2, 2, 2) + 0.1)
	# for (prov in units) {
		
		# plot(opSpCropped, col='white', main=prov)
		# plot(shape, border='gray', add=TRUE)
		# plot(shape[as.data.frame(shape)[ , shapeFieldName] == prov, ], add=TRUE, border=NA, col='red')
		# points(opSp[which(op[ , presFieldOrigName] == prov), ], cex=1.2, pch=1, col='black')

	# }
		
# dev.off()

# saveRDS(op, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/10 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Elevational Bands above Min PME.rds'))


# say('#########################################################################')
# say('### assign scheme division names to presences: physiography (Hammond) ###')
# say('#########################################################################')

# shape <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!Hammond_HLR'), 'hlrus_conusWest', verbose=FALSE)

# tallyPointsInZone <- function(shape, pres, presFieldName, shapeFieldName) {

	# tally <- data.frame(zone=sort(unique(as.data.frame(shape)[ , shapeFieldName])), n=0)
	# for (i in 1:nrow(pres)) {
		# whichZone <- which(tally$zone %in% pres[i, presFieldName])
		# tally$n[whichZone] <- tally$n[whichZone] + 1
	# }
	
	# return(tally)
	
# }

# op <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/10 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Elevational Bands above Min PME.rds'))
# opSp <- SpatialPointsDataFrame(cbind(op$longWgs84, op$latWgs84), data=op, proj4string=CRS(crsWgs84))
# opSp <- sp::spTransform(opSp, CRS(projection(shape)))

# # extract divisions
# op$origPhysioHammond <- op$physioHammond <- raster::extract(shape, opSp)$HLR

# presFieldOrigName <- 'origPhysioHammond'
# presFieldNewName <- 'physioHammond'
# shapeFieldName <- 'HLR'

# opSpCropped <- opSp[which(!is.na(op[ , presFieldOrigName])), ]

# units <- as.character(sort(unique(op[ , presFieldOrigName])))

# qualCols <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a', '#b15928')
# cols <- colorRampPalette(colors=qualCols)
# polyCol <- cols(n=length(units))

# unitsCols <- rev(c('red', 'orange', 'yellow', 'green', 'blue', 'purple', 'darkred', 'black', 'darkgreen', 'darkblue'))
# unitsCols <- unitsCols[1:length(units)]

# say('UNITS: ', paste(units, collapse=' | '))

# # tally and plot
# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!Hammond_HLR/!Division of Presence Records by Hammond Physiographic HLR Regions.pdf'), height=8.5, width=11)

	# ## plot before re-assigned
	
	# par(layout(matrix(c(1, 1, 1, 2), nrow=1)), mar=0.5 * c(2, 2, 2, 2) + 0.1)

	# # map all units
	# plot(opSpCropped, col='white', main=NA)
	# for (prov in units) plot(shape[as.data.frame(shape)[ , shapeFieldName] == prov, ], add=TRUE, border=NA, col=alpha(polyCol[which(units %in% prov)], 0.7))
	# points(opSp, cex=2, pch=seq_along(units)[match(op[, presFieldNewName], units)], col=unitsCols[match(op[, presFieldNewName], units)])
	# legend('bottomleft', legend=units, fill=polyCol)

	# # list of presences per unit
	# frame()
	# tally <- tallyPointsInZone(shape=shape, pres=op, presFieldName=presFieldOrigName, shapeFieldName=shapeFieldName)
	# tally <- tally[tally$n > 0, ]
	# tally <- apply(tally, 1, paste, collapse=' ')
	# legend('left', inset=0, xpd=NA, cex=1, legend=tally, pch=1:length(units), title='Presences / Unit', col=unitsCols)
	
	# title(main='Assigned by Original Hammond Physiographic Unit - NO REASSIGNMENTS NEEDED', outer=TRUE, line=-1, sub=date())

	# say('## NO RE-ASSIGNMENTS NEEDED')

	# # map each units
	# par(mfrow=c(1, 1), mar=0.5 * c(2, 2, 2, 2) + 0.1)
	# for (prov in units) {
		
		# plot(opSpCropped, col='white', main=paste('HLR', prov))
		# plot(shape, border='gray', add=TRUE)
		# plot(shape[as.data.frame(shape)[ , shapeFieldName] == prov, ], add=TRUE, border=NA, col=polyCol[which(units %in% prov)])
		# points(opSp[which(op[ , presFieldOrigName] == prov), ], cex=1.2, pch=21, bg='black', col='white')

	# }
	
# dev.off()

# saveRDS(op, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/11 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Regions (Hammond).rds'))


# say('#################################################################################')
# say('### RE-assign scheme division names to presences: elevational bands above PME ###')
# say('#################################################################################')

	# say('Using Mimis quintiles yields just 65 presences in the lowest quintile, so now using different cut-offs for elevation above PME (esp. PME min).')

	# pres <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/11 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Regions (Hammond).rds'))

	# demPrism <- raster('F:/ecology/Climate/PRISM/PRISM_us_dem_800m.tif')
	# demPrismGrid <- as(demPrism, 'SpatialGrid') # for GRASS
	# demWc <- raster('C:/ecology/Climate/WORLDCLIM Ver 1pt4 Rel 3/30 arcsec/Elevation - 30 arcsec/elevation.tif')
	# pres$elevWorldclim_m <- raster::extract(demWc, cbind(pres$longWgs84, pres$latWgs84))

	# pmeMin <- raster(paste0(workDir, 'Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/!minPME.tif'))
	# pmeMean <- raster(paste0(workDir, 'Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/!meanPME.tif'))
	# pmeMax <- raster(paste0(workDir, 'Extents_Masks_Maps/PikaMinElevation/!ModelEnsembles/!maxPME.tif'))

	# # get PME
	# pres$pmeMin <- raster::extract(pmeMin, cbind(pres$longWgs84, pres$latWgs84))
	# pres$pmeMean <- raster::extract(pmeMean, cbind(pres$longWgs84, pres$latWgs84))
	# pres$pmeMax <- raster::extract(pmeMax, cbind(pres$longWgs84, pres$latWgs84))

	# projection(pmeMin) <- projection(pmeMean) <- projection(pmeMax) <- getCRS('nad83')

	# # calculate difference in elevation to PME
	# pres$elevDiffToPmeMax <- pres$elevDiffToPmeMean <- pres$elevDiffToPmeMin <- NA

	# pres$elevDiffToPmeMax <- pres$elevPrism_m - pres$pmeMax
	# pres$elevDiffToPmeMean <- pres$elevPrism_m - pres$pmeMean
	# pres$elevDiffToPmeMin <- pres$elevPrism_m - pres$pmeMin

	# if (any(is.na(pres$elevDiffToPmeMax))) pres$elevDiffToPmeMax[is.na(pres$elevDiffToPmeMax)] <- pres$elevWorldclim_m[is.na(pres$elevDiffToPmeMax)] - pres$pmeMax[is.na(pres$elevDiffToPmeMax)]
	# if (any(is.na(pres$elevDiffToPmeMean))) pres$elevDiffToPmeMean[is.na(pres$elevDiffToPmeMean)] <- pres$elevWorldclim_m[is.na(pres$elevDiffToPmeMean)] - pres$pmeMean[is.na(pres$elevDiffToPmeMean)]
	# if (any(is.na(pres$elevDiffToPmeMin))) pres$elevDiffToPmeMin[is.na(pres$elevDiffToPmeMin)] <- pres$elevWorldclim_m[is.na(pres$elevDiffToPmeMin)] - pres$pmeMin[is.na(pres$elevDiffToPmeMin)]

	# for (x in c('Min', 'Mean', 'Max')) {

		# quants <- quantile(pres[ , paste0('elevDiffToPme', x)], c(0, 0.2, 0.4, 0.6, 0.8, 1))
		
		# pres$DUMMY <- NA
		# names(pres)[ncol(pres)] <- paste0('elevQuantWrtPae', x)
		
		# for (bin in 1:(length(quants) - 1)) {
		
			# pres[pres[ , paste0('elevDiffToPme', x)] >= quants[bin] & pres[ , paste0('elevDiffToPme', x)] < quants[bin + 1], paste0('elevQuantWrtPae', x)] <- if (bin == 1) {
				# 'Lowest'
			# } else if (bin == 2) {
				# 'Low'
			# } else if (bin == 3) {
				# 'Middle'
			# } else if (bin == 4) {
				# 'High'
			# } else if (bin == 5) {
				# 'Highest'
			# }
		
		# }

		# if (any(pres[ , paste0('elevDiffToPme', x)] == last(quants))) pres[pres[ , paste0('elevDiffToPme', x)] == last(quants), paste0('elevQuantWrtPae', x)] <- 'Highest'
		
	# }

	# saveRDS(pres, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/12 Ochotona princeps - Usable - Presences 1990-2015 - Added Updated PME Quantiles.rds'))
	
	# ### create PME mask shapefile (PME min only)
	# demPrism <- crop(demPrism, pmeMin)
	# pmeMin <- resample(pmeMin, demPrism)
	# diff <- demPrism - pmeMin

	# dirCreate(workDir, 'Extents_Masks_Maps/EcoRegions/!ElevationQuintiles_ver2')

	# if (exists('maskCombo')) rm(maskCombo)
	# quants <- quantile(pres$elevQuantWrtPaeMin, c(0, 0.2, 0.4, 0.6, 0.8, 1))
	# for (bin in 1:(length(quants) - 1)) {

		# say(bin)
	
		# mask <- diff >= quants[bin] & diff < quants[bin + 1]
		# if (bin == length(quants) - 1) {
			# maskUpper <- pmeMin == quants[bin + 1]
			# mask <- mask + maskUpper
		# }

		# mask <- calc(mask, function(x) ifelse(x == 0, NA, 1))
		# mask <- mask + (bin - 1)
		
		# maskCombo <- if (exists('maskCombo')) {
			# stack(maskCombo, mask)
		# } else {
			# mask
		# }

	# }

	# maskCombo <- calc(maskCombo, fun=function(x) sum(x, na.rm=TRUE))
	# maskCombo <- calc(maskCombo, function(x) ifelse(x == 0, NA, x))
	
	# writeRaster(maskCombo, 'C:/ecology/!Scratch/maskCombo', format='GTiff', overwrite=TRUE)

	# # initialize GRASS
	# grass <- initGRASS(
		# gisBase='C:/Program Files/GRASS GIS 7.0.5',
		# gisDbase='C:/ecology/!Scratch/grass',
		# SG=demPrismGrid,
		# location='nad83',
		# mapset='Adam',
		# home='C:/ecology/!Scratch/grass',
		# remove_GISRC=TRUE,
		# override=TRUE
	# )

	# # mask into GRASS as raster
	# writeRAST(x=as(maskCombo, 'SpatialGridDataFrame'), vname='maskCombo', overwrite=TRUE)
	
	# # convert mask raster to polygon
	# execGRASS(
		# cmd='r.to.vect',
		# flags=c('overwrite'),
		# parameters=list(
			# input='maskCombo',
			# output='maskPoly',
			# type='area'
		# )
	# )
	
	# maskPoly <- readVECT(vname='maskPoly', type='area', driver='ESRI Shapefile')
	
	# maskPoly$elevQuantWrtPaeMin <- NA	
	# maskPoly$elevQuantWrtPaeMin[maskPoly$value == 1] <- 'Lowest'
	# maskPoly$elevQuantWrtPaeMin[maskPoly$value == 2] <- 'Low'
	# maskPoly$elevQuantWrtPaeMin[maskPoly$value == 3] <- 'Middle'
	# maskPoly$elevQuantWrtPaeMin[maskPoly$value == 4] <- 'High'
	# maskPoly$elevQuantWrtPaeMin[maskPoly$value == 5] <- 'Highest'

	# maskPoly <- sp::spTransform(maskPoly, CRS(projection(demPrism)))
	# shapefile(maskPoly, paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!ElevationQuintiles_ver2/elevWrtMinPmeQuintiles'), overwrite=TRUE)

	# ### plot
	# pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!ElevationQuintiles_ver2/!!Division of Presence Records by Revised Elevation Quantiles WRT Min PME.pdf'), height=8.5, width=11)
		
		# # plot map
		# plot(maskPoly, col=alpha(maskPoly$value, 0.3), border=NA, main='Elevation Quantiles WRT Pika Minimum Elevation')
		# for (countUnit in seq_along(units)) points(pres[pres$elevQuantWrtPaeMin == units[countUnit], c('longWgs84', 'latWgs84')], pch=countUnit, col=countUnit)
		
		# legend('bottomleft', legend=units, col=seq_along(units), pch=seq_along(units))
	
		# for (countUnit in seq_along(units)) {
			
			# plot(maskPoly, border=NA, main=paste0(units[countUnit], '\n', sum(pres$elevQuantWrtPaeMin == units[countUnit]), 'presences'), col=alpha('black', 0.1))
			
			# plot(maskPoly[as.data.frame(maskPoly)$elevQuantWrtPaeMin == units[countUnit], ], col=alpha(countUnit, 0.5), border=NA, add=TRUE)
			
			# points(pres[pres$elevQuantWrtPaeMin == units[countUnit], c('longWgs84', 'latWgs84')], pch=16, cex=0.3)
			
		# }
	
	# dev.off()

say('#################################################################################################')
say('### RE-assign scheme division names to presences: physiography (Fennemann modified provinces) ###')
say('#################################################################################################')

	say('Using physiographic provinces modified from Fenneman. Shapefile was created 2017-09-06.')

	pres <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/12 Ochotona princeps - Usable - Presences 1990-2015 - Added Updated PME Quantiles.rds'))

	pres$physioFennemanL2 <- pres$origPhysioFennemanL2 <- NULL
	
	presSp <- SpatialPoints(cbind(pres$longWgs84, pres$latWgs84), CRS(getCRS('wgs84')))
	
	out <- schemeInfo('physioFenneman', TRUE)
	schemeNice <- out$schemeNice
	divisionFieldPres <- out$divisionFieldPres
	divisionFieldPoly <- out$divisionFieldPoly
	divisionPoly <- out$divisionPoly
	rm(out); gc()
	divisionPolyDf <- as.data.frame(divisionPoly)
	divisionPoly <- sp::spTransform(divisionPoly, getCRS('wgs84', TRUE))

	pres$physioFenneman <- over(presSp, divisionPoly)$physio
	
	saveRDS(pres, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/13 Ochotona princeps - Usable - Presences 1990-2015 - Updated Fenneman Physiographic Provinces.rds'))
	
	
	
	### plot
	pdf(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!FennemansPhysiography/!!Division of Presence Records by Modified Fenneman Provinces.pdf'), height=8.5, width=11)
		
		units <- getUnits('physioFenneman', incAll=FALSE, pres=pres)
		
		# plot map
		plot(divisionPoly, col=alpha(unitMeta$color[match(divisionPoly$physio, unitMeta$unit[unitMeta$scheme=='physioFenneman'])], 0.3), main='Physiographic Provinces (Modified Fenneman)')
		col <- unitMeta$color[match(divisionPoly$physio, unitMeta$unit[unitMeta$scheme=='physioFenneman'])]
		for (countUnit in seq_along(divisionPoly$physio)) points(pres[pres$physioFenneman == divisionPoly$physio[countUnit], c('longWgs84', 'latWgs84')], pch=countUnit, col=col[countUnit])
		
	dev.off()

# say('#####################################################')
# say('### calculate similarity between division schemes ###')
# say('#####################################################')

# pres <- getPres()

# say('Similarity: Given division scheme A and B, draw a point x and note which unit of A (call it alpha) and B (call it beta) the point is in.  Then draw another point y at random.  Note if y is also in alpha and beta.  If so, increment a counter by 1.  Repeat a large number of times and report the total sum of increments divided by the total number of draws.')

# schemes <- c('ecoregionEpaLevel3Modified', 'ecoregionBaileyProvince', 'physioFennemanL2', 'physioHammond', 'climateGEnSZone', 'climateKoppen', 'elevZoneAboveMinPme')
	
# similarity <- matrix(0, nrow=7, ncol=7)
# diag(similarity) <- NA
# rownames(similarity) <- colnames(similarity) <- schemes

# count <- 1
# while (count <= 10000) {

	# x <- pres[sample(1:nrow(pres), 1), ]
	# y <- pres[sample(1:nrow(pres), 1), ]

	# # see if any region was assigned NA
	# wasAssignedAllRegions <- if (any(is.na(x[ , schemes])) | any(is.na(y[ , schemes]))) {
		# FALSE
	# } else {
		# TRUE
	# }
	
	# if (wasAssignedAllRegions) {

		# count <- count + 1
	
		# for (A in schemes) {

			# alphaX <- x[ , A]
			# alphaY <- y[ , A]
				
			# for (B in schemes) {

				# betaX <- x[ , B]
				# betaY <- y[ , B]
					
				# if (A != B) {
				
					# if (alphaX == alphaY & betaX == betaY) similarity[A, B] <- similarity[A, B] + 1
				
				# }
			
			# }
			
		# }
		
	# }
		
# }

# similarity <- round(similarity / 10000, 2)

# write.csv(similarity, paste0(workDir, 'Extents_Masks_Maps/EcoRegions/Similarity between Ecoregions Calculated Using Presences.csv'))

# say('########################################')
# say('### analyze sample intensity in time ###')
# say('########################################')

# say('Calculate best date at which 10-yr window covers as many 10-yr windows of presence sites as possible')

# pres <- getPres()

# date <- as.Date(paste(pres$obsYear, pres$obsMonth, pres$obsDayOfMonth, sep='-'))

# samples <- hist(date, breaks='days')

# sampleIntensity <- data.frame(sampleDate=sort(unique(date)), noPres=NA, intensity=NA)

# for (i in 1:nrow(sampleIntensity)) {
	
	# # sample dates in 10 yr window prior to this date
	# datesIn <- which(date > sampleIntensity$sampleDate[i] - (365 * 10) & date <= sampleIntensity$sampleDate[i])
	
	# sampleIntensity$noPres[i] <- length(datesIn)
	# sampleIntensity$intensity[i] <- sum(365 * 10 - (sampleIntensity$sampleDate[i] - date[datesIn])) / 365
	
# }

# plot(sampleIntensity$intensity, xlab=sampleIntensity$date, ylab='years of sample windows covered by this date', type='l')
# say('Date which maximizes coverage is ', sampleIntensity$sampleDate[which.max(sampleIntensity$intensity)])
	


say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=2)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!', post=2)

