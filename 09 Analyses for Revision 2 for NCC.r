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
### extract loadings for variables on PCA ###
### create base graphics for conceptual figure of climate coherency ###
### derived-variable models: null model (~Tukey) test for differences WITHIN schemes between PMEs ###
### derived-variable models: null model (~Tukey) test for differences BETWEEN schemes and extents ###

###########################################
### libraries, variables, and functions ###
###########################################

	source(paste0(drive, 'Ecology/Drive/Research/Iconic Species/pika_climateCoherency/!Omnibus Variables for Pika Non-Stationarity Analysis.r'))

	# schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')
	schemes <- c('cladeNonOverlap', 'physioFenneman')
	# schemes <- c('ecoregionEpa3Modified')
	# schemes <- c('elevQuantWrtPaeMin')
	# schemes <- c('physioFenneman')
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
						
						# # area of unit in scheme #1
						# unitAreaRast1 <- raster::area(mask, na.rm=TRUE)
						# unitArea1 <- cellStats(unitAreaRast1, 'sum')
						
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
	
	# ### calculate area-weighted probabilities
	# #########################################
	
	# areaWeightedSim <- data.frame()
	
	# # by SCHEME #1
	# for (scheme1 in schemes) {
	
		# # by SCHEME #2
		# for (scheme2 in schemes[!(schemes %in% scheme1)]) {
		
			# # by PME
			# for (pme1 in pmes) {

				# # by PME of scheme #2
				# for (pme2 in pmes) {
				
					# indices <- which(similarity$scheme1 == scheme1 & similarity$pme1 == pme1 & similarity$scheme2 == scheme2 & similarity$pme2 == pme2)
					# areaScheme1 <- similarity$areaUnitScheme1[indices]
					# simScheme1 <- similarity$similarity[indices]
					
					# thisAreaWeightedSim <- sum(areaScheme1 * simScheme1) / sum(areaScheme1)
					
					# areaWeightedSim <- rbind(
						# areaWeightedSim,
						# data.frame(
							# scheme1 = scheme1,
							# pme1 = pme1,
							# scheme2 = scheme2,
							# pme2 = pme2,
							# areaWeightedSim = thisAreaWeightedSim
						# )
					# )
					
				# } # next PME for SCHEME #2
				
			# } # next PME in SCHEME #1
			
		# } # next SCHEME #2
		
	# } # next SCHEME #1
	
	# write.csv(areaWeightedSim, paste0(workDir, '/Analysis - Non-stationarity/Similarity Test between Divisions and PMEs - Area-Weighted.csv'), row.names=FALSE)
	
	# ### average area-weighted probabilities
	# #######################################
	
	# # similarities are not necessarily symmetrical, so calculating average of sim(scheme A pme a --> scheme B pme b) and sim(scheme B pme b --> scheme A pme a)
	
	# averagedAreaSim <- data.frame()
	
	# # by SCHEME #1
	# for (scheme1 in schemes[1:(length(schemes) - 1)]) {
	
		# # by SCHEME #2
		# for (scheme2 in schemes[(1 + which(scheme1 == schemes)):length(schemes)]) {
		
			# # by PME
			# for (pme1 in pmes) {

				# # by PME of scheme #2
				# for (pme2 in pmes) {
				
					# areaWeightedSimOneWayIndex <- which(areaWeightedSim$scheme1 == scheme1 & areaWeightedSim$pme1 == pme1 & areaWeightedSim$scheme2 == scheme2 &areaWeightedSim$pme2 == pme2)
					# areaWeightedSimOtherWayIndex <- which(areaWeightedSim$scheme1 == scheme2 & areaWeightedSim$pme1 == pme2 & areaWeightedSim$scheme2 == scheme1 &areaWeightedSim$pme2 == pme1)
					
					# simOneWay <- areaWeightedSim$areaWeightedSim[areaWeightedSimOneWayIndex]
					# simOtherWay <- areaWeightedSim$areaWeightedSim[areaWeightedSimOtherWayIndex]
					
					# meanAreaSim <- mean(c(simOneWay, simOtherWay))
					
					# averagedAreaSim <- rbind(
						# averagedAreaSim,
						# data.frame(
							# scheme1=scheme1,
							# pme1=pme1,
							# scheme2=scheme2,
							# pme2=pme2,
							# meanAreaSim=meanAreaSim
						# )
					# )
					
				# }
				
			# }
			
		# }
		
	# }
	
	# write.csv(averagedAreaSim, paste0(workDir, '/Analysis - Non-stationarity/Similarity Test between Divisions and PMEs - Average of Area-Weighted.csv'), row.names=FALSE)
	
# say('###########################################################################################')
# say('### correlations between variables calculated using temporal windows of different sizes ###')
# say('###########################################################################################')
	
	# op <- readRDS(paste0(workDir, '/Species Records - Pika/!Collated Data 2016-06-30 1256/03 Ochotona princeps - Usable - Presences 1990-2015 - PRISM & DayMet Climate Data Extracted.rds'))
	
	# # calculate derived variables for all intervals <= 10 years
	# derived <- list()
	# for (interval in 1:10) {
	
		# say('interval ', interval)
	
		# vars <- calcDerivedVars(op, window=interval)
		# vars <- vars[ , predictorsToUse]
		# derived[[interval]] <- vars
	
	# }
	
	# # calculate correlations
	# cors <- data.frame()
	# for (interval in 9:1) {
	
		# theseCors <- cor(derived[[10]], derived[[interval]], use='pairwise.complete.obs')
		# theseCors <- diag(theseCors)
		# cors <- rbind(cors, theseCors)
		
	# }
	
	# names(cors) <- predictorsToUse
	
	# cors <- t(cors)
	# rownames(cors) <- predictorsToUse
	
	# colnames(cors) <- paste0('interval', 9:1)
	# write.csv(cors, paste0(workDir, './Analysis - Non-stationarity/Correlations between Predictors Calculated Using Different Temporal Windows.csv'))

# say('#############################################')
# say('### extract loadings for variables on PCA ###')
# say('#############################################')

	# pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))
	
	# write.csv(pca$loadings, paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07 Loadings.csv'))
	
# say('#######################################################################')
# say('### create base graphics for conceptual figure of climate coherency ###')
# say('#######################################################################')

	# out <- schemeInfo('ecoregionEpa3Modified', poly=TRUE)
	# scheme1 <- out$divisionPoly
	# scheme1_prism <- sp::spTransform(scheme1, getCRS('prism', TRUE))
	
	# # basemap
	# elev <- raster('D:/Ecology/Climate/PRISM/30 arcsec/elevation.tif')
	# elev <- crop(elev, scheme1_prism)
	# slope <- terrain(elev, 'slope')
	# aspect <- terrain(elev, 'aspect')
	# hs <- hillShade(slope, aspect, direction=135)
	
	# grays <- paste0('gray', 100:1)
	# png(paste0(workDir, '/The Writing Process/Non-Stationarity in Range Limits/Western US.png'), width=2400, height=2400, res=1200)
	# par(oma=rep(0.01, 4), mar=rep(0, 4))
	# plot(hs, col=grays, maxpixels=ncell(hs))
	# dev.off()
	
# # say('###########################################################################################################')
# # say('### derived-variable models: null model (~Tukey) test for differences WITHIN schemes between PMEs ###')
# # say('###########################################################################################################')	

	# # # number of randomization iterations
	# # iters <- 1000
	
	# # # data frame to remember results
	# # test <- data.frame()

	# # # by SCHEME
	# # for (scheme in schemes) {

		# # out <- schemeInfo(scheme)
		# # schemeNice <- out$schemeNice
		# # schemeShort <- out$schemeShort
		# # divisionFieldPres <- out$divisionFieldPres
		# # rm(out); gc()

		# # thisUnitMeta <- unitMeta[unitMeta$scheme == scheme, ]
		# # units <- thisUnitMeta$unit
		# # units <- units[-which(units %in% 'all')]

		# # say('SCHEME ', schemeShort)
				
		# # ### OBSERVED WITHIN vs AMONG
		# # ############################

		# # load(paste0(workDir, '/ENMs - Derived/Heterogeneity in Response Curves ', scheme, ' pmeNone.RData'))
		# # masterHetero <- hetero
		# # load(paste0(workDir, '/ENMs - Derived/Heterogeneity in Response Curves ', scheme, ' pmeMin.RData'))
		# # masterHetero <- rbind(masterHetero, hetero)
				
		# # for (thisPred in predictorsToUse) {
			
			# # obsDiff <- numeric()
			# # withinsAmongs <- numeric()
			
			# # # by FROM UNIT
			# # for (fromUnit in units) {
			
				# # withins1 <- masterHetero$heteroWithin[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeNone' & masterHetero$predictor == thisPred]
				# # withins2 <- masterHetero$heteroWithin[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeMin' & masterHetero$predictor == thisPred]
				# # withinsScales1 <- masterHetero$changeUnitAllSites[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeNone' & masterHetero$predictor == thisPred]
				# # withinsScales2 <- masterHetero$changeUnitAllSites[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeMin' & masterHetero$predictor == thisPred]
				
				# # amongs1 <- masterHetero$heteroAmong[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeNone' & masterHetero$predictor == thisPred]
				# # amongs2 <- masterHetero$heteroAmong[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeMin' & masterHetero$predictor == thisPred]
				# # amongsScales1 <- masterHetero$changeCompositeAllSites[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeNone' & masterHetero$predictor == thisPred]
				# # amongsScales2 <- masterHetero$changeCompositeAllSites[masterHetero$scheme == scheme & masterHetero$fromUnit == fromUnit & masterHetero$pme == 'pmeMin' & masterHetero$predictor == thisPred]
				
				# # withinsScaled1 <- withins1 * withinsScales1
				# # withinsScaled2 <- withins2 * withinsScales2
				
				# # amongsScaled1 <- amongs1 * amongsScales1
				# # amongsScaled2 <- amongs2 * amongsScales2
				
				# # responses1 <- amongsScaled1 - withinsScaled1
				# # responses2 <- amongsScaled2 - withinsScaled2
				
				# # response1 <- mean(responses1, na.rm=TRUE)
				# # response2 <- mean(responses2, na.rm=TRUE)
				
				# # # observed among - within
				# # obsDiff <- c(obsDiff, abs(response1 - response2))
				
				# # # randomized among - within
				# # withinsAmongs <- c(withinsAmongs, responses1, responses2)
				
			# # } # next from unit

			# # randDiff <- rep(NA, iters)
			
			# # # iteratively randomly draw scaled (within - among) values and calculate null difference
			# # for (iter in 1:iters) {
			
				# # rands <- sample(seq_along(withinsAmongs), 2 * kFolds, replace=TRUE)
				# # withinAmongsRand1 <- withinsAmongs[rands[1:kFolds]]
				# # withinAmongsRand2 <- withinsAmongs[rands[(kFolds + 1):(2 * kFolds)]]
				
				# # responseRand1 <- mean(withinAmongsRand1, na.rm=TRUE)
				# # responseRand2 <- mean(withinAmongsRand2, na.rm=TRUE)
				
				# # randDiff[iter] <- abs(responseRand1 - responseRand2)
				
			# # }
				
			# # obsDiff <- mean(obsDiff)
			# # pValue <- sum(randDiff >= obsDiff) / length(randDiff)
				
			# # # remember
			# # test <- rbind(
				# # test,
				# # data.frame(
					# # scheme=scheme,
					# # predictor=thisPred,
					# # obsDiff=obsDiff,
					# # pValue=pValue
				# # )
			# # )
			
		# # } # next predictor
			
	# # } # next scheme
	
	# # write.csv(test, file=paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves - P Values for Testing Difference between PMEs in Same Scheme.Rdata'), row.names=FALSE)

# say('#####################################################################################################')
# say('### derived-variable models: null model (~Tukey) test for differences WITHIN schemes between PMEs ###')
# say('#####################################################################################################')	

	# # number of randomization iterations
	# iters <- 1000
	
	# # type of prediction
	# predType <- 'Marginal'
	
	# # quantiles across which to calculate response (response in tails sometimes depends only on a few presences and so sometimes seems erratic)
	# across <- c(0.025, 0.975)

	# # number of environmental values across occupied environmental breadth to calculate comparison
	# n <- 100

	# say('This permutation procedure tests the null hypothesis that the difference between the among - within differences between PMEs of a given scheme is = 0 (ie it compares, for example, clades w/ broad background and clades w/ narrow backgrounds) using the "Derived Variables" LARS models. It is based on the assumption that the predictions from the k-fold models are drawn from the same distribution, the all-sites unit models from the same distribution, and the composite all-sites models from the same distribution (ie, within a combination of scheme and unit, it controls for differences between unit all-sites models and composite all-sites models). Heterogeneity is calculated as described in previous steps. Predictions for a particular combination of models are truncated by the environmental width of the narrrower k-fold model. Thus the predictions cannot be simply calculated and swapped randomly between within/among components. The first part of this script simply produces a bank of randomized within- and among-unit heterogeneity values for use in a randomization test.', breaks=120, post=2)

	# # by SCHEME
	# for (scheme in schemes) {

		# out <- schemeInfo(scheme)
		# schemeNice <- out$schemeNice
		# schemeShort <- out$schemeShort
		# divisionFieldPres <- out$divisionFieldPres
		# rm(out); gc()

		# thisUnitMeta <- unitMeta[unitMeta$scheme == scheme, ]
		# units <- thisUnitMeta$unit
		# units <- units[-which(units %in% 'all')]

		# # define PMEs
		# pme1 <- 'pmeNone'
		# pme2 <- 'pmeMin'
		
		# pmeNice1 <- pmeNiceName(pme1)
		# pmeNice2 <- pmeNiceName(pme2)

		# # data frame to remember results
		# test <- data.frame()

		# # load COMPOSITE UNIT ALL-SITES MODEL: PME #1
		# load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/INCLUDING ALL - ', pmeNice1, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
		# allSitesCompositeModel1 <- model$model
		# allSitesCompositePres1 <- model$trainingPresences
		# rm(model); gc()

		# # load COMPOSITE UNIT ALL-SITES MODEL: PME #2
		# load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/INCLUDING ALL - ', pmeNice2, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
		# allSitesCompositeModel2 <- model$model
		# allSitesCompositePres2 <- model$trainingPresences
		# rm(model); gc()

		# ### OBSERVED WITHIN vs AMONG
		# ############################

		# # by FROM UNIT
		# for (fromUnit in units) {
		
			# say('SCHEME ', schemeShort, ' | UNIT ', fromUnit, post=0)
			
			# # load ALL-SITES UNIT model for PME #1
			# load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/INCLUDING ', toupper(fromUnit), ' - ', pmeNice1, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
			# allSitesUnitModel1 <- model$model
			# allSitesUnitPres1 <- model$trainingPresences
			# rm(model); gc()

			# # load ALL-SITES UNIT model for PME #2
			# load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/INCLUDING ', toupper(fromUnit), ' - ', pmeNice2, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
			# allSitesUnitModel2 <- model$model
			# allSitesUnitPres2 <- model$trainingPresences
			# rm(model); gc()

			# # lists to hold all 8 k-fold models and k-fold presence environmental data
			# kModels1 <- kPres1 <- list()
			# kModels2 <- kPres2 <- list()
			
			# # load K-FOLD UNIT models for PME#1
			# for (k in 1:kFolds) {

				# modelFileName <- paste0(workDir, 'ENMs - Derived/', schemeShort, '/INCLUDING ', toupper(fromUnit), ' - ', pmeNice1, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata')

				# load(modelFileName)
				# kModels1[[k]] <- model$model
				# kPres1[[k]] <- model$trainingPresences
				# rm(model); gc()
				
				# modelFileName <- paste0(workDir, 'ENMs - Derived/', schemeShort, '/INCLUDING ', toupper(fromUnit), ' - ', pmeNice2, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata')

				# load(modelFileName)
				# kModels2[[k]] <- model$model
				# kPres2[[k]] <- model$trainingPresences
				# rm(model); gc()
				
			# }
			
			# # list of all models available for this scheme/PME/unit... will draw from these at random
			# allKModels <- c(kModels1, kModels2)
			# allAllSitesUnitModels <- c(list(allSitesUnitModel1), list(allSitesUnitModel2))
			# allAllSitesCompositeModels <- c(list(allSitesCompositeModel1), list(allSitesCompositeModel2))
			# allPresCollated <- c(kPres1, kPres2)

			# rm(kModels1, kModels2, kPres1, kPres2); gc()
			
			# # by PREDICTOR
			# for (thisPred in predictorsToUse) {

				# say(thisPred, post=0)

				# # by ITERATION
				# for (iter in 1:iters) {

					# # randomization of models
					# index <- sample(seq_along(allKModels), 2)
					# kModelRand1 <- allKModels[[index[1]]]
					# kModelRand2 <- allKModels[[index[2]]]
					# kPresRand1 <- allPresCollated[[index[1]]]
					# kPresRand2 <- allPresCollated[[index[2]]]

					# # keep unit all-sites and composite all-sites models together
					# index <- sample(seq_along(allAllSitesUnitModels), 2)
					# allSitesUnitModelRand1 <- allAllSitesUnitModels[[index[1]]]
					# allSitesUnitModelRand2 <- allAllSitesUnitModels[[index[2]]]
					# allSitesCompositeModelRand1 <- allAllSitesCompositeModels[[index[1]]]
					# allSitesCompositeModelRand2 <- allAllSitesCompositeModels[[index[2]]]
				
					# # get environment to which to predict
					# env1 <- kPresRand1
					# env2 <- kPresRand2

					# env1 <- env1[order(env1[ , thisPred]), ]
					# envRange1 <- quantile(env1[ , thisPred], across)
					# low1 <- envRange1[1]
					# high1 <- envRange1[2]

					# env2 <- env2[order(env2[ , thisPred]), ]
					# envRange2 <- quantile(env2[ , thisPred], across)
					# low2 <- envRange2[1]
					# high2 <- envRange2[2]

					# low <- max(low1, low2)
					# high <- min(high1, high2)
					
					# # values of the target variable against which to make comparisons
					# compareValues <- seq(low, high, length.out=n)

					# # within occupied environmental range find sites that most closely match the values at which to make comparisons
					# matchingIndex <- rep(NA, n)
					# for (i in 1:n) matchingIndex[i] <- which.min.simple(abs(compareValues[i] - env1[ , thisPred]), tie_value='random')
					# matchingIndex <- unique(matchingIndex)
					# nEffective1 <- length(matchingIndex)
					# env1 <- env1[matchingIndex, predictorsToUse]

					# matchingIndex <- rep(NA, n)
					# for (i in 1:n) matchingIndex[i] <- which.min.simple(abs(compareValues[i] - env2[ , thisPred]), tie_value='random')
					# matchingIndex <- unique(matchingIndex)
					# nEffective2 <- length(matchingIndex)
					# env2 <- env2[matchingIndex, predictorsToUse]

					# # predict
					# preds <- if (predType == 'Full Model') { NULL } else { thisPred }
					# predKFold1 <- try(predictLars(object=kModelRand1, newdata=env1, type='response', preds=preds))
					# predKFold2 <- try(predictLars(object=kModelRand2, newdata=env2, type='response', preds=preds))
					# predAllSitesUnit1 <- try(predictLars(object=allSitesUnitModelRand1, newdata=env1, type='response', preds=preds))
					# predAllSitesUnit2 <- try(predictLars(object=allSitesUnitModelRand2, newdata=env2, type='response', preds=preds))
					# predAllSitesComposite1 <- try(predictLars(object=allSitesCompositeModelRand1, newdata=env1, type='response', preds=preds))
					# predAllSitesComposite2 <- try(predictLars(object=allSitesCompositeModelRand2, newdata=env2, type='response', preds=preds))

					# # calculate heterogeneity of response WITHIN unit
					# if (!is.null(predAllSitesUnit1) & !is.null(predKFold1) & length(predAllSitesUnit1) > 0 & length(predKFold1) > 0 & class(predAllSitesUnit1) != 'try-error' & class(predKFold1) != 'try-error') {
						# heteroWithin1 <- mean((predKFold1 - predAllSitesUnit1)^2, na.rm=TRUE)
						# changeUnitAllSites1 <- diff(range(predAllSitesUnit1))
					# } else {
						# heteroWithin1 <- 0
						# changeUnitAllSites1 <- 0
					# }

					# if (!is.null(predAllSitesUnit2) & !is.null(predKFold2) & length(predAllSitesUnit2) > 0 & length(predKFold2) > 0 & class(predAllSitesUnit2) != 'try-error' & class(predKFold2) != 'try-error') {
						# heteroWithin2 <- mean((predKFold2 - predAllSitesUnit2)^2, na.rm=TRUE)
						# changeUnitAllSites2 <- diff(range(predAllSitesUnit2))
					# } else {
						# heteroWithin2 <- 0
						# changeUnitAllSites2 <- 0
					# }

					# # calculate heterogeneity of response BETWEEN focal unit all-sites model and composite unit all-sites model
					# if (!is.null(predAllSitesComposite1) & !is.null(predAllSitesUnit1) & length(predAllSitesComposite1) > 0 & length(predAllSitesUnit1) > 0 & class(predAllSitesComposite1) != 'try-error' & class(predAllSitesUnit1) != 'try-error') {
						# heteroAmong1 <- mean((predAllSitesComposite1 - predAllSitesUnit1)^2, na.rm=TRUE)
						# changeCompositeAllSites1 <- diff(range(predAllSitesComposite1))
					# } else {
						# heteroAmong1 <- 0
						# changeCompositeAllSites1 <- 0
					# }

					# if (!is.null(predAllSitesComposite2) & !is.null(predAllSitesUnit2) & length(predAllSitesComposite2) > 0 & length(predAllSitesUnit2) > 0 & class(predAllSitesComposite2) != 'try-error' & class(predAllSitesUnit2) != 'try-error') {
						# heteroAmong2 <- mean((predAllSitesComposite2 - predAllSitesUnit2)^2, na.rm=TRUE)
						# changeCompositeAllSites2 <- diff(range(predAllSitesComposite2))
					# } else {
						# heteroAmong2 <- 0
						# changeCompositeAllSites2 <- 0
					# }

					# # remember
					# test <- rbind(
						# test,
						# data.frame(
							# scheme=scheme,
							# pme1=pme1,
							# pme2=pme2,
							# fromUnit=fromUnit,
							# predType=tolower(predType),
							# envWidth='kfold',
							# across=paste(across, collapse=' '),
							# n=n,
							# nEffective1=nEffective1,
							# nEffective2=nEffective2,
							# iters=iters,
							# iter=iter,
							# predictor=thisPred,
							# heteroWithin1=heteroWithin1,
							# heteroWithin2=heteroWithin2,
							# heteroAmong1=heteroAmong1,
							# heteroAmong2=heteroAmong2,
							# changeUnitAllSites1=changeUnitAllSites1,
							# changeUnitAllSites2=changeUnitAllSites2,
							# changeCompositeAllSites1=changeCompositeAllSites1,
							# changeCompositeAllSites2=changeCompositeAllSites2
						# )
					# )
				
				# } # next iteration
		
			# } # next predictor
			
			# say('')
			
		# } # next from unit
			
		# save(test, file=paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves - Randomization Values for Testing Difference between PMEs in Same Scheme for ', schemeNice, '.Rdata'))
		# rm(test)
		
	# } # next scheme

say('#####################################################################################################')
say('### derived-variable models: null model (~Tukey) test for differences BETWEEN schemes and extents ###')
say('#####################################################################################################')	

	# number of randomization iterations
	iters <- 100
	
	# type of prediction
	predType <- 'Marginal'
	
	# quantiles across which to calculate response (response in tails sometimes depends only on a few presences and so sometimes seems erratic)
	across <- c(0.025, 0.975)

	# number of environmental values across occupied environmental breadth to calculate comparison
	n <- 100

	say('This permutation procedure tests the null hypothesis that the difference between the among - within differences between PMEs of a given scheme is = 0 (ie it compares, for example, clades w/ broad background and clades w/ narrow backgrounds) using the "Derived Variables" LARS models. It is based on the assumption that the predictions from the k-fold models are drawn from the same distribution, the all-sites unit models from the same distribution, and the composite all-sites models from the same distribution (ie, within a combination of scheme and unit, it controls for differences between unit all-sites models and composite all-sites models). Heterogeneity is calculated as described in previous steps. Predictions for a particular combination of models are truncated by the environmental width of the narrrower k-fold model. Thus the predictions cannot be simply calculated and swapped randomly between within/among components. The first part of this script simply produces a bank of randomized within- and among-unit heterogeneity values for use in a randomization test.', breaks=120, post=2)

	# takes two data frames and returns 100 sites from each that have same outer
	selectSitesToCompare <- function(env1, env2, pred, across=c(0.025, 0.975), n=100) {
	
		# env1, env2		data frames with environmental data at predictors
		# pred				name of predictor by which to cull data frames
		# across			two values, minimum and maximum quantile of "pred" to consider... excludes all sites outside this range
		# n					number of sites to select

		# get range of focal variable from each data set
		env1 <- env1[order(env1[ , pred]), ]
		envRange1 <- quantile(env1[ , pred], across)
		low1 <- envRange1[1]
		high1 <- envRange1[2]

		env2 <- env2[order(env2[ , pred]), ]
		envRange2 <- quantile(env2[ , pred], across)
		low2 <- envRange2[1]
		high2 <- envRange2[2]

		# get overall smallest range across data sets
		low <- max(low1, low2)
		high <- min(high1, high2)
		
		# values of the target variable against which to make comparisons
		compareValues <- seq(low, high, length.out=n)

		# within occupied environmental range find sites that most closely match the values at which to make comparisons
		matchingIndex <- rep(NA, n)
		for (i in 1:n) matchingIndex[i] <- which.min.simple(abs(compareValues[i] - env1[ , pred]), tie_value='random')
		matchingIndex <- unique(matchingIndex)
		env1 <- env1[matchingIndex, predictorsToUse]

		matchingIndex <- rep(NA, n)
		for (i in 1:n) matchingIndex[i] <- which.min.simple(abs(compareValues[i] - env2[ , pred]), tie_value='random')
		matchingIndex <- unique(matchingIndex)
		env2 <- env2[matchingIndex, predictorsToUse]
		
		list(env1, env2)
		
	}
	
	# by SCHEME #1
	for (countScheme1 in 1:(length(schemes) - 1)) {

		# scheme info
		scheme1 <- schemes[countScheme1]
	
		out <- schemeInfo(scheme1)
		schemeNice1 <- out$schemeNice
		schemeShort1 <- out$schemeShort
		divisionFieldPres1 <- out$divisionFieldPres
		rm(out)

		thisUnitMeta1 <- unitMeta[unitMeta$scheme == scheme1, ]
		units1 <- thisUnitMeta1$unit
		units1 <- units1[-which(units1 %in% 'all')]

		for (countScheme2 in (countScheme1 + 1):length(schemes)) {

			# scheme info
			scheme2 <- schemes[countScheme2]
			
			out <- schemeInfo(scheme2)
			schemeNice2 <- out$schemeNice
			schemeShort2 <- out$schemeShort
			divisionFieldPres2 <- out$divisionFieldPres
			rm(out)

			thisUnitMeta2 <- unitMeta[unitMeta$scheme == scheme2, ]
			units2 <- thisUnitMeta2$unit
			units2 <- units2[-which(units2 %in% 'all')]

			# by PME SCHEME #1
			for (pme1 in pmes) {
		
				pmeNice1 <- pmeNiceName(pme1)
					
				# observed for SCHEME 1
				load(paste0(workDir, '/ENMs - Derived/Heterogeneity in Response Curves ', scheme1, ' ', pme1, '.RData'))
				hetero1 <- hetero
				
				# load COMPOSITE UNIT ALL-SITES MODEL: PME #1
				load(paste0(workDir, 'ENMs - Derived/', schemeShort1, '/INCLUDING ALL - ', pmeNice1, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
				allSitesCompositeModel1 <- model$model
				allSitesCompositePres1 <- model$trainingPresences
				rm(model); gc()

				# by PME SCHEME #2
				for (pme2 in pmes) {

					# data frame to remember results
					randomized <- data.frame()
				
					pmeNice2 <- pmeNiceName(pme2)

					say(schemeNice1, ' ', pmeNice1, ' PME vs ', schemeNice2, ' ', pmeNice2, ' PME:', post=0)
					
					# observed for SCHEME 2
					load(paste0(workDir, '/ENMs - Derived/Heterogeneity in Response Curves ', scheme2, ' ', pme2, '.RData'))
					hetero2 <- hetero

					# load COMPOSITE UNIT ALL-SITES MODEL: PME #2
					load(paste0(workDir, 'ENMs - Derived/', schemeShort2, '/INCLUDING ALL - ', pmeNice2, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
					allSitesCompositeModel2 <- model$model
					allSitesCompositePres2 <- model$trainingPresences
					rm(model); gc()

					# by UNIT in SCHEME #1
					for (unit1 in units1) {

						# SCHEME #1 UNIT ALL-SITES model
						load(paste0(workDir, 'ENMs - Derived/', schemeShort1, '/INCLUDING ', unit1, ' - ', pmeNice1, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
						allSitesUnitModel1 <- model$model
						allSitesUnitPres1 <- model$trainingPresences
						rm(model); gc()
					
						# load k-fold models for SCHEME #1
						kFoldModels1 <- kFoldPres1 <- list()
						for (k in 1:kFolds) {
						
							kFoldModelFile <- paste0(workDir, 'ENMs - Derived/', schemeShort1, '/INCLUDING ', unit1, ' - ', pmeNice1, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata')
							
							if (file.exists(kFoldModelFile)) {
							
								load(kFoldModelFile)
								kFoldModels1[[k]] <- model$model
								kFoldPres1[[k]] <- model$trainingPresences
								rm(model); gc()

							}
						
						}
					
						# by UNIT in SCHEME #2
						for (unit2 in units2) {
					
							# SCHEME #1 UNIT ALL-SITES model
							load(paste0(workDir, 'ENMs - Derived/', schemeShort2, '/INCLUDING ', unit2, ' - ', pmeNice2, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
							allSitesUnitModel2 <- model$model
							allSitesUnitPres2 <- model$trainingPresences
							rm(model); gc()
					
							# load k-fold models for SCHEME #2
							kFoldModels2 <- kFoldPres2 <- list()
							for (k in 1:kFolds) {
								
								kFoldModelFile <- paste0(workDir, 'ENMs - Derived/', schemeShort2, '/INCLUDING ', unit2, ' - ', pmeNice2, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata')
							
								if (file.exists(kFoldModelFile)) {
							
									load(kFoldModelFile)
									kFoldModels2[[k]] <- model$model
									kFoldPres2[[k]] <- model$trainingPresences
									rm(model); gc()
									
								}
							
							}

							### collate models
							allKFoldModels <- c(kFoldModels1, kFoldModels2)
							allUnitModels <- c(list(allSitesUnitModel1), list(allSitesUnitModel2))
							allCompositeModels <- c(list(allSitesCompositeModel1), list(allSitesCompositeModel2))
							
							allKPres <- c(kFoldPres1, kFoldPres2)
							allUnitPres <- c(list(allSitesUnitPres1), list(allSitesUnitPres2))
							allCompositePres <- c(list(allSitesCompositePres1), list(allSitesCompositePres2))
							
							### by PREDICTOR
							for (pred in predictorsToUse) {
							
								say(pred, post=0)
								
								# by ITERATION
								for (iter in 1:iters) {
									
									### randomly assign models
									rands <- sample(1:length(allKFoldModels), length(allKFoldModels))
									randKFoldModels <- allKFoldModels[rands]
									randKPres <- allKPres[rands]
									
									rands <- sample(1:2, 2)
									randUnitModels <- allUnitModels[rands]
									randAllCompositeModels <- allCompositeModels[rands]

									randAllUnitPres <- allUnitPres[rands]
									randAllCompositePres <- allCompositePres[rands]
									
									# by K-FOLD
									for (kFold in 1:kFolds) {
									
										### get environment to which to predict across unit models
										testSites <- selectSitesToCompare(env1=randKPres[[kFold]], env2=randKPres[[kFolds + kFold]], pred=pred, across=across, n=n)
										
										# predict using all-sites composite models
										randPredAllSitesComposite1 <- try(predictLars(object=randAllCompositeModels[[1]], newdata=testSites[[1]], type='response', preds=pred))
										randPredAllSitesComposite2 <- try(predictLars(object=randAllCompositeModels[[2]], newdata=testSites[[2]], type='response', preds=pred))
										
										# predict using randomized all-sites unit models
										randPredAllSitesUnit1 <- try(predictLars(object=randUnitModels[[1]], newdata=testSites[[1]], type='response', preds=pred))
										randPredAllSitesUnit2 <- try(predictLars(object=randUnitModels[[2]], newdata=testSites[[2]], type='response', preds=pred))
										
										# predict random k-fold models
										randPredKFold1 <- try(predictLars(object=randKFoldModels[[kFold]], newdata=testSites[[1]], type='response', preds=pred))
										randPredKFold2 <- try(predictLars(object=randKFoldModels[[kFolds + kFold]], newdata=testSites[[2]], type='response', preds=pred))
										
										# if predictions are successful
										validPreds <- (
											!is.null(randPredKFold1) &
											!is.null(randPredKFold2) &
											!is.null(randPredAllSitesUnit1) &
											!is.null(randPredAllSitesUnit2) &
											!is.null(randPredAllSitesComposite1) &
											!is.null(randPredAllSitesComposite2) &
											length(randPredKFold1) > 0 &
											length(randPredKFold2) > 0 &
											length(randPredAllSitesUnit1) > 0 &
											length(randPredAllSitesUnit2) > 0 &
											length(randPredAllSitesComposite1) > 0 &
											length(randPredAllSitesComposite2) > 0 &
											class(randPredKFold1) != 'try-error' &
											class(randPredKFold2) != 'try-error' &
											class(randPredAllSitesUnit1) != 'try-error' &
											class(randPredAllSitesUnit2) != 'try-error' &
											class(randPredAllSitesComposite1) != 'try-error' &
											class(randPredAllSitesComposite2) != 'try-error'
										)
										
										if (validPreds) {
											
											# calculate "raw" randomized among-unit heterogeneity
											randHeteroAmong1 <- mean((randPredAllSitesComposite1 - randPredAllSitesUnit1)^2, na.rm=TRUE)
											randHeteroAmong2 <- mean((randPredAllSitesComposite2 - randPredAllSitesUnit2)^2, na.rm=TRUE)
											
											# scale for randomized among-unit heterogeneity
											randChangeCompositeAllSites1 <- diff(range(randPredAllSitesComposite1, na.rm=TRUE))
											randChangeCompositeAllSites2 <- diff(range(randPredAllSitesComposite2, na.rm=TRUE))

											# "raw" randomized within-unit heterogeneity
											randHeteroWithin1 <- mean((randPredAllSitesUnit1 - randPredKFold1)^2, na.rm=TRUE)
											randHeteroWithin2 <- mean((randPredAllSitesUnit2 - randPredKFold2)^2, na.rm=TRUE)
											
											# scale for randomized within-unit heterogeneity
											randChangeUnitAllSites1 <- diff(range(randPredAllSitesUnit1, na.rm=TRUE))
											randChangeUnitAllSites2 <- diff(range(randPredAllSitesUnit2, na.rm=TRUE))
										
											# remember
											randomized <- rbind(
												randomized,
												data.frame(
													scheme1 = scheme1,
													pme1 = pme1,
													unit1 = unit1,
													scheme2 = scheme2,
													pme2 = pme2,
													unit2 = unit2,
													predType = predType,
													envWidth = 'kfold',
													n = n,
													k = kFold,
													iters = iters,
													iter = iter,
													predictor = pred,
													randHeteroWithin1 = randHeteroWithin1,
													randHeteroWithin2 = randHeteroWithin2,
													randHeteroAmong1 = randHeteroAmong1,
													randHeteroAmong2 = randHeteroAmong2,
													randChangeUnitAllSites1 = randChangeUnitAllSites1,
													randChangeUnitAllSites2 = randChangeUnitAllSites2,
													randChangeCompositeAllSites1 = randChangeCompositeAllSites1,
													randChangeCompositeAllSites2 = randChangeCompositeAllSites2
												)
											)
													
										} # if predictions from random k-fold models and random unit models are valid
										
										rm(randPredKFold1, randPredKFold2, randPredAllSitesUnit1, randPredAllSitesUnit2, randPredAllSitesComposite1, randPredAllSitesComposite2)
													
									} # next k-fold

								} # next iteration
								
							} # next predictor
							
						} # next UNIT #2
					
						rm(kFoldModels1, kFoldPres1); gc()
					
					} # next UNIT #1
					
					rm(allSitesCompositeModel2, allSitesCompositePres2); gc()
					say('')
				
					fileName <- paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves - Randomized Values for Testing Difference between SCHEMES and PMEs for ', schemeNice1, ' ', pmeNice1, ' PME vs ', schemeNice2, ' ', pmeNice2, ' PME.Rdata')
					save(randomized, file=fileName)
					rm(randomized)
		
				} # next PME #2
				
				rm(allSitesCompositeModel1, allSitesCompositePres1); gc()
				
			} # next PME #1
			
		} # next SCHEME #2
		
	} # next SCHEME #1

say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
