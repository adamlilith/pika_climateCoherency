### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2016-11

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/05 ENM Analysis - Multivariate - PCs.r')
# source('E:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/05 ENM Analysis - Multivariate - PCs.r')

rm(list=ls())

	drive <- 'C:/'
	# drive <- 'D:/'
	# drive <- 'E:/'

	# cores <- 4 # used for evaluation of ensemble

	# do <- c('collate', 'map folds', 'pca', 'train')
	# do <- c('collate')
	# do <- c('map folds')
	# do <- c('pca')
	# do <- c('train')
	# do <- c('flag missing')
	# do <- c('eval')
	# do <- c('eval ensemble')
	# do <- c('recalculate CBI for ensemble')
	# do <- c('overlap')
	# do <- c('map')
	# do <- c('train', 'eval')
	# do <- 'statistically adjust performance of ENSEMBLES to control for nuisance parameters'
	# do <- 'ensemble map (with statistically-adjusted performance?)'
	# do <- c('boxplot within AND among')
	# do <- 'statistical analysis of difference between (within - among) vs 0'
	# do <- 'statistical analysis of difference between (within - among) between PMEs of the same scheme'
	# do <- 'statistical analysis of difference between (within - among) between schemes/PMEs of different schemes'
	# do <- 'boxplot within MINUS among'
	# do <- 'summarize model performance'

	# algos <- c('brt', 'gam', 'glm', 'maxent', 'rf')
	# algos <- c('brt')
	# algos <- c('gam')
	# algos <- c('glm')
	# algos <- c('brt')
	# algos <- c('maxent')
	# algos <- c('rf')
	algos <- c('ensemble')

	pmes <- c('pmeNone', 'pmeMin')
	# pmes <- 'pmeNone'
	# pmes <- 'pmeMin'

	# valances <- c('including', 'excluding')
	valances <- 'including'
	# valances <- 'excluding'

	schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')
	# forceUnit <- NULL

	# schemes <- 'cladeNonOverlap'
		# forceUnit <- NULL
		# forceUnit <- c('all', 'fenisex', 'princeps', 'saxatilis', 'schisticeps', 'uinta')
		# forceUnit <- c('saxatilis', 'uinta')
		# forceUnit <- 'all'
		# forceUnit <- 'fenisex'
		# forceUnit <- 'princeps'
		# forceUnit <- 'saxatilis'
		# forceUnit <- 'schisticeps'
		# forceUnit <- 'uinta'

	# schemes <- 'ecoregionEpa3Modified'
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

	## settings for "map"
	genre <- 'performance'

		testStat <- 'cbi'
		# testStat <- 'cbiPresBgWeight'
		# testStat <- 'sensTholdMaxSumSS'
		# testStat <- 'specTholdMaxSumSS'
		# testStat <- 'sensTholdMinDiffSS'
		# testStat <- 'specTholdMinDiffSS'

		# testStat <- 'tholdMaxSumSS'

	# genre <- 'overlap'

		# testStat <- 'schoenerDBg'
		# testStat <- 'schoenerDPres'
		# testStat <- 'schoenerDPresWeighted'

		# testStat <- 'rankCorBg'
		# testStat <- 'rankCorPres'
		# testStat <- 'rankCorPresWeighted'

		# testStat <- 'corBg'
		# testStat <- 'corPres'
		# testStat <- 'corPresWeighted'

### CONTENTS ###
### libraries, variables, and functions ###
### collate model training data for all division schemes ###
### make maps of g-folds ###

### train MULTIVARIATE ENMs using PCs ###
### flag missing model files ###
### evaluate MULTIVARIATE ENMs using PCs against self and other units ###
### evaluate MULTIVARIATE ENSEMBLE ENMs using PCs against self and other units ###
### update CBI of MULTIVARIATE ENMs using PCs against self and other units ###
### calculate niche overlap from ENMs based on PCs ###

### visualize multivariate PC ENM analysis - maps ###
### visualize multivariate PC ENM analysis - boxplots of within and among performance ###
### visualize multivariate PC ENM analysis - boxplots of within minus among performance ###
### statistical analysis of PC ENM - within MINUS among performance ###

###########################################
### libraries, variables, and functions ###
###########################################

source(paste0(drive, 'ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r'))

testStatNice <- statNice(testStat)


############################################################
### collate model training data for all division schemes ###
############################################################

if ('collate' %in% do) {

	say('collate model training data for all division schemes', level=1)

	# get presences
	pres <- getPres()
	pres$bgIndex <- NA

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=TRUE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		divisionPoly <- out$divisionPoly
		rm(out); gc()
		divisionPolyDf <- as.data.frame(divisionPoly)
		divisionPoly <- sp::spTransform(divisionPoly, getCRS('nad83', TRUE))

		# by PME
		for (pmeVariant in pmes) {

			pmeNice <- pmeNiceName(pmeVariant)

			# by VALANCE
			for (fromValance in valances) {

				# get names of focal units to include/exclude
				units <- if (is.null(forceUnit)) {
					getUnits(scheme=scheme, incAll=(fromValance == 'including'))
				} else {
					forceUnit
				}

				say('UNITS: ', paste(units, collapse=' | '), pre=1, post=1)

				# by UNIT
				for (thisUnit in units) {

					say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', thisUnit, breaks=NULL)

					# get this unit's polygon... defined as all polygons containing at least one presence assigned to this thisUnit
					thisUnitPres <- if (thisUnit=='all') {
						pres
					} else if (fromValance == 'including') {
						pres[which(pres[ , divisionFieldPres] == thisUnit), ]
					} else if (fromValance == 'excluding') {
						pres[which(pres[ , divisionFieldPres] != thisUnit), ]
					}

					# get background sites for this clade
					bgFileName <- paste0(workDir, '/Background Sites/Spatiotemporally Weighted - ', schemeNice, '/', pmeNice, ' PME 02 Derived Climate/BG Sites 02 Set 01 ', schemeNice, ' - ', toupper(fromValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME - Derived Variables.rds')

					bg <- readRDS(bgFileName)

					bg$obsDayOfMonth <- day(parse_date_time(paste0(bg$obsYear, '-', bg$obsDayOfYear), orders='yj')	)

					# assign units to BG sites
					bg$DUMMY <- NA
					names(bg)[ncol(bg)] <- divisionFieldPres
					if (fromValance == 'including' & thisUnit != 'all') {
						bg[ , divisionFieldPres] <- thisUnit
					} else {
						bgSp <- SpatialPoints(cbind(bg$longWgs84, bg$latWgs84), getCRS('nad83', TRUE))
						bg[ , divisionFieldPres] <- over(bgSp, divisionPoly)[ , divisionFieldPoly]
					}

					# collate data
					collateData(
						speciesList='Ochotona princeps',
						outputDir=paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(fromValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME'),
						allSpeciesData=thisUnitPres,
						allTrainAbsenceEnvData=bg,
						allTestAbsenceEnvData=bg,
						maxNumTrainingAbsences=10000,
						kFolds=kFolds,
						split=ifelse(thisUnit=='all', 'subsample', 'geographic'),
						userSplitField=NULL,
						trainingProportion=(kFolds - 1) / kFolds,
						minNumTrainSites=round((kFolds - 1) * minNumPres / kFolds),
						minNumTestSites=round(minNumPres / kFolds),
						splitAbs=TRUE,
						predictorsToUse=c(extendedPredictorsToUse, divisionFieldPres),
						longLatFields=c('longWgs84', 'latWgs84'),
						removeNaRows=TRUE,
						CRS=getCRS('nad83'),
						speciesField='species',
						speciesDataFileAppend=NULL,
						verbose=0
					)

				} # next unit

				### re-assign presence and BG records of "ALL" unit

				if (fromValance == 'including') {

					say('Assigning g-folds to "ALL" clade using test/training sites from each clade', pre=1)
					say('Each g-fold for the "ALL" clade is composed of the corresponding sets of presences from g-folds from each clade, and from background sites obtained for the "ALL" clade by re-assigned to each (new) g-fold based on proximity.', preBreak=1)

					### collate presences for "ALL" clade

					units <- getUnits(scheme=scheme, incAll=FALSE)
					thisUnit <- units[1]

					# get template speciesData object... use first unit as "template" then add subsequent units
					load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/INCLUDING ', toupper(thisUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'))

					newAllSpeciesData <- speciesData

					# k-fold holds unique number for each k-fold for ALL unit, j-fold holds unique number across all units' k-fold
					centroids <- data.frame()

					for (k in 1:kFolds) {

						testPres <- speciesData$testPresences$kFold[[k]][ , speciesData$longLatFields]
						testPres <- SpatialPoints(coords=testPres, proj4string=CRS(newAllSpeciesData$CRS))

						thisCenter <- coordinates(gCentroid(testPres))

						centroids <- rbind(centroids, data.frame(k=k, j=ifelse(k == 1, 1, max(centroids$j) + 1), x=thisCenter[1], y=thisCenter[2]))

					}

					for (thisUnit in units[2:length(units)]) {

						load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/INCLUDING ', toupper(thisUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'))

						newAllSpeciesData$allPresences <- rbind(newAllSpeciesData$allPresences, speciesData$allPresences)

						for (countK in 1:kFolds) {

							newAllSpeciesData$trainingPresences$kFold[[countK]] <- rbind(newAllSpeciesData$trainingPresences$kFold[[countK]], speciesData$trainingPresences$kFold[[countK]])

							newAllSpeciesData$testPresences$kFold[[countK]] <- rbind(newAllSpeciesData$testPresences$kFold[[countK]], speciesData$testPresences$kFold[[countK]])

							testSites <- speciesData$testPresences$kFold[[countK]][ , speciesData$longLatFields]
							testSites <- SpatialPoints(coords=testSites, proj4string=CRS(newAllSpeciesData$CRS))

							thisCenter <- coordinates(gCentroid(testSites))

							centroids <- rbind(centroids, data.frame(k=countK, j=max(centroids$j) + 1, x=thisCenter[1], y=thisCenter[2]))

						}

					}

					## collate background sites for "ALL" unit using g-folds... assigning each background site to g-fold of test presences with centroid closest to it

					# calculate distance from each background site to each centroid... for each BG site calculate distance to centroids then choose closest background sites for ALL unit

					# get ALL species data (original) to get background sites
					load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'))

					bg <- speciesData$allTrainingAbsences

					bg$kFold <- NA

					newAllSpeciesData$allTrainingAbsences <- newAllSpeciesData$allTestAbsences <- bg

					# find k-fold of presences each BG site is nearest
					for (i in 1:nrow(bg)) {

						dists <- distCosine(bg[i , newAllSpeciesData$longLatFields], centroids[ , c('x', 'y')])

						thisJ <- which.min(dists)
						thisK <- centroids$k[thisJ]

						bg$kFold[i] <- thisK

					}

					# assign k-fold training/test contrast sites
					for (countK in 1:kFolds) {
						newAllSpeciesData$trainingAbsences$kFold[[countK]] <- bg[bg$kFold != countK, ]
						newAllSpeciesData$testAbsences$kFold[[countK]] <- bg[bg$kFold == countK, ]
					}

					speciesData <- newAllSpeciesData

					save(speciesData, file=paste0(workDir, 'ENMs - PCs/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'), compress=TRUE)

				} # re-assign "ALL" unit presences/contrast sites

			} # next unit valance

		} # next PME

	} # next scheme

}

############################
### make maps of g-folds ###
############################

if ('map folds' %in% do) {

	say('make maps of g-folds', level=1)

	# gadm
	gadm <- readOGR(paste0('ecology/Political Geography/GADM/ver2', 'gadm2_northAmericaAndCentralAmerica_sansAlaska_dissolvedToLevel1'), verbose=FALSE)
	gadm <- spTransform(gadm, getCRS('climateNA'))

	pres <- getPres()

	for (scheme in schemes) {

		dirCreate(paste0(workDir, 'ENMs - PCs/!Maps - G-Folds'))

		# assign:
		# schemeNice text
		# name of field with scheme divisions in presences
		# name of field with division names in division polygon
		# load division scheme polygons
		out <- schemeInfo(scheme, poly=TRUE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		divisionPoly <- out$divisionPoly
		rm(out); gc()

		divisionPoly <- spTransform(divisionPoly, getCRS('climateNA'))

		# by PME
		for (pmeVariant in pmes) {

			pmeNice <- pmeNiceName(pmeVariant)

			# by "from" VALANCE
			for (fromValance in valances) {

				# get names of focal units to include/exclude
				units <- if (is.null(forceUnit)) {
					getUnits(scheme=scheme, incAll=(fromValance == 'including'))
				} else {
					forceUnit
				}

				say('UNITS: ', paste(units, collapse=' | '))

				# by UNIT
				for (thisUnit in units) {

					say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', thisUnit)

					png(paste0(workDir, 'ENMs - PCs/!Maps - G-Folds/', schemeNice, ' - ', toupper(fromValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME.png'), width=1000, height=1200)

						load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(fromValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME', '/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'))

						presSp <- SpatialPoints(cbind(speciesData$allPresences$longWgs84, speciesData$allPresences$latWgs84), proj4string=getCRS('wgs84', TRUE))
						presSp <- spTransform(presSp, getCRS('climateNA', TRUE))

						main <- paste0(toupper(schemeNice), ' scheme | ', toupper(pmeNice), ' PME | ', toupper(fromValance), ' ', toupper(thisUnit), ' unit')

						# get unit polygon(s)
						if (fromValance == 'including') {

							# which unit(s) has these points?
							polyContainsUnitPoints <- over(divisionPoly, presSp, returnList=TRUE)
							polyContainsUnitPoints <- unlist(lapply(polyContainsUnitPoints, length)) > 0

							thisUnitPoly <- divisionPoly[polyContainsUnitPoints, ]

						} else if (fromValance == 'excluding') {

							# get all but focal unit
							thisUnitPoly <- divisionPoly[which(as.data.frame(divisionPoly)[ , divisionFieldPoly] != thisUnit), ]

							# which selected units have any points?
							hasNonFocalPoints <- over(thisUnitPoly, presSp, returnList=TRUE)
							hasNonFocalPoints <- unlist(lapply(hasNonFocalPoints, length)) > 0

							# get units with points that are not in focal unit
							thisUnitPoly <- thisUnitPoly[hasNonFocalPoints, ]

						}

						plot(thisUnitPoly, border='white', main=main, cex.main=1.8, xpd=NA)
						plot(gadm, add=TRUE, border='gray', xpd=NA)
						plot(thisUnitPoly, add=TRUE, col=alpha('orange', 0.1), border='black', xpd=NA)

						# plot k-folds
						for (countK in 1:kFolds) {

							absSp <- SpatialPoints(speciesData$testAbsences$kFold[[countK]][ , c('longWgs84', 'latWgs84')], proj4string=getCRS('wgs84', TRUE))
							presSp <- SpatialPoints(speciesData$testPresences$kFold[[countK]][ , c('longWgs84', 'latWgs84')], proj4string=getCRS('wgs84', TRUE))
							absSp <- spTransform(absSp, getCRS('climateNA', TRUE))
							presSp <- spTransform(presSp, getCRS('climateNA', TRUE))

							points(absSp, col=countK, pch=16, cex=0.3)
							points(presSp, col=alpha('black', 0.5), bg=alpha(countK, 0.5), pch=21, cex=1.8)

						}

						legend('bottomright', inset=0.02, legend=paste('k =', 1:kFolds), col=1:kFolds, pch=16, cex=1.8, pt.cex=1.6)

					dev.off()

				} # next unit

			} # next PME

		} # next unit valance

	} # next division scheme

}

#########################################
### train MULTIVARIATE ENMs using PCs ###
#########################################

if ('train' %in% do) {

	say('train MULTIVARIATE ENMs using PCs', level=1)

	say('Using ', paste(toupper(algos), collapse=' '), '!!!')

	# get presences
	pres <- getPres()

	# by ALGORITHM
	for (thisAlgo in algos) {

		# by SCHEME
		for (scheme in schemes) {

			out <- schemeInfo(scheme)
			schemeNice <- out$schemeNice
			schemeShort <- out$schemeShort
			divisionFieldPres <- out$divisionFieldPres
			rm(out); gc()

			# by PME
			for (pmeVariant in pmes) {

				pmeNice <- pmeNiceName(pmeVariant)

				# load PCA
				pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

				# selecting PCs that cumulatively explain x% of variance
				pcNames <- paste0('PC', prefix(1:length(pca$sdev), 2))
				cumulExplain <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
				pcsUsed <- pcNames[cumulExplain <= pcaMinCumulExplained]
				if (length(pcsUsed) < length(pcNames)) pcsUsed <- c(pcsUsed, paste0('PC', prefix(length(pcsUsed) + 1, 2)))

				# by VALANCE
				for (fromValance in valances) {

					# get names of focal units to include/exclude
					units <- if (is.null(forceUnit)) {
						getUnits(scheme=scheme, incAll=(fromValance == 'including'))
					} else {
						forceUnit
					}

					say('UNITS: ', paste(units, collapse=' | '), pre=1)

					# by unit
					for (fromUnit in units) {

						say(Sys.time(), ' | ', toupper(thisAlgo), ' | SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, pre=1)

						say('  Training ENMs...')

						trainSdm(
							speciesList='Ochotona princeps',
							outputDir=paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME'),
							predictorsToUse=pcsUsed,
							modelType='multivariate',
							centeringStats=NULL,
							responseVar=NULL,
							responseTrans=NULL,
							responseTransInv=NULL,
							modelMethodsToUse=thisAlgo,
							equalPresAbsWeight=TRUE,
							kFolds=TRUE,
							wantAllSitesModels=TRUE,
							outNames='short',
							speciesDataFileAppend=' - PCA',
							modelParams=modelParams,
							tempDir=paste0(drive, 'ecology/!Scratch/_temp'),
							verbose=2
						)

					} # next from unit

				} # next valance

			} # next PME

		} # next scheme

	} # next algorithm

}

################################
### flag missing model files ###
################################

if ('flag missing' %in% do) {

	say('flagging missing model files', level=1)
	say('Flagging for "Multivariate - PCs" using these algorithms: ', paste(algos, collapse=' '))

	missingFiles <- character()

	# by ALGORITHM
	for (thisAlgo in algos) {

		# by SCHEME
		for (scheme in schemes) {

			out <- schemeInfo(scheme)
			schemeNice <- out$schemeNice
			rm(out); gc()

			# by PME
			for (pmeVariant in pmes) {

				pmeNice <- pmeNiceName(pmeVariant)

				# by VALANCE
				for (fromValance in valances) {

					# get names of focal units to include/exclude
					units <- getUnits(scheme=scheme, incAll=(fromValance == 'including'))

					# by unit
					for (fromUnit in units) {

						fileName <- paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate all sites.Rdata')

						if (!file.exists(fileName)) missingFiles <- c(missingFiles, fileName)

						for (k in 1:kFolds) {

							fileName <- paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(k, 2), '.Rdata')
							if (!file.exists(fileName)) missingFiles <- c(missingFiles, fileName)

						}

					} # next from unit

				} # next valance

			} # next PME

		} # next scheme

	} # next algorithm

	say('THERE ARE ', length(missingFiles), ' MISSING MODEL FILES:')
	print(missingFiles)

}

#########################################################################
### evaluate MULTIVARIATE ENMs using PCs against self and other units ###
#########################################################################

if ('eval' %in% do) {

	say('evaluate MULTIVARIATE ENMs using PCs against self and other units', level=1)

	say('Evaluate multivariate ENMs trained with PCs against same unit and other units.')
	say('Using ', paste(toupper(algos), collapse= ' '), '!!!')

	# PRISM DEM to use as mask for KDE
	prismDem <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m.tif'))
	prismMask <- prismDem * 0 + 1

	# get presences
	pres <- getPres()

	## PCA
	pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

	# selecting PCs that cumulatively explain x% of variance
	pcNames <- paste0('PC', prefix(1:length(pca$sdev), 2))
	cumulExplain <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
	pcsUsed <- pcNames[cumulExplain <= pcaMinCumulExplained]
	if (length(pcsUsed) < length(pcNames)) pcsUsed <- c(pcsUsed, paste0('PC', prefix(length(pcsUsed) + 1, 2)))

	# by ALGORITHM
	for (thisAlgo in algos) {

		# by SCHEME
		for (scheme in schemes) {

			out <- schemeInfo(scheme, poly=TRUE)
			schemeNice <- out$schemeNice
			schemeShort <- out$schemeShort
			divisionFieldPres <- out$divisionFieldPres
			divisionFieldPoly <- out$divisionFieldPoly
			divisionPoly <- out$divisionPoly
			rm(out); gc()

			divisionPoly <- sp::spTransform(divisionPoly, getCRS('nad83', TRUE))

			divisionPolyDf <- as.data.frame(divisionPoly)

			# crop PRISM mask to division polygons that contain presences
			thisPrismMask <- crop(prismMask, divisionPoly[divisionPolyDf[ , divisionFieldPoly] %in% unique(pres[ , divisionFieldPres]), ])

			# by PME
			for (pmeVariant in pmes) {

				if (exists('evalFrame')) rm(evalFrame)

				pmeNice <- pmeNiceName(pmeVariant)

				# by "from" VALANCE
				for (fromValance in valances) {

					# get names of focal units to include/exclude
					units <- getUnits(scheme=scheme, incAll=(fromValance == 'including'))

					say('UNITS: ', paste(units, collapse=' | '), pre=1)

					# by UNIT
					for (fromUnit in units) {

						say(Sys.time(), ' | ', toupper(thisAlgo), ' | SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, pre=1)

						say('  Evaluating against:')

						for (toValance in valances) {

							if (!(fromValance == 'including' & toValance == 'excluding' & fromUnit == 'all')) {

								# if "from" and "to" valances are both "include" then test against all other units. If not, then test only against own unit.
								toUnits <- if (fromValance == 'including' & toValance == 'including') {
									units
								} else {
									fromUnit
								}

								for (toUnit in toUnits) {

									say('    ', toupper(toValance), ' ', toUnit, post=0)

									## load "to" test data
									toDataFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(toValance), ' ', toupper(toUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
									load(toDataFile)

									toSpeciesData <- speciesData

									# for each k-fold of model evaluate against either same k-fold of test data (if model is for same unit/valance) or *all* k-folds of test data is model is not for unit being tested against
									for (kFrom in 1:kFolds) {

										say(kFrom, post=0)

										## load model
										fromFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(kFrom, 2), '.Rdata')
										load(fromFile)

										predictors <- model$predictorsToUse
										fromModel <- model$model

										kFoldsTo <- if (fromValance == 'including' & toValance == 'including' & fromUnit == toUnit) {
											kFrom
										} else if (fromValance == 'including' & toValance == 'including' & fromUnit != toUnit & fromUnit != 'all' & toUnit != 'all') {
											1:kFolds
										} else if (fromValance == 'including' & toValance == 'including' & fromUnit == 'all' & toUnit != 'all') {
											kFrom
										} else if (fromValance == 'including' & toValance == 'including' & fromUnit != 'all' & toUnit == 'all') {
											kFrom
										} else if (fromValance == 'excluding' & toValance == 'excluding' & fromUnit == toUnit) {
											kFrom
										} else if (fromValance == 'excluding' & toValance == 'excluding' & fromUnit != toUnit) {
											1:kFolds
										}

										# for each fold in test data
										for (kTo in kFoldsTo) {

											testPresTo <- toSpeciesData$testPresences$kFold[[kTo]]
											testBgTo <- toSpeciesData$testAbsences$kFold[[kTo]]

											testBgTo$kFold <- NULL

											## calculate geographic distance between training/test presences... using 10% quantile of minimum of pairwise distances (for each test presence) between training and test presences... similar to mobility-oriented parity (cf. Owens, H.L., Campbell, L.P., Dornak, L.L., Saupe, E.E., Barve, N., Soberón, Ingenloff, K., Lira-Noriega, A., Hensz, C., Myers, C.E., and Peterson, A.T.  2013.  Constraints on interpretation of ecological niche models by limited environmental ranges on calibration area.  Ecological Modeling 263:10-18.)

											dists <- rep(NA, nrow(testPresTo))
											for (i in 1:nrow(testPresTo)) {

												dists[i] <- min(distCosine(toSpeciesData$testPresences$kFold[[kTo]][i, c('longWgs84', 'latWgs84')], toSpeciesData$trainingPresences$kFold[[kTo]][ , c('longWgs84', 'latWgs84')]), na.rm=TRUE)

											}

											testTrainGeogDist <- quantile(dists, 0.10, na.rm=TRUE) / 1000

											## predict
											predPresFrom <- predictEnm(thisData=testPresTo, theModel=fromModel, predictors=model$predictorsToUse, predArgs=predArgs)
											predBgFrom <- predictEnm(thisData=testBgTo, theModel=fromModel, predictors=model$predictorsToUse, predArgs=predArgs)

											## evaluate
											cbi <- contBoyce(pres=predPresFrom, bg=predBgFrom, na.rm=TRUE, numBins=1001)

											thisEval <- evaluate(p=as.vector(na.omit(predPresFrom)), a=as.vector(na.omit(predBgFrom)), tr=seq(0, 1, by=0.0001))
											tholdMaxSumSS <- threshold(thisEval, stat='spec_sens')
											tholdMinDiffSS <- threshold(thisEval, stat='equal_sens_spec')
											thold10PercTest <- threshold(thisEval, stat='sensitivity', sensitivity=0.9)

											sensTholdMaxSumSS <- sum(predPresFrom >= tholdMaxSumSS, na.rm=TRUE) / length(na.omit(predPresFrom))
											specTholdMaxSumSS <- sum(predBgFrom < tholdMaxSumSS, na.rm=TRUE) / length(na.omit(predBgFrom))

											sensTholdMinDiffSS <- sum(predPresFrom >= tholdMinDiffSS, na.rm=TRUE) / length(na.omit(predPresFrom))
											specTholdMinDiffSS <- sum(predBgFrom < tholdMinDiffSS, na.rm=TRUE) / length(na.omit(predBgFrom))

											sensThold10PercTest <- sum(predPresFrom >= thold10PercTest, na.rm=TRUE) / length(na.omit(predPresFrom))
											specThold10PercTest <- sum(predBgFrom < thold10PercTest, na.rm=TRUE) / length(na.omit(predBgFrom))

											# remember
											thisEval <- data.frame(

												algorithm=thisAlgo,
												scheme=scheme,
												pme=pmeVariant,
												toValance=toValance,
												toUnit=toUnit,
												fromValance=fromValance,
												fromUnit=fromUnit,
												kTo=kTo,
												kFrom=kFrom,

												testTrainGeogDist=testTrainGeogDist,
												nTrain=nrow(model$trainingPresences),
												nTest=nrow(testPresTo),

												cbi=cbi,

												tholdMaxSumSS=tholdMaxSumSS,
												tholdMinDiffSS=tholdMinDiffSS,
												thold10PercTest=thold10PercTest,

												sensTholdMaxSumSS=sensTholdMaxSumSS,
												specTholdMaxSumSS=specTholdMaxSumSS,

												sensTholdMinDiffSS=sensTholdMinDiffSS,
												specTholdMinDiffSS=specTholdMinDiffSS,

												sensThold10PercTest=sensThold10PercTest,
												specThold10PercTest=specThold10PercTest

											)

											### variable importance

											# for each predictor:
											# 1) combine presence/BG env data
											# 2) get index of permuted values
											# 3) permute test data and weights
											# 4) compare to unpermuted predictions

											# (weighted?) predictions for observed data
											# x <- logitAdj(c(presWeight * predPresFrom, bgWeight * predBgFrom))
											x <- logitAdj(c(predPresFrom, predBgFrom))

											for (thisPredictor in predictors) {

												corStat <- rankStat <- cbiPerm <- rep(NA, 100)

												for (j in 1:100) {

													# combine pres/BG
													permutedSites <- rbind(testPresTo, testBgTo)

													# permute
													permIndex <- sample(1:nrow(permutedSites), nrow(permutedSites))

													permutedSites[ , thisPredictor] <- permutedSites[permIndex, thisPredictor]

													# predict to permuted data
													predPerm <- predictEnm(thisData=permutedSites, theModel=fromModel, predictors=predictors, predArgs=predArgs)

													# get permuted predictions for pres/BG
													predPresPerm <- predPerm[1:length(predPresFrom)]
													predBgPerm <- predPerm[(length(predPresFrom) + 1):length(predPerm)]

													y <- logitAdj(c(predPresPerm, predBgPerm))

													# importance statistics
													corStat[j] <- cor(x, y)
													rankStat[j] <- cor(x, y, method='spearman')
													cbiPerm[j] <- contBoyce(pres=predPresPerm,  bg=predBgPerm, na.rm=TRUE, numBins=1001)

												}

												# summarize
												corStat <- mean(corStat)
												rankStat <- mean(rankStat)
												cbiPerm <- mean(cbiPerm)

												# remember
												thisImp <- data.frame(cosStat=corStat, rankStat=rankStat, cbiPerm=cbiPerm)
												names(thisImp) <- c(paste0('corStat_perm', capIt(thisPredictor)), paste0('rankStat_perm', capIt(thisPredictor)), paste0('cbiPerm_perm', capIt(thisPredictor)))

												thisEval <- cbind(thisEval, thisImp)

											} # next predictor

											evalFrame <- if (exists('evalFrame')) {
												rbind(evalFrame, thisEval)
											} else {
												thisEval
											}

											# print(evalFrame)
											# say('')

										} # next "to" k-fold

									} # next "from" k-fold

									say('')

								} # next to unit

							} # if valid comparison

						} # next to valance

					} # next from unit

				} # next from valance

				saveRDS(evalFrame, paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ', toupper(thisAlgo), '.rds'))

				rm(evalFrame)

			} # next PME

		} # next scheme

	} # next algorithm

}

##################################################################################
### evaluate MULTIVARIATE ENSEMBLE ENMs using PCs against self and other units ###
##################################################################################

if ('eval ensemble' %in% do) {

	say('evaluate MULTIVARIATE ENSEMBLE ENMs using PCs against self and other units', level=1)

	say('Using ', paste(toupper(algos), collapse= ' '), '!!!')
	
	# percent of train/test sites environmentally closest to evaluate
	mopPerc <- c(0.5, 1)

	### get PCA axes to use (as predictors)

		# load PCA
		pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

		# selecting PCs that cumulatively explain x% of variance
		pcNames <- paste0('PC', prefix(1:length(pca$sdev), 2))
		cumulExplain <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
		pcsUsed <- pcNames[cumulExplain <= pcaMinCumulExplained]
		if (length(pcsUsed) < length(pcNames)) pcsUsed <- c(pcsUsed, paste0('PC', prefix(length(pcsUsed) + 1, 2)))
		
	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by PME
		for (pmeVariant in pmes) {

			if (exists('evalFrame')) rm(evalFrame)

			pmeNice <- pmeNiceName(pmeVariant)

			# by "from" VALANCE
			for (fromValance in valances) {

				# get names of focal units to include/exclude
				units <- getUnits(scheme=scheme, incAll=(fromValance == 'including'))

				say('UNITS: ', paste(units, collapse=' | '), pre=1)

				# by UNIT
				for (fromUnit in units) {

					say(Sys.time(), '| ENSEMBLE ENM | SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, pre=1)

					say('  Evaluating against:')

					for (toValance in valances) {

						if (!(fromValance == 'including' & toValance == 'excluding' & fromUnit == 'all')) {

							# if "from" and "to" valances are both "include" then test against all other units. If not, then test only against own unit.
							toUnits <- if (fromValance == 'including' & toValance == 'including') {
								units
							} else {
								fromUnit
							}

							for (toUnit in toUnits) {

								say('    ', toupper(toValance), ' ', toUnit, post=0)

								## load "to" test data
								testDataFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(toValance), ' ', toupper(toUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
								load(testDataFile)

								testSpeciesData <- speciesData

								# for each k-fold of model evaluate against either same k-fold of test data (if model is for same unit/valance) or *all* k-folds of test data is model is not for unit being tested against
								for (kFrom in 1:kFolds) {

									say(kFrom, post=0)

									## load training data
									trainDataFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
									load(trainDataFile)

									trainSpeciesData <- speciesData
									trainPres <- trainSpeciesData$trainingPresences$kFold[[kFrom]]
									trainBg <- trainSpeciesData$trainingAbsences$kFold[[kFrom]]
									trainPres$obsDate <- as.Date(paste0(trainPres$obsYear, '-', trainPres$obsMonth, '-', trainPres$obsDayOfMonth))

									kFoldsTo <- if (fromValance == 'including' & toValance == 'including' & fromUnit == toUnit) {
										kFrom
									} else if (fromValance == 'including' & toValance == 'including' & fromUnit != toUnit & fromUnit != 'all' & toUnit != 'all') {
										1:kFolds
									} else if (fromValance == 'including' & toValance == 'including' & fromUnit == 'all' & toUnit != 'all') {
										kFrom
									} else if (fromValance == 'including' & toValance == 'including' & fromUnit != 'all' & toUnit == 'all') {
										kFrom
									} else if (fromValance == 'excluding' & toValance == 'excluding' & fromUnit == toUnit) {
										kFrom
									} else if (fromValance == 'excluding' & toValance == 'excluding' & fromUnit != toUnit) {
										1:kFolds
									}

									# for each fold in test data
									for (kTo in kFoldsTo) {

										### remember
										thisEval <- data.frame(

											algorithm='ensemble',
											algos=paste(algos, collapse=' '),
											scheme=scheme,
											pme=pmeVariant,
											fromValance=fromValance,
											toValance=toValance,
											fromUnit=fromUnit,
											toUnit=toUnit,
											kFrom=kFrom,
											kTo=kTo

										)

										# get test sites
										testPres <- testSpeciesData$testPresences$kFold[[kTo]]
										testBg <- testSpeciesData$testAbsences$kFold[[kTo]]

										testPres$obsDate <- as.Date(paste0(testPres$obsYear, '-', testPres$obsMonth, '-', testPres$obsDayOfMonth))
										testBg$kFold <- NULL

										### matrices for predictions at training/test data... each is a matrix, with one row per site and one column per ENM... will average over these to get ensemble prediction

										predPresTrain <- matrix(NA, nrow=nrow(trainPres), ncol=length(algos))
										predPresTest <- matrix(NA, nrow=nrow(testPres), ncol=length(algos))

										predBgTrain <- matrix(NA, nrow=nrow(trainBg), ncol=length(algos))
										predBgTest <- matrix(NA, nrow=nrow(testBg), ncol=length(algos))

										### get prediction for each algorithm
										for (countAlgo in seq_along(algos)) {

											thisAlgo <- algos[countAlgo]

											## load model
											fromFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(kFrom, 2), '.Rdata')

											load(fromFile)

											fromModel <- model$model
											predictors <- model$predictorsToUse
											rm(model); gc()

											## predict to training sites
											thisPredPresTrain <- predictEnm(thisData=trainPres, theModel=fromModel, predictors=predictors, predArgs=predArgs)
											thisPredBgTrain <- predictEnm(thisData=trainBg, theModel=fromModel, predictors=predictors, predArgs=predArgs)

											## predict to test sites
											thisPredPresTest <- predictEnm(thisData=testPres, theModel=fromModel, predictors=predictors, predArgs=predArgs)
											thisPredBgTest <- predictEnm(thisData=testBg, theModel=fromModel, predictors=predictors, predArgs=predArgs)

											predPresTrain[ , countAlgo] <- thisPredPresTrain
											predBgTrain[ , countAlgo] <- thisPredBgTrain

											predPresTest[ , countAlgo] <- thisPredPresTest
											predBgTest[ , countAlgo] <- thisPredBgTest
											
											rm(fromModel); gc()

										}

										predPresTrain <- rowMeans(predPresTrain, na.rm=TRUE)
										predBgTrain <- rowMeans(predBgTrain, na.rm=TRUE)

										predPresTest <- rowMeans(predPresTest, na.rm=TRUE)
										predBgTest <- rowMeans(predBgTest, na.rm=TRUE)

										### evaluate for each set of MOP-selected sites
										###############################################
										
										### get MOP-selected test sites... return *index* of each site in a set
										presMop <- mop(set1=trainPres[ , pcsUsed], set2=testPres[ , pcsUsed], p=mopPerc, index=TRUE, na.rm=TRUE, cores=cores)
										bgMop <- mop(set1=trainBg[ , pcsUsed], set2=testBg[ , pcsUsed], p=mopPerc, index=TRUE, na.rm=TRUE, cores=cores)
										
										### evaluate for each MOP-defined set of points
										for (countPerc in seq_along(mopPerc)) {
											
											# get train/test sites
											thisTrainPres <- trainPres[presMop$set1[[countPerc]], , drop=FALSE]
											thisTrainBg <- trainBg[bgMop$set1[[countPerc]], , drop=FALSE]

											thisTestPres <- testPres[presMop$set2[[countPerc]], , drop=FALSE]
											thisTestBg <- testBg[bgMop$set2[[countPerc]], , drop=FALSE]

											### calculate geographic/temporal differences between training/test sites to use as covariates later
											####################################################################################################
												
												## calculate geographic distance between training/TRAINING presences
												dists <- pointDist(thisTrainPres[ , c('longWgs84', 'latWgs84')]) / 1000
												trainTrainGeogDist <- mean(dists, na.rm=TRUE)

												## calculate temporal difference between training/TRAINING presences
												diffs <- matrix(NA, nrow=nrow(thisTrainPres), ncol=nrow(thisTrainPres))
												for (i in 1:nrow(thisTrainPres)) {
													diffs[i, i:nrow(thisTrainPres)] <- as.numeric(thisTrainPres$obsDate[i] - thisTrainPres$obsDate[i:nrow(thisTrainPres)])
												}

												diffs[lower.tri(diffs)] <- -1 * diffs[upper.tri(diffs)] # does weird mirroring of upper/lower triangulars but summary results are still the same
												trainTrainTemporalDiffs <- mean(abs(diffs), na.rm=TRUE)
												trainTrainTemporalDiffsSigned <- mean(diffs, na.rm=TRUE)

												## calculate geographic distance between training/TEST presences
												dists <- pointDist(thisTrainPres[ , c('longWgs84', 'latWgs84')], thisTestPres[ , c('longWgs84', 'latWgs84')]) / 1000
												trainTestGeogDist <- mean(dists, na.rm=TRUE)

												## calculate temporal difference between training/TEST presences
												diffs <- matrix(NA, nrow=nrow(thisTestPres), ncol=nrow(thisTrainPres))
												for (i in 1:nrow(thisTestPres)) {
													diffs[i, ] <- as.numeric(thisTrainPres$obsDate - thisTestPres$obsDate[i])
												}

												trainTestTemporalDiffs <- mean(abs(diffs), na.rm=TRUE)
												trainTestTemporalDiffsSigned <- mean(diffs, na.rm=TRUE)

											### evaluate
											############
												
											## evaluate vs training sites
											cbiTrain <- contBoyce(pres=predPresTrain[presMop$set1[[countPerc]]], bg=predBgTrain[bgMop$set1[[countPerc]]], na.rm=TRUE, numBins=1001)
											thisEvalTrain <- evaluate(p=as.vector(predPresTrain[presMop$set1[[countPerc]]]), a=as.vector(predBgTrain[bgMop$set1[[countPerc]]]), tr=seq(0, 1, by=0.0001))

											aucTrain <- thisEvalTrain@auc

											tholdMaxSumSSTrain <- threshold(thisEvalTrain, stat='spec_sens')
											tholdMinDiffSSTrain <- threshold(thisEvalTrain, stat='equal_sens_spec')
											thold10PercTrain <- threshold(thisEvalTrain, stat='sensitivity', sensitivity=0.9)

											sensTholdMaxSumSSTrain <- sum(predPresTrain[presMop$set1[[countPerc]]] >= tholdMaxSumSSTrain) / length(predPresTrain[presMop$set1[[countPerc]]])
											specTholdMaxSumSSTrain <- sum(predBgTrain[bgMop$set1[[countPerc]]] < tholdMaxSumSSTrain) / length(predBgTrain[bgMop$set1[[countPerc]]])

											sensTholdMinDiffSSTrain <- sum(predPresTrain[presMop$set1[[countPerc]]] >= tholdMinDiffSSTrain) / length(predPresTrain[presMop$set1[[countPerc]]])
											specTholdMinDiffSSTrain <- sum(predBgTrain[bgMop$set1[[countPerc]]] < tholdMinDiffSSTrain) / length(predBgTrain[bgMop$set1[[countPerc]]])

											sensThold10PercTrain <- sum(predPresTrain[presMop$set1[[countPerc]]] >= thold10PercTrain) / length(predPresTrain[presMop$set1[[countPerc]]])
											specThold10PercTrain <- sum(predBgTrain[bgMop$set1[[countPerc]]] < thold10PercTrain) / length(predBgTrain[bgMop$set1[[countPerc]]])

											## evaluate vs test sites
											cbiTest <- contBoyce(pres=predPresTest[presMop$set2[[countPerc]]], bg=predBgTest[bgMop$set2[[countPerc]]], na.rm=TRUE, numBins=1001)
											thisEvalTest <- evaluate(p=as.vector(predPresTest[presMop$set2[[countPerc]]]), a=as.vector(predBgTest[bgMop$set2[[countPerc]]]), tr=seq(0, 1, by=0.0001))

											aucTest <- thisEvalTest@auc

											tholdMaxSumSSTest <- threshold(thisEvalTest, stat='spec_sens')
											tholdMinDiffSSTest <- threshold(thisEvalTest, stat='equal_sens_spec')
											thold10PercTest <- threshold(thisEvalTest, stat='sensitivity', sensitivity=0.9)

											sensTholdMaxSumSSTest <- sum(predPresTest[presMop$set2[[countPerc]]] >= tholdMaxSumSSTest) / length(predPresTest[presMop$set2[[countPerc]]])
											specTholdMaxSumSSTest <- sum(predBgTest[bgMop$set2[[countPerc]]] < tholdMaxSumSSTest) / length(predBgTest[bgMop$set2[[countPerc]]])

											sensTholdMinDiffSSTest <- sum(predPresTest[presMop$set2[[countPerc]]] >= tholdMinDiffSSTest) / length(predPresTest[presMop$set2[[countPerc]]])
											specTholdMinDiffSSTest <- sum(predBgTest[bgMop$set2[[countPerc]]] < tholdMinDiffSSTest) / length(predBgTest[bgMop$set2[[countPerc]]])

											sensThold10PercTest <- sum(predPresTest[presMop$set2[[countPerc]]] >= thold10PercTest) / length(predPresTest[presMop$set2[[countPerc]]])
											specThold10PercTest <- sum(predBgTest[bgMop$set2[[countPerc]]] < thold10PercTest) / length(predBgTest[bgMop$set2[[countPerc]]])

											# remember
											thisEvalSub <- data.frame(

												mopPerc=mopPerc[countPerc],
											
												medianEnvDistPresTrainTest=presMop$stats[countPerc, 'medianDist'],
												medianEnvDistBgTrainTest=bgMop$stats[countPerc, 'medianDist'],
											
												trainTrainGeogDist=trainTrainGeogDist,
												trainTestGeogDist=trainTestGeogDist,

												trainTrainTemporalDiffs=trainTrainTemporalDiffs,
												trainTestTemporalDiffs=trainTestTemporalDiffs,

												trainTrainTemporalDiffsSigned=trainTrainTemporalDiffsSigned,
												trainTestTemporalDiffsSigned=trainTestTemporalDiffsSigned,
											
												presTrain=length(presMop$set1[[countPerc]]),
												presTest=length(presMop$set2[[countPerc]]),

												bgTrain=length(bgMop$set1[[countPerc]]),
												bgTest=length(bgMop$set2[[countPerc]]),

												cbiTrain=cbiTrain,
												cbiTest=cbiTest,

												aucTrain=aucTrain,
												aucTest=aucTest,

												tholdMaxSumSSTrain=tholdMaxSumSSTrain,
												tholdMaxSumSSTest=tholdMaxSumSSTest,

												tholdMinDiffSSTrain=tholdMinDiffSSTrain,
												tholdMinDiffSSTest=tholdMinDiffSSTest,

												thold10PercTrain=thold10PercTrain,
												thold10PercTest=thold10PercTest,

												sensTholdMaxSumSSTrain=sensTholdMaxSumSSTrain,
												sensTholdMaxSumSSTest=sensTholdMaxSumSSTest,

												specTholdMaxSumSSTrain=specTholdMaxSumSSTrain,
												specTholdMaxSumSSTest=specTholdMaxSumSSTest,

												sensTholdMinDiffSSTrain=sensTholdMinDiffSSTrain,
												sensTholdMinDiffSSTest=sensTholdMinDiffSSTest,

												specTholdMinDiffSSTrain=specTholdMinDiffSSTrain,
												specTholdMinDiffSSTest=specTholdMinDiffSSTest,

												sensThold10PercTrain=sensThold10PercTrain,
												sensThold10PercTest=sensThold10PercTest,

												specThold10PercTrain=specThold10PercTrain,
												specThold10PercTest=specThold10PercTest

											)
											
											names(thisEvalSub) <- paste0(names(thisEvalSub), '_mop', gsub(as.character(mopPerc[countPerc]), pattern='[.]', replacement='p'))
											thisEval <- cbind(thisEval, thisEvalSub)
											
										} # next MOP percentile
										
										evalFrame <- if (exists('evalFrame')) {
											rbind(evalFrame, thisEval)
										} else {
											thisEval
										}

										# print(evalFrame)
										# say('')

									} # next "to" k-fold

								} # next "from" k-fold

								say('')

							} # next to unit

						} # if valid comparison

					} # next to valance

				} # next from unit

			} # next from valance

			rownames(evalFrame) <- 1:nrow(evalFrame)
			save(evalFrame, file=paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ENSEMBLE - MOP-Adjusted.Rdata'))
			rm(evalFrame)

		} # next PME

	} # next scheme

}

##############################################################################
### update CBI of MULTIVARIATE ENMs using PCs against self and other units ###
##############################################################################

if ('recalculate CBI for ensemble' %in% do) {

	say('update CBI of MULTIVARIATE ENMs using PCs against self and other units', level=1)

	say('The ecospat package has a function ecospat.boyce that calculates the CBI but using smaller steps when moving the moving window than used by the OLD contBoyce function in the enmSdm package. I will add the values of CBI from enmSdm 2.2.0 to existing evaluations using this script.')

	say('Using ', paste(toupper(algos), collapse= ' '), '!!!')

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by PME
		for (pmeVariant in pmes) {

			if (exists('evalFrame')) rm(evalFrame)

			pmeNice <- pmeNiceName(pmeVariant)

			evalFrame <- readRDS(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ENSEMBLE.rds'))
			evalFrame$cbiFromTo <- evalFrame$cbiFromFrom <- NA

			# by "from" VALANCE
			for (fromValance in valances) {

				# get names of focal units to include/exclude
				units <- getUnits(scheme=scheme, incAll=(fromValance == 'including'))

				say('UNITS: ', paste(units, collapse=' | '), pre=1)

				# by UNIT
				for (fromUnit in units) {

					say(Sys.time(), '| ENSEMBLE ENM | SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, pre=1)

					say('  Adding CBI to evaluation against:')

					for (toValance in valances) {

						if (!(fromValance == 'including' & toValance == 'excluding' & fromUnit == 'all')) {

							# if "from" and "to" valances are both "include" then test against all other units. If not, then test only against own unit.
							toUnits <- if (fromValance == 'including' & toValance == 'including') {
								units
							} else {
								fromUnit
							}

							for (toUnit in toUnits) {

								say('    ', toupper(toValance), ' ', toUnit, post=0)

								## load "to" test data
								toDataFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(toValance), ' ', toupper(toUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
								load(toDataFile)

								toSpeciesData <- speciesData

								# for each k-fold of model evaluate against either same k-fold of test data (if model is for same unit/valance) or *all* k-folds of test data is model is not for unit being tested against
								for (kFrom in 1:kFolds) {

									say(kFrom, post=0)

									## load "from" test data
									fromDataFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
									load(fromDataFile)

									fromSpeciesData <- speciesData
									trainPresFrom <- fromSpeciesData$trainingPresences$kFold[[kFrom]]
									trainBgFrom <- fromSpeciesData$trainingAbsences$kFold[[kFrom]]

									kFoldsTo <- if (fromValance == 'including' & toValance == 'including' & fromUnit == toUnit) {
										kFrom
									} else if (fromValance == 'including' & toValance == 'including' & fromUnit != toUnit & fromUnit != 'all' & toUnit != 'all') {
										1:kFolds
									} else if (fromValance == 'including' & toValance == 'including' & fromUnit == 'all' & toUnit != 'all') {
										kFrom
									} else if (fromValance == 'including' & toValance == 'including' & fromUnit != 'all' & toUnit == 'all') {
										kFrom
									} else if (fromValance == 'excluding' & toValance == 'excluding' & fromUnit == toUnit) {
										kFrom
									} else if (fromValance == 'excluding' & toValance == 'excluding' & fromUnit != toUnit) {
										1:kFolds
									}

									# for each fold in test data
									for (kTo in kFoldsTo) {

										testPresTo <- toSpeciesData$testPresences$kFold[[kTo]]
										testBgTo <- toSpeciesData$testAbsences$kFold[[kTo]]

										testBgTo$kFold <- NULL

										### matrices for predictions at training/test data

										predPresFromFrom <- matrix(NA, nrow=nrow(trainPresFrom), ncol=length(algos))
										predPresFromTo <- matrix(NA, nrow=nrow(testPresTo), ncol=length(algos))

										predBgFromFrom <- matrix(NA, nrow=nrow(trainBgFrom), ncol=length(algos))
										predBgFromTo <- matrix(NA, nrow=nrow(testBgTo), ncol=length(algos))

										### load model
										for (countAlgo in seq_along(algos)) {

											thisAlgo <- algos[countAlgo]

											## load model
											fromFile <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(kFrom, 2), '.Rdata')

											load(fromFile)

											fromModel <- model$model
											predictors <- model$predictorsToUse
											rm(model); gc()

											## predict to training sites
											thisPredPresFromFrom <- predictEnm(thisData=trainPresFrom, theModel=fromModel, predictors=predictors, predArgs=predArgs)
											thisPredBgFromFrom <- predictEnm(thisData=trainBgFrom, theModel=fromModel, predictors=predictors, predArgs=predArgs)

											## predict to test sites
											thisPredPresFromTo <- predictEnm(thisData=testPresTo, theModel=fromModel, predictors=predictors, predArgs=predArgs)
											thisPredBgFromTo <- predictEnm(thisData=testBgTo, theModel=fromModel, predictors=predictors, predArgs=predArgs)

											rm(fromModel); gc()

											predPresFromFrom[ , countAlgo] <- thisPredPresFromFrom
											predBgFromFrom[ , countAlgo] <- thisPredBgFromFrom

											predPresFromTo[ , countAlgo] <- thisPredPresFromTo
											predBgFromTo[ , countAlgo] <- thisPredBgFromTo

										}

										if (length(naRows(predPresFromFrom)) > 0) predPresFromFrom <- predPresFromFrom[-naRows(predPresFromFrom), ]
										if (length(naRows(predBgFromFrom)) > 0) predBgFromFrom <- predBgFromFrom[-naRows(predBgFromFrom), ]

										if (length(naRows(predPresFromTo)) > 0) predPresFromTo <- predPresFromTo[-naRows(predPresFromTo), ]
										if (length(naRows(predBgFromTo)) > 0) predBgFromTo <- predBgFromTo[-naRows(predBgFromTo), ]

										# calculate ensemble prediction
										predPresFromFrom <- rowMeans(predPresFromFrom)
										predBgFromFrom <- rowMeans(predBgFromFrom)

										predPresFromTo <- rowMeans(predPresFromTo)
										predBgFromTo <- rowMeans(predBgFromTo)

										# evaluate vs training sites
										cbiFromFromNew <- contBoyce(pres=predPresFromFrom, bg=predBgFromFrom, na.rm=TRUE, numBins=1001)

										# evaluate vs test sites
										cbiFromToNew <- contBoyce(pres=predPresFromTo, bg=predBgFromTo, na.rm=TRUE, numBins=1001)

										this <- which(
											evalFrame$algorithm == 'ensemble' &
											evalFrame$scheme == scheme &
											evalFrame$pme == pmeVariant &
											evalFrame$fromValance == fromValance &
											evalFrame$toValance == toValance &
											evalFrame$fromUnit == fromUnit &
											evalFrame$toUnit == toUnit &
											evalFrame$kFrom == kFrom &
											evalFrame$kTo == kTo
										)

										evalFrame$cbiFromFrom[this] <- cbiFromFromNew
										evalFrame$cbiFromTo[this] <- cbiFromToNew

									} # next "to" k-fold

								} # next "from" k-fold

								say('')

							} # next to unit

						} # if valid comparison

					} # next to valance

				} # next from unit

			} # next from valance

			saveRDS(evalFrame, paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ENSEMBLE updated CBI.rds'))
			rm(evalFrame)

		} # next PME

	} # next scheme

}

######################################################
### calculate niche overlap from ENMs based on PCs ###
######################################################

if ('overlap' %in% do) {

	say('calculate niche overlap from ENMs based on PCs', level=1)

	say('Using ', toupper(algo), '!!!')

	say('NEW: For each pair of units within a division scheme, calculate niche overlap using ENM-based projections adapted from Broennimann et al.  Assume we wish to know the overlap of unit B in the environmental space of A.  The ENM-based metric is calculated first by projecting A into the space of A using the ENM trained on A and B into the space of A using the ENM trained on B. Then, at a set of randomly chosen background sites overlap is calculated. These random background sites are chosen randomly from the background area avalable to A.', post=2)

	# say('OLD: For each pair of units within a division scheme, calculate niche overlap using ENM-based projections adapted from Broennimann et al.  Assume we wish to know the overlap of unit B in the environmental space of A.  The ENM-based metric is calculated first by projecting A into the space of A using the ENM trained on A and B into the space of A using the ENM trained on B. Then, at a set of randomly chosen background sites overlap is calculated. These random background sites are chosen in proportion to the prevalance of background environmental space available to A. This is calculated from an ENM trained on the _background_ of A versus the background composed of all units in the scheme.', post=2)

	# say('OLD: Metrics are calculated using the prevalance of the background either as a weighting factor or from randomly random points in environmental space drawn in proportion to the prevalence of the background. Metrics can include any measure of niche overlap (Godsoes ESP, Schoeners D, Warrens I, or the rank or normal correlation coefficient). I interpret the rank correlation coefficient to be a measure of niche similarity and the normal (Pearson) coefficient to be a measure of niche equivalency.', post=2)

	say('NEW: Metrics can include any measure of niche overlap (Godsoes ESP, Schoeners D, Warrens I, or the rank or normal correlation coefficient). I interpret the rank correlation coefficient to be a measure of niche similarity and the normal (Pearson) coefficient to be a measure of niche equivalency.', post=2)

	mask <- raster(paste0(drive, 'ecology/Climate/PRISM/PRISM_us_dem_800m.tif'))

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=FALSE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by PME
		for (pmeVariant in pmes) {

			pmeNice <- pmeNiceName(pmeVariant)

			# load focal background
			repFile <- paste0(workDir, '/Representative Unit Sites/', schemeNice, '/Representative Sites 02 Set 01 ', schemeNice, ' - ', pmeNice, ' PME - UNITS - Derived Variables.rds')
			repBg <- readRDS(repFile)

			# by VALANCE
			for (toValance in valances) {

				# fromValances <- if (toValance == 'including') {
					# c('including', 'excluding')
				# } else if (toValance == 'excluding') {
					# 'including'
				# }

				fromValances <- 'including'

				for (fromValance in fromValances) {

					# get names of focal units to include/exclude
					toUnits <- getUnits(scheme=scheme, incAll=(toValance == 'including'))

					# "to" unit is the unit to which other units' niches are to be projected
					for (toUnit in toUnits) {

						# get "to" BG
						toBgSites <- if (toValance == 'including' & toUnit != 'all') {
							repBg[which(repBg$unit == toUnit), ]
						} else if (toValance == 'including' & toUnit == 'all') {
							repBg
						} else if (toValance == 'excluding') {
							repBg[which(repBg$unit != toUnit), ]
						}

						# load "to" unit model
						toModelFile <- paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(toValance), ' ', toupper(toUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(algo), ' multivariate all sites.Rdata')
						load(toModelFile)
						toModel <- model$model
						predictors <- model$predictorsToUse

						# load "to" unit presences
						toDataFile <- paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(toValance), ' ', toupper(toUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
						load(toDataFile)
						toPres <- speciesData$allPresences

						# load PCA and convert representative sites to PCA axes
						pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))
						toBgPc <- as.data.frame(predict(pca, toBgSites))
						names(toBgPc) <- paste0('PC', prefix(1:ncol(toBgPc), 2))

						# predict "to" unit's representative model and "to" model to representative sites
						toPresPred <- predictEnm(thisData=toPres, theModel=toModel, predictors=predictors, predArgs=predArgs)
						toBgPred <- predictEnm(thisData=toBgPc, theModel=toModel, predictors=predictors, predArgs=predArgs)

						toPresWeight <- kdeWeighting(scheme=scheme, valance=toValance, unit=toUnit, sites=toPres, mask=mask)
						toPresWeight <- 1 - (toPresWeight / max(toPresWeight))

						# calculate overlap for each "from" unit to "to" unit
						fromUnits <- if (toValance == 'including' & toUnit == 'all' & fromValance == 'including') {
							getUnits(scheme=scheme, incAll=FALSE)
						} else if (toValance == 'including' & toUnit != 'all' & fromValance == 'including') {
							getUnits(scheme=scheme, incAll=TRUE)[-which(getUnits(scheme=scheme, incAll=TRUE) %in% toUnit)]
						} else if (toValance == 'including' & toUnit == 'all' & fromValance == 'excluding') {
							getUnits(scheme=scheme, incAll=FALSE)
						} else if (toValance == 'including' & toUnit != 'all' & fromValance == 'excluding') {
							toUnit
						} else if (toValance == 'excluding' & fromValance == 'including') {
							toUnit
						} else if (toValance == 'excluding' & fromValance == 'excluding') {
							NULL # not done
						}

						# if comparison is valid
						if (!is.null(fromUnits)) {

							for (fromUnit in fromUnits) {

								say(Sys.time(), ' SCHEME ', schemeNice, ' | PME ', pmeNice, ' | FROM VALANCE ', fromValance, ' FROM UNIT ', fromUnit, ' | TO VALANCE ', toValance, ' | TO UNIT ', toUnit, breaks=NULL)

								# load "from" model
								fromModelFile <- paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(algo), ' multivariate all sites.Rdata')
								load(fromModelFile)
								fromModel <- model$model

								fromPresPred <- predictEnm(thisData=toPres, theModel=fromModel, predictors=predictors, predArgs=predArgs)
								fromBgPred <- predictEnm(thisData=toBgPc, theModel=fromModel, predictors=predictors, predArgs=predArgs)

								# similarity
								simAtBg <- compareNiches(toBgPred, fromBgPred, na.rm=TRUE)
								simAtPres <- compareNiches(toPresPred, fromPresPred, na.rm=TRUE)
								simAtPresWeighted <- compareNiches(toPresPred, fromPresPred, na.rm=TRUE, weights=toPresWeight)

								# remember
								thisOverlap <- data.frame(

									scheme=scheme,
									pme=pmeVariant,
									toValance=toValance,
									toUnit=toUnit,
									fromValance=fromValance,
									fromUnit=fromUnit,

									algo=algo,

									meanDiffAtBg=simAtBg$meanDiff,
									meanDiffAtPres=simAtPres$meanDiff,
									meanDiffAtPresWeighted=simAtPresWeighted$meanDiff,

									meanAbsDiffAtBg=simAtBg$meanAbsDiff,
									meanAbsDiffAtPres=simAtPres$meanAbsDiff,
									meanAbsDiffAtPresWeighted=simAtPresWeighted$meanAbsDiff,

									godsoeAtBg=simAtBg$esp,
									godsoeAtPres=simAtPres$esp,
									godsoeAtPresWeighted=simAtPresWeighted$esp,

									schoenerDBg=simAtBg$d,
									schoenerDPres=simAtPres$d,
									schoenerDPresWeighted=simAtPresWeighted$d,

									warrenIBg=simAtBg$i,
									warrenIPres=simAtPres$i,
									warrenIPresWeighted=simAtPresWeighted$i,

									corBg=simAtBg$rho,
									corPres=simAtPres$rho,
									corPresWeighted=simAtPresWeighted$rho,

									rankCorBg=simAtBg$rankCor,
									rankCorPres=simAtPres$rankCor,
									rankCorPresWeighted=simAtPresWeighted$rankCor

								)

								### compare response curves
								for (thisPred in predictors) {

									respSmooth <- compareResponse(
										pred1=c(fromPresPred, fromBgPred),
										pred2=c(toPresPred, toBgPred),
										data=rbind(toPres[ , predictors], toBgPc[ , predictors]),
										pred=thisPred,
										adjust=TRUE,
										gap=0.2,
										smooth=TRUE,
										smoothN=1000,
										smoothRange=c(0, 1),
										graph=FALSE,
										verbose=FALSE,
										na.rm=TRUE
									)

									w <- c(toPresWeight, rep(sum(toPresWeight) / length(fromBgPred), length(fromBgPred)))
									w <- w / max(w, na.rm=TRUE)

									respWeighted <- compareResponse(
										pred1=c(fromPresPred, fromBgPred),
										pred2=c(toPresPred, toBgPred),
										data=rbind(toPres[ , predictors], toBgPc[ , predictors]),
										pred=thisPred,
										adjust=TRUE,
										gap=0.2,
										smooth=FALSE,
										smoothN=1000,
										smoothRange=c(0, 1),
										graph=FALSE,
										verbose=FALSE,
										na.rm=TRUE,
										w=w
									)

									respSmooth <- respSmooth[ , c('meanDiff', 'meanAbsDiff', 'areaAbsDiff', 'd', 'i', 'esp', 'rho', 'rankCor')]
									respWeighted <- respWeighted[ , c('meanDiff', 'meanAbsDiff', 'areaAbsDiff', 'd', 'i', 'esp', 'rho', 'rankCor')]

									names(respSmooth) <- paste0(names(respSmooth), '_', thisPred, '_smooth')
									names(respWeighted) <- paste0(names(respWeighted), '_', thisPred, '_weighted')

									keep <- cbind(
										data.frame(
											pred=thisPred
										),
										respSmooth,
										respWeighted
									)

									thisOverlap <- cbind(thisOverlap, keep)

								} # next predictor

								overlap <- if (exists('overlap')) {
									rbind(overlap, thisOverlap)
								} else {
									thisOverlap
								}

								print(overlap)

							} # next from unit

						} # if any valid "from" units

					} # next focal unit

				} # next fromValance

			} # next toValance

			saveRDS(overlap, paste0(workDir, 'ENMs - PCs/', schemeNice, '/ENM-Based Niche Overlap - ', pmeNice, ' PME - Using ', toupper(algo), '.rds'))
			rm(overlap)

		} # next PME

	} # next division scheme

}

#####################################################
### visualize multivariate PC ENM analysis - maps ###
#####################################################

if ('map' %in% do) {

	say('visualize multivariate PC ENM analysis - maps', level=1)

	say('For each division scheme, create map showing ENM performance OR ENM-based niche overlap against self/other units.')

	resp <- 'cbiFromTo' # enmSdm version

	respNice <- if (resp == 'cbiFromTo') {
		'CBI (enmSdm)'
	}

	genreNice <- if (genre == 'performance') {
		'ENM Performance'
	} else if (genre == 'overlap') {
		'Niche Overlap'
	}

	say('Using ', paste(algos, collapse=' '), ' with ', respNice, '!!!')

	# site for ALL unit on map
	allCoords <- cbind(-880018.7, 6640130)

	# make polygon representing ALL clade
	allPoly <- SpatialPoints(cbind(allCoords), getCRS('climateNA', TRUE))
	allPoly <- gBuffer(allPoly, width=150000)

	pres <- getPres()

	gadm <- readOGR(paste0(drive, 'ecology/Political Geography/GADM/ver2pt8/WGS84'), 'USA_adm1', verbose=FALSE)
	gadm <- sp::spTransform(gadm, getCRS('climateNA', TRUE))

	# by SCHEME
	for (scheme in schemes) {

		# graphical layout
		if (scheme == 'cladeNonOverlap') {

			# figure layout and dimensions
			ea <- 4
			layout <- matrix(
				c(
					rep(c(1, rep(2:4, each=ea)), ea),
					rep(c(1, rep(5:7, each=ea)), ea)
				),
				byrow=FALSE, ncol=2 * ea
			)

			width <- 1700
			height <- 2200

			unitLabels <- TRUE # labels units with name

			statCexOther <- 0.7 # model performance statistic (vs another unit)
			statCexSelf <- 1.2 # model performance statistic (vs self)

			unitLabelCex <- 0.5 # unit label size
			cexMain <- 0.7 # title of a panel
			lineMain <- -0.5 # offset of panel title
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 0 # rotation of title for entire plot

			latOffset <- 170000 # distance by which to "raise" performance statistic text on map

			fadeFill <- 0.3 # transparancy
			fadeFillAll <- 0.3 # transparancy
			fadeBorder <- 1 # transparancy
			fadeBorderAll <- 1 # transparancy

			lwd <- 0.5 # line width

		} else if (scheme == 'ecoregionEpa3Modified') {

			# figure layout and dimensions
			ea <- 4
			layout <- matrix(
				c(
					rep(c(1, rep(2:6, each=ea)), ea),
					rep(c(1, rep(7:11, each=ea)), ea),
					rep(c(1, rep(12:16, each=ea)), ea)
				),
				byrow=TRUE, nrow=3 * ea
			)

			width <- 2600
			height <- 1700

			unitLabels <- FALSE # labels units with name

			statCexOther <- 0.45 # model performance statistic (vs another unit)
			statCexSelf <- 0.6 # model performance statistic (vs self)

			unitLabelCex <- 0.3 # unit label size
			cexMain <- 0.3 # title of a panel
			lineMain <- -0.3 # offset for panel title
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 90 # rotation of title for entire plot

			latOffset <- 170000 # distance by which to "raise" performance statistic text on map

			fadeFill <- 0.7 # transparancy
			fadeFillAll <- 0.7 # transparancy
			fadeBorder <- 1 # transparancy
			fadeBorderAll <- 1 # transparancy

			lwd <- 0.5 # line width

		} else if (scheme == 'elevQuantWrtPaeMin') {

			# figure layout and dimensions
			ea <- 4
			layout <- matrix(
				c(
					rep(c(1, rep(2:4, each=ea)), ea),
					rep(c(1, rep(5:7, each=ea)), ea)
				),
				byrow=FALSE, ncol=2 * ea
			)

			width <- 1700
			height <- 2200

			unitLabels <- TRUE # labels units with name

			statCexOther <- 0.45 # model performance statistic (vs another unit)
			statCexSelf <- 0.7 # model performance statistic (vs self)

			unitLabelCex <- 0.4 # unit label size
			cexMain <- 0.7 # title of a panel
			lineMain <- -0.5 # offset for panel title
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 0 # rotation of title for entire plot

			latOffset <- 170000 # distance by which to "raise" performance statistic text on map

			fadeFill <- 0.1 # transparancy
			fadeFillAll <- 0.3 # transparancy
			fadeBorder <- 0.1 # transparancy
			fadeBorderAll <- 1 # transparancy

			lwd <- 0.5 # line width

			# anchor coordinates for unit names and performance
			x <- -2632550
			y <- 6784494
			mult <- 3.2E5 # y coordinate adjustment for each unit label

		}

		# get scheme info and division polygon
		out <- schemeInfo(scheme, poly=TRUE)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		divisionPoly <- out$divisionPoly
		rm(out); gc()

		### process division polygon... get only units with presences

		unitNames <- as.data.frame(divisionPoly)[ , divisionFieldPoly]
		divisionPoly <- divisionPoly[order(unitNames), ]
		unitNames <- sort(unitNames)
		divisionPoly <- divisionPoly[unitNames %in% getUnits(scheme=scheme, incAll=FALSE), ]
		if (scheme == 'ecoregionEpa3Modified') {
			unitNames <- unique(as.data.frame(divisionPoly)[ , divisionFieldPoly])
			divisionPoly <- gUnaryUnion(divisionPoly, id=divisionPoly$L3_KEY)
			divisionPoly$L3_KEY <- unitNames
		}
		divisionPolyDf <- as.data.frame(divisionPoly)
		divisionPolyNames <- divisionPolyDf[ , divisionFieldPoly]
		divisionPoly <- sp::spTransform(divisionPoly, getCRS('climateNA', TRUE))

		centroids <- gCentroid(divisionPoly, byid=TRUE)

		# by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# by ALGORITHM
			for (algo in algos) {

				### get and process evaluation data
				statFrame <- if (genre == 'performance' & algos != 'ensemble') {
					readRDS(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ', toupper(algo), '.rds'))
				} else if (genre == 'performance' & algos == 'ensemble') {
					readRDS(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ', toupper(algo), ' updated with ecospat CBI.rds'))
				} else if (genre == 'overlap') {
					readRDS(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM-Based Niche Overlap - ', pmeNice, ' PME - Using ', toupper(algo), '.rds'))
				}

				if (genre == 'performance') {

					statFrame <- aggregate(statFrame, by=list(statFrame$scheme, statFrame$pme, statFrame$fromValance, statFrame$fromUnit, statFrame$toValance, statFrame$toUnit), FUN=median, na.rm=TRUE)
					statFrame$scheme <- statFrame$pme <- statFrame$fromValance <- statFrame$fromUnit <- statFrame$toValance <- statFrame$toUnit <- NULL
					names(statFrame)[1:6] <- c('scheme', 'pme', 'fromValance', 'fromUnit', 'toValance', 'toUnit')

				}

				# by "from" VALANCE
				for (fromValance in valances) {

					# by "to" VALANCE
					for (toValance in valances) {

						# get names of focal units to include/exclude
						fromUnits <- getUnits(scheme=scheme, incAll=(fromValance == 'including' & toValance == 'including'))

						say(Sys.time(), ' | ', toupper(respNice), ' | SCHEME ', schemeNice, ' | PME ', pmeNice, ' | FROM VALANCE ', fromValance, ' | TO VALANCE ', toValance, '| ', toupper(algo))

						pngFile <- paste0(workDir, 'ENMs - PCs/Multivariate ENM on PCs Comparison - ', schemeNice, ' - ', pmeNice, ' PME - ', genreNice, ' - ', valanceShort(fromValance), ' vs ', valanceShort(toValance), ' - ', respNice, ' - ', toupper(algo), '.png')

						png(pngFile, width=width, height=height, res=600)

							par(layout(layout), mar=0.1 * c(5, 4, 4, 2) + 0)

							plot(0, 0, col='white', fg='white', col.main='white', col.lab='white', col.axis='white')
							text(0, 0, labels=paste0(toupper(genreNice), ' - ', schemeNice, ' - ', pmeNice, ' PME - ', valanceShort(fromValance), ' vs ', valanceShort(toValance), '\n', respNice, ' - ', toupper(algo)), srt=srtBigMain, cex=cexBigMain, xpd=NA)
							title(sub=Sys.time(), cex.sub=0.2, outer=TRUE)

							# for each unit of analysis
							for (countFromUnit in seq_along(fromUnits)) {

								### make base map
								fromUnit <- fromUnits[countFromUnit]
								theseUnitCols <- getUnitCols(divisionPolyDf[ , divisionFieldPoly])

								# plot geography
								plot(divisionPoly, border='white') # template
								plot(gadm, add=TRUE, lwd=lwd / 3, border='gray30')
								plot(divisionPoly, add=TRUE, col=alpha(theseUnitCols, fadeFill), border=alpha(theseUnitCols, fadeBorder), lwd=lwd)
								plot(divisionPoly[which(divisionPolyDf[ , divisionFieldPoly] %in% fromUnit), ], col=getUnitCols(fromUnit), border=alpha('black', fadeBorder), lwd=lwd, add=TRUE)
								title(main=niceUnitName(scheme, fromUnit), line=lineMain, xpd=NA, cex.main=cexMain)

								# plot "all" unit
								if (fromUnit == 'all') {
									plot(allPoly, add=TRUE, col=alpha('gray', fadeFillAll), border=alpha('black', 0.5), lwd=lwd)
									text(allCoords + cbind(0, latOffset), labels='Composite', cex=unitLabelCex, xpd=NA)
								}

								# label units
								if (unitLabels) {
									if (scheme != 'elevQuantWrtPaeMin') {
										x1 <- coordinates(centroids)[ , 1]
										y1 <- coordinates(centroids)[ , 2] + latOffset
										labs <- niceUnitName(scheme, divisionPolyDf[ , divisionFieldPoly])
									} else {
										x1 <- x
										y1 <- y - (0:4 * mult)
										labs <- c('Highest', 'High', 'Middle', 'Low', 'Lowest')
									}
									text(x1, y1, labels=labs, cex=unitLabelCex, xpd=NA)
								}

								### label each unit with performance statistic from model trained on "from" unit
								toUnits <- getUnits(scheme=scheme, incAll=FALSE)

								for (countToUnit in seq_along(toUnits)) {

									toUnit <- toUnits[countToUnit]

									if (fromUnit != toUnit) {

										if (scheme != 'elevQuantWrtPaeMin') {

											toCent <- centroids[divisionPolyNames == toUnit, ]
											toCent <- gCentroid(toCent)
											x1 <- coordinates(toCent)[1]
											y1 <- coordinates(toCent)[2]

										} else {

											if (toUnit == 'Highest') {
												y1 <- y - 0 * mult
											} else if (toUnit == 'High') {
												y1 <- y - 1 * mult
											} else if (toUnit == 'Middle') {
												y1 <- y - 2 * mult
											} else if (toUnit == 'Low') {
												y1 <- y - 3 * mult
											} else if (toUnit == 'Lowest') {
												y1 <- y - 4 * mult
											}

											y1 <- y1 - 1E5

										}

										# estimate if significant difference between same-unit model and cross-unit model
										if (genre == 'performance') vsSelf <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == fromValance & statFrame$toUnit == fromUnit, resp]

										vsOther <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == toValance & statFrame$toUnit == toUnit, resp]

										vsOther <- median(vsOther, na.rm=TRUE)
										vsOther <- round(vsOther, 2)
										vsOther <- format(c(vsOther, 0.123456789), digits=2)[1]
										text(x1, y1, labels=vsOther, cex=statCexOther, xpd=NA)

									}

								} # next "to" unit

								# label performance vs self
								if (genre == 'performance') {

									if (fromUnit != 'all') {
										if (scheme != 'elevQuantWrtPaeMin') {
											fromCent <- centroids[divisionPolyNames == fromUnit, ]
										} else {
											if (fromUnit == 'Highest') {
												y1 <- y - 0 * mult
											} else if (fromUnit == 'High') {
												y1 <- y - 1 * mult
											} else if (fromUnit == 'Middle') {
												y1 <- y - 2 * mult
											} else if (fromUnit == 'Low') {
												y1 <- y - 3 * mult
											} else if (fromUnit == 'Lowest') {
												y1 <- y - 4 * mult
											}

											y1 <- y1 - 1E5

											fromCent <- matrix(c(x, y1), nrow=1)
										}
									} else {
										fromCent <- allCoords
									}

									vsSelf <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == fromValance & statFrame$toUnit == fromUnit, resp]
									vsSelf <- median(vsSelf, na.rm=TRUE)
									vsSelf <- round(vsSelf, 2)
									vsSelf <- format(c(vsSelf, 0.123456789), digits=2)[1]

									text(fromCent, labels=vsSelf, cex=statCexSelf, xpd=NA)

								}

								### performance of other vs "all"
								if (fromValance == 'including' & toValance == 'including' & fromUnit != 'all') {

									plot(allPoly, add=TRUE, col=alpha('gray', fadeFillAll), border=alpha('gray', fadeBorderAll), lwd=lwd)
									text(allCoords + cbind(0, latOffset), labels='Composite', cex=unitLabelCex, xpd=NA)

									vsOther <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == toValance & statFrame$toUnit == 'all', resp]

									vsOther <- median(vsOther, na.rm=TRUE)
									vsOther <- round(vsOther, 2)
									vsOther <- format(c(vsOther, 0.123456789), digits=2)[1]
									text(allCoords, labels=vsOther, cex=statCexOther, xpd=NA)

								} # if tested against ALL unit

							} # next fromUnit

						dev.off()

					} # next toValance

				} # next fromValance

			} # next ALGORITHM

		} # next PME

	} # next division scheme

}

########################################################################################
### statistically adjust performance of ENSEMBLES to control for nuisance parameters ###
########################################################################################

if (do %in% 'statistically adjust performance of ENSEMBLES to control for nuisance parameters') {

	say('statistically adjust performance of ENSEMBLES to control for nuisance parameters', level=1)

	# generalization
	mopPerc <- 1
	
	
	mopNice <- gsub(as.character(mopPerc), pattern='[.]', replacement='p')
	
	### load ENM evaluations
	########################
	masterEval <- data.frame()

	# by SCHEME
	for (scheme in schemes) {

		schemeNice <- schemeInfo(scheme)$schemeNice
		schemeShort <- schemeInfo(scheme)$schemeShort

		# by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# by ALGORITHM
			for (thisAlgo in algos) {

				load(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ', toupper(thisAlgo), ' - MOP-Adjusted.Rdata'))
				masterEval <- rbind(masterEval, evalFrame)

			} # next alogorithm

		} # next PME

	} # next scheme

	say('ADJUSTING PERFORMANCE BASED ON CONFOUNDING VARIABLES')
	say('Candidate confounding variables:')
	confVars <- c('distScaled', 'distLog', 'temporalDiff', 'temporalDiffLog', 'presTrainScaled', 'presTestScaled')

	adjFrame <- masterEval

	# calculate covariates
	# adjFrame$cbi <- logitAdj(0.5 + (adjFrame$cbiFromTo / 2))
	adjFrame$cbiTest <- (adjFrame$cbiTest_mop1 + 1) / 2
	adjFrame$distScaled <- as.numeric(scale(adjFrame[ , paste0('trainTestGeogDist_mop', mopNice)]))
	adjFrame$distLog <- as.numeric(scale(log10(adjFrame[ , paste0('trainTestGeogDist_mop', mopNice)])))
	adjFrame$temporalDiff <- as.numeric(scale(adjFrame[ , paste0('trainTestTemporalDiffs_mop', mopNice)]))
	adjFrame$temporalDiffLog <- as.numeric(scale(log10(abs(adjFrame[ , paste0('trainTestTemporalDiffs_mop', mopNice)]))))
	adjFrame$presTrainScaled <- as.numeric(scale(adjFrame[ , paste0('presTrain_mop', mopNice)]))
	adjFrame$presTestScaled <- as.numeric(scale(adjFrame[ , paste0('presTest_mop', mopNice)]))

	# examine collinearity
	vif <- usdm::vifstep(adjFrame[ , confVars])

	excluded <- vif@excluded
	if (length(excluded) > 0) {
		say('Removing ', paste(excluded, collapse=' '), ' because of high VIF.', pre=1)
		confVars <- confVars[-which(confVars %in% vif@excluded)]
	}


	### BETA model
	fullModelCbi <- betareg::betareg(
		cbiTest ~
			distScaled +
			temporalDiff +
			presTrainScaled +
			presTestScaled +
			distScaled:temporalDiff +
			distScaled:presTrainScaled +
			distScaled:presTestScaled +
			distScaled:presTrainScaled +
			distScaled:presTestScaled +
			temporalDiff:presTrainScaled +
			temporalDiff:presTestScaled +
			presTrainScaled:presTestScaled |
			distScaled +
			temporalDiff +
			presTrainScaled +
			presTestScaled +
			distScaled:temporalDiff +
			distScaled:presTrainScaled +
			distScaled:presTestScaled +
			distScaled:presTrainScaled +
			distScaled:presTestScaled +
			temporalDiff:presTrainScaled +
			temporalDiff:presTestScaled +
			presTrainScaled:presTestScaled,
		data=adjFrame, na.action=stats::na.fail, link='loglog'
	)

	# select best model across all possible models
	allModelsCbi <- MuMIn::dredge(
		global.model=fullModelCbi,
		rank='AICc',
		trace=TRUE
	)

	# # # # LINEAR model

	# # # # fullModel <- lm(cbi ~ scheme + pme + scheme:pme + scheme:fromUnit + scheme:toUnit + pme:fromUnit + pme:toUnit + pme:fromUnit:toUnit + fromUnit:toUnit + scheme:fromUnit:toUnit + distScaled + distLog + temporalDiffLog + distScaled:temporalDiffLog + distLog:temporalDiffLog + nTrainScaled + nTestScaled, data=adjFrame, na.action=stats::na.fail)
	# # # fullModel <- lm(cbi ~ distScaled + distLog + temporalDiffLog + distScaled + distLog + distScaled:temporalDiffLog + distLog:temporalDiffLog + nTrainScaled + nTestScaled, data=adjFrame, na.action=stats::na.fail)

	# # # allModels <- MuMIn::dredge(
		# # # global.model=fullModel,
		# # # rank='AICc',
		# # # trace=FALSE
	# # # )
	# # # bestModel <- MuMIn::get.models(allModels, subset = 1)[[1]]


	# # get best model
	# bestModelCbi <- MuMIn::get.models(allModelsCbi, subset = 1)[[1]]

	# sink(paste0(workDir, '/ENMs - PCs/!Beta Regression Model for Adjusting Performance.txt'), split=TRUE)

		# say('CBI: FULL MODEL', pre=2, level=1)
		# print(summary(fullModelCbi))

		# say('CBI: ALL MODELS', pre=2, level=1)
		# print(allModelsCbi)

		# say('CBI: BEST MODEL', pre=2, level=1)
		# print(summary(bestModelCbi))

	# sink()

	# adjFrame$distScaled <- median(adjFrame$distScaled)
	# adjFrame$distLog <- median(adjFrame$distLog)
	# adjFrame$temporalDiff <- median(adjFrame$temporalDiff)
	# adjFrame$temporalDiffLog <- median(adjFrame$temporalDiffLog)
	# adjFrame$nTrainScaled <- median(adjFrame$nTrainScaled)
	# adjFrame$nTestScaled <- median(adjFrame$nTestScaled)

	# masterEval$cbiFromToPred <- predict(bestModel, adjFrame)
	# masterEval$cbiFromToPred <- 2 * masterEval$cbiFromToPred - 1
	# masterEval$cbiFromToResid <- masterEval$cbiFromTo - masterEval$cbiFromToPred

	# out <- list()
	# out$masterEval <- masterEval
	# out$cbi$modelType <- 'beta regression on CBI (enmSdm) with non-constant variance'
	# out$cbi$fullModel <- fullModelCbi
	# out$cbi$allModels <- allModelsCbi
	# out$cbi$bestModel <- bestModelCbi

	# saveRDS(out, paste0(workDir, 'ENMs - PCs/!Collated Evaluation Results for ENSEMBLE ENMs - Statistically Adjusted for Nuisance Variables Using Beta Regression.rds'))

}

############################################################
### ensemble map with statistically-adjusted performance ###
############################################################

if ('ensemble map (with statistically-adjusted performance?)' %in% do) {

	say('ensemble map (with statistically-adjusted performance?)', level=1)

	resp <- 'cbiFromTo' # normal CBI
	# resp <- 'cbiFromToResid' # statistically adjusted

	###########
	### map ###
	###########

	genreNice <- if (genre == 'performance') {
		'ENM Performance'
	}

	# site for ALL unit on map
	allCoords <- cbind(-880018.7, 6640130)

	if (resp == 'cbiFromToResid') {

		evalFrame <- readRDS(paste0(workDir, 'ENMs - PCs/!Collated Evaluation Results for ENSEMBLE ENMs - Statistically Adjusted for Nuisance Variables.rds'))
		evalFrame <- evalFrame$evalFrame

	} else {

		evalFrame <- data.frame()

		# by SCHEME
		for (scheme in c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')) {

			schemeNice <- schemeInfo(scheme)$schemeNice
			schemeShort <- schemeInfo(scheme)$schemeShort

			# by PME
			for (pme in c('pmeMin', 'pmeNone')) {

				pmeNice <- pmeNiceName(pme)

				# by ALGORITHM
				for (thisAlgo in c('ensemble')) {

					thisEval <- readRDS(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ', toupper(thisAlgo), ' updated CBI.rds'))
					evalFrame <- rbind(evalFrame, thisEval)

				} # next alogorithm

			} # next PME

		} # next scheme

	}


	# make polygon representing ALL clade
	allPoly <- SpatialPoints(cbind(allCoords), getCRS('climateNA', TRUE))
	allPoly <- gBuffer(allPoly, width=150000)

	pres <- getPres()

	gadm <- readOGR(paste0(drive, 'ecology/Political Geography/GADM/ver2pt8/WGS84'), 'USA_adm1', verbose=FALSE)
	gadm <- sp::spTransform(gadm, getCRS('climateNA', TRUE))

	# by SCHEME
	for (scheme in schemes) {

		# graphical layout
		if (scheme == 'cladeNonOverlap' | scheme == 'physioFenneman') {

			# figure layout and dimensions
			ea <- 4
			layout <- matrix(
				c(
					rep(c(1, rep(2:4, each=ea)), ea),
					rep(c(1, rep(5:7, each=ea)), ea)
				),
				byrow=FALSE, ncol=2 * ea
			)

			width <- 1700
			height <- 2200

			unitLabels <- TRUE # labels units with name

			statCexOther <- 0.7 # model performance statistic (vs another unit)
			statCexSelf <- 1.2 # model performance statistic (vs self)

			unitLabelCex <- 0.5 # unit label size
			cexMain <- 0.7 # title of a panel
			lineMain <- -0.5 # offset of panel title
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 0 # rotation of title for entire plot

			latOffset <- 170000 # distance by which to "raise" performance statistic text on map

			fadeFill <- 0.3 # transparancy
			fadeFillAll <- 0.3 # transparancy
			fadeBorder <- 1 # transparancy
			fadeBorderAll <- 1 # transparancy

			lwd <- 0.5 # line width

		} else if (scheme == 'ecoregionEpa3Modified') {

			# figure layout and dimensions
			ea <- 4
			layout <- matrix(
				c(
					rep(c(1, rep(2:6, each=ea)), ea),
					rep(c(1, rep(7:11, each=ea)), ea),
					rep(c(1, rep(12:16, each=ea)), ea)
				),
				byrow=TRUE, nrow=3 * ea
			)

			width <- 2600
			height <- 1700

			unitLabels <- FALSE # labels units with name

			statCexOther <- 0.45 # model performance statistic (vs another unit)
			statCexSelf <- 0.6 # model performance statistic (vs self)

			unitLabelCex <- 0.3 # unit label size
			cexMain <- 0.3 # title of a panel
			lineMain <- -0.3 # offset for panel title
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 90 # rotation of title for entire plot

			latOffset <- 170000 # distance by which to "raise" performance statistic text on map

			fadeFill <- 0.7 # transparancy
			fadeFillAll <- 0.7 # transparancy
			fadeBorder <- 1 # transparancy
			fadeBorderAll <- 1 # transparancy

			lwd <- 0.5 # line width

		} else if (scheme == 'elevQuantWrtPaeMin') {

			# figure layout and dimensions
			ea <- 4
			layout <- matrix(
				c(
					rep(c(1, rep(2:4, each=ea)), ea),
					rep(c(1, rep(5:7, each=ea)), ea)
				),
				byrow=FALSE, ncol=2 * ea
			)

			width <- 1700
			height <- 2200

			unitLabels <- TRUE # labels units with name

			statCexOther <- 0.45 # model performance statistic (vs another unit)
			statCexSelf <- 0.7 # model performance statistic (vs self)

			unitLabelCex <- 0.4 # unit label size
			cexMain <- 0.7 # title of a panel
			lineMain <- -0.5 # offset for panel title
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 0 # rotation of title for entire plot

			latOffset <- 170000 # distance by which to "raise" performance statistic text on map

			fadeFill <- 0.1 # transparancy
			fadeFillAll <- 0.3 # transparancy
			fadeBorder <- 0.1 # transparancy
			fadeBorderAll <- 1 # transparancy

			lwd <- 0.5 # line width

			# anchor coordinates for unit names and performance
			x <- -2632550
			y <- 6784494
			mult <- 3.2E5 # y coordinate adjustment for each unit label

		}

		# get scheme info and division polygon
		out <- schemeInfo(scheme, poly=TRUE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		divisionPoly <- out$divisionPoly
		rm(out); gc()

		### process division polygon... get only units with presences

		unitNames <- as.data.frame(divisionPoly)[ , divisionFieldPoly]
		divisionPoly <- divisionPoly[order(unitNames), ]
		unitNames <- sort(unitNames)
		divisionPoly <- divisionPoly[unitNames %in% getUnits(scheme=scheme, incAll=FALSE), ]
		if (scheme == 'ecoregionEpa3Modified') {
			unitNames <- unique(as.data.frame(divisionPoly)[ , divisionFieldPoly])
			divisionPoly <- gUnaryUnion(divisionPoly, id=divisionPoly$L3_KEY)
			divisionPoly$L3_KEY <- unitNames
		}
		divisionPolyDf <- as.data.frame(divisionPoly)
		divisionPolyNames <- divisionPolyDf[ , divisionFieldPoly]
		divisionPoly <- sp::spTransform(divisionPoly, getCRS('climateNA', TRUE))

		centroids <- gCentroid(divisionPoly, byid=TRUE)

		# by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# by ALGORITHM
			for (algo in algos) {

				### get and process evaluation data
				statFrame <- evalFrame[evalFrame$scheme == scheme & evalFrame$pme == pme & evalFrame$algorithm == algo, ]

				if (genre == 'performance') {

					statFrame <- aggregate(statFrame, by=list(statFrame$scheme, statFrame$pme, statFrame$fromValance, statFrame$fromUnit, statFrame$toValance, statFrame$toUnit), FUN=median, na.rm=TRUE)
					statFrame$scheme <- statFrame$pme <- statFrame$fromValance <- statFrame$fromUnit <- statFrame$toValance <- statFrame$toUnit <- NULL
					names(statFrame)[1:6] <- c('scheme', 'pme', 'fromValance', 'fromUnit', 'toValance', 'toUnit')

				}

				# by "from" VALANCE
				for (fromValance in valances) {

					# by "to" VALANCE
					for (toValance in valances) {

						# get names of focal units to include/exclude
						fromUnits <- getUnits(scheme=scheme, incAll=(fromValance == 'including' & toValance == 'including'))

						say(Sys.time(), ' SCHEME ', schemeNice, ' | PME ', pmeNice, ' | FROM VALANCE ', fromValance, ' | TO VALANCE ', toValance, '| ', toupper(algo))

						pngFile <- paste0(workDir, 'ENMs - PCs/Multivariate ENM on PCs Comparison - ', schemeNice, ' - ', pmeNice, ' PME - ', genreNice, ' - ', valanceShort(fromValance), ' vs ', valanceShort(toValance), ' - CBI - ', toupper(algo), ifelse(resp == 'cbiFromToResid', ' - (Residuals after Statistical Adjustment)', ''), '.png')

						png(pngFile, width=width, height=height, res=600)

							par(layout(layout), mar=0.1 * c(5, 4, 4, 2) + 0)

							plot(0, 0, col='white', fg='white', col.main='white', col.lab='white', col.axis='white')
							text(0, 0, labels=paste0(toupper(genreNice), ' - ', schemeNice, ' - ', pmeNice, ' PME - ', valanceShort(fromValance), ' vs ', valanceShort(toValance), '\nCBI (Adjusted) - ', toupper(algo)), srt=srtBigMain, cex=cexBigMain, xpd=NA)
							title(sub=Sys.time(), cex.sub=0.2, outer=TRUE)

							# for each unit of analysis
							for (countFromUnit in seq_along(fromUnits)) {

								### make base map
								fromUnit <- fromUnits[countFromUnit]
								theseUnitCols <- getUnitCols(divisionPolyDf[ , divisionFieldPoly])

								# plot geography
								plot(divisionPoly, border='white') # template
								plot(gadm, add=TRUE, lwd=lwd / 3, border='gray30')
								plot(divisionPoly, add=TRUE, col=alpha(theseUnitCols, fadeFill), border=alpha(theseUnitCols, fadeBorder), lwd=lwd)
								plot(divisionPoly[which(divisionPolyDf[ , divisionFieldPoly] %in% fromUnit), ], col=getUnitCols(fromUnit), border=alpha('black', fadeBorder), lwd=lwd, add=TRUE)
								title(main=niceUnitName(scheme, fromUnit), line=lineMain, xpd=NA, cex.main=cexMain)

								# plot "all" unit
								if (fromUnit == 'all') {
									plot(allPoly, add=TRUE, col=alpha('gray', fadeFillAll), border=alpha('black', 0.5), lwd=lwd)
									text(allCoords + cbind(0, latOffset), labels='Composite', cex=unitLabelCex, xpd=NA)
								}

								# label units
								if (unitLabels) {
									if (scheme != 'elevQuantWrtPaeMin') {
										x1 <- coordinates(centroids)[ , 1]
										y1 <- coordinates(centroids)[ , 2] + latOffset
										labs <- niceUnitName(scheme, divisionPolyDf[ , divisionFieldPoly])
									} else {
										x1 <- x
										y1 <- y - (0:4 * mult)
										labs <- c('Highest', 'High', 'Middle', 'Low', 'Lowest')
									}
									text(x1, y1, labels=labs, cex=unitLabelCex, xpd=NA)
								}

								### label each unit with performance statistic from model trained on "from" unit
								toUnits <- getUnits(scheme=scheme, incAll=FALSE)

								for (countToUnit in seq_along(toUnits)) {

									toUnit <- toUnits[countToUnit]

									if (fromUnit != toUnit) {

										if (scheme != 'elevQuantWrtPaeMin') {

											toCent <- centroids[divisionPolyNames == toUnit, ]
											toCent <- gCentroid(toCent)
											x1 <- coordinates(toCent)[1]
											y1 <- coordinates(toCent)[2]

										} else {

											if (toUnit == 'Highest') {
												y1 <- y - 0 * mult
											} else if (toUnit == 'High') {
												y1 <- y - 1 * mult
											} else if (toUnit == 'Middle') {
												y1 <- y - 2 * mult
											} else if (toUnit == 'Low') {
												y1 <- y - 3 * mult
											} else if (toUnit == 'Lowest') {
												y1 <- y - 4 * mult
											}

											y1 <- y1 - 1E5

										}

										# estimate if significant difference between same-unit model and cross-unit model
										if (genre == 'performance') vsSelf <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == fromValance & statFrame$toUnit == fromUnit, resp]

										vsOther <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == toValance & statFrame$toUnit == toUnit, resp]

										# is difference between self and other significant?
										# sig <- estPerformSig(vsSelf=vsSelf, vsOther=vsOther, n=100)

										vsOther <- median(vsOther, na.rm=TRUE)
										vsOther <- round(vsOther, 2)
										vsOther <- format(c(vsOther, 0.123456789), digits=2)[1]
										# if (sig) vsOther <- paste0(vsOther, '*')
										text(x1, y1, labels=vsOther, cex=statCexOther, xpd=NA)

									}

								} # next "to" unit

								# label performance vs self
								if (genre == 'performance') {

									if (fromUnit != 'all') {
										if (scheme != 'elevQuantWrtPaeMin') {
											fromCent <- centroids[divisionPolyNames == fromUnit, ]
										} else {
											if (fromUnit == 'Highest') {
												y1 <- y - 0 * mult
											} else if (fromUnit == 'High') {
												y1 <- y - 1 * mult
											} else if (fromUnit == 'Middle') {
												y1 <- y - 2 * mult
											} else if (fromUnit == 'Low') {
												y1 <- y - 3 * mult
											} else if (fromUnit == 'Lowest') {
												y1 <- y - 4 * mult
											}

											y1 <- y1 - 1E5

											fromCent <- matrix(c(x, y1), nrow=1)
										}
									} else {
										fromCent <- allCoords
									}

									vsSelf <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == fromValance & statFrame$toUnit == fromUnit, resp]
									vsSelf <- median(vsSelf, na.rm=TRUE)
									vsSelf <- round(vsSelf, 2)
									vsSelf <- format(c(vsSelf, 0.123456789), digits=2)[1]

									text(fromCent, labels=vsSelf, cex=statCexSelf, xpd=NA)

								}

								### performance of other vs "all"
								if (fromValance == 'including' & toValance == 'including' & fromUnit != 'all') {

									plot(allPoly, add=TRUE, col=alpha('gray', fadeFillAll), border=alpha('gray', fadeBorderAll), lwd=lwd)
									text(allCoords + cbind(0, latOffset), labels='Composite', cex=unitLabelCex, xpd=NA)

									vsOther <- statFrame[statFrame$scheme == scheme & statFrame$pme == pme & statFrame$fromValance == fromValance & statFrame$fromUnit == fromUnit & statFrame$toValance == toValance & statFrame$toUnit == 'all', resp]

									vsOther <- median(vsOther, na.rm=TRUE)
									vsOther <- round(vsOther, 2)
									vsOther <- format(c(vsOther, 0.123456789), digits=2)[1]
									text(allCoords, labels=vsOther, cex=statCexOther, xpd=NA)

								} # if tested against ALL unit

							} # next fromUnit

						dev.off()

					} # next toValance

				} # next fromValance

			} # next ALGORITHM

		} # next PME

	} # next division scheme

}

#########################################################################################
### visualize multivariate PC ENM analysis - boxplots of within and among performance ###
#########################################################################################

if ('boxplot within AND among' %in% do) {

	say('visualize multivariate PC ENM analysis - boxplots of within and among performance', level=1)

	testStat <- 'cbiFromTo' # raw CBI
	# testStat <- 'cbiFromToResid' # observed CBI minus statistically-adjusted CBI (for unit A k-fold B model this is CBI of that model tested against unit A's k-fold B test data MINUS all CBIs of other-unit k-folds)
	# testStat <- 'sensTholdMaxSumSSFromTo' # Se @ MSSS minus Se @ MSSS (for unit A k-fold B model this is Se @ MSSS of that model tested against unit A's k-fold B test data MINUS all Se's @ MSSS of other-unit k-folds)

	colWithin <- 'darkblue'
	colAmong <- 'darkred'

	say('Create a boxplot with two boxes per division scheme, one showing performance *within* a unit and another *across* units.')
	say('Using ', toupper(algos), ' with ', testStat, '!!!')

	if (testStat == 'cbiFromTo') {
		testStatNice <- 'CBI'
		ylim <- c(-1, 1)
	} else if (testStat == 'cbiFromToResid') {
		testStatNice <- 'Adjusted CBI'
		ylim <- c(-1, 1)
	} else if (testStat == 'sensTholdMaxSumSSFromTo') {
		testStatNice <- 'Sensitivity'
		ylim <- c(0.2, 1)
	}


	offset <- 0.13

	# evalFrame <- readRDS(paste0(workDir, 'ENMs - PCs/!Collated Evaluation Results for ENSEMBLE ENMs - Statistically Adjusted for Nuisance Variables.rds'))
	# evalFrame <- evalFrame$evalFrame

	### load ENM evaluations
	########################
	evalFrame <- data.frame()

	# by SCHEME
	for (scheme in c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')) {

		schemeNice <- schemeInfo(scheme)$schemeNice
		schemeShort <- schemeInfo(scheme)$schemeShort

		# by PME
		for (pme in c('pmeMin', 'pmeNone')) {

			pmeNice <- pmeNiceName(pme)

			# by ALGORITHM
			for (thisAlgo in c('ensemble')) {

				thisEval <- readRDS(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ', toupper(thisAlgo), ' updated CBI.rds'))
				evalFrame <- rbind(evalFrame, thisEval)

			} # next alogorithm

		} # next PME

	} # next scheme

	### plot
	png(paste0(workDir, './ENMs - PCs/!Within versus Among Performance - ', testStatNice, '.png'), width=2000, height=1200, res=300)

		# position for each boxplot
		par(mfrow=c(1, 2), mar=0.5 * c(7, 7, 4, 0) + 0.1, mgp=c(2, 1, 0), cex.main=0.6, cex.axis=0.6, cex.lab=0.8)

		# by PME
		for (pmeVariant in pmes) {

			pmeNice <- pmeNiceName(pmeVariant)

			plot(1:4, 1:4, col='white', xlim=c(0.5, 4.5), ylim=ylim, xaxt='n', main=paste(pmeNiceNamePrint(pmeVariant) , 'Background'), xlab='', ylab=testStatNice)
			axis(side=1, at=1:4, labels=FALSE, tck=-0.03)
			if (testStat %in% c('cbiFromTo', 'cbiFromToResid')) lines(c(0.5, 4.5), c(0, 0), col='gray')

			at <- 1

			# by SCHEME
			for (countScheme in seq_along(schemes)) {

				scheme <- schemes[countScheme]
				out <- schemeInfo(scheme)
				schemeNice <- out$schemeNice
				schemeShort <- out$schemeShort

				### get and process evaluation data
				thisEvalFrame <- evalFrame[evalFrame$scheme == scheme & evalFrame$pme == pmeVariant & evalFrame$algorithm == algos & evalFrame$fromUnit != 'all' & evalFrame$toUnit != 'all', ]
				thisEvalFrameAgg <- aggregate(thisEvalFrame, by=list(thisEvalFrame$fromUnit, thisEvalFrame$toUnit), FUN=mean)
				thisEvalFrameAgg$fromUnit <- thisEvalFrameAgg$toUnit <- NULL
				names(thisEvalFrameAgg)[1:2] <- c('fromUnit', 'toUnit')

				withinFrame <- thisEvalFrameAgg[thisEvalFrameAgg$fromUnit == thisEvalFrameAgg$toUnit, ]
				amongFrame <- thisEvalFrameAgg[thisEvalFrameAgg$fromUnit != thisEvalFrameAgg$toUnit, ]

				within <- withinFrame[ , testStat]
				among <- amongFrame[ , testStat]

				### plot results

				# scheme label
				text(x=at, y=ylim[1] - 0.225 * diff(ylim), xpd=NA, labels=schemeNice, srt=0, cex=0.6, adj=c(0.5, 1))

				# within
				boxplot(within, at=at - offset, offset=TRUE, col=alpha(colWithin, 0.6), border=colWithin, add=TRUE)
				text(x=at - offset, y=ylim[1] - 0.075 * diff(ylim), xpd=NA, labels='Within', srt=90, adj=1, cex=0.6, col=colWithin)
				# points(at - offset, mean(within))

				# among
				boxplot(among, at=at + offset, offset=TRUE, col=alpha(colAmong, 0.6), border=colAmong, add=TRUE)
				text(x=at + offset, y=ylim[1] - 0.075 * diff(ylim), xpd=NA, labels='Among', srt=90, adj=1, cex=0.6, col=colAmong)
				# points(at + offset, mean(among))

				at <- at + 1

			} # next division scheme

		} # next PME

	dev.off()
}

########################################################################
### statistical analysis of difference between (within - among) vs 0 ###
########################################################################

if ('statistical analysis of difference between (within - among) vs 0' %in% do) {

	say('statistical analysis of difference between (within - among) vs 0', level=1)

	say('I will use randomization tests to determine the statistical significance of differences between schemes and PMEs. The response metric will be the average difference of within-unit performance minus among-unit performance (CBI). Randomization tests are the appropriate test because schemes/PMEs are not strictly independent since they share they same generative presence data.  The difference is the appropriate metric since the dividend can have undesirable properties because CBI can equal 0 or values < 0. Note that random within/among values will be drawn such that the probability of drawing *any* within values is equal to the probability of drawing the among value.', breaks=90, post=2)
	say('To test for differences of scheme-PME sets from 0 I will permute each set of within-among measures (randomly assigning which value is within/among--with weighted probabilities).', post=2)
	say('To test for differences within schemes between PME sets I will randomly assign a within-unit measure to each k-fold drawn from either the observed measure of the measure from the same scheme but opposing PME.', post=2)

	# number of times to iterate drawing values for null model tests
	iters <- 1000

	testStat <- 'cbiTest'
	
	mopPerc <- c(0.5, 1)
	# mopPerc <- c(1)
	
	# TRUE ==> draw within/among performance with equal probability per value; FALSE ==> draw so probability of pulling a within is equal to probability of pulling any of the amongs
	weightedDraws <- TRUE
	# weightedDraws <- FALSE
	
	### load ENM evaluations
	########################
	master <- data.frame()

	# by SCHEME
	for (scheme in c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')) {

		schemeNice <- schemeInfo(scheme)$schemeNice
		schemeShort <- schemeInfo(scheme)$schemeShort

		# by PME
		for (pme in c('pmeMin', 'pmeNone')) {

			pmeNice <- pmeNiceName(pme)

				file <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ENSEMBLE - MOP-Adjusted.Rdata')
				load(file)
				master <- rbind(master, evalFrame)

		} # next PME

	} # next scheme

	### Is each scheme-by-PME set different from 0?
	###############################################

	say('Is each scheme-by-PME set different from 0?', level=2)

	stats <- data.frame()

	# by MOP
	for (thisMopPerc in mopPerc) {

		say('Using ', toupper(algos), ' with MOP% ', thisMopPerc, '!!!', pre=2)

			# by SCHEME
			for (scheme in schemes) {

				units <- getUnits(scheme, FALSE)

				# by PME
				for (pme in pmes) {

					say(scheme, ' ', pme)

					### OBSERVED differences by SCHEME x PME
					########################################

					# matrix to contain OBSERVED within-among (from-to) values... rows are training set, columns are test set
					obsDiff <- matrix(NA, nrow=length(units), ncol=length(units))
					colnames(obsDiff) <- rownames(obsDiff) <- units
					
					# by FROM unit
					for (fromUnit in units) {

						toUnits <- units[-which(units %in% fromUnit)]

						# by TO unit
						for (toUnit in toUnits) {

							# holds observed/randomized differences between each from unit's k-fold and to-unit's kfoldS (note plural!), will have one value per training k-fold
							diffFromToByFold <- rep(NA, kFolds)
							names(diffFromToByFold) <- paste0(fromUnit, '_', 1:kFolds)

							# by K FOLD
							for (k in 1:kFolds) {

								# observed difference
								withinFrame <- master[master$scheme == scheme & master$pme == pme & master$fromUnit == fromUnit & master$toUnit == fromUnit & master$kFrom == k & master$kTo == k, ]
								within <- withinFrame[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

								amongFrame <- master[master$scheme == scheme & master$pme == pme & master$fromUnit == fromUnit & master$toUnit == toUnit & master$kFrom == k, ]
								among <- amongFrame[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

								diffFromToByFold[k] <- mean(within - among)

							} # next k fold

							obsDiff[fromUnit, toUnit] <- mean(diffFromToByFold)

						} # next TO unit

					} # next from unit

					# say(mean(obsDiff, na.rm=TRUE))

					### RANDOMIZED DIFFERENCES - test if set of differences for each scheme x PME is > 0
					####################################################################################

					# holds randomized differences, one matrix of randomized differences per iteration
					randDiff <- list()
					
					# by ITERATION
					for (i in 1:iters) {
					
						randDiff[[i]] <- NA * obsDiff

						# by FROM unit
						for (fromUnit in units) {

							toUnits <- units[-which(units %in% fromUnit)]

							# by TO unit
							for (toUnit in toUnits) {

								# holds observed/randomized differences between each from unit's k-fold and to-unit's kfoldS (note plural!), will have one value per training k-fold
								diffFromToByFold <- rep(NA, kFolds)
								names(diffFromToByFold) <- paste0(fromUnit, '_', 1:kFolds)

								# by K FOLD
								for (k in 1:kFolds) {

									# observed difference
									withinFrame <- master[master$scheme == scheme & master$pme == pme & master$fromUnit == fromUnit & master$toUnit == fromUnit & master$kFrom == k & master$kTo == k, ]
									within <- withinFrame[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

									amongFrame <- master[master$scheme == scheme & master$pme == pme & master$fromUnit == fromUnit & master$toUnit == toUnit & master$kFrom == k, ]
									among <- amongFrame[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

									# randomize which value is within and which are among (without replacement)
									withinAmongSet <- c(within, among)

									w <- if (weightedDraws) { 
										# weight so equally likely to draw the one within or among all among
										c(1, rep(1 / length(among), length(among)))
									} else {
										# weigh so any value equally likely
										rep(1, 1 + length(among))
									}
									
									withinAmongSet <- sample(withinAmongSet, length(withinAmongSet), replace=FALSE, prob=w)
									randWithin <- withinAmongSet[1]
									randAmong <- withinAmongSet[2:(kFolds + 1)]

									diffFromToByFold[k] <- mean(randWithin - randAmong)

								} # next k fold

								randDiff[[i]][fromUnit, toUnit] <- mean(diffFromToByFold)

							} # next TO unit

						} # next from unit

					} # next iteration

					# STATISTICAL TEST

					# see p. 2164 in Swenson, N.G. and Enquist, B.J.  2009.  Opposing assembly mechanisms in a Neotropical dry forest: implications for phylogenetic and functional community assembly.  Ecology 90:2161-2170.
					# This test's significance depends on sample size, which is arbitrary! So I'm also reporting the p-value assuming z is normally distributed.

					obsDiffCollapse <- rowMeans(obsDiff, na.rm=TRUE)
					randDiffCollapse <- lapply(randDiff, rowMeans, na.rm=TRUE)

					z <- rep(NA, iters)
					for (i in 1:iters) z[i] <- (mean(obsDiffCollapse, na.rm=TRUE) - mean(randDiffCollapse[[i]], na.rm=TRUE)) / sd(randDiffCollapse[[i]], na.rm=TRUE)

					# Wilcoxon test
					test <- wilcox.test(z, mu=0, alternative='two.sided')
					pWilcox <- test$p.val

					# test assuming normal distribution of within-among differences
					pNorm <- dnorm(mean(z))

					# test assuming permutation
					permuteDiff <- rep(NA, iters)
					for (i in 1:iters) {
						diffMat <- obsDiff - randDiff[[i]]
						permuteDiff[i] <- mean(rowMeans(diffMat, na.rm=TRUE))
					}
					pPermute <- 1 - sum(permuteDiff > 0) / iters

					thisStats <- data.frame(
						scheme=scheme,
						pme=pme,
						iters=iters,
						testStat=testStat,
						mopPerc=thisMopPerc,
						weightedDraws=weightedDraws,
						obsDiffMin=min(obsDiffCollapse),
						obsDiffMedian=median(obsDiffCollapse),
						obsDiffMean=mean(obsDiffCollapse),
						obsDiffMax=max(obsDiffCollapse),
						obsDiff=paste(obsDiffCollapse, collapse=' '),
						W=test$statistic,
						pWilcox=pWilcox,
						pNorm=pNorm,
						pPermute=pPermute
					)

					print(thisStats)
					
					stats <- rbind(stats, thisStats)

				} # next PME

			} # next division scheme

	} # next MOP value

	rownames(stats) <- 1:nrow(stats)
	write.csv(stats, paste0(workDir, 'ENMs - PCs/Randomization Test for Within Minus Among Differences from 0 using ', toupper(testStat), '.csv'), row.names=FALSE)

}

###################################################################################################
### statistical analysis of difference between (within - among) between PMEs of the same scheme ###
###################################################################################################

if ('statistical analysis of difference between (within - among) between PMEs of the same scheme' %in% do) {

	say('statistical analysis of difference between (within - among) between PMEs of the same scheme', level=1)

	say('I will use randomization tests to determine the statistical significance of differences between schemes and PMEs. The response metric will be the average difference of within-unit performance minus among-unit performance (CBI). Randomization tests are the appropriate test because schemes/PMEs are not strictly independent since they share they same generative presence data.  The difference between (within and among) is the appropriate metric since the dividend can have undesirable properties arising from the fact that CBI can equal 0 or values < 0. Note that random within/among values will be drawn such that the probability of drawing *any* within values is equal to the probability of drawing the among value.', breaks=90, post=2)
	say('To test for differences of scheme-PME sets from 0 I will permute each set of within-among measures (randomly assigning which value is within/among--with weighted probabilities).', post=2)
	say('To test for differences within schemes between PME sets I will randomly assign a within-unit measure to each k-fold drawn from either the observed measure of the measure from the same scheme but opposing PME.', post=2)

	# number of times to iterate drawing values for null model tests
	iters <- 1000

	testStat <- 'cbiTest'
	
	mopPerc <- c(0.5, 1)
	# mopPerc <- c(1)
	
	### load ENM evaluations
	########################
	master <- data.frame()

	# by SCHEME
	for (scheme in c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')) {

		schemeNice <- schemeInfo(scheme)$schemeNice
		schemeShort <- schemeInfo(scheme)$schemeShort

		# by PME
		for (pme in c('pmeMin', 'pmeNone')) {

			pmeNice <- pmeNiceName(pme)

				file <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ENSEMBLE - MOP-Adjusted.Rdata')
				load(file)
				master <- rbind(master, evalFrame)

		} # next PME

	} # next scheme

	### Is each PME-set within a scheme different from 0?
	#####################################################

	stats <- data.frame()

	# by MOP
	for (thisMopPerc in mopPerc) {

		say('Using ', toupper(algos), ' with MOP% ', thisMopPerc, '!!!', pre=2)

		# by SCHEME
		for (scheme in schemes) {

			say(scheme, ' ', post=0)

			units <- getUnits(scheme, FALSE)

			### OBSERVED differences by SCHEME x PME
			########################################

			# matrix to contain OBSERVED within-among (from-to) values... rows are training set, columns are test set
			obsDiffPmeNone <- obsDiffPmeMin <- matrix(NA, nrow=length(units), ncol=length(units))
			colnames(obsDiffPmeNone) <- rownames(obsDiffPmeNone) <- colnames(obsDiffPmeMin) <- rownames(obsDiffPmeMin) <- units
			
			# by FROM unit
			for (fromUnit in units) {

				toUnits <- units[-which(units %in% fromUnit)]

				# by TO unit
				for (toUnit in toUnits) {

					# holds observed/randomized differences between each from unit's k-fold and to-unit's kfoldS (note plural!), will have one value per training k-fold
					diffFromToByFoldPmeNone <- diffFromToByFoldPmeMin <- rep(NA, kFolds)
					names(diffFromToByFoldPmeNone) <- names(diffFromToByFoldPmeMin) <- paste0(fromUnit, '_', 1:kFolds)

					# by K FOLD
					for (k in 1:kFolds) {

						# observed difference
						withinFramePmeNone <- master[master$scheme == scheme & master$pme == 'pmeNone' & master$fromUnit == fromUnit & master$toUnit == fromUnit & master$kFrom == k & master$kTo == k, ]
						withinFramePmeMin <- master[master$scheme == scheme & master$pme == 'pmeMin' & master$fromUnit == fromUnit & master$toUnit == fromUnit & master$kFrom == k & master$kTo == k, ]
						withinPmeNone <- withinFramePmeNone[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]
						withinPmeMin <- withinFramePmeMin[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

						amongFramePmeNone <- master[master$scheme == scheme & master$pme == 'pmeNone' & master$fromUnit == fromUnit & master$toUnit == toUnit & master$kFrom == k, ]
						amongFramePmeMin <- master[master$scheme == scheme & master$pme == 'pmeMin' & master$fromUnit == fromUnit & master$toUnit == toUnit & master$kFrom == k, ]
						amongPmeNone <- amongFramePmeNone[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]
						amongPmeMin <- amongFramePmeMin[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

						diffFromToByFoldPmeNone[k] <- mean(withinPmeNone - amongPmeNone)
						diffFromToByFoldPmeMin[k] <- mean(withinPmeMin - amongPmeNone)

					} # next k fold

					obsDiffPmeNone[fromUnit, toUnit] <- mean(diffFromToByFoldPmeNone)
					obsDiffPmeMin[fromUnit, toUnit] <- mean(diffFromToByFoldPmeMin)

				} # next TO unit

			} # next from unit

			obsDiffBetweenPmes <- rowMeans(abs(obsDiffPmeNone - obsDiffPmeMin), na.rm=TRUE)
			obsDiffBetweenPmesCollapse <- mean(obsDiffBetweenPmes)
			
			say('obs diff = ', round(obsDiffBetweenPmesCollapse, 3))
			
			### RANDOMIZED differences by SCHEME x PME
			##########################################

			randDiffBetweenPmes <- randDiffBetweenPmesSd <- rep(NA, iters)
			
			# by ITERATION
			for (iter in 1:iters) {
				
				# matrix to contain OBSERVED within-among (from-to) values... rows are training set, columns are test set
				randDiffPmeNone <- randDiffPmeMin <- matrix(NA, nrow=length(units), ncol=length(units))
				colnames(randDiffPmeNone) <- rownames(randDiffPmeNone) <- colnames(randDiffPmeMin) <- rownames(randDiffPmeMin) <- units
				
				# by FROM unit
				for (fromUnit in units) {

					toUnits <- units[-which(units %in% fromUnit)]

					# by TO unit
					for (toUnit in toUnits) {

						# holds observed/randomized differences between each from unit's k-fold and to-unit's kfoldS (note plural!), will have one value per training k-fold
						diffFromToByFoldPmeNone <- diffFromToByFoldPmeMin <- rep(NA, kFolds)
						names(diffFromToByFoldPmeNone) <- names(diffFromToByFoldPmeMin) <- paste0(fromUnit, '_', 1:kFolds)

						# by K FOLD
						for (k in 1:kFolds) {

							# observed difference
							withinFramePmeNone <- master[master$scheme == scheme & master$pme == 'pmeNone' & master$fromUnit == fromUnit & master$toUnit == fromUnit & master$kFrom == k & master$kTo == k, ]
							withinFramePmeMin <- master[master$scheme == scheme & master$pme == 'pmeMin' & master$fromUnit == fromUnit & master$toUnit == fromUnit & master$kFrom == k & master$kTo == k, ]
							withinPmeNone <- withinFramePmeNone[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]
							withinPmeMin <- withinFramePmeMin[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

							amongFramePmeNone <- master[master$scheme == scheme & master$pme == 'pmeNone' & master$fromUnit == fromUnit & master$toUnit == toUnit & master$kFrom == k, ]
							amongFramePmeMin <- master[master$scheme == scheme & master$pme == 'pmeMin' & master$fromUnit == fromUnit & master$toUnit == toUnit & master$kFrom == k, ]
							amongPmeNone <- amongFramePmeNone[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]
							amongPmeMin <- amongFramePmeMin[ , paste0(testStat, '_mop', gsub(thisMopPerc, pattern='[.]', replacement='p'))]

							# sample by scrambling "within" between PMEs and "amongs" between PMEs... note this makes it more likely that the amongs are swapped versus the withins... what effect does this have?
							withinCombined <- c(withinPmeNone, withinPmeMin)
							amongCombined <- c(amongPmeNone, amongPmeMin)
			
							withinIndices <- sample(1:2, 2)
							amongIndices <- sample(seq_along(amongCombined), length(amongCombined))
							
							withinPmeNone <- withinCombined[withinIndices == 1]
							withinPmeMin <- withinCombined[withinIndices == 2]
							amongPmeNone <- amongCombined[amongIndices >= 1 & amongIndices <= 8]
							amongPmeMin <- amongCombined[amongIndices >= 9 & amongIndices <= 16]
							
							diffFromToByFoldPmeNone[k] <- mean(withinPmeNone - amongPmeNone)
							diffFromToByFoldPmeMin[k] <- mean(withinPmeMin - amongPmeNone)

						} # next k fold

						randDiffPmeNone[fromUnit, toUnit] <- mean(diffFromToByFoldPmeNone)
						randDiffPmeMin[fromUnit, toUnit] <- mean(diffFromToByFoldPmeMin)

					} # next TO unit

				} # next from unit

				randDiffBetweenPmes[iter] <- mean(rowMeans(abs(randDiffPmeNone - randDiffPmeMin), na.rm=TRUE))
				randDiffBetweenPmesSd[iter] <- sd(rowMeans(abs(randDiffPmeNone - randDiffPmeMin), na.rm=TRUE))

			} # next iteration

			# STATISTICAL TEST
			# see p. 2164 in Swenson, N.G. and Enquist, B.J.  2009.  Opposing assembly mechanisms in a Neotropical dry forest: implications for phylogenetic and functional community assembly.  Ecology 90:2161-2170.
			# This test's significance depends on sample size, which is arbitrary! So I'm also reporting the p-value assuming z is normally distributed.

			z <- rep(NA, iters)
			for (i in 1:iters) z[i] <- (obsDiffBetweenPmesCollapse - mean(randDiffBetweenPmes[i], na.rm=TRUE)) / randDiffBetweenPmesSd[[i]]

			# Wilcoxon test
			test <- wilcox.test(z, mu=0, alternative='two.sided')
			pWilcox <- test$p.val

			# test assuming normal distribution of within-among differences
			pNorm <- dnorm(mean(z))

			# test assuming permutation (1-tailed)
			pPermute <- sum(randDiffBetweenPmes >= obsDiffBetweenPmes) / iters

			thisStats <- data.frame(
				scheme=scheme,
				iters=iters,
				testStat=testStat,
				mopPerc=thisMopPerc,
				obsDiffMin=min(obsDiffBetweenPmes),
				obsDiffMedian=median(obsDiffBetweenPmes),
				obsDiffMean=mean(obsDiffBetweenPmes),
				obsDiffMax=max(obsDiffBetweenPmes),
				obsDiff=paste(obsDiffBetweenPmes, collapse=' '),
				W=test$statistic,
				pWilcox=pWilcox,
				pNorm=pNorm,
				pPermute=pPermute
			)

			print(thisStats)
			
			stats <- rbind(stats, thisStats)

		} # next scheme
		
	} # next MOP percent

	rownames(stats) <- 1:nrow(stats)
	write.csv(stats, paste0(workDir, 'ENMs - PCs/Randomization Test for Within Minus Among Differences within Scheme between PMEs using ', toupper(testStat), '.csv'), row.names=FALSE)

}

#############################################################################################################
### statistical analysis of difference between (within - among) between schemes/PMEs of different schemes ###
#############################################################################################################

if ('statistical analysis of difference between (within - among) between schemes/PMEs of different schemes' %in% do) {

	say('statistical analysis of difference between (within - among) between schemes/PMEs of different schemes', level=1)

	# number of times to iterate drawing values for null model tests
	iters <- 1000

	testStat <- 'cbiTest'
	
	weightedDraw <- TRUE
	# weightedDraw <- FALSE
	
	mopPerc <- c(0.5, 1)
	# mopPerc <- c(1)
	
	### load ENM evaluations
	########################
	master <- data.frame()

	# by SCHEME
	for (scheme in schemes) {

		schemeNice <- schemeInfo(scheme)$schemeNice
		schemeShort <- schemeInfo(scheme)$schemeShort

		# by PME
		for (pme in c('pmeMin', 'pmeNone')) {

			pmeNice <- pmeNiceName(pme)

			file <- paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ENSEMBLE - MOP-Adjusted.Rdata')
			load(file)
			master <- rbind(master, evalFrame)

		} # next PME

	} # next scheme

	### Is each PME-set within a scheme different from 0?
	#####################################################

	stats <- data.frame()

	# by MOP
	for (thisMopPerc in mopPerc) {

		say('Using ', toupper(algos), ' with MOP% ', thisMopPerc, '!!!', pre=2)

		# by SCHEME
		for (countScheme1 in 1:(length(schemes) - 1)) {

			scheme1 <- schemes[countScheme1]

			for (countScheme2 in (countScheme1 + 1):length(schemes)) {
		
				scheme2 <- schemes[countScheme2]
				
				for (pme1 in pmes) {
				
					for (pme2 in pmes) {
			
						say(scheme1, ' ', pme1, ' VS ', scheme2, ' ', pme2, post=0)

						### OBSERVED DIFFERENCES BETWEEN SCHEMES/PMES
						masterEval <- read.csv(paste0(workDir, '/ENMs - PCs/Randomization Test for Within Minus Among Differences from 0 using CBITEST.csv'))
						
						stats1 <- masterEval[masterEval$scheme == scheme1 & masterEval$pme == pme1 & masterEval$weightedDraw == weightedDraw, ]
						stats2 <- masterEval[masterEval$scheme == scheme2 & masterEval$pme == pme2 & masterEval$weightedDraw == weightedDraw, ]
						response1 <- as.numeric(strsplit(stats1$obsDiff, split=' ')[[1]])
						response2 <- as.numeric(strsplit(stats2$obsDiff, split=' ')[[1]])
						
						obsDiff <- abs(mean(response1) - mean(response2))
						
						### PERMUTATION TEST
						randDiff <- rep(NA, iters)
						
						for (iter in 1:iters) {
						
							pooled <- c(response1, response2)
							scrambled <- sample(seq_along(pooled), length(pooled))
							response1 <- pooled[scrambled >= 1 & scrambled <= length(response1)]
							response2 <- pooled[scrambled >= length(response1) + 1 & scrambled <= length(pooled)]
							
							randDiff[iter] <- abs(mean(response1) - mean(response2))
							
						}
							
						# STATISTICAL TEST

						# test assuming permutation (1-tailed)
						pPermute <- 1 - sum(obsDiff >= randDiff) / iters

						thisStats <- data.frame(
							scheme1=scheme1,
							scheme2=scheme2,
							pme1=pme1,
							pme2=pme2,
							iters=iters,
							testStat=testStat,
							mopPerc=thisMopPerc,
							weightedDraw=weightedDraw,
							obsDiff1Minus2=obsDiff,
							randDiffMin=min(randDiff),
							randDiff0pt025=quantile(randDiff, 0.025),
							randDiffMean=mean(randDiff),
							randDiffMedian=median(randDiff),
							randDiff0pt975=quantile(randDiff, 0.975),
							randDiffMax=max(randDiff),
							pPermute=pPermute
						)

						print(thisStats)
						
						stats <- rbind(stats, thisStats)
						
					} # next PME 2
					
				} # next PME 1
				
			} # next scheme 2
			
		} # next scheme 1
		
	} # next MOP
				
	rownames(stats) <- 1:nrow(stats)
	write.csv(stats, paste0(workDir, 'ENMs - PCs/Randomization Test between Schemes and PMEs using ', toupper(testStat), '.csv'), row.names=FALSE)

}

###########################################################################################
### visualize multivariate PC ENM analysis - boxplots of within minus among performance ###
###########################################################################################

if ('boxplot within MINUS among' %in% do) {

	say('The plot shows the the mean of the mean of the mean difference across within - among measures of performance. Ergo, for a given scheme/PME there are the same number of values as there are units.')

	say('visualize multivariate PC ENM analysis - boxplots of within minus among performance', level=1)

	say('This plot shows the difference between among- and within- unit performance. Each scheme gets a column occupied by two bars, one for the broad background (no PME) and one for the narrow background (min PME). Bars represent within-unit performance minus among-unit performance for ensemble models POSSIBLY adjusted for confounding covariates.', breaks=60)

	# generalization
	mopPerc <- c(0.5, 1)
	
	# colMinPme <- 'darkgreen'
	# colNoPme <- 'firebrick'

	colMinPme <- '#1b9e77'
	colNoPme <- '#d95f02'

	offset <- 0.13

	# by MOP
	for (thisMopPerc in mopPerc) {
		
		### load ENM evaluations
		########################
		
		stats <- read.csv(paste0(workDir, 'ENMs - PCs/Randomization Test for Within Minus Among Differences from 0 using CBITEST.csv'))
		stats <- stats[stats$mopPerc == thisMopPerc, ]

		# testStat <- 'cbiFromTo' # raw CBI
		# testStat <- 'cbiFromToResid' # observed CBI minus statistically-adjusted CBI (for unit A k-fold B model this is CBI of that model tested against unit A's k-fold B test data MINUS all CBIs of other-unit k-folds)
		# testStat <- 'aucFromTo' # raw AUC
		# testStat <- 'sensTholdMaxSumSSFromTo' # Se @ MSSS minus Se @ MSSS (for unit A k-fold B model this is Se @ MSSS of that model tested against unit A's k-fold B test data MINUS all Se's @ MSSS of other-unit k-folds)
		# testStat <- 'sensThold10PercTestFromTo' # Se @ 10th% training threshold minus Se @ 10th% training threshold (for unit A k-fold B model this is Se @ 10th% training threshold of that model tested against unit A's k-fold B test data MINUS all Se's @ 10th% training threshold of other-unit k-folds)

		# by TEST STATISTIC
		# for (testStat in c('cbiFromTo', 'aucFromTo', 'sensTholdMaxSumSSFromTo', 'sensThold10PercTestFromTo')) {
		for (testStat in c('cbiFromTo')) {

			say('Using ', toupper(algos), ' with ', testStat, '!!!', pre=2)

			if (testStat == 'cbiFromTo') {
				testStatNice <- 'CBI'
				testStatLab <- 'CBI'
				ylim <- c(-0.5, 0.7)
			} else if (testStat == 'cbiFromToResid') {
				testStatNice <- 'Adjusted CBI'
				testStatLab <- 'Adjusted CBI'
				ylim <- c(-0.4, 0.6)
			} else if (testStat == 'aucFromTo') {
				testStatNice <- 'AUC'
				testStatLab <- 'AUC'
				ylim <- c(-0.2, 0.4)
			} else if (testStat == 'sensTholdMaxSumSSFromTo') {
				testStatNice <- 'Sensitivity (MSSS)'
				testStatLab <- 'Sensitivity'
				ylim <- c(-0.3, 0.3)
			} else if (testStat == 'sensThold10PercTestFromTo') {
				testStatNice <- 'Sensitivity (10th Percentile Test)'
				testStatLab <- 'Sensitivity'
				ylim <- c(-0.05, 0.05)
			}

			png(paste0(workDir, './ENMs - PCs/!Within minus Among Performance - ', testStatNice, ' Using MOP = ', thisMopPerc, '.png'), width=1000, height=1200, res=300)

				# position for each boxplot
				par(mfrow=c(1, 1), mar=1 * c(7, 4, 4, 2) + 0.1, mgp=c(2, 1, 0), cex.main=0.6, cex.axis=0.6, cex.lab=0.8)

				plot(1:4, 1:4, col='white', xlim=c(0.5, 4.5), ylim=ylim, xaxt='n', main=paste0('Within - Among ', testStatNice, ' with MOP = ', thisMopPerc), xlab='', ylab=paste0('Climate coherency\n(within - among model performance)'))
				axis(side=1, at=1:4, labels=FALSE)
				lines(c(0.5, 4.5), c(0, 0), col='gray')

				at <- 1

				# by SCHEME
				for (scheme in c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')) {

					out <- schemeInfo(scheme)
					schemeNice <- out$schemeNice

					# by PME
					for (pme in c('pmeMin', 'pmeNone')) {

						say(scheme, ' ', pme)

						# plot settings
						if (pme == 'pmeMin') {
							col <- colMinPme
							off <- -offset
						} else {
							col <- colNoPme
							off <- offset
						}

						thisStats <- stats[stats$scheme == scheme & stats$pme == pme, ]
						diff <- as.numeric(strsplit(thisStats$obsDiff, split=' ')[[1]])

						pPermute <- thisStats$pPermute

						# plot (note alpha varies by significance using Bonferonni correction)
						alpha <- if (pPermute < 0.05 / 8) { 0.5 } else { 0.05 }
						boxplot(diff, at=at + off, col=alpha(col, alpha), border=col, add=TRUE)

						# # Bonferroni correction
						# labels <- if (pPermute < 0.001 / 8) { '***' } else if (pPermute < 0.01 / 8) { '**' } else if (pPermute < 0.05 / 8) { '*' } else { '' }
						# text(at + off, ylim[2], labels=labels, col=col, cex=0.6)

						# significance groups
						labels <- if (thisMopPerc == 0.5) {
						
							if (scheme %in% c('ecoregionEpa3Modified', 'physioFenneman')) {
								'a'
							} else if (scheme %in% c('cladeNonOverlap')) {
								'ab'
							} else if (scheme %in% c('elevQuantWrtPaeMin')) {
								'b'
							}
						
						} else if (thisMopPerc == 1) {
						
							if (scheme %in% c('ecoregionEpa3Modified', 'physioFenneman')) {
								'a'
							} else if (scheme %in% c('cladeNonOverlap')) {
								'ab'
							} else if (scheme %in% c('elevQuantWrtPaeMin')) {
								'b'
							}
						
						}
						
						text(at + off, ylim[2] + 0.07 * diff(ylim), labels=labels, cex=0.5, pos=1, xpd=NA)
					
					} # next PME
					
					text(x=at, y=ylim[1] - 0.15 * diff(ylim), xpd=NA, labels=schemeNice, srt=90, adj=1, cex=0.8, srt=55)

					at <- at + 1

				} # next division scheme

				legend('bottomleft', legend=c('Narrow background', 'Broad background'), fill=c(alpha(colMinPme, 0.5), alpha(colNoPme, 0.5)), border=c(colMinPme, colNoPme), ncol=1, inset=0, bty='n', cex=0.5)

			dev.off()

		} # next test statistic
		
	} # next MOP

}

###################################
### summarize model performance ###
###################################

if ('summarize model performance' %in% do) {

	say('summarize model performance', level=1)

	overall <- data.frame()
	
	for (scheme in schemes) {
		for (pme in pmes) {

			say(scheme, ' ', pme)
		
			pmeNice <- pmeNiceName(pme)
			
			out <- schemeInfo(scheme, poly=FALSE)
			schemeNice <- out$schemeNice
			schemeShort <- out$schemeShort

			load(paste0(workDir, 'ENMs - PCs/', schemeShort, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ENSEMBLE - MOP-Adjusted.Rdata'))

			evalFrame <- evalFrame[-which(evalFrame$fromUnit == 'all' | evalFrame$toUnit == 'all'), ]
			
			evalFrame <- aggregate(evalFrame, by=list(evalFrame$pme, evalFrame$fromUnit, evalFrame$toUnit), mean, na.rm=TRUE)
			evalFrame$pme <- evalFrame$fromUnit <- evalFrame$toUnit <- NULL
			names(evalFrame)[1:3] <- c('pme', 'fromUnit', 'toUnit')
			
			vsSelf <- evalFrame[evalFrame$fromUnit == evalFrame$toUnit, ]
			vsOther <- evalFrame[evalFrame$fromUnit != evalFrame$toUnit, ]
			
			overall <- rbind(overall,
				data.frame(
					scheme=scheme,
					pme=pme,
					cbiMinVsSelf=min(vsSelf$cbiTest_mop1, na.rm=TRUE),
					cbiMeanVsSelf=mean(vsSelf$cbiTest_mop1, na.rm=TRUE),
					cbiMaxVsSelf=max(vsSelf$cbiTest_mop1, na.rm=TRUE),
					cbiMinVsOther=min(vsOther$cbiTest_mop1, na.rm=TRUE),
					cbiMeanVsOther=mean(vsOther$cbiTest_mop1, na.rm=TRUE),
					cbiMaxVsOther=max(vsOther$cbiTest_mop1, na.rm=TRUE)
				)
			)	
			
		}
	}
	
	clean <- data.frame(
		scheme=overall$scheme,
		pme=overall$pme,
		vsSelf=paste0(sprintf('%.2f', overall$cbiMeanVsSelf), ' (', sprintf('%.2f', overall$cbiMinVsSelf), ' to ', sprintf('%.2f', overall$cbiMaxVsSelf), ')'),
		vsOther=paste0(sprintf('%.2f', overall$cbiMeanVsOther), ' (', sprintf('%.2f', overall$cbiMinVsOther), ' to ', sprintf('%.2f', overall$cbiMaxVsOther), ')'),
		diff=sprintf('%.2f', overall$cbiMeanVsSelf - overall$cbiMeanVsOther)
	)
	
	
	write.csv(clean, paste0(workDir, 'ENMs - PCs/Summary of Model Performance.csv'), row.names=FALSE)

}

say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
