### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2016-11

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/05 ENM Analysis - Multivariate - PCs + Units.r')
# source('E:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/05 ENM Analysis - Multivariate - PCs + Units.r')

rm(list=ls())

	drive <- 'C:/'
	# drive <- 'D:/'
	# drive <- 'E:/'

	# do <- c('copy data', 'train', 'eval', 'map', 'boxplots')
	# do <- 'copy data'
	# do <- 'train'
	# do <- 'flag missing'
	# do <- 'eval'
	# do <- 'eval ensemble'
	# do <- 'statistical test of significance between PME extents WITHIN the same scheme'
	# do <- 'statistical test of significance between SCHEMES and PMEs'
	# do <- 'boxplots'
	# do <- 'boxplots ensemble'
	do <- 'boxplots ensemble with 1 panel'
	# do <- 'response plots'
	# do <- c('train', 'eval')

	# for training/evaluating
	# algos <- c('brt', 'gam', 'glm', 'maxent') # for plots using multiple algorithms
	# algos <- c('brt', 'gam', 'glm', 'maxent', 'rf') # for plots using multiple algorithms
	# algos <- c('maxent', 'brt', 'gam', 'glm') # for plots using multiple algorithms

	algos <- c('brt', 'gam', 'glm', 'maxent', 'rf')
	# algos <- c('rf')
	# algos <- c('glm')
	# algos <- c('maxent')
	# algos <- c('ensemble')

	schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')
	# schemes <- c('cladeNonOverlap', 'elevQuantWrtPaeMin', 'physioFenneman')
	# schemes <- 'cladeNonOverlap'
	# schemes <- 'ecoregionEpa3Modified'
	# schemes <- 'elevQuantWrtPaeMin'
	# schemes <- 'physioFenneman'


	pmes <- c('pmeNone', 'pmeMin')
	# pmes <- 'pmeMin'
	# pmes <- 'pmeNone'

	## response plots
	pc <- 1
	# pc <- 2



### CONTENTS ###
### libraries, variables, and functions ###

### copy model training data for ALL unit of each division schemes and convert unit field to factor ###
### train MULTIVARIATE ENMs using PCs + UNITS ###
### flag missing model files ###
### evaluate MULTIVARIATE ENMs using PCs + UNITS against self and other units ###
### statistical test of significance between PME extents WITHIN the same scheme ###
### statistical test of significance between SCHEMES and PMEs ###

### visualize results for MULTIVARIATE ENMs vs PCs + Units using maps ###


### visualize predictor importance for MULTIVARIATE ENMs using PCs + UNITS against self and other units ###
### visualize predictor importance for ENSEMBLE MULTIVARIATE ENMs using PCs + UNITS against self and other units ###
### visualize response plots for MULTIVARIATE ENMs using PCs + UNITS

###########################################
### libraries, variables, and functions ###
###########################################

source(paste0(drive, 'ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r'))

#######################################################################################################
### copy model training data for ALL unit of each division schemes and convert unit field to factor ###
#######################################################################################################

if ('copy data' %in% do) {

	say('copy model training data for ALL unit of each division schemes and convert unit field to factor', level=1)

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=FALSE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by PME
		for (pmeVariant in pmes) {

			pmeNice <- pmeNiceName(pmeVariant)

			say(Sys.time(), ' SCHEME ', schemeNice, ' | PME ', pmeNice, pre=1)

			fromDir <- paste0(workDir, 'ENMs - PCs/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/')
			toDir <- paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/')

			dirCreate(toDir)

			file.copy(paste0(fromDir, '!Species Data - Ochotona princeps - PCA.Rdata'), paste0(toDir, '!Species Data - Ochotona princeps - PCA.Rdata'), overwrite=TRUE)

			## convert unit names to integers
			load(paste0(toDir, '!Species Data - Ochotona princeps - PCA.Rdata'))

			speciesData$predictorsToUse <- c(speciesData$predictorsToUse, divisionFieldPres)
			speciesData$predictorsAsFactors <- c(speciesData$predictorsAsFactors, TRUE)

			if (length(naRows(speciesData$allPresences[ , divisionFieldPres])) > 0) speciesData$allPresences <- speciesData$allPresences[-naRows(speciesData$allPresences[ , divisionFieldPres]), ]
			if (length(naRows(speciesData$allTrainingAbsences[ , divisionFieldPres])) > 0) speciesData$allTrainingAbsences <- speciesData$allTrainingAbsences[-naRows(speciesData$allTrainingAbsences[ , divisionFieldPres]), ]
			if (length(naRows(speciesData$allTestAbsences[ , divisionFieldPres])) > 0) speciesData$allTestAbsences <- speciesData$allTestAbsences[-naRows(speciesData$allTestAbsences[ , divisionFieldPres]), ]

			speciesData$allPresences[ , divisionFieldPres] <- unitNameToInteger(speciesData$allPresences[ , divisionFieldPres])
			speciesData$allTrainingAbsences[ , divisionFieldPres] <- unitNameToInteger(speciesData$allTrainingAbsences[ , divisionFieldPres])
			speciesData$allTestAbsences[ , divisionFieldPres] <- unitNameToInteger(speciesData$allTestAbsences[ , divisionFieldPres])

			speciesData$allPresences[ , divisionFieldPres] <- as.factor(speciesData$allPresences[ , divisionFieldPres])
			speciesData$allTrainingAbsences[ , divisionFieldPres] <- as.factor(speciesData$allTrainingAbsences[ , divisionFieldPres])
			speciesData$allTestAbsences[ , divisionFieldPres] <- as.factor(speciesData$allTestAbsences[ , divisionFieldPres])

			# by K-FOLD
			for (k in 1:kFolds) {

				if (length(naRows(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$trainingPresences$kFold[[k]] <- speciesData$trainingPresences$kFold[[k]][-naRows(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres]), ]

				if (length(naRows(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$trainingAbsences$kFold[[k]] <- speciesData$trainingAbsences$kFold[[k]][-naRows(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres]), ]

				if (length(naRows(speciesData$testPresences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$testPresences$kFold[[k]] <- speciesData$testPresences$kFold[[k]][-naRows(speciesData$testPresences$kFold[[k]][ , divisionFieldPres]), ]

				if (length(naRows(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$testAbsences$kFold[[k]] <- speciesData$testAbsences$kFold[[k]][-naRows(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres]), ]

				speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres])
				speciesData$testPresences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$testPresences$kFold[[k]][ , divisionFieldPres])
				speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres])
				speciesData$testAbsences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres])

				speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres])
				speciesData$testPresences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$testPresences$kFold[[k]][ , divisionFieldPres])
				speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres])
				speciesData$testAbsences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres])

			}

			save(speciesData, file=paste0(toDir, '!Species Data - Ochotona princeps - PCA.Rdata'), compress=TRUE)

		} # next PME

	} # next scheme

}

#################################################
### train MULTIVARIATE ENMs using PCs + UNITS ###
#################################################

if ('train' %in% do) {

	say('train MULTIVARIATE ENMs using PCs + UNITS', level=1)

	say('Using ', paste(toupper(algos), collapse=' '), '!!!')

	# get presences
	pres <- getPres()

	# by ALGORITHM
	for (thisAlgo in algos) {

		# by SCHEME
		for (scheme in schemes) {

			out <- schemeInfo(scheme, poly=FALSE)
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
				for (fromValance in c('including')) {

					# get names of focal units to include/exclude
					# units <- getUnits(scheme=scheme, incAll=(fromValance == 'including'))
					units <- 'all'

					say('UNITS: ', paste(units, collapse=' | '), pre=1)

					# by UNIT
					for (fromUnit in units) {

						say(Sys.time(), ' SCHEME ', schemeShort, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, pre=1)

						say('  Training ENMs...')

						thesePredictors <- c(pcsUsed, divisionFieldPres)

						trainSdm(
							speciesList='Ochotona princeps',
							outputDir=paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME'),
							predictorsToUse=thesePredictors,
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
							verbose=1
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

				fileName <- paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate all sites.Rdata')

				if (!file.exists(fileName)) missingFiles <- c(missingFiles, fileName)

				for (k in 1:kFolds) {

					fileName <- paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(k, 2), '.Rdata')
					if (!file.exists(fileName)) missingFiles <- c(missingFiles, fileName)

				}

			} # next PME

		} # next scheme

	} # next algorithm

	say('THERE ARE ', length(missingFiles), ' MISSING MODEL FILES:')
	print(missingFiles)

}


#################################################################################
### evaluate MULTIVARIATE ENMs using PCs + UNITS against self and other units ###
#################################################################################

if ('eval' %in% do) {

	say('evaluate MULTIVARIATE ENMs using PCs + UNITS against self and other units', level=1)
	say('Using ', paste(toupper(algos), collapse=' '), '!!!')

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

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=TRUE)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		divisionFieldPoly <- out$divisionFieldPoly
		divisionPoly <- out$divisionPoly
		rm(out); gc()

		divisionPoly <- sp::spTransform(divisionPoly, getCRS('nad83', TRUE))

		divisionPolyDf <- as.data.frame(divisionPoly)

		# predictors used
		thesePredictors <- c(pcsUsed, divisionFieldPres)

		# crop PRISM mask to division polygons that contain presences
		thisPrismMask <- crop(prismMask, divisionPoly[divisionPolyDf[ , divisionFieldPoly] %in% unique(pres[ , divisionFieldPres]), ])

		# by PME
		for (pmeVariant in pmes) {

			if (exists('evalFrame')) rm(evalFrame)

			pmeNice <- pmeNiceName(pmeVariant)

			### "from" valance
			fromValance <- 'including'

			# get names of focal units to include/exclude
			# units <- getUnits(scheme=scheme, incAll=(fromValance == 'including'))

			units <- 'all'

			say('FROM UNITS: ', paste(units, collapse=' | '), pre=1)

			# by FROM UNIT
			for (fromUnit in units) {

				say(Sys.time(), ' SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, pre=1)

				say('  Evaluating against:')

				### "to" valance
				toValance <- 'including'

				if (!(fromValance == 'including' & toValance == 'excluding' & fromUnit == 'all')) {

					# # if "from" and "to" valances are both "include" then test against all other units. If not, then test only against own unit.
					# toUnits <- if (fromValance == 'including' & toValance == 'including') {
						# units
					# } else {
						# fromUnit
					# }

					toUnits <- 'all'

					## load test data
					toFile <- paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/', toupper(toValance), ' ', toupper(toUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
					load(toFile)

					# by K-FOLD
					for (kFrom in 1:kFolds) {

						kTo <- kFrom

						## load model
						fromFile <- paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(kFrom, 2), '.Rdata')
						load(fromFile)

						testPres <- speciesData$testPresences$kFold[[kTo]][ , thesePredictors]
						testBg <- speciesData$testAbsences$kFold[[kTo]][ , thesePredictors]

						## calculate geographic distance between training/test presences... using 10% quantile of minimum of pairwise distances (for each test presence) between training and test presences... similar to mobility-oriented parity (cf. Owens, H.L., Campbell, L.P., Dornak, L.L., Saupe, E.E., Barve, N., Soberón, Ingenloff, K., Lira-Noriega, A., Hensz, C., Myers, C.E., and Peterson, A.T.  2013.  Constraints on interpretation of ecological niche models by limited environmental ranges on calibration area.  Ecological Modeling 263:10-18.)

						dists <- rep(NA, nrow(testPres))
						for (i in 1:nrow(testPres)) {

							dists[i] <- min(distCosine(speciesData$testPresences$kFold[[kTo]][i, c('longWgs84', 'latWgs84')], model$trainingPresences[ , c('longWgs84', 'latWgs84')]), na.rm=TRUE)

						}

						testTrainGeogDist <- quantile(dists, 0.10, na.rm=TRUE)

						## predict
						predPres <- predictEnm(thisData=testPres, theModel=model$model, predictors=model$predictorsToUse, predArgs=predArgs, na.rm=TRUE)
						predBg <- predictEnm(thisData=testBg, theModel=model$model, predictors=model$predictorsToUse, predArgs=predArgs, na.rm=TRUE)

						# ## weights
						# presWeight <- calcSiteWeight(x=testPres, proximity=TRUE, temporal=TRUE)
						# bgWeight <- calcSiteWeight(x=testBg, proximity=TRUE, temporal=TRUE)

						## weights
						maxWeight <- max(testPres$kdeWeight, testBg$kdeWeight, na.rm=TRUE)
						presWeight <- 1 - (testPres$kdeWeight / maxWeight)
						bgWeight <- 1- (testBg$kdeWeight / maxWeight)

						## evaluate
						cbi <- contBoyce(pres=predPres, bg=predBg, na.rm=TRUE, numBins=1001)
						cbiPresWeight <- contBoyce(pres=predPres, bg=predBg, presWeight=presWeight, na.rm=TRUE, numBins=1001)
						cbiPresBgWeight <- contBoyce(pres=predPres, bg=predBg, presWeight=presWeight, bgWeight=bgWeight, na.rm=TRUE, numBins=1001)

						thisEval <- evaluate(p=as.vector(na.omit(predPres)), a=as.vector(na.omit(predBg)), tr=seq(0, 1, by=0.00001))
						tholdMaxSumSS <- threshold(thisEval, stat='spec_sens')
						tholdMinDiffSS <- threshold(thisEval, stat='equal_sens_spec')
						thold10PercTest <- threshold(thisEval, stat='sensitivity', sensitivity=0.1)

						sensTholdMaxSumSS <- sum(predPres >= tholdMaxSumSS, na.rm=TRUE) / length(na.omit(predPres))
						specTholdMaxSumSS <- sum(predBg < tholdMaxSumSS, na.rm=TRUE) / length(na.omit(predBg))

						sensTholdMinDiffSS <- sum(predPres >= tholdMinDiffSS, na.rm=TRUE) / length(na.omit(predPres))
						specTholdMinDiffSS <- sum(predBg < tholdMinDiffSS, na.rm=TRUE) / length(na.omit(predBg))

						sensThold10PercTest <- sum(predPres >= thold10PercTest, na.rm=TRUE) / length(na.omit(predPres))
						specThold10PercTest <- sum(predBg < thold10PercTest, na.rm=TRUE) / length(na.omit(predBg))

						# remember
						thisEval <- data.frame(

							algorithm=thisAlgo,
							scheme=scheme,
							pme=pmeVariant,
							toValance=toValance,
							toUnit=toUnit,
							fromValance=fromValance,
							fromUnit=fromUnit,
							kFrom=kFrom,
							kTo=kTo,

							testTrainGeogDist=testTrainGeogDist,

							cbi=cbi,
							cbiPresWeight=cbiPresWeight,
							cbiPresBgWeight=cbiPresBgWeight,

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
						# x <- logitAdj(c(presWeight * predPres, bgWeight * predBg))
						x <- logitAdj(c(predPres, predBg))

						for (thisPredictor in thesePredictors) {

							corStat <- rankStat <- cbiPerm <- rep(NA, 100)

							for (j in 1:100) {

								# combine pres/BG
								permutedSites <- rbind(testPres, testBg)
								# permutedWeights <- c(presWeight, bgWeight)

								# permute
								permIndex <- sample(1:nrow(permutedSites), nrow(permutedSites))

								permutedSites[ , thisPredictor] <- permutedSites[permIndex, thisPredictor]
								# presWeightPerm <- permutedWeights[permIndex[1:length(predPres)]]
								# bgWeightPerm <- permutedWeights[permIndex[(length(predPres) + 1):length(permutedWeights)]]

								# predict to permuted data
								predPerm <- predictEnm(thisData=permutedSites, theModel=model$model, predictors=model$predictorsToUse, predArgs=predArgs)

								# get permuted predictions for pres/BG
								predPresPerm <- predPerm[1:length(predPres)]
								predBgPerm <- predPerm[(length(predPres) + 1):length(predPerm)]

								y <- logitAdj(c(predPresPerm, predBgPerm))

								# importance statistics
								corStat[j] <- cor(x, y)
								rankStat[j] <- cor(x, y, method='spearman')
								cbiPerm[j] <- contBoyce(pres=predPresPerm, bg=predBgPerm, na.rm=TRUE, numBins=1001)

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

						print(evalFrame)
						say('')

					} # next "from" k-fold

				} # if valid comparison

			} # next to valance

			save(evalFrame, file=paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ', toupper(thisAlgo), ' - FROM ', toupper(fromUnit), ' (INCLUDING).Rdata'))
			rm(evalFrame)

		} # next PME

	} # next scheme

}

##########################################################################################
### evaluate MULTIVARIATE ENSEMBLE ENMs using PCs + Units against self and other units ###
##########################################################################################

if ('eval ensemble' %in% do) {

	say('evaluate MULTIVARIATE ENSEMBLE ENMs using PCs + Units', level=1)
	say('Using ', paste(toupper(algos), collapse= ' '), '!!!')

	iter <- 30 # number of permutation iterations for variable importance

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by PME
		for (pme in pmes) {

			if (exists('evalFrame')) rm(evalFrame)

			pmeNice <- pmeNiceName(pme)

			say(Sys.time(), ' | ENSEMBLE ENM | SCHEME ', schemeNice, ' | PME ', pmeNice, ' | UNIT all | k =', post=0)

			## load "to" test data
			speciesDataFile <- paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata')
			load(speciesDataFile)

			# for each k-fold of model evaluate against either same k-fold of test data (if model is for same unit/valance) or *all* k-folds of test data is model is not for unit being tested against
			for (k in 1:kFolds) {

				say(k, post=ifelse(k == kFolds, 1, 0))

				trainPres <- speciesData$trainingPresences$kFold[[k]]
				trainBg <- speciesData$trainingAbsences$kFold[[k]]
				trainPres$obsDate <- as.Date(paste0(trainPres$obsYear, '-', trainPres$obsMonth, '-', trainPres$obsDayOfMonth))

				testPres <- speciesData$testPresences$kFold[[k]]
				testBg <- speciesData$testAbsences$kFold[[k]]
				testPres$obsDate <- as.Date(paste0(testPres$obsYear, '-', testPres$obsMonth, '-', testPres$obsDayOfMonth))

				## calculate geographic distance between training/TRAINING presences... average of 10% quantile of pairwise distances (for each test presence) between training and test presences... similar to mobility-oriented parity (cf. Owens, H.L., Campbell, L.P., Dornak, L.L., Saupe, E.E., Barve, N., Soberón, Ingenloff, K., Lira-Noriega, A., Hensz, C., Myers, C.E., and Peterson, A.T.  2013.  Constraints on interpretation of ecological niche models by limited environmental ranges on calibration area.  Ecological Modeling 263:10-18.)

					# vs training sites
					dists <- matrix(NA, nrow=nrow(trainPres), ncol=nrow(trainPres))
					for (i in 1:nrow(trainPres)) {
						dists[i, i:nrow(trainPres)] <- distCosine(trainPres[i, c('longWgs84', 'latWgs84')], trainPres[i:nrow(trainPres) , c('longWgs84', 'latWgs84')])
					}

					dists[lower.tri(dists)] <- dists[upper.tri(dists)]

					dists <- apply(dists, 1, quantile, prob=0.1, na.rm=TRUE)
					trainTrainGeogDist <- mean(dists, na.rm=TRUE) / 1000
					rm(dist); gc()

				## calculate temporal difference between training/TRAINING presences... using 10% quantile of minimum of pairwise distances (for each training presence) between training and training presences

					# vs training sites
					diffs <- matrix(NA, nrow=nrow(trainPres), ncol=nrow(trainPres))
					for (i in 1:nrow(trainPres)) {
						diffs[i, i:nrow(trainPres)] <- as.numeric(trainPres$obsDate[i] - trainPres$obsDate[i:nrow(trainPres)])
					}

					diffs[lower.tri(diffs)] <- -1 * diffs[upper.tri(diffs)] # does weird mirroring of upper/lower triangulars but summary results are still the same
					trainTrainTemporalDiffs <- mean(apply(abs(diffs), 1, quantile, prob=0.1))
					trainTrainTemporalDiffsSigned <- mean(apply(diffs, 1, quantile, prob=0.1))

					rm(diffs); gc()

				## calculate geographic distance between training/TEST presences... average of 10% quantile of pairwise distances (for each test presence) between training and test presences... similar to mobility-oriented parity (cf. Owens, H.L., Campbell, L.P., Dornak, L.L., Saupe, E.E., Barve, N., Soberón, Ingenloff, K., Lira-Noriega, A., Hensz, C., Myers, C.E., and Peterson, A.T.  2013.  Constraints on interpretation of ecological niche models by limited environmental ranges on calibration area.  Ecological Modeling 263:10-18.)

					dists <- matrix(NA, nrow=nrow(testPres), ncol=nrow(trainPres))
					for (i in 1:nrow(testPres)) {
						dists[i, ] <- distCosine(testPres[i, c('longWgs84', 'latWgs84')], trainPres[ , c('longWgs84', 'latWgs84')])
					}

					dists <- apply(dists, 1, quantile, prob=0.1)
					trainTestGeogDist <- mean(dists, na.rm=TRUE) / 1000
					rm(dists); gc()

				## calculate temporal difference between training/TEST presences... using 10% quantile of minimum of pairwise distances (for each training presence) between training and training presences

					# vs training sites
					diffs <- matrix(NA, nrow=nrow(testPres), ncol=nrow(trainPres))
					for (i in 1:nrow(testPres)) {
						diffs[i, ] <- as.numeric(trainPres$obsDate - testPres$obsDate[i])
					}

					trainTestTemporalDiffs <- mean(abs(diffs), na.rm=TRUE)
					trainTestTemporalDiffsSigned <- mean(diffs, na.rm=TRUE)
					rm(diffs); gc()

				### containers for predictions at training/test data

					predPresTrain <- matrix(NA, nrow=nrow(trainPres), ncol=length(algos))
					predPresTest <- matrix(NA, nrow=nrow(testPres), ncol=length(algos))

					predBgTrain <- matrix(NA, nrow=nrow(trainBg), ncol=length(algos))
					predBgTest <- matrix(NA, nrow=nrow(testBg), ncol=length(algos))

				### containers for predictions at permuted test sites

					modelFile <- paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/', toupper(algos[1]), ' multivariate all sites.Rdata')
					load(modelFile)

					predictors <- model$predictorsToUse
					rm(model); gc()

					predPresTestPerm <- predBgTestPerm <- list()
					for (i in seq_along(predictors)) {
						predPresTestPerm[[i]] <- matrix(NA, nrow=nrow(testPres), ncol=iter * length(algos))
						predBgTestPerm[[i]] <- matrix(NA, nrow=nrow(testBg), ncol=iter * length(algos))
					}

					names(predPresTestPerm) <- names(predPresTestPerm) <- predictors

				### load model
				for (countAlgo in seq_along(algos)) {

					thisAlgo <- algos[countAlgo]

					## load model
					modelFile <- paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(k, 2), '.Rdata')

					load(modelFile)

					model <- model$model
					gc()

					## predict to training sites
					predPresTrain[ , countAlgo] <- predictEnm(thisData=trainPres, theModel=model, predictors=predictors, predArgs=predArgs)
					predBgTrain[ , countAlgo] <- predictEnm(thisData=trainBg, theModel=model, predictors=predictors, predArgs=predArgs)

					## predict to test sites
					predPresTest[ , countAlgo] <- predictEnm(thisData=testPres, theModel=model, predictors=predictors, predArgs=predArgs)
					predBgTest[ , countAlgo] <- predictEnm(thisData=testBg, theModel=model, predictors=predictors, predArgs=predArgs)

					## variable importance: predict to permuted sites

					# by PREDICTOR
					for (countPredictor in seq_along(predictors)) {

						thisPredictor <- predictors[countPredictor]

						testPresPerm <- testPres
						testBgPerm <- testBg

						# by PERMUTATION
						for (i in 1:iter) {

							predictorPerm <- c(testPres[ , thisPredictor], testBg[ , thisPredictor])
							if (class(testPres[ , thisPredictor]) == 'factor') {
								predictorPerm <- as.factor(predictorPerm)
								levels(predictorPerm) <- levels(testPres[ , thisPredictor])
							}
							predictorPerm <- sample(predictorPerm, length(predictorPerm))

							testPresPerm[ , thisPredictor] <- predictorPerm[1:nrow(testPres)]
							testBgPerm[ , thisPredictor] <- predictorPerm[(nrow(testPres) + 1):length(predictorPerm)]

							# if (class(testPres[ , thisPredictor]) == 'factor') {

								# testPresPerm[ , thisPredictor] <- factor(testPresPerm[ , thisPredictor], levels=levels(testPres$physioFenneman))
								# testBgPerm[ , thisPredictor] <- factor(testBgPerm[ , thisPredictor], levels=levels(testBg$physioFenneman))

							# }

							# predict to permuted test sites
							predPresTestPerm[[countPredictor]][ , i + iter * (which(algos %in% thisAlgo) - 1)] <- predictEnm(thisData=testPresPerm, theModel=model, predictors=predictors, predArgs=predArgs)
							predBgTestPerm[[countPredictor]][ , i + iter * (which(algos %in% thisAlgo) - 1)] <- predictEnm(thisData=testBgPerm, theModel=model, predictors=predictors, predArgs=predArgs)

						} # next permutation iteration

					} # next predictor

					rm(model); gc()

				} # next algorithm

				### calculate ensemble prediction

					if (length(naRows(predPresTrain)) > 0) predPresTrain <- predPresTrain[-naRows(predPresTrain), ]
					if (length(naRows(predBgTrain)) > 0) predBgTrain <- predBgTrain[-naRows(predBgTrain), ]

					if (length(naRows(predPresTest)) > 0) predPresTest <- predPresTest[-naRows(predPresTest), ]
					if (length(naRows(predBgTest)) > 0) predBgTest <- predBgTest[-naRows(predBgTest), ]

					predPresTrain <- rowMeans(predPresTrain)
					predBgTrain <- rowMeans(predBgTrain)

					predPresTest <- rowMeans(predPresTest)
					predBgTest <- rowMeans(predBgTest)

				### calculate permuted prediction

					for (countPredictor in seq_along(predictors)) {

						if (length(naRows(predPresTestPerm[[countPredictor]])) > 0) predPresTestPerm[[countPredictor]] <- predPresTestPerm[[countPredictor]][-naRows(predPresTestPerm), ]
						if (length(naRows(predBgTestPerm[[countPredictor]])) > 0) predBgTestPerm[[countPredictor]] <- predBgTestPerm[[countPredictor]][-naRows(predBgTestPerm), ]

					}

					predPresTestPerm <- lapply(predPresTestPerm, rowMeans)
					predBgTestPerm <- lapply(predBgTestPerm, rowMeans)

				### predictor importance

					corPerm <- cbiPerm <- rep(NA, length(predictors))
					names(corPerm) <- names(cbiPerm) <- predictors

					# by PREDICTOR
					for (countPredictor in seq_along(predictors)) {

						cbiPerm[countPredictor] <- contBoyce(pres=predPresTestPerm[[countPredictor]], bg=predBgTestPerm[[countPredictor]], na.rm=TRUE, numBins=1001)

						x <- c(predPresTest, predBgTest)
						y <- c(predPresTestPerm[[countPredictor]], predBgTestPerm[[countPredictor]])

						x <- logitAdj(x)
						y <- logitAdj(y)

						corPerm[countPredictor] <- cor(x, y)

					}

				### evaluate vs training sites

					cbiTrain <- contBoyce(pres=predPresTrain, bg=predBgTrain, na.rm=TRUE, numBins=1001)

					thisEvalTrain <- evaluate(p=as.vector(predPresTrain), a=as.vector(predBgTrain), tr=seq(0, 1, by=0.0001))

					aucTrain <- thisEvalTrain@auc

					tholdMaxSumSSTrain <- threshold(thisEvalTrain, stat='spec_sens')
					tholdMinDiffSSTrain <- threshold(thisEvalTrain, stat='equal_sens_spec')
					thold10PercTrain <- threshold(thisEvalTrain, stat='sensitivity', sensitivity=0.9)

					sensTholdMaxSumSSTrain <- sum(predPresTrain >= tholdMaxSumSSTrain) / length(predPresTrain)
					specTholdMaxSumSSTrain <- sum(predBgTrain < tholdMaxSumSSTrain) / length(predBgTrain)

					sensTholdMinDiffSSTrain <- sum(predPresTrain >= tholdMinDiffSSTrain) / length(predPresTrain)
					specTholdMinDiffSSTrain <- sum(predBgTrain < tholdMinDiffSSTrain) / length(predBgTrain)

					sensThold10PercTrain <- sum(predPresTrain >= thold10PercTrain) / length(predPresTrain)
					specThold10PercTrain <- sum(predBgTrain < thold10PercTrain) / length(predBgTrain)

				### evaluate vs test sites

					cbiTest <- contBoyce(pres=predPresTest, bg=predBgTest, na.rm=TRUE, numBins=1001)

					thisEvalTest <- evaluate(p=as.vector(predPresTest), a=as.vector(predBgTest), tr=seq(0, 1, by=0.0001))

					aucTest <- thisEvalTest@auc

					tholdMaxSumSSTest <- threshold(thisEvalTest, stat='spec_sens')
					tholdMinDiffSSTest <- threshold(thisEvalTest, stat='equal_sens_spec')
					thold10PercTest <- threshold(thisEvalTest, stat='sensitivity', sensitivity=0.9)

					sensTholdMaxSumSSTest <- sum(predPresTest >= tholdMaxSumSSTest) / length(predPresTest)
					specTholdMaxSumSSTest <- sum(predBgTest < tholdMaxSumSSTest) / length(predBgTest)

					sensTholdMinDiffSSTest <- sum(predPresTest >= tholdMinDiffSSTest) / length(predPresTest)
					specTholdMinDiffSSTest <- sum(predBgTest < tholdMinDiffSSTest) / length(predBgTest)

					sensThold10PercTest <- sum(predPresTest >= thold10PercTest) / length(predPresTest)
					specThold10PercTest <- sum(predBgTest < thold10PercTest) / length(predBgTest)

				# remember
				thisEval <- data.frame(

					algorithm='ensemble',
					algos=paste(algos, collapse=' '),
					scheme=scheme,
					pme=pme,
					unit='all',
					k=k,

					trainTrainGeogDist=trainTrainGeogDist,
					trainTestGeogDist=trainTestGeogDist,

					trainTrainTemporalDiffs=trainTrainTemporalDiffs,
					trainTestTemporalDiffs=trainTestTemporalDiffs,

					trainTrainTemporalDiffsSigned=trainTrainTemporalDiffsSigned,
					trainTestTemporalDiffsSigned=trainTestTemporalDiffsSigned,

					nTrain=nrow(trainPres),
					nTest=nrow(testPres),

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

				# remember variable importance
				cbiPerm <- as.data.frame(t(as.matrix(cbiPerm)))
				corPerm <- as.data.frame(t(as.matrix(corPerm)))

				names(corPerm)[which(names(corPerm) %in% divisionFieldPres)] <- 'unit'
				names(cbiPerm)[which(names(cbiPerm) %in% divisionFieldPres)] <- 'unit'

				names(corPerm) <- paste0('corImport_', names(corPerm))
				names(cbiPerm) <- paste0('cbiImport_', names(cbiPerm))

				thisEval <- cbind(thisEval, corPerm, cbiPerm)

				evalFrame <- if (exists('evalFrame')) {
					rbind(evalFrame, thisEval)
				} else {
					thisEval
				}

				# print(evalFrame)
				# say('')

			} # next "from" k-fold

			say('')

			save(evalFrame, file=paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ENSEMBLE.Rdata'))
			rm(evalFrame)

		} # next PME

	} # next scheme

}

###################################################################################
### statistical test of significance between PME extents WITHIN the same scheme ###
###################################################################################

if ('statistical test of significance between PME extents WITHIN the same scheme' %in% do) {

	say('statistical test of significance between PME extents WITHIN the same scheme', level=1)

	iters <- 1000 # iterations for null model distribution

	results <- data.frame()
	
	# by SCHEME
	for (scheme in schemes) {
	
		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		say(schemeNice)

		load(paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - Min PME - ENSEMBLE.Rdata'))
		pmeMinEval <- evalFrame
		load(paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - No PME - ENSEMBLE.Rdata'))
		pmeNoEval <- evalFrame
		
		obsDiff <- median(pmeNoEval$cbiTest) - median(pmeMinEval$cbiTest)
		obsAbsDiff <- abs(obsDiff)
		
		nullAbsDiff <- rep(NA, iters)
		for (i in 1:iters) {
		
			combinedPerform <- c(pmeMinEval$cbiTest, pmeNoEval$cbiTest)
			
			randPerform <- combinedPerform[sample(seq_along(combinedPerform))]
			set1 <- randPerform[1:nrow(pmeMinEval)]
			set2 <- randPerform[(nrow(pmeMinEval) + 1):length(randPerform)]
			
			nullAbsDiff[i] <- abs(median(set1) - median(set2))
		
		}
		
		pPermute <- sum(nullAbsDiff > obsAbsDiff) / iters
		
		thisResults <- data.frame(
			scheme=scheme,
			iters=iters,
			whichGreater=ifelse(obsDiff > 0, 'pmeNo', 'pmeMin'),
			obsDiff=obsDiff,
			obsAbsDiff=obsAbsDiff,
			pPermute=pPermute
		)
		
		results <- rbind(results, thisResults)
		
	} # next scheme

	save(results, file=paste0(workDir, '/ENMs - PCs + Units/Permutation Tests of PMEs within Schemes.Rdata'))

}

#################################################################
### statistical test of significance between SCHEMES and PMEs ###
#################################################################

if ('statistical test of significance between SCHEMES and PMEs' %in% do) {

	say('statistical test of significance between SCHEMES and PMEs', level=1)

	iters <- 1000 # iterations for null model distribution

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

				file <- paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ENSEMBLE.Rdata')
				load(file)
				master <- rbind(master, evalFrame)

		} # next PME

	} # next scheme

	### Is difference between PME-set within a scheme and a PME-set from another scheme significantly > 0?
	######################################################################################################

	stats <- data.frame()

	# by SCHEME
	for (countScheme1 in 1:(length(schemes) - 1)) {

		scheme1 <- schemes[countScheme1]

		for (countScheme2 in (countScheme1 + 1):length(schemes)) {
	
			scheme2 <- schemes[countScheme2]
			
			for (pme1 in pmes) {
			
				for (pme2 in pmes) {
		
					say(scheme1, ' ', pme1, ' VS ', scheme2, ' ', pme2, post=0)

					### OBSERVED DIFFERENCES BETWEEN SCHEMES/PMES
					stats1 <- master[master$scheme == scheme1 & master$pme == pme1, ]
					stats2 <- master[master$scheme == scheme2 & master$pme == pme2, ]
					
					response1 <- stats1$corImport_unit
					response2 <- stats2$corImport_unit
					
					obsAbsDiff <- abs(mean(response1) - mean(response2))
					
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
					pPermute <- 1 - sum(obsAbsDiff > randDiff) / iters

					response1 <- 1 - response1
					response2 <- 1 - response2
					
					thisStats <- data.frame(
						scheme1=scheme1,
						scheme2=scheme2,
						pme1=pme1,
						pme2=pme2,
						iters=iters,
						resp1=median(response1),
						resp2=median(response2),
						obsAbsDiff=obsAbsDiff,
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
		
	rownames(stats) <- 1:nrow(stats)
	write.csv(stats, paste0(workDir, 'ENMs - PCs + Units/Randomization Test between Schemes and PMEs using COR.csv'), row.names=FALSE)

}

###########################################################################################################
### visualize predictor importance for MULTIVARIATE ENMs using PCs + UNITS against self and other units ###
###########################################################################################################

if ('boxplots' %in% do) {

	say('visualize predictor importance for MULTIVARIATE ENMs using PCs + UNITS against self and other units', level=1)

	say('Plot has one row per scheme.  Each row has one set of boxplots per algorithm.  Each algorithm has three boxplots, one showing "observed" values (CBI only), one showing permuted values for "unit", and one showing permuted values for the PC that has the greatest median importance.')

	say('Using ', paste(toupper(algos), collapse=' & '), '!!!')

	ylim <- c(-0.5, 1)
	offset <- 0.23 # amount by which to nudge boxes left/right of center

	## PCA
	pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

	# selecting PCs that cumulatively explain x% of variance
	pcNames <- paste0('PC', prefix(1:length(pca$sdev), 2))
	cumulExplain <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
	pcsUsed <- pcNames[cumulExplain <= pcaMinCumulExplained]
	if (length(pcsUsed) < length(pcNames)) pcsUsed <- c(pcsUsed, paste0('PC', prefix(length(pcsUsed) + 1, 2)))

	# y coordinate for box labels
	barLabelY <- ylim[1] - 0.07 * diff(ylim)
	algoLabelY <- ylim[1] - 0.4 * diff(ylim)

	# plot setup
	par(mfrow=c(2, length(schemes)), mar=1 * c(7, 6, 3, 2) + 0.1)

	### SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		schemeShortSingular <- out$schemeShortSingular
		rm(out); gc()

		# predictors used
		thesePredictors <- c(divisionFieldPres, pcsUsed)

		### by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)
			bgNice <- bgNiceName(pme)

			# by ALGORITHM
			for (countAlgo in seq_along(algos)) {

				say(algos[countAlgo])

				thisAlgo <- algos[countAlgo]

				plot(seq_along(schemes), seq_along(schemes), col='white', xlab='', ylab=paste0(testStatNice, '\n\U2190 (more important)                           (less important) \U2192'), ylim=ylim, main=paste0(schemeNice, ' with ', bgNice, ' Background'), xaxt='n', xlim=c(0.5, 3.5))

				# evaluation data for this algorithm
				if (algos[1] != 'ensemble') {
					load(paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ', toupper(thisAlgo), ' - FROM ALL (INCLUDING).Rdata'))
				} else {
					load(paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/ENM Evaluations - ', schemeNice, ' - ', pmeNice, ' PME - ENSEMBLE.Rdata'))
				}

				## each subplot has three bars: observed CBI, minimum(CBI using PCs permuted), and CBI using unit permuted

				# 1) observed (CBI only) (left, behind)
				if (testStat == 'cbi') {
					boxplot(evalFrame$cbiTest, at=countAlgo - offset, add=TRUE)
					text(countAlgo - offset, barLabelY, labels='Observed', srt=90, adj=1, xpd=NA, col='black')
				}

				# 2) test stat of PC with greatest importance (lowest permuted value) (right, behind)
				evalFrameAgg <- apply(evalFrame[ , names(evalFrame) %in% paste0(testStat, 'Import_', toupper(pcsUsed))], 2, median, na.rm=TRUE)
				mostImpPc <- names(evalFrameAgg)[which.min(evalFrameAgg)]

				boxplot(evalFrame[ , mostImpPc], at=countAlgo + offset, col=alpha('dodgerblue', 0.5), border='dodgerblue4', add=TRUE)
				text(countAlgo + offset, barLabelY, labels=paste0('PC', prefix(which.min(evalFrameAgg), 2)), srt=90, adj=1, xpd=NA, col='dodgerblue4')

				# 3) test stat of units permuted (center, front)
				boxplot(evalFrame[ , paste0(testStat, 'Import_unit')], at=countAlgo, col=alpha('firebrick1', 0.5), border='firebrick4', add=TRUE)
				text(countAlgo, barLabelY, labels=schemeShortSingular, srt=90, adj=1, xpd=NA, col='firebrick4')

				# # algorithm label
				# text(countAlgo, algoLabelY, labels=toupper(algos[countAlgo]), xpd=NA, adj=0.5)

			} # next algorithm

		} # next PME

	} # next scheme

}

####################################################################################################################
### visualize predictor importance for ENSEMBLE MULTIVARIATE ENMs using PCs + UNITS against self and other units ###
####################################################################################################################

if ('boxplots ensemble' %in% do) {

	say('visualize predictor importance for ENSEMBLE MULTIVARIATE ENMs using PCs + UNITS against self and other units', level=1)

	say('Plot has one panel per PME. Each panel has one set of boxplots per scheme.  Each set three boxplots, one showing "observed" values (CBI only), one showing permuted values for "unit", and one showing permuted values for the PC that has the greatest median importance.')

	say('Using ', paste(toupper(algos), collapse=' & '), '!!!')

	## test statistic for boxplot

		# testStat <- 'cbi'
		testStat <- 'cor'

	testStatNice <- statNice(testStat)

	ylim <- c(0, 1)
	offset <- 0.23 # amount by which to nudge boxes left/right of center
	cexBar <- 0.5 # bar text

	## PCA
	pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

	# selecting PCs that cumulatively explain x% of variance
	pcNames <- paste0('PC', prefix(1:length(pca$sdev), 2))
	cumulExplain <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
	pcsUsed <- pcNames[cumulExplain <= pcaMinCumulExplained]
	if (length(pcsUsed) < length(pcNames)) pcsUsed <- c(pcsUsed, paste0('PC', prefix(length(pcsUsed) + 1, 2)))

	# y coordinate for box labels
	barLabelY <- ylim[1] - 0.07 * diff(ylim)

	png(paste0(workDir, '/ENMs - PCs + Units/Variable Importance - ', testStatNice, '.png'), width=2400, height=800, res=300)

		# plot setup
		par(mfrow=c(1, 2), mar=0.3 * c(9, 15, 3, 2) + 0.1, cex.main=0.7, cex.lab=0.7, cex.axis=0.6, mgp=c(2, 1, 0))

		### by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)
			bgNice <- bgNiceName(pme)

			plot(1:10, 1:10, col='white', xlab='', ylab=paste0(testStatNice, '\n\U2190 more important   less important \U2192'), ylim=ylim, main=paste0(bgNice, ' Background'), xaxt='n', xlim=c(0.5, 4.5))

			# position for each scheme
			at <- 1

			### SCHEME
			for (scheme in schemes) {

				say(pme, ' ', scheme)

				out <- schemeInfo(scheme)
				schemeNice <- out$schemeNice
				schemeShort <- out$schemeShort
				divisionFieldPres <- out$divisionFieldPres
				schemeShortSingular <- out$schemeShortSingular
				rm(out); gc()

				# predictors used
				thesePredictors <- c(divisionFieldPres, pcsUsed)

				# evaluation data for this algorithm
				load(paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ENSEMBLE.Rdata'))

				## each subplot has three bars: observed CBI, minimum(CBI using PCs permuted), and CBI using unit permuted

				# 1) observed (CBI only) (left, behind)
				if (testStat == 'cbi') {
					boxplot(evalFrame$cbiTest, at=at - offset, add=TRUE)
					text(at - offset, barLabelY, labels='Observed', srt=90, adj=1, xpd=NA, col='black', cex=cexBar)
				}

				# 2) test stat of PC with greatest importance (lowest permuted value) (right, behind)
				evalFrameAgg <- apply(evalFrame[ , names(evalFrame) %in% paste0(testStat, 'Import_', toupper(pcsUsed))], 2, median, na.rm=TRUE)
				mostImpPc <- names(evalFrameAgg)[which.min(evalFrameAgg)]

				boxplot(evalFrame[ , mostImpPc], at=at + offset, col=alpha('darkblue', 0.5), border='darkblue', add=TRUE)
				text(at + offset, barLabelY, labels=paste0('PC', prefix(which.min(evalFrameAgg), 2)), srt=90, adj=1, xpd=NA, col='darkblue', cex=cexBar)

				# 3) test stat of units permuted (center, front)
				boxplot(evalFrame[ , paste0(testStat, 'Import_unit')], at=at, col=alpha('red', 0.5), border='darkred', add=TRUE)
				text(at, barLabelY, labels=schemeShortSingular, srt=90, adj=1, xpd=NA, col='darkred', cex=cexBar)

				at <- at + 1

			} # next scheme

		} # next PME

	dev.off()
}

######################################
### boxplots ensemble with 1 panel ###
######################################

if ('boxplots ensemble with 1 panel' %in% do) {

	say('visualize predictor importance for ENSEMBLE MULTIVARIATE ENMs using PCs + UNITS against self and other units -- using 1 PANEL', level=1)

	say('One set of boxplots per scheme. Each set has two boxplots, one for min PME and one for no PME.')

	say('Using ', paste(toupper(algos), collapse=' & '), '!!!')

	## test statistic for boxplot

		# testStat <- 'cbi'
		testStat <- 'cor'

	testStatNice <- statNice(testStat)

	ylim <- c(0, 0.45)
	cexBar <- 0.7 # bar text

	## PCA
	pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

	# y coordinate for box labels
	barLabelY <- ylim[1] - 0.07 * diff(ylim)

	png(paste0(workDir, '/ENMs - PCs + Units/Variable Importance - ', testStatNice, ' with One Panel.png'), width=1000, height=1200, res=300)

		# plot setup
		par(mfrow=c(1, 1), mar=1 * c(7, 4, 4, 2) + 0.1, mgp=c(2, 1, 0), cex.main=0.6, cex.axis=0.6, cex.lab=0.8)

		# plot(1:10, 1:10, col='white', xlab='', ylab=paste0(testStatNice, '\n\U2190 more important   less important \U2192'), ylim=ylim, xaxt='n', xlim=c(0.5, 4.5))
		plot(1:10, 1:10, col='white', xlab='', ylab='Subdivision unit importance', ylim=ylim, xaxt='n', xlim=c(0.5, 4.5))
		axis(side=1, at=1:4, labels=FALSE)

		### by PME
		for (pme in rev(pmes)) {

			pmeNice <- pmeNiceName(pme)
			bgNice <- bgNiceName(pme)

			# position for each scheme
			at <- if (pme == 'pmeMin') { 0.87 } else { 1.13 }

			### SCHEME
			for (scheme in schemes) {

				say(pme, ' ', scheme)

				out <- schemeInfo(scheme)
				schemeNice <- out$schemeNice
				schemeShort <- out$schemeShort
				divisionFieldPres <- out$divisionFieldPres
				schemeShortSingular <- out$schemeShortSingular
				col <- out$col
				
				col <- if (pme == 'pmeMin') {
					'#1b9e77'
				} else {
					'#d95f02'
				}
				
				rm(out); gc()

				# evaluation data for this algorithm
				load(paste0(workDir, 'ENMs - PCs + Units/', schemeShort, '/ENM Evaluations - ', schemeShort, ' - ', pmeNice, ' PME - ENSEMBLE.Rdata'))

				# 3) test stat of units permuted (center, front)
				resp <- 1 - evalFrame[ , paste0(testStat, 'Import_unit')]
				boxplot(resp, at=at, col=alpha(col, 0.5), border=col, add=TRUE)
				# if (pme == 'pmeMin') text(at + 0.13, barLabelY, labels=schemeShortSingular, srt=90, adj=1, xpd=NA, col='black', cex=cexBar)
				if (pme == 'pmeMin') text(x=at + 0.13, y=ylim[1] - 0.15 * diff(ylim), xpd=NA, labels=schemeNice, srt=90, adj=1, cex=0.8, srt=55)
				
				if (scheme == 'ecoregionEpa3Modified' & pme == 'pmeMin') {
					text(at, 0.90 * ylim[2], 'a', pos=3, cex=0.6)
				}

				if (scheme == 'elevQuantWrtPaeMin' & pme == 'pmeMin') {
					text(at, 0.90 * ylim[2], 'ab', pos=3, cex=0.6)
				}

				if (
					(scheme == 'cladeNonOverlap' & pme == 'pmeMin') |
					(scheme == 'cladeNonOverlap' & pme == 'pmeNone') |

					# (scheme == 'ecoregionEpa3Modified' & pme == 'pmeMin') |
					(scheme == 'ecoregionEpa3Modified' & pme == 'pmeNone') |

					# (scheme == 'elevQuantWrtPaeMin' & pme == 'pmeMin') |
					(scheme == 'elevQuantWrtPaeMin' & pme == 'pmeNone') |

					(scheme == 'physioFenneman' & pme == 'pmeMin')
					# (scheme == 'physioFenneman' & pme == 'pmeNone')
				) {
					text(at, 0.90 * ylim[2], 'b', pos=3, cex=0.6)
				}

				at <- at + 1

			} # next scheme

		} # next PME

	dev.off()
}

########################################################################
### visualize response plots for MULTIVARIATE ENMs using PCs + UNITS ###
########################################################################

if ('response plots' %in% do) {

	say('visualize response plots for MULTIVARIATE ENMs using PCs + UNITS', level=1)

	say('Creates one plot per scheme x PME.')
	say('Each plot has a one subpanel for each algorithm. Each subplot graphs predicted response along y vs PCx along x with all other PCs held constant. Different units are indicated by lines of different colors.')

	say('Using ', paste(toupper(algos), collapse=' & '), '!!!')
	say('Using PC', prefix(pc, 2), '!!!')

	# alpha values
	fadePoints <- 0.2 # plot across entire extent's env range
	fadeUnit <- 0.4 # plot across just unit's env range
	fadeUnitAllSites <- 0.8 # plot across just unit's env range

	lwdUnit <- 1
	lwdUnitAllSites <- 4

	legendCex <- 1
	loadCex <- 1

	focalPc <- paste0('PC', prefix(pc, 2))

	## PCA
	pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

	# selecting PCs that cumulatively explain x% of variance
	pcNames <- paste0('PC', prefix(1:length(pca$sdev), 2))
	cumulExplain <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
	pcsUsed <- pcNames[cumulExplain <= pcaMinCumulExplained]
	if (length(pcsUsed) < length(pcNames)) pcsUsed <- c(pcsUsed, paste0('PC', prefix(length(pcsUsed) + 1, 2)))

	### get and format text indicating highest loadings for focal PC
	loadings <- pca$loadings
	loadings <- as.matrix(loadings[1:nrow(loadings), 1:ncol(loadings)])
	loadings <- as.data.frame(loadings)
	loadings <- loadings[ , seq_along(pcsUsed)]
	names(loadings) <- pcsUsed
	tholdTopLoadings <- quantile(abs(loadings[ , focalPc]), 0.75)
	topLoadNames <- rownames(loadings)[abs(loadings[ , focalPc]) >= tholdTopLoadings]
	topLoad <- loadings[topLoadNames, focalPc]
	names(topLoad) <- topLoadNames

	# format for printing
	topLoadSignLeft <- ifelse(topLoad < 0, '\U2190', '')
	topLoadSignRight <- ifelse(topLoad < 0, '', '\U2192')
	niceLoadNames <- fields$nameShort[match(names(topLoad), fields$factor)]
	loadAmount <- paste0(' (', format(topLoad, digits = 2), ')')
	loadText <- paste0(topLoadSignLeft, ' ', niceLoadNames, loadAmount, ' ', topLoadSignRight)
	loadText <- paste(loadText, collapse='\n')

	par(mfrow=c(2, 3), mar=1 * c(4, 4, 3, 2) + 0.1)

	### SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		schemeShortSingular <- out$schemeShortSingular
		rm(out); gc()

		# get meta data on units for plotting
		thisUnitMeta <- unitMeta[unitMeta$scheme == scheme & unitMeta$unit != 'all', ]
		units <- thisUnitMeta$unit
		cols <- thisUnitMeta$color

		### by PME
		for (pmeVariant in pmes) {

			pmeNice <- pmeNiceName(pmeVariant)
			bgNice <- bgNiceName(pmeVariant)

			## load species/BG data
			load(paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata'))

			allPres <- speciesData$allPresences

			# by ALGORITHM
			for (thisAlgo in algos) {

				say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | ALGO ', thisAlgo)

				### base plot
				x <- range(allPres[ , focalPc])
				x <- c(x[1] - 0.05 * diff(x), x[2] + 0.05 * diff(x))
				plot(x=x, y=x, col='white', ylab='Environmental Suitability', xlab=focalPc, ylim=c(0, 1.1), main=paste0(schemeNice, ': ', toupper(thisAlgo)))

				### rugs: one per unit
				rugYStart <- 1.131
				for (countUnit in seq_along(units)) {

					rug(x=allPres[speciesData$allPresences[ , divisionFieldPres] == countUnit, focalPc], col=cols[countUnit], pos=rugYStart - 0.012 * (countUnit - 1), ticksize=0.015, side=3, xpd=NA)

				}

				### plot k-fold model responses
				###############################

				## by K-FOLD
				for (k in 1:kFolds) {

					# load model file
					load(paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate k=', prefix(k, 2), '.Rdata'))

					# by UNIT
					for (countUnit in thisUnitMeta$factorInteger) {

						### predict along range of unit
						col <- cols[countUnit]

						# environmental data at presences of this unit
						unitEnv <- allPres[allPres[ , divisionFieldPres] == countUnit, c(pcsUsed, divisionFieldPres)]
						unitEnv <- unitEnv[order(unitEnv[ , focalPc]), ]

						# prediction
						predUnit <- predictEnm(thisData=unitEnv, theModel=model$model, predictors=names(unitEnv), na.rm=TRUE, predArgs=predArgs)

						# predicted points
						points(unitEnv[ , focalPc], predUnit, pch='.', col=alpha(col, fadePoints))

						# lowess estimator of response
						approxPredUnit <- lowess(unitEnv[ , focalPc], predUnit, f=1/3)
						approxPredUnit <- as.data.frame(approxPredUnit)
						approxPredUnit <- approxPredUnit[which(approxPredUnit$y <= 1), ]
						lines(approxPredUnit, col=alpha(col, fadeUnit), lwd=lwdUnit)

					} # next unit

				} # next k-fold

				### plot ALL SITES response
				###########################

				# load model
				load(paste0(workDir, 'ENMs - PCs + Units/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/', toupper(thisAlgo), ' multivariate all sites.Rdata'))

				# by UNIT
				for (countUnit in seq_along(units)) {

					### predict along range of unit
					col <- cols[countUnit]

					# environmental data at presences of this unit
					unitEnv <- allPres[allPres[ , divisionFieldPres] == countUnit, c(pcsUsed, divisionFieldPres)]
					unitEnv <- unitEnv[order(unitEnv[ , focalPc]), ]

					# predictions
					predUnit <- predictEnm(thisData=unitEnv, theModel=model$model, predictors=names(unitEnv), na.rm=TRUE, predArgs=predArgs)

					# lowess estimator
					approxPredUnit <- lowess(unitEnv[ , focalPc], predUnit, f=1/3)
					approxPredUnit <- as.data.frame(approxPredUnit)
					approxPredUnit <- approxPredUnit[which(approxPredUnit$y <= 1), ]
					lines(approxPredUnit, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

				} # next unit

				### legend
				legend('bottomleft', inset=0.01, legend=niceUnitName(scheme, thisUnitMeta$unit, short=TRUE, veryShort=FALSE), col=cols, lwd=lwdUnitAllSites, cex=legendCex, bty='n')

				### print helpful text indicating highest loadings for this axis
				bx <- par('usr')
				text(x=bx[2] - 0.01 * diff(bx[1:2]), y=0.01, labels=loadText, adj=c(1, 0), cex=loadCex)

			} # next algorithm

		} # next PME

	} # next scheme

}

say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
