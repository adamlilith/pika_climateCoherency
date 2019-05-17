### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2016-11

# source('C:/Ecology/Drive/Research/Iconic Species/pika_climateCoherency/05 ENM Analysis - Multivariate - Derived Variables.r')

rm(list=ls())

	drive <- 'C:/'
	# drive <- 'D:/'
	# drive <- 'E:/'
	
	# do <- c('collate', 'map folds', 'pca', 'train', 'eval', 'overlap', 'map', 'boxplot')
	# do <- 'copy data'
	# do <- c('train')
	# do <- 'calculate response heterogeneity'
	# do <- 'response heterogeneity permutation test > 0'
	# do <- 'plot hetero ratio'
	do <- 'plot hetero difference'
	# do <- 'table of variable importance by scheme and PME'
	# do <- 'response curves'
	# do <- 'illustrate calculation of heterogeneity in within/among responses'
	# do <- 'summary plot of response curves'

	pmes <- c('pmeNone', 'pmeMin')
	# pmes <- 'pmeNone'
	# pmes <- 'pmeMin'

	# valances <- c('including', 'excluding')
	valances <- 'including'
	# valances <- 'excluding'


	schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')
	# schemes <- c('ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')

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

### CONTENTS ###
### libraries, variables, and functions ###
### copy model training data for ALL unit of each division schemes and convert unit field to factor ###
### train regularized GLMs ###
### plot response curves of regularized GLMs ###
### calculate heterogeneity of response ###
### plot within/among heterogeneity in response ###
### plot among - within heterogeneity in response ###
### summary plot of response curves ###

### collate model training data for all division schemes ###
### make maps of g-folds ###
### summary plot ###



### train PCA for an entire scheme ###

### train MULTIVARIATE ENMs using PCs ###
### evaluate MULTIVARIATE ENMs using PCs against self and other units ###
### calculate niche overlap from ENMs based on PCs ###

### visualize multivariate PC ENM analysis - maps ###
### visualize multivariate PC ENM analysis - boxplots ###




###########################################
### libraries, variables, and functions ###
###########################################

	source(paste0(drive, '/ecology/Drive/Research/Iconic Species/pika_climateCoherency/!Omnibus Variables for Pika Non-Stationarity Analysis.r'))

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
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			say(base::date(), ' | SCHEME ', schemeNice, ' | PME ', pmeNice, pre=1)

			# by VALANCE
			for (fromValance in valances) {

				for (thisUnit in getUnits(scheme, incAll=TRUE)) {

					fromDir <- paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(fromValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME/Ochotona princeps/')
					toDir <- paste0(workDir, 'ENMs - Derived/', schemeNice, '/', toupper(fromValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME/Ochotona princeps/')

					dirCreate(toDir)

					file.copy(paste0(fromDir, '!Species Data - Ochotona princeps.Rdata'), paste0(toDir, '!Species Data - Ochotona princeps.Rdata'), overwrite=TRUE)

					## convert unit names to integers
					load(paste0(toDir, '!Species Data - Ochotona princeps.Rdata'))

					if (length(naRows(speciesData$allPresences[ , divisionFieldPres])) > 0) speciesData$allPresences <- speciesData$allPresences[-naRows(speciesData$allPresences[ , divisionFieldPres]), ]
					if (length(naRows(speciesData$allTrainingAbsences[ , divisionFieldPres])) > 0) speciesData$allTrainingAbsences <- speciesData$allTrainingAbsences[-naRows(speciesData$allTrainingAbsences[ , divisionFieldPres]), ]
					if (length(naRows(speciesData$allTestAbsences[ , divisionFieldPres])) > 0) speciesData$allTestAbsences <- speciesData$allTestAbsences[-naRows(speciesData$allTestAbsences[ , divisionFieldPres]), ]

					# by K-FOLD
					for (k in 1:kFolds) {

						if (length(naRows(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$trainingPresences$kFold[[k]] <- speciesData$trainingPresences$kFold[[k]][-naRows(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres]), ]

						if (length(naRows(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$trainingAbsences$kFold[[k]] <- speciesData$trainingAbsences$kFold[[k]][-naRows(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres]), ]

						if (length(naRows(speciesData$testPresences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$testPresences$kFold[[k]] <- speciesData$testPresences$kFold[[k]][-naRows(speciesData$testPresences$kFold[[k]][ , divisionFieldPres]), ]

						if (length(naRows(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres])) > 0) speciesData$testAbsences$kFold[[k]] <- speciesData$testAbsences$kFold[[k]][-naRows(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres]), ]

						# speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres])
						# speciesData$testPresences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$testPresences$kFold[[k]][ , divisionFieldPres])
						# speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres])
						# speciesData$testAbsences$kFold[[k]][ , divisionFieldPres] <- unitNameToInteger(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres])

						# speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$trainingPresences$kFold[[k]][ , divisionFieldPres])
						# speciesData$testPresences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$testPresences$kFold[[k]][ , divisionFieldPres])
						# speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$trainingAbsences$kFold[[k]][ , divisionFieldPres])
						# speciesData$testAbsences$kFold[[k]][ , divisionFieldPres] <- as.factor(speciesData$testAbsences$kFold[[k]][ , divisionFieldPres])

					}

					save(speciesData, file=paste0(toDir, '!Species Data - Ochotona princeps.Rdata'), compress=TRUE)

				} # next unit

			} # next valance

		} # next PME

	} # next scheme

}


##############################
### train regularized GLMs ###
##############################

if ('train' %in% do) {

	say('train regularized GLMs', level=1)

	# get presences
	pres <- getPres()

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# by VALANCE
			for (fromValance in c('including')) {

				# get names of focal units to include/exclude
				units <- if (is.null(forceUnit)) {
					getUnits(scheme=scheme, incAll=TRUE)
				} else {
					forceUnit
				}

				say('UNITS: ', paste(units, collapse=' | '), pre=1)

				# by UNIT
				for (fromUnit in units) {

					say(base::date(), '|  SCHEME ', schemeShort, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, pre=1)

					trainSdm(
						speciesList='Ochotona princeps',
						outputDir=paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME'),
						predictorsToUse=predictorsToUse,
						modelType='multivariate',
						centeringStats=NULL,
						responseVar=NULL,
						responseTrans=NULL,
						responseTransInv=NULL,
						modelMethodsToUse='lars',
						equalPresAbsWeight=TRUE,
						kFolds=TRUE,
						wantAllSitesModels=FALSE,
						outNames='short',
						speciesDataFileAppend=NULL,
						modelParams=modelParams,
						tempDir=paste0(drive, 'ecology/!Scratch/_temp'),
						verbose=1
					)

				} # next from unit

			} # next valance

		} # next PME

	} # next scheme

}

################################################
### plot response curves of regularized GLMs ###
################################################

if ('response curves' %in% do) {

	say('plot response curves of regularized GLMs', level=1)

	# predType <- 'Full Model'
	predType <- 'Marginal'

	includeK <- TRUE # include k-fold response curves
	# includeK <- FALSE # include k-fold response curves

	say('Plotting ', toupper(predType), ' predictions', ifelse(includeK, ' with k-folds', ''), '.')

	# alpha values
	fadePoints <- 0.2 # plot across entire extent's env range
	fadeUnit <- 0.4 # plot across just unit's env range
	fadeUnitAllSites <- 0.8 # plot across just unit's env range

	lwdUnit <- 0.5
	lwdUnitAllSites <- 1.8 + includeK

	legendCex <- 0.3
	mainCex <- 0.6
	labCex <- 0.5
	axisCex <- 0.5

	outDir <- paste0(workDir, 'ENMs - Derived/!Response Curves - ', predType, ifelse(includeK, ' with K-folds', ''))
	dirCreate(outDir)

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		thisUnitMeta <- unitMeta[unitMeta$scheme == scheme, ]
		units <- thisUnitMeta$unit


		# by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# by "from" unit valance...inclusive or exclusive of focal region
			for (fromValance in valances) {

				# load species' data and background
				load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'))

				allPres <- speciesData$allPresences

				# by PREDICTOR
				for (thisPred in predictorsToUse) {

					say('SCHEME ', schemeShort, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | PREDICTOR ', thisPred)

						png(paste0(outDir, '/', schemeShort, ' - ', thisPred, ' - ', pmeNice, ' PME - ', toupper(fromValance), ' - ', predType, ' Prediction - LARS.png'), width=800, height=600, res=300)

							par(mar=1 * 0.3 * c(5, 4, 4, 2) + 0.1, cex.main=mainCex, cex.lab=labCex, cex.axis=axisCex, mgp=c(0.7, 0.1, 0), tck=-0.02)

							x <- range(allPres[ , thisPred])
							x <- c(x[1] - 0 * diff(x), x[2] + 0 * diff(x))
							plot(x=x, y=x, col='white', ylab='Suitability', main=paste0('Regularized GLM - ', schemeNice, ' - ', pmeNice, ' PME\n', fields$nameShort[fields$factor %in% thisPred], ' - ', predType, ' Prediction'), ylim=c(0, 1.1), xlab=fields$nameShortWithUnits[fields$factor %in% thisPred])

							### rugs: one per unit
							rugYStart <- 1.131
							for (countUnit in seq_along(getUnits(scheme, incAll=FALSE))) {

								thisUnit <- getUnits(scheme, incAll=FALSE)[countUnit]

								rug(x=allPres[allPres[ , divisionFieldPres] == thisUnit, thisPred], col=getUnitCols(thisUnit), pos=rugYStart - 0.012 * (countUnit - 1), ticksize=0.015, side=3, xpd=NA)

							}

							### plot K-FOLD MODELS
							######################
							
							if (includeK) {

								for (k in 1:kFolds) {

									# by "from" UNIT
									for (fromUnit in units) {

										# load k-fold model
										load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata'))

										env <- model$trainingPresences
										model <- model$model

										# restrict environment
										env <- env[order(env[ , thisPred]), ]
										low <- quantile(env[ , thisPred], 0.025)
										high <- quantile(env[ , thisPred], 0.975)
										env <- env[env[ , thisPred] >= low & env[ , thisPred] < high, ]

										# predict
										preds <- if (predType == 'Full Model') { NULL } else { thisPred }
										pred <- predictLars(object=model, newdata=env, type='response', preds=preds)

										rm(model); gc()

										# plot if valid prediction
										if (!is.null(pred) && length(pred) > 0) {

											# plot
											col <- getUnitCols(fromUnit)
											points(env[ , thisPred], pred, col=alpha(col, fadePoints), pch=16, cex=0.1)

											if (predType == 'Full Model') {

												# lowess estimator of response
												points(env[ , thisPred], pred, col=alpha(col, fadePoints), pch='.')
												smooth <- lowess(pred ~ env[ , thisPred])
												smooth$y[smooth$y > 1] <- 1
												smooth$y[smooth$y < 0] <- 0
												lines(smooth$x, smooth$y, col=alpha(col, fadeUnitAllSites), lwd=lwdUnit)

											} else {

												lines(env[ , thisPred], pred, col=alpha(col, fadeUnitAllSites), lwd=lwdUnit)

											}

										} # if valid prediction

									} # next unit

								} # next k-fold

							} # if including k-folds

							## ALL-SITES MODELS
							###################

							# by "from" UNIT
							for (fromUnit in units) {

								# load unit model
								load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))

								# get environment to which to predict
								env <- model$trainingPresences
								env <- env[order(env[ , thisPred]), ]
								low <- quantile(env[ , thisPred], 0.025)
								high <- quantile(env[ , thisPred], 0.975)

								env <- env[env[ , thisPred] >= low & env[ , thisPred] < high, ]

								# predict
								preds <- if (predType == 'Full Model') { NULL } else { thisPred }
								pred <- predictLars(object=model$model, newdata=env, type='response', preds=preds)
								rm(model); gc()

								if (!is.null(pred) && length(pred) > 0) {

									# plot
									col <- getUnitCols(fromUnit)

									if (predType == 'Full Model') {

										# lowess estimator of response
										points(env[ , thisPred], pred, col=alpha(col, fadePoints), pch='.')
										smooth <- lowess(pred ~ env[ , thisPred])
										smooth$y[smooth$y > 1] <- 1
										smooth$y[smooth$y < 0] <- 0
										lines(smooth$x, smooth$y, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

									} else {

										lines(env[ , thisPred], pred, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

									}

								}

							} # next fromUnit

							### legend
							legend('topright', inset=0.01, legend=niceUnitName(scheme, units, short=FALSE, veryShort=TRUE), col=getUnitCols(units), lwd=0.8 * lwdUnitAllSites, cex=legendCex, bty='n')

					dev.off()

				} # next predictor

			} # next fromValance

		} # next PME

	} # next division scheme

}

#########################################################################
### illustrate calculation of heterogeneity in within/among responses ###
#########################################################################

if ('illustrate calculation of heterogeneity in within/among responses' %in% do) {

	say('illustrate calculation of heterogeneity in within/among responses', level=1)

	scheme <- 'cladeNonOverlap'
	fromUnit <- 'schisticeps'
	pme <- 'pmeNone'
	thisPred <- 'acuteHeat_days'
	focalKFold <- 5
	
	fromValance <- 'including'
	
	predType <- 'Marginal'

	# quantiles across which to calculate response (response in tails sometimes depends only on a few presences and so sometimes seems erratic)
	across <- c(0.025, 0.975)

	# number of environmental values across occupied environmental breadth to calculate comparison
	n <- 100
	
	# alpha values
	fadePoints <- 0.2 # plot across entire extent's env range
	fadeUnit <- 0.4 # plot across just unit's env range
	fadeUnitAllSites <- 0.8 # plot across just unit's env range

	lwdUnit <- 0.8 # k-folds
	lwdUnitAllSites <- 2.4 # all-sites models

	mainCex <- 0.6
	labCex <- 0.5
	axisCex <- 0.5

	outDir <- paste0(workDir, 'ENMs - Derived/!Response Curves - ', predType, ' with K-folds')

	out <- schemeInfo(scheme)
	schemeNice <- out$schemeNice
	schemeShort <- out$schemeShort
	divisionFieldPres <- out$divisionFieldPres
	rm(out); gc()

	thisUnitMeta <- unitMeta[unitMeta$scheme == scheme, ]
	units <- thisUnitMeta$unit

	pmeNice <- pmeNiceName(pme)

	### pre-process: load models, process environment, make predictions
		
		# load composite model
		load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ALL - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
		compModel <- model$model

		# load unit model
		load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
		unitModel <- model$model

		# selected k-fold model
		load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate k=', prefix(focalKFold, 2), '.Rdata'))
		kFoldModel <- model$model
		env <- envResampled <- model$trainingPresences

	### process environment
	
		# resampled environment (points at which to compare responses)

		envResampled <- envResampled[order(envResampled[ , thisPred]), ]
		low <- quantile(envResampled[ , thisPred], across[1])
		high <- quantile(envResampled[ , thisPred], across[2])

		# values of the target variable against which to make comparisons
		compareValues <- seq(low, high, length.out=n)

		# within occupied environmental range find sites that most closely match the values at which to make comparisons
		matchingIndex <- rep(NA, n)
		for (i in 1:n) matchingIndex[i] <- which.min.simple(abs(compareValues[i] - envResampled[ , thisPred]), tie_value='random')
		matchingIndex <- unique(matchingIndex)
		nEffective <- length(matchingIndex)
		envResampled <- envResampled[matchingIndex, predictorsToUse]
		
	# environment across which to predict selected k-fold response (not resampled)
	
		env <- env[order(env[ , thisPred]), ]
		low <- quantile(env[ , thisPred], across[1])
		high <- quantile(env[ , thisPred], across[2])
		env <- env[env[ , thisPred] >= low & env[ , thisPred] < high, ]

	# get predictions

		preds <- if (predType == 'Full Model') { NULL } else { thisPred }

		predComp <- predictLars(object=compModel, newdata=env, type='response', preds=preds)
		predUnit <- predictLars(object=unitModel, newdata=env, type='response', preds=preds)
		predKFold <- predictLars(object=kFoldModel, newdata=env, type='response', preds=preds)

	# range of x-axis

		x <- pretty(range(env[ , thisPred]), 5)
		x <- range(x)

	png(paste0(workDir, 'ENMs - Derived/Illustration of Calculation of Within and Among Unit Heterogeneity.png'), width=800, height=1200, res=300)

		par(mfrow=c(2, 1), mar=1 * 0.3 * c(5, 4, 4, 2) + 0.1, cex.main=mainCex, cex.lab=labCex, cex.axis=axisCex, mgp=c(0.7, 0.1, 0), tck=-0.02, lwd=0.7)

		### WITHIN-UNIT HETEROGENEITY
		#############################
		
		# This plot will show responses for one unit. It will display the all-sites unit response and each k-fold response, with one fold highlighted. The difference between this k-fold response and the all-sites unit response will be illustrated.

		plot(x=x, y=x, col='white', ylab='Suitability', main='Within-unit heterogeneity', ylim=c(0, 1), xlab=fields$nameShortWithUnits[fields$factor %in% thisPred], xaxt='n', yaxt='n', bty='l')
		axis(1, lwd=0.7)
		axis(2, lwd=0.7, ylim=c(0, 1))
		
		### PLOT DIFFERENCE BETWEEN: SELECTED K-FOLD MODEL AND UNIT ALL-SITES MODEL and UNIT ALL-SITES MODEL AND COMPOSITE ALL-SITES MODEL
		
			# area between curves
			polygon(x=c(env[ , thisPred], rev(env[ , thisPred])), y=c(predUnit, rev(predKFold)), border=NA, col=alpha('cornflowerblue', 0.5))
			rug(x=envResampled[ , thisPred], ticksize=0.027)

		### PLOT EACH K-FOLD MODEL
			
			col <- getUnitCols(fromUnit)

			for (k in 1:kFolds) {
			# for (k in focalKFold) {

				# load k-fold model
				load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata'))

				thisEnv <- model$trainingPresences
				model <- model$model

				# restrict environment
				thisEnv <- thisEnv[order(thisEnv[ , thisPred]), ]
				low <- quantile(thisEnv[ , thisPred], across[1])
				high <- quantile(thisEnv[ , thisPred], across[2])
				thisEnv <- thisEnv[thisEnv[ , thisPred] >= low & thisEnv[ , thisPred] < high, ]

				pred <- predictLars(object=model, newdata=thisEnv, type='response', preds=preds)

				if (predType == 'Full Model') {

					# lowess estimator of response
					points(thisEnv[ , thisPred], pred, col=alpha(col, fadePoints), pch='.')
					smooth <- lowess(pred ~ thisEnv[ , thisPred])
					smooth$y[smooth$y > 1] <- 1
					smooth$y[smooth$y < 0] <- 0
					lines(smooth$x, smooth$y, col=alpha(col, fadeUnitAllSites), lwd=lwdUnit)

				} else {

					# lines(thisEnv[ , thisPred], pred, col=alpha(col, fadeUnitAllSites), lwd=lwdUnit, lty=ifelse(k == focalKFold, 'solid', 'dashed'))
					lines(thisEnv[ , thisPred], pred, col=alpha(col, fadeUnitAllSites), lwd=lwdUnit, lty=ifelse(k == focalKFold, 'dashed', 'dashed'))

				}
				
			} # next k-fold

		### PLOT UNIT ALL-SITES MODEL

			col <- getUnitCols(fromUnit)
		
			if (predType == 'Full Model') {

				# lowess estimator of response
				points(env[ , thisPred], predUnit, col=alpha(col, fadePoints), pch='.')
				smooth <- lowess(predUnit ~ env[ , thisPred])
				smooth$y[smooth$y > 1] <- 1
				smooth$y[smooth$y < 0] <- 0
				lines(smooth$x, smooth$y, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			} else {

				lines(env[ , thisPred], predUnit, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			}

		### SCALING FACTOR
		
			xScale <- 0.95
			xScaleNudge <- 0.06
		
			lines(c(min(env[ , thisPred]), xScale * max(x)), c(max(predUnit), max(predUnit)), lty='dotted', xpd=NA)
			lines(c(max(env[ , thisPred]), xScale * max(x)), c(min(predUnit), min(predUnit)), lty='dotted', xpd=NA)
			arrows(xScale * max(x), min(predUnit), xScale * max(x), max(predUnit), length=0.05, angle=20, code=3, xpd=NA)
			# text((xScale + xScaleNudge) * max(x), mean(c(min(predUnit), max(predUnit))), labels=parse(text='R[k]^(u)'), xpd=NA, vfont=NULL, font=3, cex=0.5)

		legend('bottomleft', inset=c(0.01, 0.06), legend=c('K-Fold Models', 'Unit Model'), lwd=c(lwdUnit, lwdUnitAllSites, lwdUnitAllSites), lty=c('dashed', 'solid', 'solid'), col=getUnitCols(fromUnit), cex=0.3)

	### AMONG-UNIT HETEROGENEITY
	############################
		
		# This plot will show responses for one unit and the composite all-sites model. The difference between the unit model and all-sites composite response will be illustrated.

		plot(x=x, y=x, col='white', ylab='Suitability', main='Among-unit heterogeneity', ylim=c(0, 1), xlab=fields$nameShortWithUnits[fields$factor %in% thisPred], xaxt='n', yaxt='n', bty='l')
		axis(1, lwd=0.7)
		axis(2, lwd=0.7, ylim=c(0, 1))
		
		### PLOT DIFFERENCE BETWEEN SELECTED UNIT ALL-SITES MODEL AND COMPOSITE ALL-SITES MODEL
		
			# area between curves
			polygon(x=c(env[ , thisPred], rev(env[ , thisPred])), y=c(predUnit, rev(predComp)), border=NA, col=alpha('cornflowerblue', 0.5))
			
			# sample sites
			rug(x=envResampled[ , thisPred], ticksize=0.027)
			
		### PLOT UNIT ALL-SITES MODELS
		for (thisUnit in getUnits(scheme, FALSE)) {
		
			load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
			thisUnitModel <- model$model
			
			predThisUnit <- predictLars(object=thisUnitModel, newdata=env, type='response', preds=preds)

			col <- getUnitCols(thisUnit)
			
			if (predType == 'Full Model') {

				# lowess estimator of response
				points(env[ , thisPred], predThisUnit, col=alpha(col, fadePoints), pch='.')
				smooth <- lowess(predThisUnit ~ env[ , thisPred])
				smooth$y[smooth$y > 1] <- 1
				smooth$y[smooth$y < 0] <- 0
				lines(smooth$x, smooth$y, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			} else {

				lines(env[ , thisPred], predThisUnit, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			}
		
		
		}
		
		### PLOT COMPOSITE ALL-SITES MODEL
		
			# plot
			col <- getUnitCols('all')

			if (predType == 'Full Model') {

				# lowess estimator of response
				points(env[ , thisPred], predComp, col=alpha(col, fadePoints), pch='.')
				smooth <- lowess(predComp ~ env[ , thisPred])
				smooth$y[smooth$y > 1] <- 1
				smooth$y[smooth$y < 0] <- 0
				lines(smooth$x, smooth$y, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			} else {

				lines(env[ , thisPred], predComp, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			}

		### PLOT UNIT ALL-SITES MODEL
		
			# plot
			col <- getUnitCols(fromUnit)

			if (predType == 'Full Model') {

				# lowess estimator of response
				points(env[ , thisPred], predUnit, col=alpha(col, fadePoints), pch='.')
				smooth <- lowess(predUnit ~ env[ , thisPred])
				smooth$y[smooth$y > 1] <- 1
				smooth$y[smooth$y < 0] <- 0
				lines(smooth$x, smooth$y, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			} else {

				lines(env[ , thisPred], predUnit, col=alpha(col, fadeUnitAllSites), lwd=lwdUnitAllSites)

			}
			
		### SCALING FACTOR
		
			xScale <- 0.95
			xScaleNudge <- 0.06
		
			lines(c(min(env[ , thisPred]), xScale * max(x)), c(max(predComp), max(predComp)), lty='dotted', xpd=NA)
			lines(c(max(env[ , thisPred]), xScale * max(x)), c(min(predComp), min(predComp)), lty='dotted', xpd=NA)
			arrows(xScale * max(x), min(predComp), xScale * max(x), max(predComp), length=0.05, angle=20, code=3, xpd=NA)
			# text((xScale + xScaleNudge) * max(x), mean(c(min(predComp), max(predComp))), labels=parse(text='R[k]^(C)'), xpd=NA, vfont=NULL, font=3, cex=0.5)
			
	units <- getUnits(scheme, FALSE)
	legend('bottomleft', inset=c(0.01, 0.12), legend=c(units, 'Composite Model'), lwd=lwdUnitAllSites, col=c(getUnitCols(units, FALSE), 'black'), cex=0.3)

	dev.off()	
		
		
}

###########################################
### calculate heterogeneity of response ###
###########################################

if ('calculate response heterogeneity' %in% do) {

	say('calculate heterogeneity of response', level=1)

	# type of prediction
	predType <- 'Marginal'
	
	# quantiles across which to calculate response (response in tails sometimes depends only on a few presences and so sometimes seems erratic)
	across <- c(0.025, 0.975)

	# number of environmental values across occupied environmental breadth to calculate comparison
	n <- 100

	# environmental width across which to calculate comparison
	# envWidth <- 'species' # select n equally-spaced values of response variables across ENTIRE range of variable across ALL presences
	envWidth <- 'kfold' # select n equally-spaced values of response variables across just the portion of the variable occupied by the k-fold training sites

	set.seed(exp(1))

	say('This permutation procedure tests the null hypothesis that the among - within difference for a given scheme and PME is > 0. It is based on the assumption that the predictions from the k-fold models and all-sites unit models are drawn from the same distribution and that the predictions from the unit all-site models and composite all-sites model are drawn from the same distribution. Heterogeneity is calculated as described in previous steps. Note that the predictions for a particular combination of k-fold model and unit all-sites model or for a combination of unit all-sites model and all-sites composite model are truncated by the environmental width of the k-fold model or unit all-sites model, respectively. Thus the predictions cannot be simply calculated and swapped randomly between within/among components. Instead, I am drawing a model from the pool ', breaks=120, post=2)
	say('Calculating heterogeneity of response curves for ', toupper(predType), ' relative to response of ALL-SITES model.')
	say('This method is for examining the heterogeneity of the response to each climatic variable across k-folds within each unit. The idea is that "true" units will have a more homogenous response than "false" units which combine populations with qualitatively different responses. WITHIN-GROUP heterogeneity will be calculated within each unit of a scheme using the response to the ALL-SITES model as a baseline. Numerically, WITHIN-GROUP heterogeneity for a given comparison between a sub-group response curve and group response curve will be W * sqrt((1/k) SUM_over_kfolds[=k] MAN_across_n_sites_along_environmental_axis ((prediction_of_model_k) - (all_sites_model_prediction))^2) for each unit, where the MEAN is either across n semi-equally-spaced value within the intersection of the range of the training kfolds values OR across n values across the entire occupied range of the variable. Points along the environmental axis are placed such that there are n. Initially n equally-spaced values along environmental variable x are used. Each is then matched to the closest value of x in the all-sites training presence set. If the same presence is chosen more than once when finding the presences climatically closest to the n sites then duplicates are discarded (i.e., thus there may be <n sites across which calculations are performed).  This process is used because there are sometimes interactions between variables, so each point on x must be matched with other variables that will not necessarily increase linearly and proportionately with x. W is equal to (max - min) prediction all-sites model (e.g., a value on [0, 1]). It serves as a weight to reflect the fact that responses to some variables are very slight.', breaks=120, post=2)
	
	say('AMONG GROUP heterogeneity will use the ALL-SITES COMPOSITE model prediction as a baseline and be equal to W * sqrt((1/U) SUM_across_units[=u] ((1/n) SUM_across_sites_arranged_along_environmental_axis[=n] (composite_all-sites_model_prediction) - (prediction_from_all-sites model_of_unit_u))^2). Sites along the variable of interest are located as per the calculation of within-unit heterogeneity. W is max - min prediction of the composite model and serves as a weight that accounts for the degree of absolute variable importance.', breaks=120)
	say('Along with each estimate is the range of suitability spanned by the ALL-SITES UNIT model and the ALL-SITES COMPOSITE model. These can be used as weights in an ANOVA-like analysis.', pre=2, breaks=60)

	# data frame to remember results
	hetero <- data.frame()

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		thisUnitMeta <- unitMeta[unitMeta$scheme == scheme, ]
		units <- thisUnitMeta$unit
		units <- units[-which(units %in% 'all')]

		# by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# by "from" unit valance...inclusive or exclusive of focal region
			for (fromValance in valances) {

				# load species' data and background
				load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'))
				allPres <- speciesData$allPresences
				rm(speciesData)

				# load COMPOSITE UNIT ALL-SITES MODEL
				load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ALL - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
				allSitesCompositeModel <- model$model
				rm(model) ; gc()

				# by UNIT
				for (fromUnit in units) {

					# load ALL-SITES UNIT model
					load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
					allSitesUnitModel <- model$model
					rm(model) ; gc()

					# by K-FOLD
					for (k in 1:kFolds) {

						say('SCHEME ', schemeShort, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, ' | K ', k, post=0)

						# load K-FOLD model
						modelFileName <- paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata')

						# if model exists
						if (file.exists(modelFileName)) {

							load(modelFileName)
							kFoldModel <- model$model
							kPres <- model$trainingPresences
							rm(model) ; gc()

							# by PREDICTOR
							for (thisPred in predictorsToUse) {

								# get environment to which to predict
								env <- if (envWidth == 'species') {
									allPres
								} else if (envWidth == 'kfold') {
									kPres
								}

								env <- env[order(env[ , thisPred]), ]
								envRange <- quantile(env[ , thisPred], across)
								low <- envRange[1]
								high <- envRange[2]

								# values of the target variable against which to make comparisons
								compareValues <- seq(envRange[1], envRange[2], length.out=n)

								# within occupied environmental range find sites that most closely match the values at which to make comparisons
								matchingIndex <- rep(NA, n)
								for (i in 1:n) matchingIndex[i] <- which.min.simple(abs(compareValues[i] - env[ , thisPred]), tie_value='random')
								matchingIndex <- unique(matchingIndex)
								nEffective <- length(matchingIndex)
								env <- env[matchingIndex, predictorsToUse]

								# predict
								preds <- if (predType == 'Full Model') { NULL } else { thisPred }
								predKFold <- try(predictLars(object=kFoldModel, newdata=env, type='response', preds=preds))
								predAllSitesUnit <- try(predictLars(object=allSitesUnitModel, newdata=env, type='response', preds=preds))
								predAllSitesComposite <- try(predictLars(object=allSitesCompositeModel, newdata=env, type='response', preds=preds))

								# calculate heterogeneity of response WITHIN unit
								if (!is.null(predAllSitesUnit) & !is.null(predKFold) & length(predAllSitesUnit) > 0 & length(predKFold) > 0 & class(predAllSitesUnit) != 'try-error' & class(predKFold) != 'try-error') {
									heteroWithin <- mean((predKFold - predAllSitesUnit)^2, na.rm=TRUE)
									changeUnitAllSites <- diff(range(predAllSitesUnit))
								} else {
									heteroWithin <- 0
									changeUnitAllSites <- 0
								}

								# calculate heterogeneity of response BETWEEN focal unit all-sites model and composite unit all-sites model
								if (!is.null(predAllSitesComposite) & !is.null(predAllSitesUnit) & length(predAllSitesComposite) > 0 & length(predAllSitesUnit) > 0 & class(predAllSitesComposite) != 'try-error' & class(predAllSitesUnit) != 'try-error') {
									heteroAmong <- mean((predAllSitesComposite - predAllSitesUnit)^2, na.rm=TRUE)
									changeCompositeAllSites <- diff(range(predAllSitesComposite))
								} else {
									heteroAmong <- 0
									changeCompositeAllSites <- 0
								}

								# remember
								hetero <- rbind(
									hetero,
									data.frame(
										scheme=scheme,
										pme=pme,
										fromValance=fromValance,
										fromUnit=fromUnit,
										predType=tolower(predType),
										envWidth=envWidth,
										across=paste(across, collapse=' '),
										n=n,
										nEffective=nEffective,
										k=k,
										predictor=thisPred,
										heteroWithin=heteroWithin,
										heteroAmong=heteroAmong,
										changeUnitAllSites=changeUnitAllSites,
										changeCompositeAllSites=changeCompositeAllSites
									)
								)

							} # next predictor

							say('')
							print(hetero)

						# if model doesn't exist
						} else {

							say('NONEXTANT!')

						}

					} # next k-fold

				} # next unit

			} # next fromValance

		} # next PME

	} # next division scheme

	saveRDS(hetero, paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves.rds'))

}

#########################################################
### permutation test if heterogeneity of response > 0 ###
#########################################################

if ('response heterogeneity permutation test > 0' %in% do) {

	say('response heterogeneity permutation test > 0', level=1)

	# number of randomization iterations
	iters <- 1000
	
	# type of prediction
	predType <- 'Marginal'
	
	# quantiles across which to calculate response (response in tails sometimes depends only on a few presences and so sometimes seems erratic)
	across <- c(0.025, 0.975)

	# number of environmental values across occupied environmental breadth to calculate comparison
	n <- 100

	# environmental width across which to calculate comparison
	# envWidth <- 'species' # select n equally-spaced values of response variables across ENTIRE range of variable across ALL presences
	envWidth <- 'kfold' # select n equally-spaced values of response variables across just the portion of the variable occupied by the k-fold training sites

	say('This permutation procedure tests the null hypothesis that the among - within difference for a given scheme and PME is > 0. It is based on the assumption that the predictions from the k-fold models and all-sites unit models are drawn from the same distribution and that the predictions from the unit all-site models and composite all-sites model are drawn from the same distribution. Heterogeneity is calculated as described in previous steps. Note that the predictions for a particular combination of k-fold model and unit all-sites model or for a combination of unit all-sites model and all-sites composite model are truncated by the environmental width of the k-fold model or unit all-sites model, respectively. Thus the predictions cannot be simply calculated and swapped randomly between within/among components. Instead, for each of the three models in one "within/among"calculation for a given scheme/PME I am drawing at random from the pool of: all k-fold models, the all-sites unit model, and the all-sites composite model, weighted such that the probability of drawing a k-fold model is equal to the probability of drawing either the all-sites unit model or the all-sites composite model.', breaks=120, post=2)

	# by SCHEME
	for (scheme in schemes) {

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		thisUnitMeta <- unitMeta[unitMeta$scheme == scheme, ]
		units <- thisUnitMeta$unit
		units <- units[-which(units %in% 'all')]

		# by PME
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# data frame to remember results
			test <- data.frame()

			# by "from" unit valance...inclusive or exclusive of focal region
			for (fromValance in valances) {

				# load species' data and background
				load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps.Rdata'))
				allPres <- speciesData$allPresences
				rm(speciesData)

				# load COMPOSITE UNIT ALL-SITES MODEL
				load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ALL - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
				allSitesCompositeModel <- model$model
				allSitesCompositePres <- model$trainingPresences
				rm(model); gc()

				### OBSERVED WITHIN vs AMONG
				############################

				# by FROM UNIT
				for (fromUnit in units) {
				
					say('SCHEME ', schemeShort, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit, post=0)

					# load ALL-SITES UNIT model
					load(paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))
					allSitesUnitModel <- model$model
					allSitesUnitPres <- model$trainingPresences
					rm(model); gc()

					# lists to hold all 8 k-fold models and k-fold presence environmental data
					kModels <- kPres <- list()
					
					# load K-FOLD UNIT models
					for (k in 1:kFolds) {

						modelFileName <- paste0(workDir, 'ENMs - Derived/', schemeShort, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/LARS multivariate k=', prefix(k, 2), '.Rdata')

						load(modelFileName)
						kModels[[k]] <- model$model
						kPres[[k]] <- model$trainingPresences
						rm(model); gc()
						
					}

					# list of all models available for this scheme/PME/unit... will draw from these at random
					allModels <- c(kModels, list(allSitesUnitModel), list(allSitesCompositeModel))
					allPresCollated <- c(kPres, list(allSitesCompositePres), list(allSitesUnitPres))
					w <- c(rep(1 / length(kModels), kFolds), 1, 1) # weights

					# by PREDICTOR
					for (thisPred in predictorsToUse) {

						say(thisPred, post=0)

						# by ITERATION
						for (iter in 1:iters) {

							# randomization of models
							index <- sample(seq_along(allModels), 3, prob=w)
							kModelRand <- allModels[[index[1]]]
							allSitesUnitModelRand <- allModels[[index[2]]]
							allSitesCompositeModelRand <- allModels[[index[3]]]
							kPresRand <- allPresCollated[[index[1]]]
						
							# get environment to which to predict
							env <- if (envWidth == 'species') {
								allPres
							} else if (envWidth == 'kfold') {
								kPresRand
							}

							env <- env[order(env[ , thisPred]), ]
							envRange <- quantile(env[ , thisPred], across)
							low <- envRange[1]
							high <- envRange[2]

							# values of the target variable against which to make comparisons
							compareValues <- seq(envRange[1], envRange[2], length.out=n)

							# within occupied environmental range find sites that most closely match the values at which to make comparisons
							matchingIndex <- rep(NA, n)
							for (i in 1:n) matchingIndex[i] <- which.min.simple(abs(compareValues[i] - env[ , thisPred]), tie_value='random')
							matchingIndex <- unique(matchingIndex)
							nEffective <- length(matchingIndex)
							env <- env[matchingIndex, predictorsToUse]

							# predict
							preds <- if (predType == 'Full Model') { NULL } else { thisPred }
							predKFold <- try(predictLars(object=kModelRand, newdata=env, type='response', preds=preds))
							predAllSitesUnit <- try(predictLars(object=allSitesUnitModelRand, newdata=env, type='response', preds=preds))
							predAllSitesComposite <- try(predictLars(object=allSitesCompositeModelRand, newdata=env, type='response', preds=preds))

							# calculate heterogeneity of response WITHIN unit
							if (!is.null(predAllSitesUnit) & !is.null(predKFold) & length(predAllSitesUnit) > 0 & length(predKFold) > 0 & class(predAllSitesUnit) != 'try-error' & class(predKFold) != 'try-error') {
								heteroWithin <- mean((predKFold - predAllSitesUnit)^2, na.rm=TRUE)
								changeUnitAllSites <- diff(range(predAllSitesUnit))
							} else {
								heteroWithin <- 0
								changeUnitAllSites <- 0
							}

							# calculate heterogeneity of response BETWEEN focal unit all-sites model and composite unit all-sites model
							if (!is.null(predAllSitesComposite) & !is.null(predAllSitesUnit) & length(predAllSitesComposite) > 0 & length(predAllSitesUnit) > 0 & class(predAllSitesComposite) != 'try-error' & class(predAllSitesUnit) != 'try-error') {
								heteroAmong <- mean((predAllSitesComposite - predAllSitesUnit)^2, na.rm=TRUE)
								changeCompositeAllSites <- diff(range(predAllSitesComposite))
							} else {
								heteroAmong <- 0
								changeCompositeAllSites <- 0
							}

							# remember
							test <- rbind(
								test,
								data.frame(
									scheme=scheme,
									pme=pme,
									fromValance=fromValance,
									fromUnit=fromUnit,
									predType=tolower(predType),
									envWidth=envWidth,
									across=paste(across, collapse=' '),
									n=n,
									nEffective=nEffective,
									iters=iters,
									iter=iter,
									predictor=thisPred,
									heteroWithin=heteroWithin,
									heteroAmong=heteroAmong,
									changeUnitAllSites=changeUnitAllSites,
									changeCompositeAllSites=changeCompositeAllSites
								)
							)
						
						} # next iteration
				
					} # next predictor
					
					say('')
					
				} # next from unit
				
			} # next from valance
			
			saveRDS(test, paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves - Randomization Values for ', schemeNice, ' ', pmeNice, ' PME to Test if Different from 0.rds'))
			rm(test)

		} # next PME
		
	} # next scheme
	
}

###################################################
### plot within/among heterogeneity in response ###
###################################################

if ('plot hetero ratio' %in% do) {

	say('plot within/among heterogeneity in response', level=1)

	# generalization
	ylim <- c(0, 6)

	say('4 x 5 plot, one per predictor.')
	say('Four bars per plot, one per scheme. Represent within/among group heterogeneity weighted by total amount of response.')

	### load observed and null distributions
	obs <- nulls <- data.frame()
	for (scheme in schemes) {
		
		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# observed heterogeneity: loads "hetero"
			load(paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves ', scheme, ' ', pme, '.Rdata'))

			# load randomized values: loads "test"
			load(paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves - Randomization Values for ', schemeNice, ' ', pmeNice, ' PME.RData'))

			obs <- rbind(obs, hetero)
			nulls <- rbind(nulls, test)
			
		}
		
	}

	# by PME
	for (pme in pmes) {

		pmeNice <- pmeNiceName(pme)

		png(paste0(workDir, '/ENMs - Derived/Ratio of Within-to-Among Heterogeneity in Response Curves for ', pmeNice, ' PME.png'), width=5 * 300, height=4 * 300, res=300)
		par(mfrow=c(4, 5), mgp=c(0.77, 0.1, 0), tck=-0.02, mar=0.5 * c(5, 4, 4, 2) + 0.1)

			# by PREDICTOR
			for (thisPred in predictorsToUse) {

				plot(1:5, 1:5, ylim=ylim, xlab='', ylab='Within / Among\nHeterogeneity', main=fields$nameShort[which(fields$factor %in% thisPred)], col='white', xaxt='n', cex.lab=0.6, cex.axis=0.6, cex.main=0.6)
				axis(1, at=1:4 + 0.5, labels=NA)

				lines(c(0, 7), c(0, 0), col='gray')

				at <- 0.5

				withinAmongs <- list()

				# by SCHEME
				for (scheme in schemes) {

					at <- at + 1

					# scheme info
					out <- schemeInfo(scheme)
					schemeNice <- out$schemeNice
					col <- out$col

					# get within/among group heterogeneity
					thisHetero <- obs[obs$scheme == scheme & obs$pme == pme & obs$predictor == thisPred, ]
					thisHetero$within <- thisHetero$heteroWithin * thisHetero$changeUnitAllSites
					thisHetero$among <- thisHetero$heteroAmong * thisHetero$changeCompositeAllSites
					thisHetero <- aggregate(thisHetero, by=list(thisHetero$fromUnit), FUN=mean, na.rm=TRUE)

					# calculate ratio of within/among
					withinAmong <- thisHetero$within / thisHetero$among
					if (anyNA(withinAmong)) withinAmong <- withinAmong[-is.na(withinAmong)]

					withinAmongs[[at]] <- withinAmong

					# plot
					width <- 0.8
					polygon(c(at - width / 2, at + width / 2, at + width / 2, at - width / 2), c(min(withinAmong), min(withinAmong), max(withinAmong), max(withinAmong)), col=col)

					lines(c(at - width / 2, at + width / 2), rep(median(withinAmong), 2), lwd=1)
					lines(c(at - width / 2, at + width / 2), rep(mean(withinAmong), 2), lwd=1, lty='dotted')

					# label
					text(at, ylim[1] - 0.1 * diff(ylim), srt=90, adj=1, labels=schemeNice, xpd=NA, cex=0.6)

				} # next SCHEME

				# put designator on scheme with lowest within/among
				withinAmongs <- sapply(withinAmongs, mean)
				designator <- which.min(withinAmongs)
				text(designator + 0.5, ylim[2] - 0.05 * diff(ylim), labels='~')

			} # next predictor

		dev.off()

	} # next PME

}

#####################################################
### plot among - within heterogeneity in response ###
#####################################################

if ('plot hetero difference' %in% do) {

	say('plot among - within heterogeneity in response', level=1)

	# generalization
	ylim <- c(-0.4, 1)

	say('5 x 4 plot, one per predictor.')
	say('Eight bars per plot, one per scheme x PME. Represent among - within group heterogeneity weighted by total amount of response.')

	### load observed and null distributions
	obs <- nulls <- data.frame()
	for (scheme in schemes) {
		
		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		
		for (pme in pmes) {

			pmeNice <- pmeNiceName(pme)

			# observed heterogeneity: loads "hetero"
			load(paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves ', scheme, ' ', pme, '.Rdata'))

			# load randomized values: loads "test"
			load(paste0(workDir, 'ENMs - Derived/Heterogeneity in Response Curves - Randomization Values for ', schemeNice, ' ', pmeNice, ' PME.RData'))

			obs <- rbind(obs, hetero)
			nulls <- rbind(nulls, test)
			
		}
		
	}
	
	
	# table tabulating times each scenario/PME has largest value of within - among
	greatestConsistencyBySchemePme <- secondGreatestConsistencyBySchemePme <- thirdGreatestConsistencyBySchemePme <- fourthGreatestConsistencyBySchemePme <- rep(0, 8)
	names(greatestConsistencyBySchemePme) <- names(secondGreatestConsistencyBySchemePme) <- names(thirdGreatestConsistencyBySchemePme) <- names(fourthGreatestConsistencyBySchemePme) <- paste0(rep(schemes, each=2), '_', c('pmeMin', 'pmeNone'))

	rep(0, 8)
	names(secondGreatestConsistencyBySchemePme) <- paste0(rep(schemes, each=2), '_', c('pmeMin', 'pmeNone'))

	tallyPredImpBySchemePme <- data.frame(predictor=predictorsToUse)
	for (schemePme in paste0(rep(schemes, each=2), '_', c('pmeMin', 'pmeNone'))) {
		tallyPredImpBySchemePme <- cbind(tallyPredImpBySchemePme, this=NA)
		names(tallyPredImpBySchemePme)[ncol(tallyPredImpBySchemePme)] <- schemePme
	}
	
	png(paste0(workDir, '/ENMs - Derived/Difference between Among and Within Group Heterogeneity in Response Curves for Both PMEs.png'), width=4 * 300, height=5 * 300, res=300)
	
	par(mfrow=c(5, 4), mgp=c(0.77, 0.1, 0), tck=-0.02, cex.lab=0.6, cex.axis=0.6, cex.main=0.6, bty='n', lwd=0.7, lwd=0.5)
	op <- par(oma = c(5, 2, 0, 0) + 0.1, mar = c(0.2, 0, 1, 2) + 0.1)

		# by PREDICTOR
		for (countPred in seq_along(predictorsToUse)) {

			thisPred <- predictorsToUse[countPred]
		
			ylab <- if (countPred %in% c(1, 5, 9, 13, 17)) {
				'Climate coherency'
			} else {
				''
			}
		
			plot(1:5, 1:5, ylim=ylim, xlab='', ylab=ylab, main='', col='white', xaxt='n', cex.lab=0.6, cex.axis=0.6, cex.main=0.5, xpd=NA)
			
			if (countPred >= 17) axis(1, at=1:4 + 0.5, labels=NA)

			labels <- paste0(letters[countPred], ') ', fields$nameShort[which(fields$factor %in% thisPred)])
			text(0, 1.2, labels=labels, xpd=NA, cex=0.6, pos=4)
			
			lines(c(1.1, 4.9), c(0, 0), col='gray', lwd=1)

			at <- 0.5
			
			# records mean difference, significance, and colors across schemes/PMEs
			allObsDiffs <- numeric()
			allSig <- logical()
			allLight <- allDark <- character()
			
			# by SCHEME
			for (scheme in schemes) {

				at <- at + 1
			
				# scheme info
				out <- schemeInfo(scheme)
				schemeNice <- out$schemeNice
				colLight <- out$col
				colDark <- out$colDark

				allLight <- c(allLight, rep(colLight, 2))
				allDark <- c(allDark, rep(colDark, 2))
				
				units <- getUnits(scheme, FALSE)

				# scheme label
				if (countPred >= 17) text(at, ylim[1] - 0.1 * diff(ylim), srt=60, adj=1, labels=schemeNice, xpd=NA, cex=0.7)
				
				# by PME
				for (pme in c('pmeMin', 'pmeNone')) {
				
					pmeNice <- pmeNiceName(pme)
					
					thisAt <- at + ifelse(pme == 'pmeMin', -0.15, 0.15)

					### collate observed/null within vs among heterogeneity in response
					###################################################################

					# observed differences and quantiles of observed versus null distribution
					obsDiff <- quants <- rep(NA, length(units))
					names(obsDiff) <- names(quants) <- units
					
					# by UNIT
					for (countUnit in seq_along(units)) {
					
						fromUnit <- units[countUnit]
					
						### observed
						############
					
						# get within/among group heterogeneity
						unitHetero <- obs[obs$scheme == scheme & obs$pme == pme & obs$fromUnit == fromUnit & obs$predictor == thisPred, ]

						# weight by total change in suitability across environmental range
						obsWithin <- unitHetero$heteroWithin * unitHetero$changeUnitAllSites
						obsAmong <- unitHetero$heteroAmong * unitHetero$changeCompositeAllSites

						# observed response
						obsDiff[countUnit] <- mean(obsAmong - obsWithin, na.rm=TRUE)

						### null
						########
						
						unitNull <- nulls[nulls$scheme == scheme & nulls$pme == pme & nulls$fromUnit == fromUnit & nulls$predictor == thisPred, ]
						nullWithin <- unitNull$heteroWithin * unitNull$changeUnitAllSites
						nullAmong <- unitNull$heteroAmong * unitNull$changeCompositeAllSites
						nullDiffs <- nullAmong - nullWithin
						if (anyNA(nullDiffs)) nullDiffs <- na.omit(nullDiffs)
						nullDiffs <- sort(nullDiffs)
						
						thisQuant <- which.min(abs(obsDiff[countUnit] - nullDiffs)) / length(nullDiffs)
						if (thisQuant == 0) thisQuant <- 1 / (2 * length(nullDiffs))
						if (thisQuant == 1) thisQuant <- (length(nullDiffs) - 0.5) / (length(nullDiffs))
						quants[countUnit] <- thisQuant
						
					} # next unit

					allObsDiffs <- c(allObsDiffs, mean(obsDiff, na.rm=TRUE))
					names(allObsDiffs)[length(allObsDiffs)] <- paste0(scheme, '_', pme)
					
					# # # # combine pseudo-p values with harmonic mean
					# # # p <- 1 - (prod(quants) ^ (1 / length(units)))

					### combine pseudo-p values with Fisher's combined probability test
					chi2 <- metap::sumlog(1 - quants)
					p <- chi2$p
					pCrit <- 0.05 * length(quants) / (2 * length(quants))
					sig <- p <= pCrit
					
					allSig <- c(allSig, sig)
					names(allSig)[length(allSig)] <- paste0(scheme, '_', pme)

					say(thisPred, ' ', pme, ' ', scheme, ' ', round(p, 3),  ' ::: ', paste(round(quants, 5), collapse=' '))
					
					### PLOT
					
					# plot
					width <- 1
					colLight <- if (sig) { colLight } else { 'white' } # color if significant
					colDark <- if (sig) { colDark } else { 'gray' } # color if significant
					
					boxplot(obsDiff, at=thisAt, border=colDark, col=colLight, add=TRUE, lwd=0.7, pars=list(boxwex=width))
					
					tallyPredImpBySchemePme[tallyPredImpBySchemePme$predictor == thisPred, names(tallyPredImpBySchemePme) == paste0(scheme, '_', pme)] <- if (allSig[paste0(scheme, '_', pme)]) {
						mean(obsDiff)
					} else {
						NaN
					}
					
					
				} # next PME
				
			} # next SCHEME

			# flag box with largest values among significant boxes
			if (any(allSig)) {

				allObsDiffs[!allSig] <- -Inf
				greatest <- which.max(allObsDiffs)
				secondGreatest <- which.max(allObsDiffs[-greatest])
				thirdGreatest <- which.max(allObsDiffs[-c(greatest, secondGreatest)])
				fourthGreatest <- which.max(allObsDiffs[-c(greatest, secondGreatest, thirdGreatest)])

				ats <- 0.5 + 1:4
				ats <- sort(c(ats - 0.15, ats + 0.15))
				
				pch <- if (greatest %% 2 == 0) { 24 } else { 25 }
				points(ats[greatest], 0.9, pch=pch, col='black', bg=allLight[greatest], xpd=NA, lwd=0.6, cex=0.8)
				
				greatestConsistencyBySchemePme[greatest] <- greatestConsistencyBySchemePme[greatest] + 1
				secondGreatestConsistencyBySchemePme[secondGreatest] <- secondGreatestConsistencyBySchemePme[secondGreatest] + 1
				thirdGreatestConsistencyBySchemePme[thirdGreatest] <- thirdGreatestConsistencyBySchemePme[thirdGreatest] + 1
				fourthGreatestConsistencyBySchemePme[fourthGreatest] <- fourthGreatestConsistencyBySchemePme[fourthGreatest] + 1
				
			}
			
			say('')
			
		} # next predictor

	dev.off()
		
	say('Number of times each schemes/PMEs was significantly >0 *and* had the largest value of (within - among) response curve heterogeneity:')
	print(as.matrix(greatestConsistencyBySchemePme))
	say('')
	say('Number of times each schemes/PMEs was significantly >0 *and* had the second-largest value of (within - among) response curve heterogeneity:')
	print(as.matrix(secondGreatestConsistencyBySchemePme))
	say('')
	say('Number of times each schemes/PMEs was significantly >0 *and* had the third-largest value of (within - among) response curve heterogeneity:')
	print(as.matrix(thirdGreatestConsistencyBySchemePme))
	say('')
	say('Number of times each schemes/PMEs was significantly >0 *and* had the fourth-largest value of (within - among) response curve heterogeneity:')
	print(as.matrix(fourthGreatestConsistencyBySchemePme))
	say('')
	say('Mean importance of each predictor by scheme/PME (NaN means not significant):')
	print(tallyPredImpBySchemePme)
	
	save(greatestConsistencyBySchemePme, file='./ENMs - Derived/Number of Times each Scheme and PME had Greatest Climate Consistency.RData')
	save(tallyPredImpBySchemePme, file='./ENMs - Derived/Predictor Importance by Scheme and PME.RData')
	
}

#################################################################
### make table of variable importance for each scheme and PME ###
#################################################################

if ('table of variable importance by scheme and PME' %in% do) {

	say('make table of variable importance for each scheme and PME', level=1)
	load('./ENMs - Derived/Predictor Importance by Scheme and PME.RData')
	
	schemePme <- paste0(rep(schemes, each=2), '_', c('pmeMin', 'pmeNone'))
	
	rankImp <- data.frame(x=rep(NA, length(predictorsToUse)))
	names(rankImp)[ncol(rankImp)] <- schemePme[1]
	
	for (thisSchemePme in schemePme[2:length(schemePme)]) {
		rankImp$DUMMY <- NA
		names(rankImp)[ncol(rankImp)] <- thisSchemePme
	}
	
	for (thisSchemePme in schemePme) {
		rankImp[ , thisSchemePme] <- tallyPredImpBySchemePme$predictor
		if (anyNA(tallyPredImpBySchemePme[ , thisSchemePme])) rankImp[is.na(tallyPredImpBySchemePme[ , thisSchemePme]), thisSchemePme] <- '-'
		rankImp[ , thisSchemePme] <- rankImp[ , thisSchemePme][order(tallyPredImpBySchemePme[ , thisSchemePme], decreasing=TRUE)]
		
	}
	
	for (row in 1:nrow(rankImp)) {
		for (col in 1:ncol(rankImp)) {
			if (!rankImp[row, col] == '-') rankImp[row, col] <- fields$veryShortName[fields$factor == rankImp[row, col]]
		}
	}
	
	write.csv(rankImp, './ENMs - Derived/Rank Importance of Variables for each Scheme and PME.csv', row.names=FALSE)
	
}

#######################################
### summary plot of response curves ###
#######################################

if ('summary plot of response curves' %in% do) {

	say('summary plot', level=1)

	# generalization

	# predType <- 'Full Model'
	predType <- 'Marginal'

	# sharedRangeOnly <- TRUE # constrain change to environmental range shared by all units
	sharedRangeOnly <- FALSE # plot change across range occupied by focal unit

	say('Plot showing the relative effect of each variable in LARS models.')
	say('Plot will have one line per scheme, one column per predictor.')
	say('For each scheme, there will be bars for each predictor, showing how much suitability changes')
	say('across the range of presences, color-coded by whether the change is positive or negative as')
	say('the variable increases.')

	say('Using ', toupper(predType), ' prediction across ', ifelse(sharedRangeOnly, 'SHARED environmental range.', 'environmental range of EACH UNIT.'), pre=1, post=2)

	pres <- getPres()

	numPreds <- sum(fields$useAsPredictor)

	# by PME
	for (pme in pmes) {

		pmeNice <- pmeNiceName(pme)

		png(paste0(workDir, 'ENMs - Derived/Summary of Response Curves - ', predType, ' Predictions across ', ifelse(sharedRangeOnly, 'Shared Environmental Range', 'Environmental Range of Each Unit'), ' for ', pmeNice, ' PME.png'), width=4000, height=2200, res=300)

			par(mfrow=c(5, 1), mgp=c(3, 1, 0), tck=-0.02, mar=c(0.5, 7, 1.5, 2) + 0.1)

			# by SCHEME
			for (scheme in schemes) {

				# scheme info
				out <- schemeInfo(scheme)
				schemeNice <- out$schemeNice
				divisionFieldPres <- out$divisionFieldPres
				rm(out); gc()

				# units
				thisUnitMeta <- unitMeta[unitMeta$scheme == scheme, ]
				units <- thisUnitMeta$unit
				units <- units[-which(units == 'all')]

				plot(1:numPreds, 1:numPreds, ylim=c(0, 1), xaxt='n', xlab='', ylab=paste0(schemeNice, '\n\nTotal Effect'), col='white')

				# y-value indicator for unit label
				upDown <- TRUE

				# by UNIT
				for (fromUnit in units) {

					say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | UNIT ', fromUnit)

					load(paste0(workDir, 'ENMs - Derived/', schemeNice, '/INCLUDING ', toupper(fromUnit), ' - ', toupper(pmeNice), ' PME/Ochotona princeps/LARS multivariate all sites.Rdata'))

					# by PREDICTOR
					at <- 0 # x-location of this predictor

					# by PREDICTOR
					for (thisPred in fields$factor[fields$useAsPredictor]) {

						at <- at + 1

						### label predictor
						if (scheme == last(schemes)) text(at, -0.3, srt=90, labels=fields$nameShort[thisPred == fields$factor], adj=1, xpd=NA, cex=0.85)

						### get environment to which to predict
						env <- model$trainingPresences
						env <- env[order(env[ , thisPred]), ]

						### calculate shared environmental range

						# use shared range within each unit's >=2.5th and <=97.5th quantiles
						if (sharedRangeOnly) {
							minVal <- maxVal <- numeric()
							for (thisUnit in units) {
								minVal <- c(minVal, quantile(pres[pres[ , divisionFieldPres] == thisUnit, thisPred], 0.025, na.rm=TRUE))
								maxVal <- c(maxVal, quantile(pres[pres[ , divisionFieldPres] == thisUnit, thisPred], 0.975, na.rm=TRUE))
							}

							low <- max(minVal)
							high <- min(maxVal)

						# use focal unit's >2.5th and <97.5th quantiles
						} else {

							low <- quantile(env[ , thisPred], 0.025, na.rm=TRUE)
							high <- quantile(env[ , thisPred], 0.0975, na.rm=TRUE)

						}


						# width of set of bars for this predictor
						uberWidth <- 0.5

						# offset
						off <- uberWidth * (which(units %in% fromUnit) - mean(seq_along(units))) / length(units)

						# width
						width <- uberWidth * 1.6 / 5 # bar width

						x <- c(
							at + off,
							at + off + width,
							at + off + width,
							at + off
						)

						# unit label
						yLab <- if (upDown) { -0.13 } else { -0.21 }
						text(x=mean(x), y=yLab, labels=unitMeta$unitAbbrev[fromUnit == unitMeta$unit & unitMeta$scheme == scheme], cex=0.5, srt=90, xpd=NA)

						# if desired range of this predictor is valid
						if (low < high) {

							env <- env[env[ , thisPred] >= low & env[ , thisPred] < high, ]

							# predict
							preds <- if (predType == 'Full Model') { NULL } else { thisPred }
							pred <- predictLars(object=model$model, newdata=env, type='response', preds=preds)

							if (!is.null(pred) && length(pred) > 0) {

								col <- getUnitCols(fromUnit)

								if (predType == 'Full Model') {

									pred <- lowess(pred ~ env[ , thisPred])
									pred$y[pred$y > 1] <- 1
									pred$y[pred$y < 0] <- 0
									pred <- pred$y

								}

								# effect of this predictor
								effect <- range(pred)
								direction <- if (which.min(pred) < which.max(pred)) {
									'darkgreen'
								} else if (which.min(pred) > which.max(pred)) {
									'darkred'
								} else {
									'none'
								}

								if (effect[2] - effect[1] < 0.1) direction <- 'black'
								y <- c(effect[1], effect[1], effect[2], effect[2])

								polygon(x, y, border=ifelse(fromUnit == 'all', 'black', NA), lwd=ifelse(fromUnit == 'all', 2, 0.01), col=alpha(direction, ifelse(fromUnit == 'all', 0.9, 0.6)))

							} # if valid

						} # if desired range of this predictor is valid

					} # next predictor

					upDown <- if (upDown) { FALSE } else { TRUE }

				} # next unit

			} # next scheme

		frame()
		dev.off()

	} # next PME

}

say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
