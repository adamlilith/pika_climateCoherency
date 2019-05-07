### PIKA NON-STATIONARITY OMNIBUS VARIABLES
# source(paste0(drive, 'ecology/Drive/Research/Iconic Species/pika_climateCoherency/!Omnibus Variables for Pika Non-Stationarity Analysis.r'))

### SETUP ###
### VARIABLES ###
### LIBRARIES & FUNCTIONS ###
### DEFINED FUNCTIONS ###
### short names for unit valance
### nice name for PME variant
### list units
### information and polygons of a division scheme
### rename Bailey Ecoregions
### rename EPA Level III Modified Ecoregions
### rename GEnS zones
### PRISM-based Koppen-Geiger
### rename elevation quantiles above PME
### calculate weights of test sites based on proximity and/or temporal overlap
### train KDE
### SNIPPETS ###
### regular loop ###
### progress report ###

#############
### SETUP ###
#############

	print(Sys.time())

	memory.limit(memory.limit() * 2^30)
    
	# library(checkpoint)
	# checkpoint(
		# snapshotDate = '2017-05-15',
		# project = paste0(drive, '/ecology/Drive/Research/Iconic Species/pika_climateCoherency'),
		# checkpointLocation = paste0(drive, '/ecology/Drive/Research/Iconic Species'),
		# use.knitr=FALSE
	# )

	options(stringsAsFactors=FALSE)
	gc()


	setwd('C:/ecology/Drive/Research/Iconic Species')

#############################
### LIBRARIES & FUNCTIONS ###
#############################

	library(omnibus)
	library(enmSdm)
	library(legendary)

	library(rgdal)
	# library(sp)
	library(raster)
	library(ncdf4)
	library(XML)
	library(rgrass7)

	# library(gbm)
	library(rJava)
	library(dismo)
	library(ecospat)
	library(usdm)
	# library(maxnet)
	library(metap)
	library(stabs)
	library(mboost)
	library(betareg)
	library(grpreg)
	library(grpregOverlap)
	library(randomForest)
	library(MuMIn)
	library(nlme)
	library(mgcv)
	library(KernSmooth)
	library(MASS)

	library(shapefiles)
	library(tkrplot)
	library(ade4)
	# library(adehabitat)
	library(adehabitatHR)
	library(adehabitatMA)

	library(geosphere)
	library(rgeos)
	library(fossil)
	# library(spatial.tools)

	library(grid)
	library(gridExtra)
	library(png)
	library(ggplot2)
	library(lubridate)
	library(RColorBrewer)
	library(scales)

	library(cluster)

	library(weathermetrics)

	library(compiler)
	enableJIT(1)
	setCompilerOptions(suppressUndefined=TRUE)

	source(paste0(drive, 'ecology/Drive/R/!Other/SDM/SDM - Train SDMs.r'))
	source(paste0(drive, 'ecology/Drive/R/!Other/SDM/SDM - Evaluate Models.r'))
	source(paste0(drive, 'ecology/Drive/R/!Other/SDM/SDM - Collate Data for a Species and Create Partitions.r'))
	source(paste0(drive, 'ecology/Drive/R/!Other/SDM/SDM - Write Raster from Model Basic Function.r'))
	source(paste0(drive, 'ecology/Drive/R/!Other/Center and Standardize Predictors.r'))

	source(paste0(drive, 'ecology/Drive/R/!Other/Calculate Day of Year from Year, Month, and Day.r'))

	source(paste0(drive, 'ecology/Drive/R/!Other/SDM/SDM - Predict Model Object.r'))
	source(paste0(drive, 'ecology/Drive/R/!Other/Broennimann.r'))


	source(paste0(drive, 'ecology/Drive/Research/Iconic Species/pika_climateCoherency/Function - Extract Environmental Data Based on Date.r'))
	source(paste0(drive, 'ecology/Drive/Research/Iconic Species/pika_climateCoherency/Function - Calculate Derived Climate Variables.r'))
	source(paste0(drive, 'ecology/Drive/Research/Iconic Species/pika_climateCoherency/Function - Calculate KDE-Based Weight for Sites.r'))

#################
### VARIABLES ###
#################

	# number of k-folds
	kFolds <- 8

	# minimum number of total training presences per division unit
	minNumPres <- 125

	# working directory
	workDir <- paste0(drive, 'ecology/Drive/Research/Iconic Species/')

	# minimum cumulative proportion explained needed to include PC axis in an analysis
	pcaMinCumulExplained <- 0.85

	# multiplier of cell size of template to create spatial bins in which to tabulate number of species' presences... should be an integer >= 1
	resMult <- 2

	# invioable presences
	getPres <- function(canada=FALSE, recordsVersionSubDir='!Collated Data 2016-06-30 1256') {
		uberPres <- readRDS(paste0(workDir, 'Species Records - Pika/', recordsVersionSubDir, '/13 Ochotona princeps - Usable - Presences 1990-2015 - Updated Fenneman Physiographic Provinces.rds'))
		if (!canada) uberPres <- uberPres[!is.na(uberPres$elevPrism_m), ]
		uberPres
	}

	# unit meta data
	unitMeta <- read.csv(paste0(workDir, 'pika_climateCoherency/Scheme and Unit Long and Short Names, Factor Levels, & Formatting.csv'))

	# colors for units
	getUnitCols <- function(units, incAll=TRUE) {
		# units		names of units
		# incAll	if "all" is in "units" then remove it before returning colors

		if (!incAll & 'all' %in% units) units <- units[-which(units %in% 'all')]
		cols <- unitMeta$color[match(units, unitMeta$unit)]
		cols
	}

	# calanedar days of the year
	doyNonLeapYear <- read.csv(paste0(drive, 'ecology/Drive/R/!Other/Ancillary Files/daysOfYear_nonLeapYear.csv'))
	doyLeapYear <- read.csv(paste0(drive, 'ecology/Drive/R/!Other/Ancillary Files/daysOfYear_leapYear.csv'))

	# predictors
	fields <- read.csv(paste0(workDir, 'pika_climateCoherency/Predictors - Definitions, Names, and Statistics.csv'))
	predictorsToUse <- fields$factor[fields$useAsPredictor]
	extendedPredictorsToUse <- c('obsYear', 'obsMonth', 'obsDayOfMonth', 'obsDayOfYear', 'cellAreaPrism_km2', 'cellAreaGmted2010_km2', 'elevPrism_m', 'elevGmted2010_m', 'slopePrism_deg', 'slopeGmted2010_deg', 'aspectPrism_deg', 'aspectGmted2010_deg', 'tpiPrism_m', 'tpiGmted2010_m', 'triPrism_m', 'triGmted2010_m', predictorsToUse)

	# training parameters
	modelParams=list(
		brt=list(
			learningRate=c(0.01, 0.001, 0.0001),
			treeComplexity=c(2, 4, 8),
			bagFraction=c(0.7),
			maxTrees=c(4000)
		),
		gam=list(
			family='binomial',
			initialGamma=1.4,
			minNumPresPerInitialTerm=20,
			minNumPresPerFinalTerm=30,
			initialTerms=6,
			interaction='te',
			rescaleAllSitesToKFold=TRUE,
			construct=TRUE,
			select=TRUE

		),
		gamboost=list(
			boostControl=boost_control(
				mstop=100,
				nu=0.1,
				risk=c('inbag', 'oobag', 'none'),
				stopintern=FALSE,
				trace=FALSE
			),
			innerKnots=20,
			boundaryKnots=NULL,
			degree=NULL,
			differences=NULL,
			family='Gaussian',
			mstopRule='minRisk',
			mstopAsym=0.95,
			baseLearners=NULL,
			constant=TRUE,
			bolsUni=TRUE,
			bbsUni=TRUE,
			bolsBi=TRUE,
			bbsBi=TRUE,
			bmonoUni=FALSE,
			bmonoBiv=FALSE,
			bmonoConstraint=NULL,
			spatial=TRUE,
			bolsSpatialEnv=TRUE,
			bbsSpatialEnv=TRUE
		),
		glm=list(
			use='brglm',
			tooBig=10E6,
			family='binomial',
			minNumPresPerInitialTerm=10,
			minNumPresPerFinalTerm=20,
			initialTerms=12,
			rescaleAllSitesToKFold=TRUE,
			construct=TRUE,
			select=TRUE,
			quadratic=TRUE,
			cubic=TRUE,
			interaction=TRUE,
			interQuad=TRUE
		),
		kde=list(
			h='epa',
			kern='epa',
			templateRast=NULL,
			templateProj4='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
			eaProj4='+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs',
			resMult=4,
			epsilon=10
		),
		lars=list(
			family='binomial',
			penalty='grMCP',
			scale=TRUE,
			alphas=c(0.01, seq(0.1, 1, by=0.1)),
			scale=TRUE,
			quadratic=TRUE,
			cubic=TRUE,
			interaction=TRUE,
			interQuad=TRUE,
			na.rm=TRUE
		),
		maxent=list(
			regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
			classes='default',
			testClasses=TRUE,
			forceLinear=TRUE,
			jackknife=TRUE,
			args='',
			anyway=TRUE
		),
		maxnet=list(
			regMult=c(seq(0.5, 5, by=0.5), 6:10, 12.5, 15, 17.5, 20),
			classes='lqph',
			testClasses=TRUE,
			clamp=FALSE
		),
		rf=list(
			package='randomForest'
		),
		svm1=list(
			nu=0.10
		),
		svm2=list(
			epsilon=0.1,
			interactions=FALSE
		),
		treeboost=list(
			family='Binomial',
			spatial=TRUE,
			boostControl=boost_control(
				mstop=100,
				nu=0.1,
				risk=c('inbag', 'oobag', 'none'),
				stopintern=FALSE,
				trace=FALSE
			),
			treeControl=party::ctree_control(
				teststat=c('quad','max'),
				testtype=c('Bonferroni','MonteCarlo','Univariate','Teststatistic'),
				mincriterion=0.95,
				minsplit=20,
				minbucket=7,
				stump=FALSE,
				nresample=9999,
				maxsurrogate=0,
				mtry=0,
				savesplitstats=TRUE,
				maxdepth=10
			)
		)
	)

	# predictor arguments
	predArgs <- list(
		gamboost=list(
			type='response',
			family='Gaussian',
			include=NULL,
			global=TRUE,
			spatial=TRUE,
			spatialEnv=TRUE,
			exclude=NULL
		),
		kde=list(
			coordsProj4=getCRS('nad83')
		),
		maxent=list(
			outputFormat='logistic',
			predictFx='default',
			doclamp=FALSE
		),
		maxnet=list(
			outputFormat='cloglog',
			clamp=TRUE
		),
		rf=list(
			type='prob'
		),
		svm2=list(
			rescale=TRUE
		),
		treeboost=list(
			type='response'
		)
	)

#########################
### DEFINED FUNCTIONS ###
#########################

	statNice <- function(stat) {

		x <- if (stat == 'cbi' | stat=='cbiPerm') {
			'CBI'
		} else if (stat == 'cbiPresWeight') {
			'CBI (Presence-Weighted)'
		} else if (stat == 'cbiPresBgWeight') {
			'CBI (Presence- & Absence-Weighted)'

		} else if (stat == 'tholdMaxSumSS') {
			'Threshold at Maximum Sens + Spec'

		} else if (stat == 'sensTholdMaxSumSS') {
			'True Positive Rate (Threshold at Max S+S)'
		} else if (stat == 'specTholdMaxSumSS') {
			'True Negative Rate (Threshold at Max S+S)'

		} else if (stat == 'sensTholdMinDiffSS') {
			'True Positive Rate (Threshold at Min |S-S|)'
		} else if (stat == 'specTholdMinDiffSS') {
			'True Negative Rate (Threshold at Min |S-S|)'

		} else if (stat == 'schoenerDBg') {
			'Schoener\'s D for Background'
		} else if (stat == 'schoenerDPres') {
			'Schoener\'s D for Presences'
		} else if (stat == 'schoenerDPresWeighted') {
			'Schoener\'s D for Presences (Weighted)'

		} else if (stat == 'rankCorBg') {
			'Rank Correlation for Background'
		} else if (stat == 'rankCorPres') {
			'Rank Correlation for Presences'
		} else if (stat == 'rankCorPresWeighted') {
			'Rank Correlation for Presences (Weighted)'

		} else if (stat == 'corBg') {
			'Correlation for Background'
		} else if (stat == 'corPres') {
			'Correlation for Presences'
		} else if (stat == 'corPresWeighted') {
			'Correlation for Presences (Weighted)'

		} else if (stat == 'cor') {
			'Correlation'
		}

		x

	}

	### nice name for statistic genre
	statGenreNice <- function(statGenre) {

		x <- if (genre == 'performance') {
			'ENM Performance'
		} else if (genre == 'overlap') {
			'Niche Overlap'
		}

		x
	}

	### modified Randin's transferability
	transfer0To1 <- function(AA, BB, AB, BA) {

		# calculate modified transferability score as per Smith et al. 2013 based on Randin et al 2006 for performance scores in the range 0 to 1
		# AA, BB 	performance against self for models A and B
		# AB, BA	performance against other region for models A and B

		top <- 0.5 * ((1 - abs(AA - AB)) + (1 - abs(BB - BA)))
		bottom <- 1 + abs(abs(AA - AB) - (BB - BA))
		return (top / bottom)

	}

	### convert unit names to nice names
	niceUnitName <- function(scheme, unit, short=FALSE, veryShort=FALSE, upper=FALSE) {

		thisMeta <- unitMeta[unitMeta$scheme == scheme, ]
		x <- if (short) {
			thisMeta$unitNiceShort[match(unit, thisMeta$unit)]
		} else if (veryShort) {
			thisMeta$unitNiceVeryShort[match(unit, thisMeta$unit)]
		} else {
			thisMeta$unitNice[match(unit, thisMeta$unit)]
		}

		if (upper) unit <- toupper(unit)
		x

	}

	### convert unit names to nice names
	niceUnitNameShort <- function(scheme, unit, upper=FALSE) {

		thisMeta <- unitMeta[unitMeta$scheme == scheme, ]
		x <- thisMeta$unitNiceShort[match(unit, thisMeta$unit)]
		if (upper) unit <- toupper(unit)
		x

	}

	### short names for unit valance
	valanceShort <- function(unitValance, upper=TRUE) {
		out <- if (unitValance == 'including') {
			'inc'
		} else if (unitValance == 'excluding') {
			'exc'
		}
		if (upper) out <- toupper(out)


		return(out)
	}

	### nice name for PME variant
	pmeNiceName <- function(pmeVariant) {

		pmeNice <- if (pmeVariant == 'pmeMin') {
			'Min'
		} else if (pmeVariant=='pmeMean') {
			'Mean'
		} else if (pmeVariant =='pmeMax') {
			'Max'
		} else if (pmeVariant =='pmeNone') {
			'No'
		}

		pmeNice

	}

	### nice name for PME variant (version for printing)
	pmeNiceNamePrint <- function(pmeVariant) {

		pmeNice <- if (pmeVariant == 'pmeMin') {
			'Narrow'
		} else if (pmeVariant =='pmeNone') {
			'Broad'
		}

		pmeNice

	}

	### nice name for background (PME) variant
	bgNiceName <- function(pmeVariant) {

		bgNice <- if (pmeVariant == 'pmeMin') {
			'Narrow'
		} else if (pmeVariant =='pmeNone') {
			'Broad'
		}

		bgNice

	}

	### comvert unit names to integers
	unitNameToInteger <- function(x) {
		# x	list of unit names
		x <- unitMeta$factorInteger[match(x, unitMeta$unit)]
		x
	}


	### list units
	getUnits <- function(scheme, incAll=TRUE, pres=getPres()) {

		units <- unitMeta$unit[unitMeta$scheme == scheme]
		if (!incAll) units <- units[-which(units %in% 'all')]
		units

	}

	### information and polygons of a division scheme
	schemeInfo <- function(scheme, poly=FALSE) {
		# schemeInfo  Returns useful strings and shapefile polygons for a scheme
		# source(paste0(drive, 'ecology/Drive/Research/Iconic Species/pika_climateCoherency/Function - Information and Polygons of a Division Scheme.r')
		#
		# scheme	name of scheme
		# workDir	working directory
		# poly		if TRUE then returns shapefile for the scheme and name of field in shapefile with units of interest
		#
		# Returns:
		# out$scheme
		# out$schemeNice
		# out$divisionFieldPres
		# out$divisionFieldPoly
		# out$divisionPoly
		# out$col

		# assign:
		# schemeNice text
		# name of field with scheme divisions in presences
		# name of field with division names in division polygon
		# load division scheme polygons

		if (scheme == 'cladeNonOverlap') {
			schemeNice <- 'Lineages'
			schemeShort <- 'Lineages'
			schemeShortSingular <- 'Lineages'
			divisionFieldPres <- 'clade'
			divisionFieldPoly <- 'subspecies'
			if (poly) divisionPoly <- readOGR(paste0(workDir, 'Extents_Masks_Maps/GeneticClades_GIS/!Method2_IUCNBuffer/!NonOverlappingBufferedGeneticClades'), '!IUCN-47kmBuffer_NonOverlapping_AllGeneticClades', verbose=FALSE)
			col <- 'cornflowerblue'
			colDark <- 'darkblue'
		} else if (scheme == 'physioFenneman') {
			schemeNice <- 'Physiography'
			schemeShort <- 'Physiography Fenneman'
			schemeShortSingular <- 'Physiography'
			divisionFieldPres <- 'physioFenneman'
			divisionFieldPoly <- 'physio'
			if (poly) divisionPoly <- readOGR(paste0(workDir, '/Extents_Masks_Maps/EcoRegions/!FennemansPhysiography/2017_09_06'), '04_fennemann_dissolvedToPhysioRegions', verbose=FALSE)
			col <- 'darkgoldenrod1'
			colDark <- 'darkgoldenrod4'
		} else if (scheme == 'ecoregionEpa3Modified') {
			schemeNice <- 'Ecoregions'
			schemeShort <- 'Ecoregions EPA L3-Mod'
			schemeShortSingular <- 'Ecoregion'
			divisionFieldPres <- 'ecoregionEpaLevel3Modified'
			divisionFieldPoly <- 'L3_KEY'
			if (poly) {
				divisionPoly <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!OmernikEcoregions'), 'us_eco_l3SarrREV_joinedUndersampledEcoregions', verbose=FALSE)
				divisionPoly <- epaRename(epa=divisionPoly)
			}
			col <- 'darkolivegreen3'
			colDark <- 'darkolivegreen'
		} else if (scheme == 'gens') {
			schemeNice <- 'Climate'
			schemeShort <- 'Climate Zones'
			schemeShortSingular <- 'Climate Zone'
			divisionFieldPres <- 'climateGEnSZone'
			divisionFieldPoly <- 'GEnZname'
			if (poly) {
				divisionPoly <- readRDS('F:/ecology/Climate/GEnS/GEnSv3_11012012/wgs84/GEnS_v3_wgs84_conus_dissolvedToGEnSname.rds')
				divisionPoly <- gensRename(gens=divisionPoly)
			}
			col <- 'blue'
			colDark <- 'blue4'
		} else if (scheme == 'physioHammond') {
			schemeNice <- 'Physiography'
			schemeShort <- 'Physiography Hammond'
			schemeShortSingular <- 'Physioregion'
			divisionFieldPres <- 'physioHammond'
			divisionFieldPoly <- 'HLR'
			if (poly) divisionPoly <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!Hammond_HLR'), 'hlrus_conusWest', verbose=FALSE)
			col <- 'darkmagenta'
			colDark <- 'blueviolet'
		} else if (scheme == 'elevQuantWrtPaeMin') {
			schemeNice <- 'Elevation'
			schemeShort <- 'Elevation Quantile'
			schemeShortSingular <- 'Elevation'
			divisionFieldPres <- 'elevQuantWrtPaeMin'
			divisionFieldPoly <- 'elvQWPM'
			if (poly) divisionPoly <- readOGR(paste0(workDir, 'Extents_Masks_Maps/EcoRegions/!ElevationQuintiles_ver2'), 'elevWrtMinPmeQuintiles', verbose=FALSE)
			col <- 'firebrick3'
			colDark <- 'firebrick4'
		}

		out <- list()
		out$scheme <- scheme
		out$schemeNice <- schemeNice
		out$schemeShort <- schemeShort
		out$schemeShortSingular <- schemeShortSingular
		out$divisionFieldPres <- divisionFieldPres
		out$divisionFieldPoly <- divisionFieldPoly
		if (poly) out$divisionPoly <- divisionPoly
		out$col <- col

		out

	}

	### rename Bailey Ecoregions
	baileyRename <- function(x) {

		x[x=='Cascade Mixed Forest-Coniferous Forest-Alpine Meadow Province'] <- 'Cascade Forest & Meadows'
		x[x=='Great Plains-Palouse Dry Steppe Province'] <- 'Great Plains'
		x[x=='Intermountain Semi-Desert and Desert Province'] <- 'Intermountain Semi-Desert & Desert'
		x[x=='Intermountain Semi-Desert Province'] <- 'Intermountain Desert'
		x[x=='Middle Rocky Mountain Steppe-Coniferous Forest-Alpine Meadow Province'] <- 'Mid-Rockies Forest & Meadow'
		x[x=='Nevada-Utah Mountains-Semi-Desert-Coniferous Forest-Alpine Meadow Province'] <- 'Nevada-Utah Mtn Forest'
		x[x=='Northern Rocky Mountain Forest-Steppe-Coniferous Forest-Alpine Meadow Province'] <- 'N Rockies Forest & Meadow'
		x[x=='Pacific Lowland Mixed Forest Province'] <- 'Pacific Lowland Forest'
		x[x=='Sierran Steppe-Mixed Forest-Coniferous Forest-Alpine Meadow Province'] <- 'Sierran Steppe & Forest'
		x[x=='Southern Rocky Mountain Steppe-Open Woodland-Coniferous Forest-Alpine Meadow Province'] <- 'S Rockies Forests'

		return(x)

	}

	### rename EPA Level III Modified Ecoregions
	epaRename <- function(epa) {

		# remove leading numbers and spaces
		for (i in 1:nrow(epa)) {

			if (!is.na(as.numeric(substr(epa$L3_KEY[i], 1, 1)))) { # if first character is a number

				epa$L3_KEY[i] <- if (substr(epa$L3_KEY[i], 4, 4) != ' ') {
					substr(epa$L3_KEY[i], 4, nchar(epa$L3_KEY[i]))
				} else if (substr(epa$L3_KEY[i], 5, 5) != ' ') {
					substr(epa$L3_KEY[i], 5, nchar(epa$L3_KEY[i]))
				}

			}

		}

		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='/', replacement=' & ')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern=' and ', replacement=' & ')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Mountain', replacement='Mtn')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Northern', replacement='N')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Southern', replacement='S')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Eastern', replacement='E')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Western', replacement='W')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Southwestern', replacement='SW')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Southwest', replacement='SW')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Northwestern', replacement='NW')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Northwest', replacement='NW')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Northeastern', replacement='NE')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Northeast', replacement='NE')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Southeastern', replacement='SE')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Southeast', replacement='SE')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='North', replacement='N')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='South', replacement='S')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='East', replacement='E')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='West', replacement='W')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Central', replacement='Cent')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Center', replacement='Cent')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Middle ', replacement='Mid-')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Californian', replacement='Calif')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='California', replacement='Calif')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Colorado', replacement='Colo')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Michigan', replacement='Mich')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='River', replacement='R')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Plateau', replacement='Plat')
		epa$L3_KEY <- gsub(epa$L3_KEY, pattern='Wasatch & Uinta', replacement='Wasatch-Uinta')

		return(epa)

	}

	### rename GEnS zones
	gensRename <- function(gens) {

		gens$GEnZname <- gsub(gens$GEnZname, pattern=' and ', replacement=' & ')
		letter <- substr(gens$GEnZname, 1, 1)
		gens$GEnZname <- paste0(substr(gens$GEnZname, 4, nchar(gens$GEnZname)), ' (', letter, ')')

		return(gens)

	}

	### PRISM-based Koppen-Geiger
	koppenRename <- function(x) {

		# assign Koppen classes (character) to numeric classes

		x=shape$GRIDCODE

		crosswalk <- read.csv('G:/ecology/Ecoregionalizations/Koppen Climate Classification/Crosswalk from Raster to Shapefile.csv', as.is=TRUE)
		class(crosswalk$GRIDCODE) <- class(x)

		for (gridcode in sort(unique(crosswalk$GRIDCODE))) {

			if (any(x %in% gridcode)) {
				reps <- sum(x %in% gridcode, na.rm=TRUE)
				x[which(x %in% gridcode)] <- rep(crosswalk$Zone[crosswalk$GRIDCODE %in% gridcode], reps)
			}

		}

		return(x)

	}

	### rename elevation quantiles above PME
	pmeRename <- function(x) {

		x[x=='0'] <- '0th Quantile (Below Lowest)'
		x[x=='1'] <- '1st Quantile (Lowest)'
		x[x=='2'] <- '2nd Quantile (Lower Middle)'
		x[x=='3'] <- '3rd Quantile (Middle)'
		x[x=='4'] <- '4th Quantile (Upper Middle)'
		x[x=='5'] <- '5th Quantile (Highest)'
		x[x=='6'] <- '6th Quantile (Above Highest)'

		return(x)

	}

	### calculate weights of test sites based on proximity and/or temporal overlap
	calcSiteWeight <- function(x, proximity=TRUE, temporal=TRUE, window=10, minDist=3000, longLatFields=c('longWgs84', 'latWgs84'), proj4=getCRS('nad83'), distFx=distHaversine, ...) {

		# calculates weighting for presences based on distance to neighbors
		# weight is defined as 1 / (number of presences within a minimum distance of the focal presence)

		# x					data frame with species presences
		# proximity			TRUE ==> calculate weight based on proximity (minDist, longLatFields, proj4, and distFx must be specified)
		# temporal				TRUE ==> calculate weight based on temporal overlap (window must be specified)
		# minDist			minimum distance (in m)
		# window			temporal (in years) prior to temporal of site's date across which to calculate temporal overlap
		# longLatFields		names of fields in x with longitude and latitude (e.g., c('longWgs84', 'latWgs84'))
		# proj4				proj4 string of coordinates in xy
		# distFx			distance function (e.g., "distCosine")

		# NOTC: if both proximity and temporal == TRUE then temporal overlap is only calculated for sites within minDist

		n <- nrow(x)

		# spatial proximity weighting
		if (proximity) {

			xy <- x[ , longLatFields]
			xy <- SpatialPoints(xy, proj4string=CRS(proj4))

			dists <- matrix(NA, nrow=n, ncol=n)
			# for (i in 1:n) dists[i, i:n] <- distFx(xy[i, ], xy[i:n, ])
			for (i in 1:n) dists[i, ] <- distFx(xy[i, ], xy)

			neighs <- dists <= minDist
			weight <- 1 / colSums(neighs, na.rm=TRUE)

		} else {
			weight <- rep(1, n)
		}


		# temporal overlap weighting
		if (temporal) {

			# observation date of focal site
			obsDates <- as.Date(paste0(x$obsYear, '-', x$obsMonth, '-', x$obsDayOfMonth))

			# calculate temporal weight for each focal site
			for (i in 1:n) {

				# get neighboring sites against which to calculate weights
				if (proximity) {
					compares <- which(neighs[ , i]) # index of neighbors
				# just get all sites
				} else {
					compares <- 1:n
				}

				# if any comparison sites
				if (length(compares) > 0) {

					focalDate <- obsDates[i]

					# get list of all dates in window
					dateWindowYearsAgo <- as.Date(paste0(x$obsYear[i] - window, '-', x$obsMonth[i], '-', x$obsDayOfMonth[i])) + 1
					allDatesInWindow <- seq.Date(dateWindowYearsAgo, focalDate, by=1)

					# for each date calculate number of sites whose windows include this date
					numWindowsCoveredBy <- rep(NA, length(allDatesInWindow))

					for (j in seq_along(allDatesInWindow)) {
						numWindowsCoveredBy[j] <- sum(obsDates[compares] >= allDatesInWindow[j] & obsDates[compares] <= focalDate)
					}

					# calculate weight
					fractCoveredBy <- 1 / numWindowsCoveredBy
					tempWeight <- sum(fractCoveredBy) / length(allDatesInWindow)

					# final weight is previous weight (from spatial weighting if any) times temporal weight
					weight[i] <- weight[i] * tempWeight

				} # if any comparison sites

			} # next focal site

		} # if temporal weighting

		weight

	} # FINIS


	#' Location of Maximum Value
	#'
	#' Locates the largest value of the input object.
	#'
	#'
	#' @param x a numeric object
	#' @param na.rm a logical indicating whether missing values should be removed.
	#' @param tie_value A character indicating how to deal with ties.  Can be "NA"
	#' (returns an NA if a tie is found) or "random" (returns a single randomly
	#' chosen member of the ties if a tie is found) or "first" (returns the first
	#' class found).
	#' @return An integer of length 1 giving the index of the maximum of x or NA if
	#' the maximum of x is not unique, x has no non-NAs, or na.rm=F.
	#' @author Jonathan A. Greenberg, Alison R. Mynsberge
	#' @seealso \code{\link[base]{which.max}}, \code{\link[base]{which}},
	#' \code{\link[base]{max}}
	#' @keywords calculate
	#' @examples \dontrun{
	#'
	#' x<-c(2:4,1,1,NA)
	#' y<-c(4,1:3,NA,4)
	#' ## The index is only calculated for a unique maximum
	#' which.max.simple(x)
	#' which.max.simple(y)
	#' which.max.simple(y,na.rm=FALSE)
	#' which.max.simple(x,na.rm=FALSE)
	#' }
	which.max.simple=function(x,na.rm=TRUE,tie_value="NA")
	{
		if(na.rm)
		{
			x=x[!is.na(x)]
		}
		if(length(x)==0)
		{
			return(NA)
		}
		maxval=max(x)
		if(is.na(maxval))
		{
			return(NA)
		}
		if(sum(x %in% maxval) > 1)
		{
			# Ties exist, figure out what to do with them.
			if(tie_value=="NA")
			{
				return(NA)
			}

			if(tie_value=="random")
			{
				tie_postions=which(x==maxval)
				return(sample(tie_postions,size=1))
			}

			if(tie_value=="first")
			{
				tie_postions=which(x==maxval)
				return(tie_postions[1])
			}

		} else
		{
			return(which.max(x))
		}
	}



	#' Location of Minimum Value
	#'
	#' Locates the smallest value of the input object.
	#'
	#'
	#' @param x a numeric object
	#' @param na.rm a logical indicating whether missing values should be removed.
	#' @param tie_value A character indicating how to deal with ties.  Can be "NA"
	#' (returns an NA if a tie is found) or "random" (returns a single randomly
	#' chosen member of the ties if a tie is found) or "first" (returns the first
	#' class found).
	#' @return An integer of length 1 giving the index of the minimum of x or NA if
	#' the minimum of x is not unique, x has no non-NAs, or na.rm=F.
	#' @author Jonathan A. Greenberg, Alison R. Mynsberge
	#' @seealso \code{\link[base]{which.min}}, \code{\link[base]{which}},
	#' \code{\link[base]{min}}
	#' @keywords calculate
	#' @examples \dontrun{
	#'
	#' x<-c(4,1:3,NA,4)
	#' y<-c(2:4,1,1,NA)
	#' ## The index is only calculated for a unique minimum
	#' which.min.simple(x)
	#' which.min.simple(y)
	#' which.min.simple(y,na.rm=FALSE)
	#' which.min.simple(x,na.rm=FALSE)
	#' }
	which.min.simple=function(x,na.rm=TRUE,tie_value="NA")
	{
		if(na.rm)
		{
			x=x[!is.na(x)]
		}
		if(length(x)==0)
		{
			return(NA)
		}
		minval=min(x)
		if(is.na(minval))
		{
			return(NA)
		}
		if(sum(x %in% minval) > 1)
		{
			# Ties exist, figure out what to do with them.
			if(tie_value=="NA")
			{
				return(NA)
			}

			if(tie_value=="random")
			{
				tie_postions=which(x==minval)
				return(sample(tie_postions,size=1))
			}

			if(tie_value=="first")
			{
				tie_postions=which(x==minval)
				return(tie_postions[1])
			}

		} else
		{
			return(which.min(x))
		}
	}


	### train KDE
	# modified from adehabitatHR to handle cases where there is a slight mismatch between distance along

		kernelUDAdapt <- function (xy, h = "href", grid = 60, same4all = FALSE, hlim = c(0.1,
			1.5), kern = c("bivnorm", "epa"), extent = 1, boundary = NULL, epsilon=10)
		{
			if (!inherits(xy, "SpatialPoints"))
				stop("xy should inherit the class SpatialPoints")
			if (ncol(coordinates(xy)) > 2)
				stop("xy should be defined in two dimensions")
			pfs1 <- proj4string(xy)
			if (inherits(xy, "SpatialPointsDataFrame")) {
				if (ncol(xy) != 1) {
					warning("xy should contain only one column (the id of the animals)\nid ignored")
					id <- rep("a", nrow(as.data.frame(xy)))
					m <- 2
				}
				else {
					id <- xy[[1]]
					m <- 1
				}
			} else {
				id <- rep("a", nrow(as.data.frame(xy)))
				m <- 2
			}
			if (!is.null(boundary)) {
				if (!inherits(boundary, "SpatialLines"))
					stop("the boundary should be an object of class SpatialLines")
			}
			if (min(table(id)) < 5)
				stop("At least 5 relocations are required to fit an home range")
			id <- factor(id)
			xy <- as.data.frame(coordinates(xy))
			if (same4all) {
				if (inherits(grid, "SpatialPixels"))
					stop("when same4all is TRUE, grid should be a number")
				grid <- .makegridUD(xy, grid, extent)
			}
			lixy <- split(xy, id)
			res <- lapply(1:length(lixy), function(i) {
				if (is.list(grid)) {
					grida <- grid[names(lixy)[i]][[1]]
				}
				else {
					grida <- grid
				}
				x <- lixy[names(lixy)[i]][[1]]
				if (!is.null(boundary)) {
					bdrk <- .boundaryk(SpatialPoints(x, proj4string = CRS(as.character(pfs1))),
						boundary, h)
					if (!is.null(bdrk)) {
						sigg <- attr(bdrk, "sign")
						bdrk <- as.data.frame(coordinates(bdrk))
						names(bdrk) <- names(x)
						x <- rbind(x, bdrk)
					}
				}
				# ud <- adehabitatHR:::.kernelUDs(SpatialPoints(x, proj4string = CRS(as.character(pfs1))),
					# h = h, grid = grida, hlim = hlim, kern = kern, extent = extent)
				ud <- kernelUDsAdapt(SpatialPoints(x, proj4string = CRS(as.character(pfs1))),
					h = h, grid = grida, hlim = hlim, kern = kern, extent = extent, epsilon)
				if (!is.null(boundary)) {
					if (!is.null(bdrk)) {
						ud <- .fbboun(ud, boundary, sigg, h)
						slot(ud, "data")[, 1] <- slot(ud, "data")[, 1]/(sum(slot(ud,
						  "data")[, 1]) * gridparameters(ud)[1, 2]^2)
					}
				}
				if (!is.na(pfs1))
					proj4string(ud) <- CRS(pfs1)
				return(ud)
			})
			names(res) <- names(lixy)
			class(res) <- "estUDm"
			if (m == 2) {
				res <- res[[1]]
			}
			return(res)
		}


		kernelUDsAdapt <- function (xy, h = "href", grid = 60, hlim = c(0.1, 1.5), kern = c("bivnorm",
			"epa"), extent = 0.5, epsilon=10)
		{
			kern <- match.arg(kern)
			if (!inherits(xy, "SpatialPoints"))
				stop("xy should be of class SpatialPoints")
			pfs1 <- proj4string(xy)
			xy <- coordinates(xy)
			if ((!is.numeric(h)) & (h != "href") & (h != "LSCV"))
				stop("h should be numeric or equal to either \"href\" or \"LSCV\"")
			if ((h == "LSCV") & (kern == "epa"))
				stop("LSCV is not implemented with an Epanechnikov kernel")
			if (!inherits(grid, "SpatialPixels")) {
				if ((!is.numeric(grid)) | (length(grid) != 1))
					stop("grid should be a number or an object inheriting the class SpatialPixels")
				grid <- .makegridUD(xy, grid, extent)
			}
			pfs2 <- proj4string(grid)
			if (!is.na(pfs2)) {
				if (!identical(pfs2, pfs1))
					stop("points and grid do not have the same proj4string")
			}
			gridded(grid) <- TRUE
			fullgrid(grid) <- TRUE
			grrw <- gridparameters(grid)
			if (nrow(grrw) > 2)
				stop("grid should be defined in two dimensions")
			if (is.loaded("adehabitatMA")) {
				opteps <- adehabitatMA::adeoptions()$epsilon
			} else {
				opteps <- epsilon
			}

			# if (grrw[1, 2] - grrw[2, 2] > opteps)
				# stop("the cellsize should be the same in x and y directions")
			if (abs(grrw[1, 2] - grrw[2, 2]) > opteps)
				stop(paste0('\nThe cell size should be (approximately) the same in x and y directions.'))
			xyg <- coordinates(grid)
			xg <- unique(xyg[, 1])
			yg <- unique(xyg[, 2])
			varx <- var(xy[, 1])
			vary <- var(xy[, 2])
			sdxy <- sqrt(0.5 * (varx + vary))
			n <- nrow(xy)
			ex <- (-1/6)
			htmp <- h
			typh <- h
			href <- sdxy * (n^ex)
			if (kern == "epa")
				href <- href * 1.77
			if (h == "href") {
				htmp <- href
			}
			if (h == "LSCV") {
				hvec <- seq(hlim[1] * href, hlim[2] * href, length = 100)
				CV <- .C("CVmise", as.integer(nrow(xy)), as.double(xy[,
					1]), as.double(xy[, 2]), as.double(hvec), double(length(hvec)),
					as.integer(length(hvec)), PACKAGE = "adehabitatHR")[[5]]
				htmp <- hvec[CV == min(CV)]
				if ((CV[CV == min(CV)] == CV[1]) | (CV[CV == min(CV)] ==
					CV[length(CV)]))
					warning("The algorithm did not converge \nwithin the specified range of hlim: try to increase it")
			}
			if (!is.numeric(htmp))
				stop("non convenient value for h")
			if (kern == "bivnorm") {
				toto <- .C("kernelhr", double(length(xg) * length(yg)),
					as.double(xg), as.double(yg), as.integer(length(yg)),
					as.integer(length(xg)), as.integer(nrow(xy)), as.double(htmp),
					as.double(xy[, 1]), as.double(xy[, 2]), PACKAGE = "adehabitatHR")
			}
			if (kern == "epa") {
				toto <- .C("kernepan", double(length(xg) * length(yg)),
					as.double(xg), as.double(yg), as.integer(length(yg)),
					as.integer(length(xg)), as.integer(nrow(xy)), as.double(htmp),
					as.double(xy[, 1]), as.double(xy[, 2]), PACKAGE = "adehabitatHR")
			}
			ud <- data.frame(ud = toto[[1]])
			coordinates(ud) <- expand.grid(yg, xg)[, 2:1]
			gridded(ud) <- TRUE
			if (typh == "LSCV") {
				CV <- data.frame(h = hvec, CV = CV)
				convergence <- min(CV[, 2]) != CV[1, 2]
				hli <- list(CV = CV, convergence = convergence, h = htmp,
					meth = "LSCV")
			}
			else {
				if (typh == "href") {
					hli <- list(h = htmp, meth = "href")
				}
				else {
					hli <- list(h = htmp, meth = "specified")
				}
			}
			ud <- new("estUD", ud)
			slot(ud, "h") <- hli
			slot(ud, "vol") <- FALSE
			return(ud)
		}



################
### SNIPPETS ###
################

	# ####################
	# ### regular loop ###
	# ####################

	# # by SCHEME
	# for (scheme in schemes) {

		# out <- schemeInfo(scheme)
		# schemeNice <- out$schemeNice
		# divisionFieldPres <- out$divisionFieldPres
		# # divisionFieldPoly <- out$divisionFieldPoly
		# # divisionPoly <- out$divisionPoly
		# rm(out); gc()

		# # by PME
		# for (pmeVariant in pmes) {

			# pmeNice <- pmeNiceName(pmeVariant)

			# # by "from" unit valance...inclusive or exclusive of focal region
			# for (fromValance in valances) {

				# # get names of focal units to include/exclude
				# units <- if (is.null(forceUnit)) {
					# getUnits(scheme=scheme, incAll=FALSE)
				# } else {
					# forceUnit
				# }

				# say('UNITS: ', paste(units, collapse=' | '), pre=1)

				# # by "from" UNIT
				# for (fromUnit in units) {

					# say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | VALANCE ', fromValance, ' | UNIT ', fromUnit)

					# ####
					# ####
					# ####

				# } # next fromUnit

			# } # next fromValance

		# } # next PME

	# } # next division scheme

say('Pika OMNIBUS loaded successfully', level=1)
