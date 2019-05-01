## ANALYZE PIKA - PRELIMINARY EXAMPLES 2015-02
## Adam B. Smith

# source('C:/ecology/Drive/Research/Pika/Scripts - Non-stationarity/Pika - Range Limits Analysis Examples.r')

memory.limit(memory.limit() * 2^30)
rm(list=ls())
options(keep.source=FALSE) # manage memory
gc()

library(raster)
library(dismo)


### load and process data
pika <- read.csv(file.choose(), as.is=T)
pika <- pika[which(pika$coordsOk & pika$coordUncerOk & pika$dateOfObs>=1974 & pika$origRecentPikaOrSignsObserved), ]

# using modern PRISM rasters
envStack <- stack(list.files('C:/ecology/!Projects/Grinnell/environmentalData/climatePastPresent_PRISM_800m/prism_30arcsec_westernNorthAmerica_1970to2009', full.names=T))
elevation <- raster('C:/ecology/!Projects/Grinnell/environmentalData/elevation_SRTM30_westUSA/elevWUS.tif')

env <- as.data.frame(extract(envStack, cbind(pika$longWgs84, pika$latWgs84)))
elev <- as.data.frame(extract(elevation, cbind(pika$longWgs84, pika$latWgs84)))
names(elev) <- 'elevation'

pika <- cbind(pika, env, elev)
pika <- pika[which(!is.na(pika$BIO01)), ]

# bg
load('C:/ecology/!Projects/Grinnell/backgroundPointsRandom/randomlyLocatedSites_training_selectedProportionalToCellArea.Rdata')
bg <- cbind(randomBgSitesTraining$sites, randomBgSitesTraining$modernEnv, randomBgSitesTraining$elevation)
names(bg)[1:2] <- c('longWgs84', 'latWgs84')

### plot in env space
source('C:/ecology/Drive/R/SDM/SDM - Graph Species Environmental Distributions from Data.r')

graphEnvDistributionsForEachSpeciesFromData(
	speciesList='Ochotona princeps',
	allSpeciesData=pika,
	geogRedundancyRaster=elevation,
	namesOfLongLatFields=c('longWgs84', 'latWgs84'),
	nameOfSpeciesField='species',
	minNumPoints=15,
	minMapScaleRange=1,
	backgroundEnvData=bg,
	overlayPolygonsFilePathAndNames='C:/ecology/!Projects/Grinnell/geographicalFeatures/usaWest/usaWest_fromPRISM800m_states_wgs84',
	targetDir='C:/ecology/Drive/Research/Pika/Analysis - Range Limits/Demonstration of Methods 2015-02',
	univariatePlotPredictors=c('elevation'),
	bivariatePlotPredictors=list(A=c('BIO05'), B=c('BIO19'))
)

### model entire western US
preds <- character()
for (count in 1:19) preds <- c(preds, paste('BIO', ifelse(count <10, '0', ''), count, sep=''))
preds <- c(preds, 'elevation')

trainData <- rbind(
	rbind(
		pika[ , preds],
		bg[ , preds]
	)
)

# full model
model <- maxent(
	x=trainData,
	p=as.vector(c(rep(1, nrow(pika)), rep(0, nrow(bg)))),
	removeDuplicates=FALSE,
	path='C:/ecology/!Scratch/temp',
	args=c(
		paste( 'betamultiplier=', 1, sep='' ),
		paste('linear=', 'true', sep=''),
		paste('quadratic=', 'true', sep=''),
		paste('product=', 'true', sep=''),
		paste('threshold=', 'false', sep=''),
		paste('hinge=', 'false', sep=''),
		'responsecurves=false',
		'jackknife=false',
		'askoverwrite=false',
		'removeduplicates=false',
		'addsamplestobackground=true'
	)
)

# predict to N presences and N absences
N <- 2000
presSelect <- pika[sample(1:nrow(pika), N), preds]
absSelect <- bg[sample(1:nrow(bg), N), preds]

predictFullModel <- predict(model, rbind(presSelect[ , preds], absSelect[ , preds]))

if (exists('varImp')) rm(varImp)
for (var in preds) {

	presScramble <- presSelect
	presScramble[ , var] <- sample(presScramble[ , var], N)

	absScramble <- absSelect
	absScramble[ , var] <- sample(absScramble[ , var], N)

	predScramble <- predict(model, rbind(presScramble, absScramble))

	thisCor <- cor(x=predictFullModel, y=predScramble, method='pearson', use='complete.obs')
	
	thisVarImp <- data.frame(
		var=var,
		imp=1 - thisCor
	)
	
	varImp <- if (exists('varImp')) { rbind(varImp, thisVarImp) } else { thisVarImp }
	
}

varImp$var <- as.character(varImp$var)
varImp$var[which(varImp$var=='elevation')] <- 'elev'


### model Great Basin
tnc <- shapefile('C:/ecology/Ecoregionalizations/Terrestrial Ecoregions 2009 - TNC/tnc_terr_ecoregions')
gb <- subset(tnc, ECO_NAME=='Great Basin')

pikaShape <- SpatialPointsDataFrame(coords=cbind(pika$longWgs84, pika$latWgs84), proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=pika)
bgShape <- SpatialPointsDataFrame(coords=cbind(bg$longWgs84, bg$latWgs84), proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=bg)

gbWithPika <- over(pikaShape, gb)
pikaGb <- pika[which(!is.na(gbWithPika$ECO_NAME)), preds]

gbWithBg <- over(bgShape, gb)
bgGb <- pika[which(!is.na(gbWithBg$ECO_NAME)), preds]

trainData <- rbind(
	rbind(
		pikaGb[ , preds],
		bgGb[ , preds]
	)
)

# great basin model
modelGb <- maxent(
	x=trainData,
	p=as.vector(c(rep(1, nrow(pikaGb)), rep(0, nrow(bgGb)))),
	removeDuplicates=FALSE,
	path='C:/ecology/!Scratch/temp',
	args=c(
		paste( 'betamultiplier=', 1, sep='' ),
		paste('linear=', 'true', sep=''),
		paste('quadratic=', 'true', sep=''),
		paste('product=', 'true', sep=''),
		paste('threshold=', 'false', sep=''),
		paste('hinge=', 'false', sep=''),
		'responsecurves=false',
		'jackknife=false',
		'askoverwrite=false',
		'removeduplicates=false',
		'addsamplestobackground=true'
	)
)

# predict to N presences and N absences
N <- 800
presSelect <- pikaGb[sample(1:nrow(pikaGb), N), ]
absSelect <- bgGb[sample(1:nrow(bgGb), N), ]

predictFullModelGb <- predict(modelGb, rbind(presSelect[ , preds], absSelect[ , preds]))

if (exists('varImpGb')) rm(varImpGb)
for (var in preds) {

	presScramble <- presSelect
	presScramble[ , var] <- sample(presScramble[ , var], N)

	absScramble <- absSelect
	absScramble[ , var] <- sample(absScramble[ , var], N)

	predScramble <- predict(modelGb, rbind(presScramble, absScramble))

	thisCor <- cor(x=predictFullModelGb, y=predScramble, method='pearson', use='complete.obs')
	
	thisVarImp <- data.frame(
		var=var,
		imp=1 - thisCor
	)
	
	varImpGb <- if (exists('varImpGb')) { rbind(varImpGb, thisVarImp) } else { thisVarImp }
	
}

varImpGb$var <- as.character(varImp$var)
varImpGb$var[which(varImpGb$var=='elevation')] <- 'elev'


par(mfrow=c(1, 2))
barplot(varImp$imp, names=preds, col='cornflowerblue', ylab='Importance', main='Entire Western US')
barplot(varImpGb$imp, names=preds, col='darkgoldenrod1', ylab='Importance', main='Great Basin')

