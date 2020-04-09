## PIKA DATA CLEANING & COLLATION
## Adam B. Smith | 2014-06

# source('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Pika - Data Collation and Cleaning.r')

memory.limit(memory.limit() * 8^30)
rm(list=ls())
gc()

options(keep.source=FALSE) # manage memory

library(sp)
library(rgdal)
library(raster)
options(java.parameters='-Xmx1g' )
library(dismo)
library(rgeos)
library(geosphere)
library(scales)
source('C:/ecology/Drive/R/!Omnibus.r')
source('C:/ecology/Drive/R/Combine Data Frames.r') # combines data frames
source('C:/ecology/Drive/R/Return Year from Messy Dates.r') # return year from messy date string
source('C:/ecology/Drive/R/Calculate Day of Year from Year, Month, and Day.r') # returns day of year from date

cat('\n', date(), '\n'); flush.console()

### CONTENTS ###
### functions ###
### collate all pika data and project to WGS84 ###
### plot each contributor\'s data for checking by them ###
### remove data "Dan Doak, Anna Chalfoun, Leah Yandow - Set A"

### functions



if (TRUE) {

	print('##################################################')
	print('### collate all pika data and project to WGS84 ###')
	print('##################################################')

	### collate all data
	print('Choose latest crosswalk file.  File name will be like this: <Crosswalk for Collating All Pika Records - YYYY-MM-DD...>')
	crosswalk <- read.csv(file.choose(), as.is=T)

	pikaData <- combineFrames(crosswalk=crosswalk, use='use', seperator='; ', verbose=TRUE, classes=NULL)

	pikaData$citizenScience <- as.logical(pikaData$citizenScience)
	pikaData$origRecentPikaOrSignsObserved <- as.logical(pikaData$origRecentPikaOrSignsObserved)

	########################################
	### project all coordinates to WGS84 ###
	########################################

	# get only records with coordinates
	pikaData$origLong <- as.numeric(pikaData$origLong)
	pikaData$origLat <- as.numeric(pikaData$origLat)

	pikaData <- subset(pikaData, origLong!=0)
	pikaData <- subset(pikaData, origLat!=0)
	pikaData <- subset(pikaData, !is.na(origLong))
	pikaData <- subset(pikaData, !is.na(origLat))

	pikaData$longWgs84 <- pikaData$latWgs84 <- NA

	pikaData$origDatumProjection[which(pikaData$origDatumProjection=='North American Datum 1927')] <- 'NAD27'
	pikaData$origDatumProjection[which(pikaData$origDatumProjection=='North American Datum 1983')] <- 'NAD83'
	pikaData$origDatumProjection[which(pikaData$origDatumProjection=='World Geodetic System 1972')] <- 'WGS72'
	pikaData$origDatumProjection[which(pikaData$origDatumProjection=='WGS-84')] <- 'WGS84'
	pikaData$origDatumProjection[which(pikaData$origDatumProjection=='WGS 84')] <- 'WGS84'
	pikaData$origDatumProjection[which(pikaData$origDatumProjection=='World Geodetic System 1984')] <- 'WGS84'

	cat('\nProjecting to WGS84...\n'); flush.console()
	cat('Datums/coordinate systems to process:\n')

	pikaData$coordsOk <- FALSE
	for (thisProj in sort(unique(pikaData$origDatumProjection))) {

		# no datum
		if (is.na(thisProj)) {
		
			cat('   ', thisProj, '\n'); flush.console()
			pikaData$coordsOk[which(is.na(pikaData$origDatumProjection))] <- FALSE
			pikaData$coordsOk[which(pikaData$origDatumProjection=='not recorded')] <- FALSE
			pikaData$coordsOk[which(pikaData$origDatumProjection=='not recorded (forced WGS84)')] <- FALSE
			pikaData$coordsOk[which(pikaData$origDatumProjection=='unknown')] <- FALSE
			
		# no datum
		} else if (thisProj=='') {
		
			cat('   ', thisProj, '\n'); flush.console()
			pikaData$coordsOk[which(pikaData$origDatumProjection=='')] <- FALSE
			
		# unknown
		} else if (thisProj=='unknown') {
		
			cat('   ', thisProj, '\n'); flush.console()
			pikaData$coordsOk[which(pikaData$origDatumProjection=='unknown')] <- FALSE
			
		# transform from given coord system/datum to WGS84
		} else {
		
			# if WGS84
			if (thisProj=='WGS84') {
			
				cat('   ', thisProj, '\n'); flush.console()
				pikaData$longWgs84[which(pikaData$origDatumProjection=='WGS84')] <- pikaData$origLong[which(pikaData$origDatumProjection=='WGS84')]
				pikaData$latWgs84[which(pikaData$origDatumProjection=='WGS84')] <- pikaData$origLat[which(pikaData$origDatumProjection=='WGS84')]
				pikaData$coordsOk[which(pikaData$origDatumProjection=='WGS84')] <- TRUE
				
			# if WGS72
			} else if (thisProj=='WGS72') {
			
				cat('   ', thisProj, '\n'); flush.console()
				coords <- spTransform(
					x=SpatialPointsDataFrame(
						coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='WGS72')], pikaData$origLat[which(pikaData$origDatumProjection=='WGS72')]),
						proj4string=CRS('+proj=longlat +ellps=WGS72 +no_defs'),
						data=pikaData[which(pikaData$origDatumProjection=='WGS72'), ]
					),
					CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
				)
				
				pikaData$longWgs84[which(pikaData$origDatumProjection=='WGS72')] <- coords@coords[ , 1]
				pikaData$latWgs84[which(pikaData$origDatumProjection=='WGS72')] <- coords@coords[ , 2]
				pikaData$coordsOk[which(pikaData$origDatumProjection=='WGS72')] <- TRUE
			
			# if WGS84 Plate Caree
			} else if (thisProj=='WGS84 Plate Carree') {
			
				# cat('   ', thisProj, '\n'); flush.console()
				# coords <- spTransform(
					# x=SpatialPointsDataFrame(
						# coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='WGS84 Plate Carree')], pikaData$origLat[which(pikaData$origDatumProjection=='WGS84 Plate Carree')]),
						# proj4string=CRS('+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'),
						# data=pikaData[which(pikaData$origDatumProjection=='WGS84 Plate Carree'), ]
					# ),
					# CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
				# )
				
				# pikaData$longWgs84[which(pikaData$origDatumProjection=='WGS84 Plate Carree')] <- coords@coords[ , 1]
				# pikaData$latWgs84[which(pikaData$origDatumProjection=='WGS84 Plate Carree')] <- coords@coords[ , 2]
				# pikaData$coordsOk[which(pikaData$origDatumProjection=='WGS84 Plate Carree')] <- TRUE
			
				print(notSupportingWgs84PlateCaree)
			
			# if NAD27
			} else if (thisProj=='NAD27') {
			
				cat('   ', thisProj, '\n'); flush.console()
				coords <- spTransform(
					x=SpatialPointsDataFrame(
						coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='NAD27')], pikaData$origLat[which(pikaData$origDatumProjection=='NAD27')]),
						proj4string=CRS('+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat'),
						data=pikaData[which(pikaData$origDatumProjection=='NAD27'), ]
					),
					CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
				)
				
				pikaData$longWgs84[which(pikaData$origDatumProjection=='NAD27')] <- coords@coords[ , 1]
				pikaData$latWgs84[which(pikaData$origDatumProjection=='NAD27')] <- coords@coords[ , 2]
				pikaData$coordsOk[which(pikaData$origDatumProjection=='NAD27')] <- TRUE
			
			# if NAD83
			} else if (thisProj=='NAD83') {
			
				cat('   ', thisProj, '\n'); flush.console()
				coords <- spTransform(
					x=SpatialPointsDataFrame(
						coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='NAD83')], pikaData$origLat[which(pikaData$origDatumProjection=='NAD83')]),
						proj4string=CRS('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'),
						data=pikaData[which(pikaData$origDatumProjection=='NAD83'), ]
					),
					CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
				)
				
				pikaData$longWgs84[which(pikaData$origDatumProjection=='NAD83')] <- coords@coords[ , 1]
				pikaData$latWgs84[which(pikaData$origDatumProjection=='NAD83')] <- coords@coords[ , 2]
				pikaData$coordsOk[which(pikaData$origDatumProjection=='NAD83')] <- TRUE
			
			# if NAD83 HARN... for Washington/Oregon ONLY!!!
			} else if (thisProj=='NAD83 HARN') {
			
				cat('   ', thisProj, '\n'); flush.console()
				coords <- spTransform(
					x=SpatialPointsDataFrame(
						coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='NAD83 HARN')], pikaData$origLat[which(pikaData$origDatumProjection=='NAD83 HARN')]),
						proj4string=CRS('+proj=longlat +ellps=GRS80 +no_defs'),
						data=pikaData[which(pikaData$origDatumProjection=='NAD83 HARN'), ]
					),
					CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
				)
				
				pikaData$longWgs84[which(pikaData$origDatumProjection=='NAD83 HARN')] <- coords@coords[ , 1]
				pikaData$latWgs84[which(pikaData$origDatumProjection=='NAD83 HARN')] <- coords@coords[ , 2]
				pikaData$coordsOk[which(pikaData$origDatumProjection=='NAD83 HARN')] <- TRUE
			
			# if Simple Cylindrical (Plate Carree) Projection (Google Earth)
			} else if (thisProj=='Simple Cylindrical (Plate Carree) Projection') {
					   
				cat('   ', thisProj, '\n'); flush.console()
				coords <- spTransform(
					x=SpatialPointsDataFrame(
						coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='Simple Cylindrical (Plate Carree) Projection')], pikaData$origLat[which(pikaData$origDatumProjection=='Simple Cylindrical (Plate Carree) Projection')]),
						proj4string=CRS('+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs'),
						data=pikaData[which(pikaData$origDatumProjection=='Simple Cylindrical (Plate Carree) Projection'), ]
					),
					CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
				)
				
				pikaData$longWgs84[which(pikaData$origDatumProjection=='Simple Cylindrical (Plate Carree) Projection')] <- coords@coords[ , 1]
				pikaData$latWgs84[which(pikaData$origDatumProjection=='Simple Cylindrical (Plate Carree) Projection')] <- coords@coords[ , 2]
				pikaData$coordsOk[which(pikaData$origDatumProjection=='Simple Cylindrical (Plate Carree) Projection')] <- TRUE
			
				print(mayNotbeAccurat_notSupportingSimpleCyclindricalPlateCaree)
			
			# if UTM WGS84
			} else if (thisProj=='UTM WGS84') {
			
				cat('   ', thisProj, '\n'); flush.console()

				for (thisZone in 9:14) {
				
					if (length(which(pikaData$origDatumProjection=='UTM WGS84' & pikaData$origUtmZone==thisZone)) > 0) {
					
						proj4string <- CRS(paste('+proj=utm +zone=', thisZone,' +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0', sep=''))
					
						coords <- spTransform(
							x=SpatialPointsDataFrame(
								coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='UTM WGS84' & pikaData$origUtmZone==thisZone)], pikaData$origLat[which(pikaData$origDatumProjection=='UTM WGS84' & pikaData$origUtmZone==thisZone)]),
								proj4string=proj4string,
								data=pikaData[which(pikaData$origDatumProjection=='UTM WGS84' & pikaData$origUtmZone==thisZone), ]
							),
							CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
						)
							
						pikaData$longWgs84[which(pikaData$origDatumProjection=='UTM WGS84' & pikaData$origUtmZone==thisZone)] <- coords@coords[ , 1]
						pikaData$latWgs84[which(pikaData$origDatumProjection=='UTM WGS84' & pikaData$origUtmZone==thisZone)] <- coords@coords[ , 2]
						pikaData$coordsOk[which(pikaData$origDatumProjection=='UTM WGS84' & pikaData$origUtmZone==thisZone)] <- TRUE

					}
					
				} # next zone

			# UTM NAD27
			} else if (thisProj=='UTM NAD27') {
			
				cat('   ', thisProj, '\n'); flush.console()

				for (thisZone in 9:14) {
				
					if (length(which(pikaData$origDatumProjection=='UTM NAD27' & pikaData$origUtmZone==thisZone)) > 0) {
					
						proj4string <- CRS(paste('+proj=utm +zone=', thisZone, ' +datum=NAD27 +units=m +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat', sep=''))
					
						coords <- spTransform(
							x=SpatialPointsDataFrame(
								coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='UTM NAD27' & pikaData$origUtmZone==thisZone)], pikaData$origLat[which(pikaData$origDatumProjection=='UTM NAD27' & pikaData$origUtmZone==thisZone)]),
								proj4string=proj4string,
								data=pikaData[which(pikaData$origDatumProjection=='UTM NAD27' & pikaData$origUtmZone==thisZone), ]
							),
							CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
						)
							
						pikaData$longWgs84[which(pikaData$origDatumProjection=='UTM NAD27' & pikaData$origUtmZone==thisZone)] <- coords@coords[ , 1]
						pikaData$latWgs84[which(pikaData$origDatumProjection=='UTM NAD27' & pikaData$origUtmZone==thisZone)] <- coords@coords[ , 2]
						pikaData$coordsOk[which(pikaData$origDatumProjection=='UTM NAD27' & pikaData$origUtmZone==thisZone)] <- TRUE

					}
					
				} # next zone
			
			# UTM NAD83
			} else if (thisProj=='UTM NAD83') {
			
				cat('   ', thisProj, '\n'); flush.console()

				for (thisZone in 9:14) {
				
					if (length(which(pikaData$origDatumProjection=='UTM NAD83' & pikaData$origUtmZone==thisZone)) > 0) {
					
						proj4string <- CRS(paste('+proj=utm +zone=', thisZone,' +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0', sep=''))
					
						coords <- spTransform(
							x=SpatialPointsDataFrame(
								coords=cbind(pikaData$origLong[which(pikaData$origDatumProjection=='UTM NAD83' & pikaData$origUtmZone==thisZone)], pikaData$origLat[which(pikaData$origDatumProjection=='UTM NAD83' & pikaData$origUtmZone==thisZone)]),
								proj4string=proj4string,
								data=pikaData[which(pikaData$origDatumProjection=='UTM NAD83' & pikaData$origUtmZone==thisZone), ]
							),
							CRSobj=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
						)
							
						pikaData$longWgs84[which(pikaData$origDatumProjection=='UTM NAD83' & pikaData$origUtmZone==thisZone)] <- coords@coords[ , 1]
						pikaData$latWgs84[which(pikaData$origDatumProjection=='UTM NAD83' & pikaData$origUtmZone==thisZone)] <- coords@coords[ , 2]
						pikaData$coordsOk[which(pikaData$origDatumProjection=='UTM NAD83' & pikaData$origUtmZone==thisZone)] <- TRUE

					}
					
				} # next zone
			
			}

		} # if datum exists	
			
	}

	pikaData <- subset(pikaData, !is.na(longWgs84))
	pikaData <- subset(pikaData, !is.na(latWgs84))

	###########################################################
	### classify points according to coordinate uncertainty ###
	###########################################################

	cat('\nClassifying coordinate uncertainty using combination of stated coordinate uncertainty, units, and methods...\n'); flush.console()

	pikaData$coordUncerOk <- NA

	# data frame with every observed combination of origCoordUncer, origCoordUncerUnits, origCoordMethod
	coordAssess <- read.csv('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Coordinate Uncertainties, Units, and Methods.csv', as.is=TRUE)
	
	coordUncerCombined <- paste(pikaData$origCoordUncer, pikaData$origCoordUncerUnits, pikaData$origCoordMethod)

	for (i in 1:nrow(coordAssess)) {
	
		pikaData$coordUncerOk[coordUncerCombined==coordAssess$coordUncerUnitsMethod[i]] <- ifelse(coordAssess$ok[i]=='yes', TRUE, FALSE)

	}

	#################
	## clean dates ##
	#################

	cat('\nAutomatic cleaning of dates...\n'); flush.console()

	pikaData$dateOfObsAuto <- returnYear(pikaData$origDateOfObs, yearLast=TRUE)
	pikaData$dateOfObsAuto[which(pikaData$origDateOfObs=='July - Sept 09')] <- 2009
	pikaData$dateOfObsAuto[which(pikaData$origDateOfObs=='summer 2008')] <- 2008
	pikaData$dateOfObsAuto[which(pikaData$origDateOfObs=='1800-01-01/2005-01-01')] <- NA

	# assume dates without century are 20th/21st century!!!
	pikaData$dateOfObsAuto[which(pikaData$dateOfObsAuto > 9915)] <- pikaData$dateOfObsAuto[which(pikaData$dateOfObsAuto > 9915)] - 9900 + 1900
	pikaData$dateOfObsAuto[which(pikaData$dateOfObsAuto >= 9900 & pikaData$dateOfObsAuto <= 9915)] <- pikaData$dateOfObsAuto[which(pikaData$dateOfObsAuto >= 9900 & pikaData$dateOfObsAuto <= 9915)] - 9900 + 2000

	cat('\nCompiling manually entered dates...\n'); flush.console()

	pikaData$obsDayOfMonth[is.na(pikaData$obsDayOfMonth) & !is.na(pikaData$obsYear) & !is.na(pikaData$obsMonth)] <- 15
	
	pikaData$obsYear <- as.integer(pikaData$obsYear)
	pikaData$obsMonth <- as.integer(pikaData$obsMonth)
	pikaData$obsDayOfMonth <- as.integer(pikaData$obsDayOfMonth)
	
	pikaData$obsDayOfYear <- doy(year=pikaData$obsYear, month=pikaData$obsMonth, dom=pikaData$obsDayOfMonth)
	
	###################################################
	### designate "species" and "subspecies" fields ###
	###################################################

	cat('\nCleaning species and subspecies...\n'); flush.console()
	
	# species... using IUCN Red List taxonomy (from EOL)
	pikaData$species <- NA
	pikaData <- pikaData[-which(pikaData$origSpecies=='Leucosticte tephrocotis dawsoni'), ]
	pikaData <- pikaData[-which(pikaData$origSpecies=='Microtus oeconomus'), ]
	pikaData <- pikaData[-which(pikaData$origSpecies=='Spermophilus undulatus'), ]

	# pikaData$species[which(is.na(pikaData$origSpecies) & pikaData$origProvider!='VertNet')] <- 'Ochotona princeps' # VertNet included non-Ochotona!
	pikaData$species[which(pikaData$origSpecies=='American pika')] <- 'Ochotona princeps'
	pikaData$species[which(pikaData$origSpecies=='Ochotona alpina')] <- 'Ochotona alpina'
	pikaData$species[which(pikaData$origSpecies=='Ochotona alpina (Pallas, 1773)')] <- 'Ochotona alpina'
	pikaData$species[which(pikaData$origSpecies=='Ochotona alpina changaica')] <- 'Ochotona alpina'
	pikaData$species[which(pikaData$origSpecies=='Ochotona alpina changaica Ognev, 1940')] <- 'Ochotona alpina'
	pikaData$species[which(pikaData$origSpecies=='Ochotona cansa')] <- 'Ochotona cansus'
	pikaData$species[which(pikaData$origSpecies=='Ochotona cansus Lyon, 1907')] <- 'Ochotona cansus'
	pikaData$species[which(pikaData$origSpecies=='Ochotona cansus stevensi Osgood, 1932')] <- 'Ochotona cansus'
	pikaData$species[which(pikaData$origSpecies=='Ochotona cansus subsp. cansus')] <- 'Ochotona cansus'
	pikaData$species[which(pikaData$origSpecies=='Ochotona collaris')] <- 'Ochotona collaris'
	pikaData$species[which(pikaData$origSpecies=='Ochotona collaris (Nelson, 1893)')] <- 'Ochotona collaris'
	pikaData$species[which(pikaData$origSpecies=='Ochotona curzoniae')] <- 'Ochotona curzoniae'
	pikaData$species[which(pikaData$origSpecies=='Ochotona curzoniae (Hodgson, 1858)')] <- 'Ochotona curzoniae'
	pikaData$species[which(pikaData$origSpecies=='Ochotona daurica')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona dauurica')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona dauurica (Pallas, 1776)')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona dauurica altaina')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona dauurica daurica')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona dauurica mursaevi')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona dauurica mursavi Bannikov, 1951')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona dauurica subsp. dauurica')] <- 'Ochotona dauurica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona erythrotis (BÃ¼chner, 1890)')] <- 'Ochotona erythrotis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona gloveri Thomas, 1922')] <- 'Ochotona gloveri'
	pikaData$species[which(pikaData$origSpecies=='Ochotona himalayana Feng, 1973')] <- 'Ochotona himalayana'
	pikaData$species[which(pikaData$origSpecies=='Ochotona hoffmanni Formozov, Yakhontov & Dmitriev, 1996')] <- 'Ochotona hoffmanni'
	pikaData$species[which(pikaData$origSpecies=='Ochotona huangensis (Matschie, 1908)')] <- 'Ochotona huangensis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona hyperborea')] <- 'Ochotona hyperborea'
	pikaData$species[which(pikaData$origSpecies=='Ochotona hyperborea (Pallas, 1811)')] <- 'Ochotona hyperborea'
	pikaData$species[which(pikaData$origSpecies=='Ochotona hyperborea cinerofusca')] <- 'Ochotona hyperborea'
	pikaData$species[which(pikaData$origSpecies=='Ochotona hyperborea mantchurica')] <- 'Ochotona hyperborea'
	pikaData$species[which(pikaData$origSpecies=='Ochotona hyperborea mantchurica Thomas, 1909')] <- 'Ochotona hyperborea'
	pikaData$species[which(pikaData$origSpecies=='Ochotona kolymensis')] <- 'Ochotona hyperborea'
	pikaData$species[which(pikaData$origSpecies=='Ochotona ladacensis')] <- 'Ochotona ladacensis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona ladacensis (GÃ¼nther, 1875)')] <- 'Ochotona ladacensis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona')] <- 'Ochotona'
	pikaData$species[which(pikaData$origSpecies=='Ochotona Link, 1795')] <- 'Ochotona'
	pikaData$species[which(pikaData$origSpecies=='Ochotona lama')] <- 'Ochotona lama'
	pikaData$species[which(pikaData$origSpecies=='Ochotona macrotis')] <- 'Ochotona macrotis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona macrotis (GÃ¼nther, 1875)')] <- 'Ochotona macrotis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona macrotis subsp. macrotis')] <- 'Ochotona macrotis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona macrotis wollastoni')] <- 'Ochotona macrotis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona macrotis wollastoni Thomas & Hinton, 1922')] <- 'Ochotona macrotis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona nubrica Thomas, 1922')] <- 'Ochotona nubrica'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pallasi')] <- 'Ochotona pallasi'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pallasi (Gray, 1867)')] <- 'Ochotona pallasi'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pallasi pricei')] <- 'Ochotona pallasi'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pallasi pricei Thomas, 1911')] <- 'Ochotona pallasi'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pricei')] <- 'Ochotona pallasi'
	pikaData$species[which(pikaData$origSpecies=='OCHOTONA PRINCEPS')] <- 'Ochotona princeps'
	pikaData$species[which(tolower(substr(x=pikaData$origSpecies, 1, 17))=='ochotona princeps')] <- 'Ochotona princeps'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pusilla')] <- 'Ochotona pusilla'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pusilla (Pallas, 1769)')] <- 'Ochotona pusilla'
	pikaData$species[which(pikaData$origSpecies=='Ochotona pusilla subsp. pusilla')] <- 'Ochotona pusilla'
	pikaData$species[which(pikaData$origSpecies=='Ochotona roylei')] <- 'Ochotona roylei'
	pikaData$species[which(pikaData$origSpecies=='Ochotona roylei nepalensis Hodgson, 1841')] <- 'Ochotona roylei'
	pikaData$species[which(pikaData$origSpecies=='Ochotona roylei roylei')] <- 'Ochotona roylei'
	pikaData$species[which(pikaData$origSpecies=='Ochotona roylei subsp. roylei')] <- 'Ochotona roylei'
	pikaData$species[which(pikaData$origSpecies=='Ochotona rufescens')] <- 'Ochotona rufescens'
	pikaData$species[which(pikaData$origSpecies=='Ochotona rufescens (Gray, 1842)')] <- 'Ochotona rufescens'
	pikaData$species[which(pikaData$origSpecies=='Ochotona rufescens regina Thomas, 1911')] <- 'Ochotona rufescens'
	pikaData$species[which(pikaData$origSpecies=='Ochotona rufescens subsp. rufescens')] <- 'Ochotona rufescens'
	pikaData$species[which(pikaData$origSpecies=='Ochotona rutila')] <- 'Ochotona rutila'
	pikaData$species[which(pikaData$origSpecies=='Ochotona rutila (Severtzov, 1873)')] <- 'Ochotona rutila'
	pikaData$species[which(pikaData$origSpecies=='Ochotona sp.')] <- 'Ochotona'
	pikaData$species[which(pikaData$origSpecies=='Ochotona spanglei')] <- 'Ochotona spanglei'
	pikaData$species[which(pikaData$origSpecies=='Ochotona thibetana (Milne-Edwards, 1871)')] <- 'Ochotona thibetana'
	pikaData$species[which(pikaData$origSpecies=='Ochotona thibetana osgoodi Anthony, 1941')] <- 'Ochotona thibetana'
	pikaData$species[which(pikaData$origSpecies=='Ochotona thibetana sikimaria Thomas, 1922')] <- 'Ochotona thibetana'
	pikaData$species[which(pikaData$origSpecies=='Ochotona thibetana subsp. thibetana')] <- 'Ochotona thibetana'
	pikaData$species[which(pikaData$origSpecies=='Ochotona thibetana thibetana')] <- 'Ochotona thibetana'
	pikaData$species[which(pikaData$origSpecies=='Ochotona turuchanensis Naumov, 1934')] <- 'Ochotona turuchanensis'
	pikaData$species[which(pikaData$origSpecies=='Ochotona valerotae')] <- 'Ochotona valerotae'
	pikaData$species[which(pikaData$origSpecies=='Ochotona whartoni')] <- 'Ochotona whartoni'
	pikaData$species[which(pikaData$origSpecies=='Ochotonidae')] <- 'Ochotonidae'

	## subspecies
	# need to decide on taxonomic authority for these!!!
	
	pikaData$subspecies <- NA

	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona alpina changaica')] <- 'changaica'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona alpina changaica Ognev, 1940')] <- 'changaica'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona cansus stevensi Osgood, 1932')] <- 'stevensi'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona cansus subsp. cansus')] <- 'cansus'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona dauurica altaina')] <- 'altaina'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona dauurica daurica')] <- 'dauurica'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona dauurica mursaevi')] <- 'mursaevi'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona dauurica mursavi Bannikov, 1951')] <- 'mursaevi'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona dauurica subsp. dauurica')] <- 'dauurica'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona hyperborea cinerofusca')] <- 'cinerofusca'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona hyperborea mantchurica')] <- 'mantchurica'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona hyperborea mantchurica Thomas, 1909')] <- 'mantchurica'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona kolymensis')] <- 'kolymensis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona macrotis subsp. macrotis')] <- 'macrotis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona macrotis wollastoni Thomas & Hinton, 1922')] <- 'wollastoni'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona pallasi pricei')] <- 'pricei'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona pallasi pricei Thomas, 1911')] <- 'pricei'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona pricei')] <- 'pricei'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona pusilla subsp. pusilla')] <- 'pusilla'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona roylei nepalensis Hodgson, 1841')] <- 'nepalensis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona roylei roylei')] <- 'roylei'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona roylei subsp. roylei')] <- 'roylei'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona rufescens regina Thomas, 1911')] <- 'regina'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona rufescens subsp. rufescens')] <- 'rufescens'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona thibetana osgoodi Anthony, 1941')] <- 'osgoodi'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona thibetana sikimaria Thomas, 1922')] <- 'sikimaria'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona thibetana subsp. thibetana')] <- 'thibetana'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona thibetana thibetana')] <- 'thibetana'
	
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps albata')] <- 'albata'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps brunnescens')] <- 'brunnescens'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps cinnamomea')] <- 'cinnamomea'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps clamosa')] <- 'clamosa'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps cuppes')] <- 'cuppes'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps fenisex')] <- 'fenisex'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps figginsi')] <- 'figginsi'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps figginsi Allen, 1912')] <- 'figginsi'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps fumosa')] <- 'fumosa'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps fuscipes')] <- 'fuscipes'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps goldmani')] <- 'goldmani'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps goldmani Howell, 1924')] <- 'goldmani'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps howelli')] <- 'howelli'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps incana')] <- 'incana'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps incana')] <- 'incana'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps jewetti')] <- 'jewetti'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps lasalensis')] <- 'lasalensis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps lasalensis Durrant & Lee, 1955')] <- 'lasalensis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps lemhi')] <- 'lemhi'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps levis')] <- 'levis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps lutescens')] <- 'lutescens'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps muiri')] <- 'muiri'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps murri')] <- 'murri'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps negrescens')] <- 'nigrescens'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps nevadensis')] <- 'nevadensis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps nevadensis Howell, 1919')] <- 'nevadensis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps nigrescens')] <- 'nigrescens'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps nigrescens Bailey, 1913')] <- 'nigrescens'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps obscura')] <- 'obscura'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps obscura Long, 1965')] <- 'obscura'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps princeps')] <- 'princeps'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps saxatilis')] <- 'saxatilis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps saxatilis Bangs, 1899')] <- 'saxatilis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps saxitilis')] <- 'saxatilis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps schisticeps')] <- 'schisticeps'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps schisticeps (Merriam, 1889)')] <- 'schisticeps'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps septentrionalis')] <- 'septentrionalis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps sheltoni')] <- 'sheltoni'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps sheltoni Grinnell, 1918')] <- 'sheltoni'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps ssp.')] <- NA
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps subsp. princeps')] <- 'princeps'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps taylori')] <- 'taylori'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps taylori Grinnell, 1912')] <- 'taylori'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps tenisex')] <- 'tenisex'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps tutelata')] <- 'tutelata'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps tutelata Hall, 1934')] <- 'tutelata'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps uinta')] <- 'uinta'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps utahensis')] <- 'utahensis'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps ventorum')] <- 'ventorum'
	pikaData$subspecies[which(pikaData$origSpecies=='Ochotona princeps wasatchensis')] <- 'wasatchensis'
	
	## contributor-supplied judgements on subspecies
	pikaData$subspeciesNotes <- NA
	
	# email from Chris Shank 2015-07-23 to Mimi's inquiry
	pikaData$subspecies[which(pikaData$dataName=='Chris Shank - HELS Alberta 2015-02-16' & pikaData$origLat==50.63914637 & pikaData$origLong==-119.2969322)] <- 'princeps'
	pikaData$subspeciesNotes[which(pikaData$dataName=='Chris Shank - HELS Alberta 2015-02-16' & pikaData$origLat==50.63914637 & pikaData$origLong==-119.2969322)] <- 'Subspecies assigned to princeps based on email from Chris Shank 2015-07-23.'
	
	# email from Philippe Henry 2015-07-23 to Mimi's inquiry
	pikaData$subspecies[which(pikaData$dataName=='Julie Timmins & Philippe Henry (Banff NP) - Set B' & pikaData$origLat==49.75895 & pikaData$origLong==-119.4303333)] <- 'princeps'
	pikaData$subspeciesNotes[which(pikaData$dataName=='Julie Timmins & Philippe Henry (Banff NP) - Set B' & pikaData$origLat==49.75895 & pikaData$origLong==-119.4303333)] <- 'Subspecies assigned to princeps based on email from Philippe 2015-07-23.'
	
	pikaData$subspecies[which(pikaData$dataName=='Julie Timmins & Philippe Henry (Banff NP) - Set B' & pikaData$origLat==49.5613 & pikaData$origLong==-119.06445)] <- 'fenisex'
	pikaData$subspeciesNotes[which(pikaData$dataName=='Julie Timmins & Philippe Henry (Banff NP) - Set B' & pikaData$origLat==49.5613 & pikaData$origLong==-119.06445)] <- 'Subspecies assigned to princeps based on email from Philippe 2015-07-23.'
	
	# email from Kim Hersey 2015-08-03 to Mimi's inquiry
	pikaData$subspecies[which(pikaData$dataName=='Kim Hersey & Phillip Gray - Set A - Kim Asmus' & pikaData$origLat==4457409 & pikaData$origLong==453438 & pikaData$origElev==2859)] <- 'princeps'
	pikaData$subspeciesNotes[which(pikaData$dataName=='Kim Hersey & Phillip Gray - Set A - Kim Asmus' & pikaData$origLat==4457409 & pikaData$origLong==453438 & pikaData$origElev==2859)] <- 'Subspecies assigned to princeps based on email from Kim 2015-08-03.'
	
	pikaData$subspecies[which(pikaData$dataName=='Kim Hersey & Phillip Gray - Set A - Kim Asmus' & pikaData$origLat==4458650 & pikaData$origLong==453616 & pikaData$origElev==2918)] <- 'princeps'
	pikaData$subspeciesNotes[which(pikaData$dataName=='Kim Hersey & Phillip Gray - Set A - Kim Asmus' & pikaData$origLat==4458650 & pikaData$origLong==453616 & pikaData$origElev==2918)] <- 'Subspecies assigned to princeps based on email from Kim 2015-08-03.'
	
	#########################
	### flag record types ###
	#########################
	
	pikaData$recordType <- 'specimen/detection/non-detection'
	pikaData$recordType[pikaData$origKindOfSighting=='fossil'] <- 'fossil'
	pikaData$recordType[pikaData$origKindOfSighting=='FOSSIL_SPECIMEN'] <- 'fossil'
	pikaData$recordType[pikaData$origKindOfSighting=='FossilSpecimen'] <- 'fossil'
	pikaData$recordType[pikaData$origKindOfSighting=='MACHINE_OBSERVATION'] <- 'unknown'
	pikaData$recordType[pikaData$origKindOfSighting=='TRUCK PARKED HERE'] <- NA
	
	#########################
	## flag usable records ##
	#########################
	
	pikaData$usable <- ifelse(pikaData$coordsOk * pikaData$coordUncerOk * !is.na(pikaData$obsYear) * !is.na(pikaData$obsMonth) * !is.na(pikaData$obsDayOfMonth) * (pikaData$obsYear <= 2015 & pikaData$obsYear >= 1800) * !is.na(pikaData$origRecentPikaOrSignsObserved)==1, TRUE, FALSE)
	pikaData$usable[pikaData$species=='Ochotona'] <- 0
	pikaData$usable[pikaData$species=='Ochotonidae'] <- 0
	pikaData$usable[which(is.na(pikaData$usable))] <- 0
	pikaData$usable <- as.logical(pikaData$usable)
	
	# until further notice, *all* 2015 ASC data is unusable because coordinates don't match verbal descriptions of localities
	pikaData$usable[which(pikaData$dataName=='Alex Hamilton & Grace Matelich (ASC) - Set A 2015-01-29')] <- FALSE
	pikaData$usable[which(pikaData$dataName=='Alex Hamilton & Grace Matelich (ASC) - Set B 2016-01-26')] <- FALSE
	
	# Connie Millar -- Mimi manually checked points and indicated certain records are to be excluded as "usable" because their locality descriptions don't match their locations on a map or the locality descriptions are vague and don't allow for checking

	# GBIF record for Ochotona princeps in northern Canada: "40 mi. S of Kananaskis" locality
	pikaData$usable[
			pikaData$dataName=='GBIF 2016-03-10' &
			pikaData$origLat > 68.96666 & pikaData$origLat < 68.96668 &
			pikaData$origLong < -115.91666 & pikaData$origLong > -115.91668] <- FALSE

	# GBIF record for Ochotona princeps in northern Canada: "Baker Lake" locality
	pikaData$usable[
			pikaData$dataName=='GBIF 2016-03-10' &
			pikaData$origLat > 64.31943 & pikaData$origLat < 64.31945 &
			pikaData$origLong < -96.02082 & pikaData$origLong > -96.02084] <- FALSE
			
	# GBIF record for "Ochotona princeps lutescens" in northern Canada: "Baker Lake" locality
	pikaData$usable[
			pikaData$dataName=='VertNet Ochotona 2016-03-10' &
			pikaData$origLat > 64.319443 & pikaData$origLat < 64.319445 &
			pikaData$origLong < -96.020832 & pikaData$origLong > -96.020834] <- FALSE
			
	# GBIF record for Ochotona princeps in Taiwan
	pikaData$usable[
			pikaData$dataName=='GBIF 2016-03-10' &
			pikaData$origLat > 24.27 & pikaData$origLat < 24.29 &
			pikaData$origLong > 120.94 & pikaData$origLong < 120.96] <- FALSE
			
	# Jo Varner iNaturalist record in Eastern Colorado with note that it was taken in Rocky Mountain National Park
	pikaData$usable[
			(pikaData$dataName=='Johanna Varner - iNaturalist - Set A' | pikaData$dataName=='Alex Hamilton & Grace Matelich (ASC) - Set B 2016-01-26') &
			pikaData$origLat > 40.400248 & pikaData$origLat < 40.400252 &
			pikaData$origLong < -104.3364166 & pikaData$origLong > -104.3364168] <- FALSE

	# VertNet northeastern Canada
	pikaData$usable[
			pikaData$dataName=='VertNet Ochotona 2016-03-10' &
			pikaData$origLat > 64.319443 & pikaData$origLat < 64.319445 &
			pikaData$origLong < -96.020832 & pikaData$origLong > -96.020834] <- FALSE
	
	# #######################################
	# ### flagging duplicates as unusable ###
	# #######################################

	# print('Flagging duplicate records as unusable... Duplicates in coordinates, original date, original state (presence/non-detection), and record type (e.g., fossil, observation).')
	
	# rownames(pikaData) <- 1:nrow(pikaData)
	
	# initiallyOk <- pikaData[!is.na(pikaData$origLong) & !is.na(pikaData$origLat) & !is.na(pikaData$dateOfObs) & !is.na(pikaData$origRecentPikaOrSignsObserved), ]
	
	# for (thisLat in unique(initiallyOk$origLat)) {
	
		# sameLatRecords <- initiallyOk[initiallyOk$origLat==thisLat, ]
		
		# for (thisLong in unique(sameLatRecords$origLong)) {
		
			# sameLatLongRecords <- sameLatRecords[sameLatRecords$origLong==thisLong, ]
			
			# for (thisDate in unique(sameLatLongRecords$dateOfObs)) {
			
				# sameLatLongDateRecords <- sameLatLongRecords[sameLatLongRecords$dateOfObs==thisDate, ]
				
				# for (thisType in unique(sameLatLongDateRecords$recordType)) {
				
					# sameLatLongDateTypeRecords <- sameLatLongDateRecords[sameLatLongDateRecords$recordType==thisType, ]
				
					# for (thisState in sameLatLongDateTypeRecords$origRecentPikaOrSignsObserved) {
					
						# sameLatLongDateTypeStateRecords <- sameLatLongDateTypeRecords[sameLatLongDateTypeRecords$origRecentPikaOrSignsObserved==thisState, ]
				
						# if (nrow(sameLatLongDateTypeStateRecords) > 1) {
						
							# if (any(sameLatLongDateRecords$usable)) {
							
								# pikaData$usable[pikaData$origLat==thisLat & pikaData$origLong==thisLong & pikaData$dateOfObs==thisDate & pikaData$recordType==thisType & pikaData$origState==thisState & pikaData$usable][2:sum(pikaData$origLat==thisLat & pikaData$origLong==thisLong & pikaData$dateOfObs==thisDate & pikaData$recordType==thisType & pikaData$origState==thisState & pikaData$usable)] <- FALSE
							
							# }
						
						# }
						
					# }
					
				# }
					
			# }
		
		# }
	
	# }
	
	##################
	## print checks ##
	##################

	print('CHECKS:')
	print('=======')
	print('origRecentPikaOrSignsObserved:')
	print(sort(unique(pikaData$origRecentPikaOrSignsObserved)))

	print('citizenScience:')
	print(sort(unique(pikaData$citizenScience)))

	print('dateOfObs:')
	print(sort(unique(pikaData$dateOfObs)))

	print('species:')
	print(sort(unique(pikaData$species)))

	print('subspecies:')
	print(sort(unique(pikaData$subspecies)))

	##################
	## save results ##
	##################

	cat('\nSaving files of points...\n'); flush.console()

	thisTime <- format(Sys.time(), '%Y-%m-%d %H%M')
	dir.create(paste0('C:/ecology/Drive/Research/Iconic Species//Species Records - Pika/!Collated Data ', thisTime), recursive=T, showWarnings=F)

	# add our key
	pikaData <- cbind(
		data.frame(num=paste(thisTime, 1:nrow(pikaData))),
		pikaData
	)
	
	# CSV - all
	write.csv(pikaData, paste0('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data ', thisTime, '/00 Pika - Cleaned Using R - All Records & All Species.csv'), row.names=F)

	pikaShape <- SpatialPointsDataFrame(coords=cbind(pikaData$longWgs84, pikaData$latWgs84), proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=pikaData)

	# # save ALL records
	# pikaShapeJitter <- SpatialPointsDataFrame(coords=cbind(pikaData$longWgs84 + runif(nrow(pikaData), -0.25, 0.25), pikaData$latWgs84 + runif(nrow(pikaData), -0.25, 0.25)), proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=pikaData)

	writeOGR(pikaShape, paste0('C:/ecology/Drive/Research/Iconic Species//Species Records - Pika/!Collated Data ', thisTime), '00_pika_cleanedUsingR_allRecords_allSpecies', driver='ESRI Shapefile')
	# writeOGR(pikaShapeJitter, paste0('C:/ecology/Drive/Research/Iconic Species//Species Records - Pika/!Collated Data ', thisTime), '00_pika_cleanedUsingR_jitteredCoords_allRecords_allSpecies', driver='ESRI Shapefile')

	### save USABLE records
	pikaDataUsable <- pikaData[which(pikaData$usable), ]
	pikaShapeUsable <- SpatialPointsDataFrame(coords=cbind(pikaDataUsable$longWgs84, pikaDataUsable$latWgs84), proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=pikaDataUsable)
	pikaShapeUsableJitter <- SpatialPointsDataFrame(coords=cbind(pikaDataUsable$longWgs84 + runif(nrow(pikaDataUsable), -0.25, 0.25), pikaDataUsable$latWgs84 + runif(nrow(pikaDataUsable), -0.25, 0.25)), proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=pikaDataUsable)

	writeOGR(pikaShapeUsable, paste0('C:/ecology/Drive/Research/Iconic Species//Species Records - Pika/Collated Data ', thisTime), '00_pika_cleanedUsingR_usableRecords_allSpecies', driver='ESRI Shapefile')
	writeOGR(pikaShapeUsableJitter, paste0('C:/ecology/Drive/Research/Iconic Species//Species Records - Pika/Collated Data ', thisTime), '00_pika_cleanedUsingR_jitteredCoords_usableRecords_allSpecies', driver='ESRI Shapefile')

	saveRDS(pikaDataUsable, file=paste0('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data ', thisTime, '/00 Pika - Cleaned Using R - Usable Records for All Ochotona.rds'))
			
	print(FORCEEND)

}		
		
# print('##########################################################')
# print('### plot each contributor\'s data for checking by them ###')
# print('##########################################################')

# pikaData <- read.csv(file.choose(), as.is=T)

# pikaData$origRecentPikaOrSignsObserved <- as.logical(pikaData$origRecentPikaOrSignsObserved)
# pikaData$coordUncerOk <- as.logical(pikaData$coordUncerOk)

# pikaData <- subset(pikaData, dateAdded!='pre-Sept 2014')

# pikaShape <- SpatialPointsDataFrame(coords=cbind(pikaData$longWgs84, pikaData$latWgs84), proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'), data=pikaData)

# elevRaster <- raster('../Climate - Rasters/WORLDCLIM Ver 1pt4 Rel 3/30 arcsec/Elevation - 30 arcsec/elevation_rectCropToPoints.tif')

# shapeCounties <- shapefile('C:/ecology/Political Geography/GADM/ver1/gadm_v1_lev2_wgs84_northAmerica')
# shapeStates <- shapefile('C:/ecology/Political Geography/GADM/ver1/gadm1_lev1_wgs84_northAmerica')

# dataNames <- unique(pikaData$dataName)[1:length(unique(pikaData$dataName))]
# for (countDataName in 1:length(dataNames)) {

	# cat(countDataName, dataNames[countDataName], '\n'); flush.console()

	# thisShape <- subset(pikaShape, dataName==dataNames[countDataName])

	# thisTime <- format(Sys.time(), '%Y-%m-%d')
	# dir.create(paste0(dirname(thisShape$source[1]), '/Map ', thisTime), recursive=T, showWarnings=F)
	
	# # save email and bad points
	# write.csv(data.frame(x=thisShape$email[1]), paste(dirname(thisShape$source[1]), '/Map ', thisTime, '/!email.csv', sep=''))
	# badRecords <- subset(as.data.frame(thisShape), !coordsOk | is.na(coordUncerOk) | is.na(dateOfObs))
	# if (nrow(badRecords) > 0) write.csv(badRecords, paste(dirname(thisShape$source[1]), '/Map ', thisTime, '/!Records without a coordinate system, without coordinate uncertainty, and-or a date.csv', sep=''))
	
	# ## get plotting extent
	
	# # calculate centroid, dist to each point around this, then circle with radius = maximum across distances... use this circle as the extent
	# if (nrow(thisShape) > 1) {
		
		# center <- gCentroid(thisShape)
		# dists <- distCosine(p1=center, p2=thisShape)
		# circle <- gBuffer(spTransform(center, CRS('+proj=aeqd +lat_0=6.231971667 +lon_0=-75.56826443999999 +x_0=835376.4399999999 +y_0=1180809.75 +ellps=intl +units=m +no_defs')), width=max(dists)) # using Azimuthal Equidistant projection
		# circle <- spTransform(circle, CRS(projection(thisShape)))
		# thisExtent <- extent(circle)
		
	# } else {
	
		# thisExtent <- extent(thisShape$longWgs84, thisShape$longWgs84, thisShape$latWgs84, thisShape$latWgs84)

	# }

	# thisExtent <- extent(thisExtent@xmin - 1.25, thisExtent@xmax + 1.25, thisExtent@ymin - 1.25, thisExtent@ymax + 1.25)
	
	# ## plot

	# png(paste0(dirname(thisShape$source[1]), '/Map ', thisTime, '/', dataNames[countDataName], ' ', thisTime, '.png'), width=1200, height=1200, res=300)
	
	# par(
		# cex=1,
		# cex.main=0.35,
		# cex.sub=0.3,
		# cex.lab=0.2,
		# cex.axis=0.4,
		# mar=c(12, 4, 4, 2) / 2
	# )
	
	
	# # points -- sets map extent
	# plot(thisShape, col='white', main=paste(as.character(thisShape$contact[1]), '\n', as.character(thisShape$dataName[1]), '\n', basename(as.character(thisShape$source[1])), '|', date(), '\n', nrow(thisShape), 'Records'),)
	
	# # elevation raster
	# try(
		# plot(
			# elevRaster,
			# xpd=FALSE,
			# maxpixels=1000000,
			# add=T,
			# xlim=c(thisExtent@xmin, thisExtent@xmax),
			# ylim=c(thisExtent@ymin, thisExtent@ymax)
		# ),
		# TRUE
	# )

	# # add shapefile overlay: counties
	# plot(shapeCounties, add=T, lwd=0.1, border='gray40')
	
	# # add shapefile overlay: states
	# plot(shapeStates, add=T, lwd=1, border='gray10')
	
	# ## add points
	
	# # points with good coordinates and dates: presences
	# thisPoints <- subset(thisShape, origRecentPikaOrSignsObserved & !is.na(coordUncerOk) & !is.na(dateOfObs) & coordsOk)
	# if (nrow(thisPoints) > 0) {
		# points(thisPoints, pch=16, cex=0.3, col='blue')
	# }
	
	# # points with good coordinates and dates: absences
	# thisPoints <- subset(thisShape, !origRecentPikaOrSignsObserved & !is.na(coordUncerOk) & !is.na(dateOfObs) & coordsOk)
	# if (nrow(thisPoints) > 0) {
		# points(thisPoints, pch=1, cex=0.3, col='black')
	# }
	
	# # points WITHOUT stated coord uncertainty but GOOD dates
	# thisPoints <- subset(thisShape, (is.na(coordUncerOk)) & !is.na(dateOfObs) & coordsOk)
	# if (nrow(thisPoints) > 0) {
		# points(thisPoints, pch=2, cex=0.4, col='red')
	# }

	# # points WITH stated coord uncertainty but BAD dates
	# thisPoints <- subset(thisShape, !is.na(coordUncerOk) & is.na(dateOfObs) & coordsOk)
	# if (nrow(thisPoints) > 0) {
		# points(thisPoints, pch=3, cex=0.4, col='red')
	# }

	# # points WITHOUT stated coord uncertainty but BAD dates
	# thisPoints <- subset(thisShape, is.na(coordUncerOk) & is.na(dateOfObs) & coordsOk)
	# if (nrow(thisPoints) > 0) {
		# points(thisPoints, pch=4, cex=0.4, col='red')
	# }

	# # points WITHOUT datum/projection
	# thisPoints <- subset(thisShape, !coordsOk)
	# if (nrow(thisPoints) > 0) {
		# points(thisPoints, pch=5, cex=0.4, col='red')
	# }

	# # legend(
		# # x=thisExtent@xmin,
		# # y=thisExtent@ymin - 0.25 * diff(range(thisExtent@ymin, thisExtent@ymax)),
		# # legend=c('Presences WITH a value for coordinate uncertainty and a date', 'Absences WITH a value for coordinate uncertainty and a date', 'Sites WITHOUT a value for coordinate uncertainty', 'Sites WITHOUT a date', 'Sites WITHOUT a value for coordinate uncertainty and WITHOUT a date', 'Sites without a coordinate system/datum (may also have other problems)'),
		# # pch=c(16, 1, 2, 3, 4, 5),
		# # col=c('blue', 'black', 'red', 'red', 'red', 'red'),
		# # ncol=1,
		# # xpd=NA,
		# # bty='o',
		# # cex=0.4,
		# # box.col='gray90',
		# # yjust=0
	# # )
			
	# legend(
		# x=mean(c(thisExtent@xmin, thisExtent@xmax)),
		# y=min(thisShape$latWgs84) - 0.05 * (max(thisShape$latWgs84) - min(thisShape$latWgs84)),
		# legend=c('Presences WITH a value for coordinate uncertainty and a date', 'Absences WITH a value for coordinate uncertainty and a date', 'Sites WITHOUT a value for coordinate uncertainty', 'Sites WITHOUT a date', 'Sites WITHOUT a value for coordinate uncertainty and WITHOUT a date', 'Sites without a coordinate system/datum (may also have other problems)'),
		# pch=c(16, 1, 2, 3, 4, 5),
		# col=c('blue', 'black', 'red', 'red', 'red', 'red'),
		# ncol=1,
		# xpd=NA,
		# bty='o',
		# cex=0.4,
		# box.col='gray90',
		# xjust=0.5
	# )
			
	# dev.off()
	
# }

# print('############################################')
# print('### calculate statistics on species data ###')
# print('############################################')

# pikData <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/00 Pika - Cleaned Using R - Usable Records for All Ochotona.rds')
# pikaData <- pikaData[pikaData$species=='Ochotona princeps' & pikaData$usable & pikaData$dateOfObs >= 1981 & !is.na(pikaData$origRecentPikaOrSignsObserved), ]
# pikaData$dateOfObs <- as.integer(pikaData$dateOfObs)

# # find out how many detections/non-detections occur in cells across a string of years (e.g., detection in year 1, non-detection in 2, detection in 3, etc.)
# prism <- raster('C:/ecology/!Projects/Grinnell/environmentalData/climatePastPresent_PRISM_800m/prism_30arcsec_westernNorthAmerica_1900to1939/BIO01.tif')

# cells <- as.data.frame(extract(prism, cbind(pikaData$longWgs84, pikaData$latWgs84), cellnumbers=T))
# pikaData$cell <- cells$cell

# report <- data.frame(
	# cellsWithOneRecord=0,
	# cellsWithOneDetect=0,
	# cellsWithOneNondetect=0,
	
	# cellsWithGte2Records=0,
	
	# cellsWithGte2RecordsSameYear=0,
	# cellsWithGte2RecordsSameYearAllDetect=0,
	# cellsWithGte2RecordsSameYearAllNondetect=0,
	
	# cellsWithGte2RecordsDiffYear=0,
	# cellsWithGte2RecordsDiffYearAllDetect=0,
	# cellsWithGte2RecordsDiffYearAllNondetect=0,
	# cellsWithGte2RecordsDiffYearMixedDetectAcrossYears=0
# )

# for (cell in sort(unique(pikaData$cell))) {

	# thisCell <- pikaData[which(pikaData$cell==cell), ]
	
	# # cells with just one record
	# if (nrow(thisCell) == 1) {
	
		# report$cellsWithOneRecord <- report$cellsWithOneRecord + 1
		# report$cellsWithOneDetect <- report$cellsWithOneDetect + (thisCell$origRecentPikaOrSignsObserved)
		# report$cellsWithOneNondetect <- report$cellsWithOneNondetect + (!thisCell$origRecentPikaOrSignsObserved)
		
	# } else {
	
		# # cells with >1 record
		# report$cellsWithGte2Records	<- report$cellsWithGte2Records + 1
	
		# # cells with >1 record within years
		# if (sd(thisCell$dateOfObs)==0) {
		
			# report$cellsWithGte2RecordsSameYear <- report$cellsWithGte2RecordsSameYear + 1
			# report$cellsWithGte2RecordsSameYearAllDetect <- report$cellsWithGte2RecordsSameYearAllDetect + all(thisCell$origRecentPikaOrSignsObserved)
			# report$cellsWithGte2RecordsSameYearAllNondetect <- report$cellsWithGte2RecordsSameYearAllNondetect + !all(thisCell$origRecentPikaOrSignsObserved)
		
		# # cells with >1 record across years
		# } else {
		
			# # print(thisCell$dateOfObs)
		
			# report$cellsWithGte2RecordsDiffYear <- report$cellsWithGte2RecordsDiffYear + 1
			
			# # tabulate mixed detections across years
			# detections <- logical()
			# for (thisYear in sort(unique(thisCell$dateOfObs))) {
				
				# if (any(thisCell$origRecentPikaOrSignsObserved[thisCell$dateOfObs==thisYear])) { detections <- c(detections, TRUE) } else { detections <- c(detections, FALSE) }

			# }
			
			# if (all(detections)) report$cellsWithGte2RecordsDiffYearAllDetect <- report$cellsWithGte2RecordsDiffYearAllDetect + 1
			# if (all(!detections)) report$cellsWithGte2RecordsDiffYearAllNondetect <- report$cellsWithGte2RecordsDiffYearAllNondetect + 1
			# if (!all(detections) & !all(!detections)) report$cellsWithGte2RecordsDiffYearMixedDetectAcrossYears <- report$cellsWithGte2RecordsDiffYearMixedDetectAcrossYears + 1
			
			# # if (!(!all(detections) & !all(!detections)) & !all(thisCell$origRecentPikaOrSignsObserved) & all(!thisCell$origRecentPikaOrSignsObserved)) print(STOP)
			
		# }
		
	# }

# }

# say('##################################################################')
# say('### remove data "Dan Doak, Anna Chalfoun, Leah Yandow - Set A" ###')
# say('##################################################################')	

# dirCreate('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b')

# pres <- read.csv('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/00 Pika - Cleaned Using R - All Records & All Species.csv')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# write.csv(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/00 Pika - Cleaned Using R - All Records & All Species.csv', row.names=FALSE)

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/00 Pika - Cleaned Using R - Usable Records for All Ochotona.rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/00 Pika - Cleaned Using R - Usable Records for All Ochotona.rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/01 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - Clade Identity and Elevation Extracted.rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/01 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - Clade Identity and Elevation Extracted.rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/02 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - PRISM & DayMet Climate Data Extracted.rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/02 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - PRISM & DayMet Climate Data Extracted.rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/04 Ochotona princeps - Usable - Presences 1990-2015 - Derived Climate Variables.rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/04 Ochotona princeps - Usable - Presences 1990-2015 - Derived Climate Variables.rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/05 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Provinces (Fenneman).rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/05 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Provinces (Fenneman).rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/06 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (EPA Level III Modified).rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/06 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (EPA Level III Modified).rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/07 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (GEnS).rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/07 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (GEnS).rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/08 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (Koppen).rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/08 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Climate Regions (Koppen).rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/09 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (Bailey).rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/09 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Ecoregions (Bailey).rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/10 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Elevational Bands above Min PME.rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/10 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Elevational Bands above Min PME.rds')

# pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/11 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Regions (Hammond).rds')
# pres <- pres[-which(pres$dataName %in% 'Dan Doak, Anna Chalfoun, Leah Yandow - Set A'), ]
# saveRDS(pres, 'C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256b/11 Ochotona princeps - Usable - Presences 1990-2015 - Extracted Physiographic Regions (Hammond).rds')




print('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
print('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
print('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
print('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
print('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
