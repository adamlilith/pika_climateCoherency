### Ochotona princeps - Extract Raw Climate Data for Ochotona princeps
### Adam B. Smith | 2016-08

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/02 Resample Background Sites to Account for Bias.r')

memory.limit(memory.limit() * 2^30)
options(stringsAsFactors=FALSE)
gc()

### CONTENTS ###
### libraries, variables, and functions ###
### amongst records choose only presences observed from 1981 through 2015 and covered by PRISM data ###
### resample background sites to match temporal distribution in presences ###


###########################################
### libraries, variables, and functions ###
###########################################

# working directory
workDir <- 'C:/ecology/Drive/Research/Iconic Species/'

# PRISM AN81d raster directory
prismAN81dDir <- 'E:/Climate/PRISM/AN81d 1981-2015/'

# PRISM AN81m raster directory
prismAN81mDir <- 'E:/Climate/PRISM/AN81m 1895-2015/'

### define variables

# proj4 string for WGS84
wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

# load libraries
library(rgdal)
library(raster)
library(dismo)

library(compiler)
enableJIT(1)
setCompilerOptions(suppressUndefined=TRUE)

### utility functions

# function to calculate day of year
source('C:/ecology/Drive/R/Calculate Day of Year from Year, Month, and Day.r')

source('C:/ecology/Drive/R/!Omnibus.r')

doyNonLeap <- read.csv('C:/ecology/Drive/R/Ancillary Files/daysOfYear_nonLeapYear.csv', as.is=TRUE)
doyLeap <- read.csv('C:/ecology/Drive/R/Ancillary Files/daysOfYear_leapYear.csv', as.is=TRUE)

# say('#######################################################################################################')
# say('### amongst records choose only presences observed from 1981 through 2015 and covered by PRISM data ###')
# say('#######################################################################################################')

	# records <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/02 Ochotona princeps - Usable - Presences & Non-detections 1895-2015 - PRISM & DayMet Climate Data Extracted.rds'))

	# # get just recordsences
	# records <- records[records$origRecentPikaOrSignsObserved, ]

	# # get recordsences between 1981 and 2015
	# dates <- as.character(as.Date(paste0(records$obsYear, '-', records$obsMonth, '-', records$obsDayOfMonth)))
	# records <- records[which(dates >= as.Date('1981-01-01') & dates <= as.Date('2015-12-31')), ]
	
	# # PRISM DEM for mask raster
	# prism <- raster(paste0(workDir, 'Environmental Data/PRISM/PRISM_us_dem_800m.tif'))
	
	# # remove recordsences outside PRISM
	# inPrism <- which(!is.na(records$elev_prism_m))
	# records <- records[inPrism, ]
	
	# saveRDS(records, paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/03 Ochotona princeps - Usable - Presences 1981-2015.rds'))
		
say('#############################################################################')
say('### resample background sites to match temporal distribution in presences ###')
say('#############################################################################')

	say('Assigning a date to each background site so it matches recording data of at least one detection site.')

	pres <- readRDS(paste0(workDir, 'Species Records - Pika/!Collated Data 2016-06-30 1256/03 Ochotona princeps - Usable - Presences 1981-2015.rds'))

	# get only records will full climate coverage
	pres <- pres[pres$obsYear >= 1990, ]

	# function to choose background with weighting
	source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/Function to Choose Background Sites with Weighting.r')

	# for each kind of bias correction
	for (bgVariant in c('Spatiotemporally Weighted')) {
		
		# for (cladeVariant in c('cladePolyOverlap', 'cladePolyNonoverlap')) {
		for (cladeVariant in c('cladePolyOverlap')) {
		
			# for (pmeVariant in c('pmeMax', 'pmeMean', 'pmeMin')) {
			for (pmeVariant in c('pmeMin')) {
			
				for (cladeType in c('all', 'fenisex', 'saxatilis', 'princeps', 'schisticeps', 'uinta')) {
				# for (cladeType in c('saxatilis', 'princeps', 'schisticeps', 'uinta')) {
				
					say('Choosing weighted background sites for', bgVariant, cladeVariant, pmeVariant, cladeType)
				
					# get presence sites
					thesePres <- if (cladeType=='all') {
						pres
					} else {
						pres[pres$clade==cladeType, ]
					}
				
					# load raw background sites
					bg <- readRDS(paste0(workDir, '/Background Sites/', bgVariant, '/01 BG Sites - ', 
						ifelse(cladeVariant=='cladePolyOverlap', 'Overlapping Clades', 'Non-overlapping Clades'), ' - ',
						ifelse(cladeType=='all', 'All Clades', paste0(substr(cladeType, 1, 1), substr(cladeType, 2, nchar(cladeType)), ' Clade')), ' - ',
						ifelse(pmeVariant=='pmeMin', 'Minimum PME Mask', ifelse(pmeVariant=='pmeMean', 'Mean PME Mask', 'Maximum PME Mask')), ' - Raw Data Extraction.rds.'))
					
					gc()

					# resample background sites in weighted manner
					bgWeighted <- chooseBgWithWeighting(
						bg=bg,
						pres=thesePres,
						presenceWeights=rep(1, nrow(thesePres)),
						multiplier=1,
						window=10,
						earliestYear=1990,
						totalBg=10000,
						mutableColumns=c(
							'tminAnnual', 'tmaxAnnual', 'pptAnnual', 'snowPackSweAnnualApril1',
							'tminMonthly', 'tmaxMonthly', 'pptMonthly', 'snowPackSweMonthly',
							'tminDaily', 'tmaxDaily', 'pptDaily', 'snowPackSweDaily'
						),
						immutableColumns=c('bgIndex', 'longWgs84', 'latWgs84', 'obsYear', 'obsMonth', 'obsDayOfMonth', 'obsDayOfYear', 'elev_gmted2010_m', 'elev_prism_m'),
						verbose=TRUE
					)
					
					gc()
					
					# save
					saveRDS(bgWeighted, paste0(workDir, '/Background Sites/', bgVariant, '/02 BG Sites - ', 
						ifelse(cladeVariant=='cladePolyOverlap', 'Overlapping Clades', 'Non-overlapping Clades'), ' - ',
						ifelse(cladeType=='all', 'All Clades', paste0(substr(cladeType, 1, 1), substr(cladeType, 2, nchar(cladeType)), ' Clade')), ' - ',
						ifelse(pmeVariant=='pmeMin', 'Minimum PME Mask', ifelse(pmeVariant=='pmeMean', 'Mean PME Mask', 'Maximum PME Mask')), ' - Temporally Weighted.rds.'))
					
					rm(bg, bgWeighted, thesePres); gc()

				} # next clade
			
			} # next PME
			
		} # next overlap/non-overlap
	
	} # next BG variant
	