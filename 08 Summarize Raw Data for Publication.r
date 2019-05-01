### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2016-11

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/08 Summarize Raw Data for Publication.r')

rm(list=ls())

	drive <- 'C:/'
	pmes <- c('pmeNone', 'pmeMin')
	valances <- 'including'
	schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin', 'physioFenneman')

### CONTENTS ###
### libraries, variables, and functions ###
### collate sample sizes ###

###########################################
### libraries, variables, and functions ###
###########################################

	source(paste0(drive, 'ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r'))

say('############################')
say('### collate sample sizes ###')
say('############################')

	presAll <- getPres(canada=TRUE)
	presUS <- getPres(canada=FALSE)
		
	summ <- data.frame()
		
	for (scheme in schemes) {

		out <- schemeInfo(scheme, poly=FALSE)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
		
		units <- getUnits(scheme=scheme, incAll=TRUE)
		
		for (thisUnit in units) {

			load(paste0(workDir, 'ENMs - PCs/', schemeShort, '/INCLUDING ', toupper(thisUnit), ' - Min PME/Ochotona princeps/!Species Data - Ochotona princeps.RData'))
		
			thisSummary <- data.frame(
				scheme=schemeNice,
				unit=thisUnit,
				nAll=nrow(speciesData$allPresences)
			)
		
			summ <- rbind(summ, thisSummary)
		
		}
		
	}
	
	write.csv(summ, paste0(workDir, 'Species Records - Pika/!Summary of Records by Scheme and Unit.csv'))

say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
