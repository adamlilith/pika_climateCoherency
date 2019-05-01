### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2017-04

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/04 Analysis and Processing of Variables.r')

rm(list=ls())

### CONTENTS ###
### libraries, variables, and functions ###
### analyze correlations between derived variables ###

###########################################
### libraries, variables, and functions ###
###########################################

source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')
source('C:/ecology/Drive/R/Graphics/Graphics - Circular Network Plot.r')
source('C:/ecology/Drive/R/Graphics/Danielle Christianson Pairs Plot.r')

say('######################################################')
say('### analyze correlations between derived variables ###')
say('######################################################')

### correlations across BG sites
################################

bg <- readRDS(paste0(workDir, 'Background Sites/Random - Western USA/BG Sites 03 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - Derived Variables for 2015-10-07 - 10000 sites with no NAs.rds'))

cor <- cor(bg[ , predictorsToUse], method='spearman')

thold <- 0.7
corPos <- cor >= thold
corNeg <- cor <= -thold

spokePlot(pos=corPos,neg=corNeg,ontop='pos', labels=fields$veryShortName[fields$useAsPredictor], shrink=1, labelOffset=1.02, pch=16, cexPoints=1, cexLabel=1, lwdPos=3, lwdNeg=3, ltyPos='solid', ltyNeg='solid', colPos='black', colNeg='red', colPoints='black', colLabel=fields$predictorGroupColor[fields$useAsPredictor])

legend('bottomright', inset=-0.1, xpd=NA, legend=c(paste0('Positive correlation >=', thold), paste0('Negative correlation <=-', thold)), col=c('black', 'red'), lwd=2)


say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')