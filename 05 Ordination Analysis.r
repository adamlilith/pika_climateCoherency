### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2017-07

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/05 Ordination Analysis.r')

rm(list=ls())

### CONTENTS ###
### libraries, variables, and functions ###
### calculate environmental distance matrix between presences ###
### ordinate ###
### cluster analysis ###

######################
### generalization ###
######################

	schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified', 'elevQuantWrtPaeMin')
	# schemes <- 'cladeNonOverlap'
	# schemes <- 'ecoregionEpa3Modified'
	# schemes <- 'elevQuantWrtPaeMin'

	# pmes <- c('pmeNone', 'pmeMin')
	pmes <- 'pmeNone'
	# pmes <- 'pmeMin'

###########################################
### libraries, variables, and functions ###
###########################################

source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')

### returns index number of background sites that are temporally and spatially close to a focal background site
getCloseBg <- function(
	presCoord,		# 2-column, 1-row matrix of presence coordinates
	presDate, 		# date of presence observation
	bgCoord,		# 2-column, 1-row matrix of presence coordinates
	bgDate,			# date of bg observation
	minBg=20,		# minimum number of background sites to collect per presence
	perc=0.05,		# percentile of background sites to select that are closest in space or time to the focal presence
	inc=0.1		# value by which to increase/decrease perc in each step to get minBg
) {

	# stores index of bg sites that are close in space and time to the focal presence
	bgClose <- integer()

	while (length(bgClose) < minBg) {
	
		# get BG close in SPACE
		distToBg <- distCosine(presCoord, bgCoord)
		spatiallyClose <- which(distToBg <= quantile(distToBg, perc))
		
		# get BG close in TIME
		timeToBg <- abs(bgDate - presDate)
		temporallyClose <- which(timeToBg <= quantile(timeToBg, perc))

		# BG close in SPACE and TIME
		bgClose <- intersect(spatiallyClose, temporallyClose)

		perc <- perc * (1 + inc)
		
	}

	bgClose

}

# #################################################################
# ### calculate environmental distance matrix between presences ###
# #################################################################

	# sayHead('calculate environmental distance matrix between presences')

	# say('I want to perform an ordination to determine if one best partitions environmental variation across presences. However, the ordinaton must account for sampling bias, so I need to adjust distances between presences in a manner that reflects this. Hence I am using an adjusted distance: Dadj = D(p_i, p_j) / D(bg_i, bg_j) where D() is a distance function calculated on the environment, p_x is the environment of presence x, and bg_x is the background of presence x.')
	# say('Here, bg_x is selected such that it is within the z% quantile of all *geographic* distances **and** the z% quantile of all temporal differences between the time of the records collection and dates assigned to background points.  If no background points are within these confines, z wil be incremented gradually until there are at least minBg points in the background set.', post=2)
	# say('The problem is that there are different backgrounds selected for each division and PME variant. So how do I make one ordination for all divisions/PMEs? What background should I use?', post=2)
	# say('I will use the combined background across all divisions and PMEs selected for the "all" or "composite" unit.', post=2)

	# # generalization

	# # weight <- TRUE # weight distances between presences by distance between matching background
	# weight <- FALSE # no weighting
	
	# minBg <- 20 # minimum number of background sites to match to a presence
	# perc <- 0.05 # the z% quantile of all distances between that the presence p_x and all relevant background points
	# inc <- 0.1 # value by which to multiply perc if minBg is not found
	# pcs <- 1:2 # PCs to use for calculation of distance

	# if (weight) say('Using closest ', perc, ' of BG sites to ensure >=', minBg, ' selected per presence to calculate distinctiveness.')
	# say('Using PCs ', paste(pcs, collapse=' '))

	# # by SCHEME
	# for (scheme in schemes) {
	
		# out <- schemeInfo(scheme)
		# schemeNice <- out$schemeNice
		# divisionFieldPres <- out$divisionFieldPres
		# rm(out)
	
		# for (pme in pmes) {
	
			# pmeNice <- pmeNiceName(pme)
	
			# load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/INCLUDING ALL - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata'))
			# pres <- speciesData$allPresences
			# bg <- speciesData$allTrainingAbsences
			
			# sites <- rbind(pres[ , predictorsToUse], bg[ , predictorsToUse])
			
# pca <- princomp(sites, cor=TRUE)
# pcaPred <- predict(pca, pres[ , predictorsToUse])
# samps <- sample(1:nrow(pres), 1000)
# pres <- pcaPred[samps, ]
# pres <- as.data.frame(pres)
# names(pres) <- paste0('PC', prefix(1:20, 2))

			# # weight presence distance by background distance
			# if (weight) {
				
				# bg <- speciesData$allTrainingAbsences
				
				# bgCoord <- cbind(bg$longWgs84, bg$latWgs84)
				# bgDate <- base::as.Date(paste0(bg$obsYear, '-', bg$obsMonth, '-', bg$obsDayOfMonth))

				# ### create list object with elements each a vector of indices of background sites that are close in space and time to each presence
				# matchingBg <- list()

				# say('Finding matching background sites for each presence...')
				# for (i in 1:nrow(pres)) {
				
					# iCoord <- matrix(c(pres$longWgs84[i], pres$latWgs84[i]), nrow=1)
					# iDate <- base::as.Date(paste0(pres$obsYear[i], '-', pres$obsMonth[i], '-', pres$obsDayOfMonth[i]))
					# matchingBg[[i]] <- getCloseBg(presCoord=iCoord, presDate=iDate, bgCoord=bgCoord, bgDate=bgDate, minBg=minBg, perc=perc, inc=inc)
				
				# }
				
			# }
				
			# ### calculate distance matrix
			# dists <- matrix(NA, nrow=nrow(pres), ncol=nrow(pres))
			# diag(dists) <- 0
			
			# say('Calculating ', ifelse(weight, 'WEIGHTED', 'UNWEIGHTED'), ' distances between presences...')
			
			# # by PRESENCE #1
			# for (i in 1:(nrow(pres) - 1)) {
			
				# t0 <- Sys.time()
				# say('Presence ', i, ' of ', nrow(pres), post=0)

				# # get environment, coordinates, and date for this record
				# iEnv <- pres[i, paste0('PC', prefix(pcs, 2))]
				# iEnv <- as.matrix(iEnv, nrow=1)
				
				# # get background for presence #1
				# if (weight) iBgEnv <- bg[matchingBg[[i]], paste0('PC', prefix(pcs, 2))]
				
				# # by PRESENCE #2
				# for (j in (i + 1):nrow(pres)) {

					# # get environment, coordinates, and date for this record
					# jEnv <- pres[j, paste0('PC', prefix(pcs, 2))]
					# jEnv <- as.matrix(jEnv, nrow=1)

					# if (weight) {
						
						# # get background for presence #2
						# jBgEnv <- bg[matchingBg[[j]], paste0('PC', prefix(pcs, 2))]
						
						# # calculate mean distance between backgrounds
						# bgDist <- fields::rdist(iBgEnv, jBgEnv)
						# bgDist <- mean(bgDist)
						
					# }
				
					# # distance between presences
					# presDist <- fields::rdist(iEnv, jEnv)
					
					# # weighted distance
					# thisDist <- if (weight) {
						# presDist / bgDist
					# } else {
						# presDist
					# }
					
					# # remember
					# dists[i, j] <- thisDist
					# dists[j, i] <- thisDist
					
				# } # next PRESENCE #2
			
				# t1 <- Sys.time()
				# say('| ', round(t1 - t0, 3), ' sec')

			# } # next PRESENCE #1
			
			# distsAndBg <- list()
			# distsAndBg$dists <- dists
			
			# if (weight) {
				# distsAndBg$matchingBg <- matchingBg
				# distsAndBg$minBg <- minBg
				# distsAndBg$perc <- perc
				# distsAndBg$inc <- inc
			# }
			
			# distsAndBg$pcs <- pcs
			# distsAndBg$weight <- weight
# distsAndBg$samps <- samps
			
			# fileOut <- if (weight) {
				# paste0(workDir, 'Ordination Analysis/Pairwise Background-Weighted Environmental Distances between Presences - ', schemeNice, ' - ', pmeNice, ' PME.rds')
			# } else {
				# paste0(workDir, 'Ordination Analysis/Pairwise Unweighted Environmental Distances between Presences - ', schemeNice, ' - ', pmeNice, ' PME SUBSAMPLE.rds')
			# }
			
			# saveRDS(distsAndBg, fileOut)
			
		# } # next PME
	
	# } # next scheme

# ################
# ### ordinate ###
# ################

	# sayHead('ordinate')

	# # presences
	# pres <- getPres()

	# # generalization
	# # # weight <- TRUE # weight distances between presences by distance between matching background
	# weight <- FALSE # no weighting
	
	# k <- 2 # NMDS dimensions
	# # k <- 3 # NMDS dimensions
	
	# # vegan::stressplot(ord)
	# # scores(ord)
	
	# dirCreate(workDir, 'Ordination Analysis/NMDS')
			
	# for (scheme in schemes) {
	
		# out <- schemeInfo(scheme)
		# schemeNice <- out$schemeNice
		# divisionFieldPres <- out$divisionFieldPres

		# # by PME
		# for (pme in pmes) {
		
			# pmeNice <- pmeNiceName(pme)
			
			# say(Sys.time(), '| SCHEME ', schemeNice, ' | PME ', pmeNice)
			
			# # distance matrix
			# distFile <- if (weight) {
				# paste0(workDir, 'Ordination Analysis/Pairwise Background-Weighted Environmental Distances between Presences - ', schemeNice, ' - ', pmeNice, ' PME.rds')
			# } else {
				# paste0(workDir, 'Ordination Analysis/Pairwise Unweighted Environmental Distances between Presences - ', schemeNice, ' - ', pmeNice, ' PME SUBSAMPLE.rds')
			# }

			# distObj <- readRDS(distFile)
			# dists <- distObj$dists
# samps <- distObj$samps
			
			# dists <- as.dist(dists)
			# rm(distObj); gc()
		
			# # ordinate
			# ord <- vegan::metaMDS(comm=dists, distance='euclidian', k=k, trymax=10, autotransform=FALSE, plot=TRUE)
			# gc()
			
			# # ordDists <- dist(ord$points)
			
			# # form <- paste('ordDists ~', divisionFieldPres)
			# # form <- as.formula(form)
			
			# # data <- pres[ , divisionFieldPres, drop=FALSE]
			# # data[ , divisionFieldPres] <- as.factor(data[ , divisionFieldPres])
			# # gc()
			
			# # partition <- vegan::adonis(form, data=data)
			# # gc()
			# # beta <- vegan::betadisper(d=dists, group=as.character(data[ , divisionFieldPres]), type='median', add=TRUE)
			# # gc()
			# # betaPerm <- vegan::permutest(beta, pairwise=TRUE)
			# # gc()
			# # betaTukey <- TukeyHSD(beta)
			# # gc()
			
			# # say('=================================================================', pre=2)
			# # print(scheme)
			# # say('-----------------------------------------------------------------')
			# # print(partition)
			# # say('-----------------------------------------------------------------')
			# # print(betaPerm)
			
			# out <- list()
			# out$meta <- list()
			# out$meta$scheme <- scheme
			# out$meta$pme <- pme
			# out$ord <- ord
			# # out$partition <- partition
			# # out$beta <- beta
			# # out$betaPerm <- betaPerm
			# # out$betaTukey <- betaTukey
# out$samps <- samps
			
			# saveRDS(out, paste0(workDir, 'Ordination Analysis/NMDS/Ordination for ', schemeNice, ' Using ', pmeNice, ' PME with ', ifelse(weight, 'Background-Weighted', 'Unweighted'), ' Distances SUBSAMPLE.rds'))
			
			# rm(out, partition, beta, betaPerm, betaTukey, ord, dists); gc()
			
		# } # next PME
		
	# } # next SCHEME
		
#######################
### plot ordination ###
#######################

	sayHead('plot ordination')

	# generalization
	# weight <- TRUE # weight distances between presences by distance between matching background
	weight <- FALSE # no weighting

	# presences
	pres <- getPres()
	
	### plot by scheme/PME
	# par(mfcol=c(2, 3))
	par(mfcol=c(1, 3))

	# by SCHEME
	for (scheme in schemes) {
	
		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		schemeShort <- out$schemeShort
		divisionFieldPres <- out$divisionFieldPres
	
		# by PME
		for (pme in pmes) {
		
			pmeNice <- pmeNiceName(pme)
			pmeNicePrint <- pmeNiceNamePrint(pme)
		
			say(scheme, ' ', pme)
		
			ord <- readRDS(paste0(workDir, 'Ordination Analysis/NMDS/Ordination for ', schemeNice, ' Using ', pmeNice, ' PME with ', ifelse(weight, 'Background-Weighted', 'Unweighted'), ' Distances SUBSAMPLE.rds'))

			# ordination points
			plot(ord$ord$points[ , 1:2], main=paste(schemeShort, 'with', pmeNicePrint, 'Background'), type='n')
			points(ord$ord$points, col=alpha(getUnitCols(pres[ , divisionFieldPres]), 0.3), pch=16, cex=1)

pres <- getPres()
pres <- pres[ord$samps, ]			
			
			# # # cluster diagram
			# # clust <- stats::hclust(dists)
			# # clust <- as.dendrogram(clust)
			
			# # tipIndex <- labels(clust)
			# # tipName <- pres[tipIndex , divisionFieldPres]
			# # tipCols <- getUnitCols(tipName, incAll=FALSE)
			# # dendextend::labels_colors(clust) <- tipCols
			# # clust <- dendextend::color_branches(clust, k=nrow(pres), col=tipCols)

			# # plot(clust, leaflab='none')
			
			# # boot <- cluster::boot.phylo(phy=phylo, x=dist, FUN=upgma, B = 1000, block = 1, trees = FALSE, quiet=TRUE)
		
			# contours of units
			for (thisUnit in getUnits(scheme, incAll=FALSE)) {
			
				pts <- ord$ord$points[pres[ , divisionFieldPres] == thisUnit, 1:2]
				
				# alpha hulls
				pts <- pts[!duplicated(pts), ]
				ah <- alphahull::ashape(pts, alpha=1)
				plot(ah, add=TRUE, wpoints=FALSE, col=getUnitCols(thisUnit), lwd=3.4)
				
			}
		
			# centroids of units
			pch <- 21
			for (thisUnit in getUnits(scheme, incAll=FALSE)) {
			
				pts <- ord$ord$points[pres[ , divisionFieldPres] == thisUnit, ]
				
				# centroid (median of coordinates)
				centroid <- apply(pts, 2, median)
				points(centroid[1], centroid[2], pch=pch, bg=getUnitCols(thisUnit), cex=2.2)
				
				pch <- if (pch == 25) { 21 } else { pch + 1 }
				
			}
		
			legend('topright', inset=0.01, legend=niceUnitName(scheme=scheme, getUnits(scheme, incAll=FALSE), short=TRUE), pch=21:25, cex=1, pt.bg=getUnitCols(getUnits(scheme, incAll=FALSE)), bty='n')
			
			# ### add climate variable contours
			# # climVar <- 'acuteHeat_days'
			# climVar <- 'chronicHeat_deg'
			# # climVar <- 'relHumidMean'
			
			# climData <- pres[ , climVar]
			
	# # climData <- logit(climData)
			
			# ordCoord <- as.data.frame(vegan::scores(ord$ord)[ , 1:2])
			# rm(ord); gc()
			
			# climData <- cbind(climData, ordCoord)
			# names(climData) <- c('clim', 'axis1', 'axis2')
			
			# # climModel <- mgcv::gam(clim ~ s(axis1) + s(axis2) + s(axis1, axis2), data=climData, family=ziP, scale=-1)
			# climModel <- mgcv::gam(clim ~ s(axis1) + s(axis2) + s(axis1, axis2), data=climData, family='gaussian', scale=-1)
			# # climModel <- mgcv::gam(clim ~ s(axis1) + s(axis2) + s(axis1, axis2), data=climData, family='gaussian', scale=-1)

			# bins <- 100

			# axis1 <- seq(min(ordCoord[ , 1]), max(ordCoord[ , 1]), length.out=bins)
			# axis2 <- seq(min(ordCoord[ , 2]), max(ordCoord[ , 2]), length.out=bins)
			
			# grid <- data.frame(
				# axis1=rep(axis1, bins),
				# axis2=rep(axis2, each=bins)
			# )

			# climPred <- predict(climModel, newdata=grid, type='response')
			# climPred <- c(climPred)
	# # climPred <- inv.logit(climPred)
			# climPred <- matrix(climPred, nrow=bins, ncol=bins, byrow=TRUE)
			
			# contour(x=axis1, y=axis2, z=climPred, add=TRUE, nlevels=5)
			# pos <- par('usr')
			# x <- pos[2]
			# y <- pos[3]
			# x <- x - 0.02 * (pos[2] - pos[1])
			# y <- y + 0.02 * (pos[4] - pos[3])
			# text(x, y, labels=fields$nameShortWithUnits[fields$factor %in% climVar], adj=c(1, 0))
			
		} # next PME
	
	} # next SCHEME

########################
### cluster analysis ###
########################
	
	# sayHead('cluster analysis')
	
	
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')