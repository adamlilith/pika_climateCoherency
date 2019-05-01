### Ochotona princeps - Spatially-varying importance of variables
### Adam B. Smith | 2016-11

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/06 Multivariate Niche Overlap.r')

rm(list=ls())

	# do <- c('niche visualize', 'calculate overlap', 'heatmaps')
	# do <- 'niche visualize'
	# do <- 'calculate overlap'
	do <- 'heatmaps'
	# do <- 'map'

	# schemes <- c('cladeNonOverlap', 'ecoregionEpa3Modified')
	# schemes <- 'cladeNonOverlap'
	schemes <- 'ecoregionEpa3Modified'
	
	# pmes <- c('pmeNone', 'pmeMin')
	pmes <- 'pmeNone'
	# pmes <- 'pmeMin'
	
	# valances <- c('including', 'excluding')
	valances <- 'including'
	# valances <- 'excluding'

	numBins <- 101 # number of numBins in which to calculate KDE for visualization
	pcs <- c(1, 2) # PC axes to use
	
	# testStat <- 'schoener'
	# testStat <- 'warren'
	# testStat <- 'esp'
	# testStat <- 'correl'
	# testStat <- 'rankCorrel'
	
### CONTENTS ###
### PCA niche visualization ###
### Broennimann niche overlap ###
### visualize Broennimann niche overlap - heatmaps ###
### visualize Broennimann niche overlap - maps ###

source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/!Omnibus Variables for Pika Non-Stationarity Analysis.r')

###############################
### PCA niche visualization ###
###############################

if (do %in% 'niche visualize') {

	sayHead('PCA niche visualization')

	pcProper <- paste0('PC', prefix(pc, 2))

	arrowScale <- 13 # multiplier for arrow length
	arrowLwd <- 0.4
	arrowLabCol <- 'blue'

	legendLwd <- 0.4

	limQuants <- c(0.0001, 0.9999) # quantiles of all background to which to limit x and y axes

	tck <- -0.05 # tick mark length/direction

	# colAllPres <- 'orange' 
	colAllBg <- 'orange'
	colAllPres <- 'darkturquoise'
	colUnitBg <- 'red'
	colUnitPres <- 'blue'

	quants <- c(0.9, 0.925, 0.95, 0.975, 0.99, 0.995, 0.999) # quantiles at which to draw contour lines
	# contour line widths
	lwds <- seq(0.3, 0.6, length.out=length(quants))
	# lwds <- rep(0.4, length(quants))

	arrowLabCex <- 0.4 # labels size for arrow

	dirCreate(workDir, 'Analysis - Non-stationarity/Niche Overlap - Multivariate')

	pres <- getPres()

	# by SCHEME
	for (scheme in schemes) {
	
		# graphical layout
		if (scheme == 'cladeNonOverlap') {
	
			# figure layout and dimensions
			layout <- matrix(1:8, byrow=FALSE, nrow=2)

			width <- 1800
			height <- 1000

			skipPlots <- 0 # number of plots to skip after displaying "all" clade
			
			mar <- 0.1 * c(5, 8, 4, 2) + 0.1
			mgp <- c(0.5, 0.15, 0)
			
			cexMain <- 0.5 # title of a panel
			lineMain <- 0.22
			
			labOffset <- 0.2
			
			cexLab <- 0.4
			cexAxis <- 0.4
			
			legendCex <- 0.35

		} else if (scheme == 'ecoregionEpa3Modified') {
		
			# figure layout and dimensions
			layout <- matrix(1:16, byrow=FALSE, nrow=4)

			width <- 1800
			height <- 1800

			skipPlots <- 2 # number of plots to skip after displaying "all" clade
			
			mar <- 0.1 * c(5, 8, 4, 2) + 0.1
			mgp <- c(0.5, 0.15, 0)
			
			cexMain <- 0.4 # title of a panel
			lineMain <- 0.2
			
			labOffset <- 0.17
			
			cexLab <- 0.4
			cexAxis <- 0.4
			
			legendCex <- 0.35
		
		}

		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# load PCA
		pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))

		# by PME
		for (pmeVariant in c('pmeNone')) {
		
			pmeNice <- pmeNiceName(pmeVariant)

			# by VALANCE
			for (unitValance in c('including')) {

				# get names of focal units to include/exclude
				units <- getUnits(scheme=scheme, incAll=TRUE)
			
				say('UNITS: ', paste(units, collapse=' | '), pre=1)

				png(paste0(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap/Niche Overlap in PC Space - ', schemeNice, ' - ', toupper(unitValance), ' - ', pmeNice, ' PME.png'), width=width, height=height, res=600)
					
					par(layout(layout), mar=mar, pty='s', cex.main=cexMain, mgp=mgp, cex.lab=cexLab, cex.axis=cexAxis, lwd=1)
					
					# load BG data
					load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/INCLUDING ALL - No PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata'))
					
					allPres <- speciesData$allPresences[ , pcProper]
					allBg <- speciesData$allTrainingAbsences[ , pcProper]

					# get range of all data
					xRange <- range(allBg[ , pc[1]], allPres[ , pc[1]])
					yRange <- range(allBg[ , pc[2]], allPres[ , pc[2]])
					
					# calculate KDEs (to use in plots)
					kdeAllBg <- kde2d(allBg[ , pc[1]], allBg[ , pc[2]], n=numBins, lims=c(xRange, yRange))
					kdeAllPres <- kde2d(allPres[ , pc[1]], allPres[ , pc[2]], n=numBins, lims=c(xRange, yRange))
					
					## biplot
					
					# get % explained by chosen axes
					explain <- pca$sdev^2 / sum(pca$sdev^2)

					pcPercA <- explain[pc[1]]
					pcPercB <- explain[pc[2]]
					
					pcPercA <- paste0('PC ', pc[1], ' (', suffix(round(100 * pcPercA, 1), len=4, pad='0'), '%)')
					pcPercB <- paste0('PC ', pc[2], ' (', suffix(round(100 * pcPercB, 1), len=4, pad='0'), '%)')

					xlim <- quantile(allBg[ , pc[1]], limQuants)
					ylim <- quantile(allBg[ , pc[2]], limQuants)
				
					smoothScatter(allBg, xlab='', ylab='', colramp=colorRampPalette(c('white', colAllBg)), nrpoints=0, tck=tck, labels=FALSE, xlim=xlim, ylim=ylim)
					title(main='PCA', line=lineMain, xpd=NA)

					contour(kdeAllBg$x, kdeAllBg$y, kdeAllBg$z, lwd=lwds, levels=quantile(kdeAllBg$z, quants), drawlabels=FALSE, add=TRUE, col=colAllBg)

					text(mean(xlim), y=ylim[1] - labOffset * diff(ylim), labels=pcPercA, cex=cexLab, xpd=NA)
					text(x=xlim[1] - labOffset * diff(xlim), y=mean(ylim), labels=pcPercB, cex=cexLab, xpd=NA, srt=90)
					
					legend('bottomleft', inset=0.01, bty='n', legend=c('all bg'), col=colAllBg, lwd=legendLwd, cex=legendCex)

					# arrows: show arrows corresponding to variables with >x% of correlation with each axis
					loadings <- pca$loadings
					loadings <- as.matrix(loadings[1:nrow(loadings), 1:ncol(loadings)])
					upperQuant <- quantile(abs(loadings), 0.8)
					loadings <- as.data.frame(loadings)
					names(loadings) <- paste0('PC', prefix(1:ncol(loadings), 2))
					
					# by PC
					for (countPc in pc) {
						
						thisLoad <- loadings[ , pc[countPc]]
						loadHigh <- abs(thisLoad) >= upperQuant
						
						if (any(loadHigh)) {
						
							thisX <- loadings[loadHigh, pc[1]]
							thisY <- loadings[loadHigh, pc[2]]
							vars <- fields$veryShortName[match(rownames(loadings[loadHigh, ]), fields$factor)]
						
							arrows(x0=0, y0=0, x1=thisX * arrowScale, y1=thisY * arrowScale, length=0.05, angle=15, col='red', lwd=arrowLwd, xpd=NA)
							text(thisX * arrowScale, thisY * arrowScale, labels=vars, xpd=NA, cex=arrowLabCex, col=arrowLabCol)
						
						}
						
					}
					
					# for each unit of analysis plot background vs background for entire species and presences
					for (thisUnit in units) {

						# plot ALL unit's presences
						if (thisUnit == 'all') {
						
							# plot
							smoothScatter(allBg, xlab='', ylab='', colramp=colorRampPalette(c('white', colAllBg)), nrpoints=0, tck=tck, labels=FALSE, xlim=xlim, ylim=ylim)

							title(main=niceUnitName(scheme, thisUnit), cex=cexMain, line=lineMain, xpd=NA)

							text(mean(xlim), y=ylim[1] - labOffset * diff(ylim), labels=pcPercA, cex=cexLab, xpd=NA)
							text(x=xlim[1] - labOffset * diff(xlim), y=mean(ylim), labels=pcPercB, cex=cexLab, xpd=NA, srt=90)
							
							contour(kdeAllBg$x, kdeAllBg$y, kdeAllBg$z, lwd=lwds, levels=quantile(kdeAllBg$z, quants), drawlabels=FALSE, add=TRUE, col=colAllBg)
							contour(kdeAllPres$x, kdeAllPres$y, kdeAllPres$z, lwd=lwds, levels=quantile(kdeAllPres$z, quants), drawlabels=FALSE, add=TRUE, col=colAllPres)
							legend('bottomleft', inset=0.01, bty='n', legend=c('sp bg', 'sp pres'), col=c(colAllBg, colAllPres), lwd=legendLwd, cex=legendCex)
							
							if (skipPlots > 0) for (i in 1:skipPlots) frame()
							
						# plot non-ALL unit's presences and background
						} else {
						
							# load unit's data
							load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(unitValance), ' ', toupper(thisUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata'))

							unitPres <- speciesData$allPresences[ , pcProper]
							unitBg <- speciesData$allTrainingAbsences[ , pcProper]

							# KDEs
							kdeUnitBg <- kde2d(unitBg[ , pc[1]], unitBg[ , pc[2]], n=numBins, lims=c(xRange, yRange))
							kdeUnitPres <- kde2d(unitPres[ , pc[1]], unitPres[ , pc[2]], n=numBins, lims=c(xRange, yRange))

							# plot
							smoothScatter(allBg, colramp=colorRampPalette(c('white', colAllBg)), nrpoints=0, xlab='', ylab='', tck=tck, xpd=NA, labels=FALSE, xlim=xlim, ylim=ylim)
							
							text(mean(xlim), y=ylim[1] - labOffset * diff(ylim), labels=pcPercA, cex=cexLab, xpd=NA)
							text(x=xlim[1] - labOffset * diff(xlim), y=mean(ylim), labels=pcPercB, cex=cexLab, xpd=NA, srt=90)
							
							title(main=niceUnitName(scheme, thisUnit), cex=cexMain, line=lineMain, xpd=NA)
							
							contour(kdeAllBg$x, kdeAllBg$y, kdeAllBg$z, lwd=lwds, levels=quantile(kdeAllBg$z, quants), drawlabels=FALSE, add=TRUE, col=colAllBg)
							contour(kdeAllPres$x, kdeAllPres$y, kdeAllPres$z, lwd=lwds, levels=quantile(kdeAllPres$z, quants), drawlabels=FALSE, add=TRUE, col=colAllPres)
							contour(kdeUnitBg$x, kdeUnitBg$y, kdeUnitBg$z, levels=quantile(kdeUnitBg$z, quants), col=colUnitBg, drawlabels=FALSE, lwd=lwds, add=TRUE)
							contour(kdeUnitPres$x, kdeUnitPres$y, kdeUnitPres$z, levels=quantile(kdeUnitPres$z, quants), col=colUnitPres, drawlabels=FALSE, lwd=lwds, add=TRUE)
							
							legend('bottomleft', inset=0.01, bty='n', legend=c('sp bg', 'sp pres', 'unit bg', 'unit pres'), col=c(colAllBg, colAllPres, colUnitBg, colUnitPres), lwd=legendLwd, cex=legendCex)
							
						}
					
					} # next fromUnit
					
				dev.off()
					
			} # next valance

		} # next PME
		
	} # next division scheme
	
}

#################################
### Broennimann niche overlap ###
#################################

if (do %in% 'calculate overlap') {

	sayHead('Broennimann niche overlap')
	
	say('Using PCs', paste(pcs, collapse=' & '), ' with ', numBins, ' numBins!!!')
	
	dirCreate(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap')

	# smoothOcc <- function(predPres, predBg) {

		# # Calculate smoothed occupancy matrix (ie, species presence proportional to background environment; z in Broennimann et al. 2011)
		# # predPres		Predictions to presences (in PCA space)
		# # predBg		Predictions to background (in PCA space)
	
		# # for each set of points
		# for (sites in c('predBg', 'predPres')) {
		
			# x <- get(sites)
		
			# ### tally number of sites in each environmental bin
			# envDensity <- matrix(0, nrow=length(binAxis1Left), ncol=length(binAxis2Left))

			# for (i in 1:nrow(envDensity)) {
				# for (j in 1:ncol(envDensity)) {
					# envDensity[i, j] <- sum(
						# x[ , 1] >= binAxis1Left[nrow(envDensity) - i + 1] & x[ , 1] < binAxis1Right[nrow(envDensity) - i + 1] &
						# x[ , 2] >= binAxis2Left[j] & x[ , 2] < binAxis2Right[j]
					# )
				# }
			# }
			
			# ## smooth then rescale to number of presences
			# kde <- kde2d(x[ , 1], x[ , 2], n=numBins, lims=c(binAxis1Left[1], last(binAxis1Right), binAxis2Left[1], last(binAxis2Right)))$z
			# kde <- sum(envDensity) * (kde / sum(kde))
			# kde[which(kde < 0.5)] <- NA # remove cells with estimated < half a presence
			# kde <- kde / max(envDensity) # Broenniman Eqs 1 and 2
			
			# if (sites == 'predPres') {
				# kdePres <- kde
			# } else {
				# kdeBg <- kde
			# }
			
		# }	
		
		# ### calculate occupancy in env space
		# kdePres[which(kdeBg < eps)] <- 0
		# kdeBg[which(kdeBg < eps)] <- 0
		# o <- kdePres / kdeBg

		# ## z in Broennimann (eq 3a... but note that equation is wrong, so dividing by SUM of values in numerator!)
		# z <- o / max(numer, na.rm=TRUE) # Broennimann Eq 3a
		# z
		
	# }
	
	pca <- readRDS(paste0(workDir, '/Background Sites/Random - Western USA/BG Sites 04 Set 01 Selected from IUCN Range Map + 800-km Buffer - 10000 Sites - PCA for 2015-10-07.rds'))
	
	uberBg <- as.data.frame(pca$scores[ , pcs])
	names(uberBg) <- paste0('PC', prefix(pcs, 2))
	if (length(naRows(uberBg)) > 0) uberBg <- uberBg[-naRows(uberBg), ]
	
	### create environmental grid
	bins <- seq(min(uberBg[ , pcs[1]]), max(uberBg[ , pcs[1]]), length.out=numBins)
	binAxis1Left <- bins[1:(numBins - 1)]
	binAxis1Right <- bins[2:numBins]
	binAxis1Left[length(binAxis1Left)] <- binAxis1Left[length(binAxis1Left)] - eps
	binAxis1Right[length(binAxis1Right)] <- binAxis1Right[length(binAxis1Right)] + eps

	bins <- seq(min(uberBg[ , pcs[2]]), max(uberBg[ , pcs[2]]), length.out=numBins)
	binAxis2Left <- bins[1:(numBins - 1)]
	binAxis2Right <- bins[2:numBins]
	binAxis2Left[length(binAxis2Left)] <- binAxis2Left[length(binAxis2Left)] - eps
	binAxis2Right[length(binAxis2Right)] <- binAxis2Right[length(binAxis2Right)] + eps

	# by SCHEME
	for (scheme in schemes) {
	
		out <- schemeInfo(scheme)
		schemeNice <- out$schemeNice
		divisionFieldPres <- out$divisionFieldPres
		rm(out); gc()

		# by PME
		for (pmeVariant in pmes) {
		
			pmeNice <- pmeNiceName(pmeVariant)
			
			# by VALANCE
			for (fromValance in valances) {

				# get names of focal units to include/exclude
				units <- getUnits(scheme=scheme, incAll=FALSE)
			
				say('UNITS: ', paste(units, collapse=' | '), pre=1)

				# to store similarity coefficients (Schoener's D, Godsoe's ESP, correlation, and rank correlation)
				schoener <- as.data.frame(matrix(NA, nrow=length(units), ncol=length(units)))
				names(schoener) <- rownames(schoener) <- units
				correl <- rankCorrel <- esp <- warrenEqualP <- warrenSimP <- warren <- schoenerEqualP  <- schoenerSimP <- schoener
				
				# by "from" UNIT
				for (fromUnit in units) {
				
					# load from unit data
					load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(fromValance), ' ', toupper(fromUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata'))
						
					presFrom <- speciesData$allPresences[ , paste0('PC', prefix(pcs, 2))]
					bgFrom <- speciesData$allTrainingAbsences[ , paste0('PC', prefix(pcs, 2))]

					occFrom <- grid.clim(glob=uberBg, glob1=bgFrom, sp=presFrom, R=numBins)

					for (toValance in c('including')) {
							
						toUnits <- getUnits(scheme, incAll=FALSE)
						toUnits <- toUnits[-which(toUnits %in% fromUnit)]
						
						# by "to" UNIT
						for (toUnit in toUnits) {
						
							say('SCHEME ', schemeNice, ' | PME ', pmeNice, ' | FROM VALANCE ', fromValance, ' | FROM UNIT ', fromUnit, ' | TO VALANCE ', toValance, ' | TO UNIT ', toUnit, breaks=NULL)		
						
							# load to unit data
							load(paste0(workDir, 'ENMs - PCs/', schemeNice, '/', toupper(toValance), ' ', toupper(toUnit), ' - ', pmeNice, ' PME/Ochotona princeps/!Species Data - Ochotona princeps - PCA.Rdata'))

							presTo <- speciesData$allPresences[ , paste0('PC', prefix(pcs, 2))]
							bgTo <- speciesData$allTrainingAbsences[ , paste0('PC', prefix(pcs, 2))]

							occTo <- grid.clim(glob=uberBg, glob1=bgTo, sp=presTo, R=numBins)
							broennimann <- niche.overlap(z1=occFrom, z2=occTo, cor=TRUE)
							
							similarity <- niche.similarity.test(z1=occFrom, z2=occTo, rep=100)
							equality <- niche.equivalency.test(z1=occFrom, z2=occTo, rep=100)
							
							x <- c(occFrom$Z)
							y <- c(occTo$Z)
							
							xx <- x / max(x, y)
							yy <- y / max(x, y)
							
							correl[rownames(correl) == fromUnit, toUnit] <- cor(logitAdj(x), logitAdj(y), use='complete.obs')
							rankCorrel[rownames(correl) == fromUnit, toUnit] <- cor(logitAdj(x), logitAdj(y), method='spearman', use='complete.obs')

							schoener[rownames(schoener) == fromUnit, toUnit] <- broennimann$D
							schoenerSimP[rownames(schoener) == fromUnit, toUnit] <- similarity$p.D
							schoenerEqualP[rownames(schoener) == fromUnit, toUnit] <- equality$p.D

							warren[rownames(schoener) == fromUnit, toUnit] <- broennimann$I
							warrenSimP[rownames(schoener) == fromUnit, toUnit] <- similarity$p.I
							warrenEqualP[rownames(schoener) == fromUnit, toUnit] <- equality$p.I
							
							esp[rownames(esp) == fromUnit, toUnit] <- 2 * sum(xx * yy, na.rm=TRUE) / sum(xx + yy, na.rm=TRUE)

						} # next "to" unit
							
					} # next "to" valance
					
				} # next "from" unit
				
				overlap <- list()
				overlap$scheme <- scheme
				overlap$PME <- pmeVariant
				overlap$date <- date()
				overlap$numBins <- numBins
				overlap$pcs <- pcs
				overlap$correl <- correl
				overlap$rankCorrel <- rankCorrel
				overlap$schoener <- schoener
				overlap$schoenerSimP <- schoenerSimP
				overlap$schoenerEqualP <- schoenerEqualP
				overlap$warren <- warren
				overlap$warrenSimP <- warrenSimP
				overlap$warrenEqualP <- warrenEqualP
				overlap$esp <- esp
				
				saveRDS(overlap, paste0(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap/Broennimann Niche Overlap - ', toupper(schemeNice), ' - ', toupper(pmeNice), ' PME - FROM ', toupper(fromValance), ' - on ', paste(paste0('PC', prefix(pcs, 2)), collapse=' & '), ' with ', numBins, ' Bins.rds'))
				
			} # next "from" valance

		} # next PME
		
	} # next division scheme

}
	
######################################################
### visualize Broennimann niche overlap - heatmaps ###
######################################################

if (do %in% 'heatmaps') {

	sayHead('visualize Broennimann niche overlap - heatmaps')

	say('Using PCs', paste(pcs, collapse=' & '), ' with ', numBins, ' numBins!!!')

	for (scheme in schemes) {

		# graphical layout
		if (scheme == 'cladeNonOverlap') {
	
			cexRow <- cexCol <- 0.6
			cexMain <- 0.5
			margins <- c(3, 3)

		} else if (scheme == 'ecoregionEpa3Modified') {
		
			cexRow <- cexCol <- 0.6
			cexMain <- 0.5
			margins <- c(5, 5)
		
		}
		

		# get scheme info and division polygon
		schemeNice <- schemeInfo(scheme)$schemeNice

		for (pmeVariant in pmes) {
		
			pmeNice <- pmeNiceName(pmeVariant)
			
			# by "from" VALANCE
			for (fromValance in valances) {

				# by "to" VALANCE
				for (toValance in c('including')) {

					say(date(), ' SCHEME ', schemeNice, ' | PME ', pmeNice, ' | FROM VALANCE ', fromValance, ' | TO VALANCE ', toValance)

					### get and process evaluation data
					overlap <- readRDS(paste0(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap/Broennimann Niche Overlap - ', toupper(schemeNice), ' - ', toupper(pmeNice), ' PME - FROM ', toupper(fromValance), ' - on ', paste(paste0('PC', prefix(pcs, 2)), collapse=' & '), ' with ', numBins, ' Bins.rds'))

					pngFiles <- character()
					
					# for each unit of analysis
					for (testStat in c('schoener', 'warren', 'esp', 'correl', 'rankCorrel')) {

						pngFile <- paste0(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap/Broennimann Niche Overlap - ', toupper(schemeNice), ' - ', toupper(pmeNice), ' PME - FROM ', toupper(fromValance), ' - on ', paste(paste0('PC', prefix(pcs, 2)), collapse=' & '), ' with ', numBins, ' Bins - ', toupper(testStat), '.png')

						pngFiles <- c(pngFiles, pngFile)
						
						png(pngFile, width=1400, height=1400, res=600)
				
							testStatNice <- if (testStat == 'schoener') {
								'Schoener\'s D'
							} else if (testStat == 'warren') {
								'Warren\'s I'
							} else if (testStat == 'esp') {
								'Godsoe\'s ESP'
							} else if (testStat == 'correl') {
								'Correlation'
							} else if (testStat == 'rankCorrel') {
								'Rank Correlation'
							}
							
							thisOver <- overlap[[which(names(overlap) %in% testStat)]]
							names(thisOver) <- rownames(thisOver) <- niceUnitNameShort(scheme, names(thisOver))
							thisOver <- as.matrix(thisOver)
							thisOver <- 1 - thisOver
							
							par(cex.main=cexMain)
							heatmap(thisOver, scale='none', main=paste0(schemeNice, ' - ', pmeNice, ' PME - ', testStatNice), cexRow=cexRow, cexCol=cexCol, margins=margins)
							
						dev.off()
						
					} # next test stat
				
					### combine images into one file
					plots <- lapply(ll <- pngFiles, function(x){
					  img <- as.raster(readPNG(x))
					  rasterGrob(img, interpolate = FALSE)
					})

					ggsave(paste0(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap/!Broennimann Niche Overlap - ', toupper(schemeNice), ' - ', toupper(pmeNice), ' PME - FROM ', toupper(fromValance), ' - on ', paste(paste0('PC', prefix(pcs, 2)), collapse=' & '), ' with ', numBins, ' Bins.png'), device='png', dpi=600,
						marrangeGrob(
							grobs=plots,
							nrow=3, ncol=2,
							widths=rep(500, 2),
							heights=rep(500, 3),
							padding=grid::unit(0, 'line'),
							top=paste0('Broennimann Niche Overlap - ', toupper(schemeNice), ' - ', toupper(pmeNice), ' PME - FROM ', toupper(fromValance), '\n', paste(paste0('PC', prefix(pcs, 2)), collapse=' & '), ' with ', numBins, ' Bins'))
					)
					
				} # next toValance
				
			} # next fromValance

		} # next PME
		
	} # next division scheme
	
}

##################################################
### visualize Broennimann niche overlap - maps ###
##################################################

if ('map' %in% do) {

	sayHead('visualize Broennimann niche overlap - maps')

	say('For each division scheme, create maps showing niche overlap.')
	say('Using ', testStat, '!!!')
			
	testStatNice <- if (testStat == 'schoener') {
		'Schoener\'s D'
	} else if (testStat == 'warren') {
		'Warren\'s I'
	} else if (testStat == 'esp') {
		'Godsoe\'s ESP'
	} else if (testStat == 'correl') {
		'Correlation'
	} else if (testStat == 'rankCorrel') {
		'Rank Correlation'
	}
							
	pres <- getPres()
	
	gadm <- readOGR('C:/ecology/Political Geography/GADM/ver2pt8/WGS84', 'USA_adm1', verbose=FALSE)
	gadm <- sp::spTransform(gadm, CRS(crsClimateNA))
	
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
			cexMain <- 0.9 # title of a panel
			lineMain <- -0.5
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 0 # rotation of title for entire plot
			
			latOffset <- 170000 # distance by which to "raise" performance statistic text on map
			
			fadeFill <- 0.3 # transparancy
			fadeBorder <- 1 # transparancy
			
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
		
			statCexOther <- 0.5 # model performance statistic (vs another unit)
			statCexSelf <- 0.8 # model performance statistic (vs self)
			
			unitLabelCex <- 0.3 # unit label size
			cexMain <- 0.4 # title of a panel
			lineMain <- -0.3
			cexBigMain <- 0.7 # title of entire plot
			srtBigMain <- 90 # rotation of title for entire plot
			
			latOffset <- 170000 # distance by which to "raise" performance statistic text on map
			
			fadeFill <- 0.7 # transparancy
			fadeBorder <- 1 # transparancy
			
			lwd <- 0.5 # line width
		
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
		divisionPoly <- sp::spTransform(divisionPoly, CRS(crsClimateNA))
		
		centroids <- gCentroid(divisionPoly, byid=TRUE)

		# by PME
		for (pmeVariant in pmes) {
		
			pmeNice <- pmeNiceName(pmeVariant)

			# by "from" VALANCE
			for (fromValance in valances) {

				### get and process evaluation data
				statFrame <- readRDS(paste0(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap/Broennimann Niche Overlap - ', toupper(schemeNice), ' - ', toupper(pmeNice), ' PME - FROM ', toupper(fromValance), ' - on ', paste(paste0('PC', prefix(pcs, 2)), collapse=' & '), ' with ', numBins, ' Bins.rds'))

				statFrame <- as.matrix(statFrame[testStat][[1]])
				
				# by "to" VALANCE
				for (toValance in valances) {

					# get names of focal units to include/exclude
					fromUnits <- getUnits(scheme=scheme, incAll=FALSE)
		
					say(date(), ' SCHEME ', schemeNice, ' | PME ', pmeNice, ' | FROM VALANCE ', fromValance, ' | TO VALANCE ', toValance)
				
					pngFile <- paste0(workDir, 'Analysis - Non-stationarity/Broennimann Multivariate Niche Overlap/!Broennimann Niche Overlap - ', schemeNice, ' - ', pmeNice, ' PME - ', valanceShort(fromValance), ' vs ', valanceShort(toValance), ' - ', testStatNice, '.png')
					png(pngFile, width=width, height=height, res=600)

						par(layout(layout), mar=0.1 * c(5, 4, 4, 2) + 0)
				
						plot(0, 0, col='white', fg='white', col.main='white', col.lab='white', col.axis='white')
						text(0, 0, labels=paste0('Niche Overlap - ', schemeNice, ' - ', pmeNice, ' PME - ', valanceShort(fromValance), ' vs ', valanceShort(toValance), '\n', testStatNice), srt=srtBigMain, cex=cexBigMain, xpd=NA)
						title(sub=date(), cex=0.2, outer=TRUE)

						fromUnits <- getUnits(scheme, incAll=FALSE)
						cols <- getUnitCols(fromUnits)
				
						# for each unit of analysis
						for (fromUnit in fromUnits) {
		
							### make base map
							
							# plot geography
							plot(divisionPoly, lwd=lwd)
							plot(gadm, add=TRUE, lwd=lwd / 3, border='gray30')
							plot(divisionPoly, add=TRUE, col=alpha(cols, fadeFill), border=alpha(cols, fadeBorder), lwd=lwd)
							plot(divisionPoly[which(divisionPolyDf[ , divisionFieldPoly] %in% fromUnit), ], col=getUnitCols(fromUnit), border='black', lwd=lwd, add=TRUE)
							title(main=niceUnitName(scheme, fromUnit), line=lineMain, xpd=NA, cex.main=cexMain)

							# label units
							if (unitLabels) {
								x <- coordinates(centroids)[ , 1]
								y <- coordinates(centroids)[ , 2] + latOffset
								text(x, y, labels=niceUnitName(scheme, divisionPolyDf[ , divisionFieldPoly]), cex=unitLabelCex, xpd=NA)
							}
							
							### label each unit with performance statistic from model trained on "from" unit
							toUnits <- getUnits(scheme=scheme, incAll=FALSE)
							
							for (countToUnit in seq_along(toUnits)) {
							
								toUnit <- toUnits[countToUnit]
							
								if (fromUnit != toUnit) {
								
									toCent <- centroids[divisionPolyNames == toUnit, ]
									x1 <- coordinates(toCent)[1]
									y1 <- coordinates(toCent)[2]

									vsOther <- statFrame[rownames(statFrame) %in% fromUnit, colnames(statFrame) %in% toUnit]
							
									vsOther <- round(vsOther, 2)
									vsOther <- format(c(vsOther, 0.123456789), digits=2)[1]
									text(toCent, labels=vsOther, cex=statCexOther, xpd=NA)
									
								}
							
							} # next "to" unit
							
						} # next fromUnit
						
					dev.off()
					
				} # next toValance
				
			} # next fromValance

		} # next PME
		
	} # next division scheme
	
}


say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', pre=1)
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
say('DONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')

