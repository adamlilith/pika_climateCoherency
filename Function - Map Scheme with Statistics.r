mapScheme <- function(
	filename,
	stats,
	aggBy=NULL,
	scheme='cladeNonOverlap',
	pme='pmeNone',
	show='byUnit',
	stat1='cbi',
	stat2=NULL,
	fromValance='including',
	toValance='including',
	vsSelf=TRUE,
	incAll=TRUE,
	...
) {

# mapScheme		This function generically creates a series of maps of a single scheme displaying a user-defined test statistic.  The outout is a file on disk. It either shows:
# 	multiple maps where each one is particular a given unit, with that unit highighted and values reflecting, for example, performance of an ENM trained on that unit and tested against others
#   a single map with values displayed on each unit pertaining to, say, ENM performamce of that unit against the same unit

#	filename	Path and file name (with extension)
#	stats		Data frame with evaluation statistics
#	aggBy		NULL (ignore) or list object by which stats needs to be aggregated
#	scheme		scheme name
# 	pme			PME variant (pmeMin, pmeNone)
#	show		'byUnit' ==> one map per unit (maybe including "composite" unit) showing stat1
#				'one' ==> one map showing stat1
#				'two' ==> two maps, first showing stat1 and second stat2
#	stat1		name of statistic to plot (should match a column in stats)
#	stat2		name of statistic to plot (should match a column in stats); ignored for all by show='two'
#	fromValance, toValance	'including', or 'excluding'
#	vsSelf		TRUE ==> display value of stat1 or stat2 on focal unit
#	incAll		TRUE ==> include "all" unit

# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/Function - Map Scheme with Statistics.r')

genre <- 'performance'

	stat1 <- 'cbi'
	# stat1 <- 'cbiPresBgWeight'
	# stat1 <- 'sensTholdMaxSumSS'
	# stat1 <- 'specTholdMaxSumSS'
	# stat1 <- 'sensTholdMinDiffSS'
	# stat1 <- 'specTholdMinDiffSS'

	# stat1 <- 'tholdMaxSumSS'

# genre <- 'overlap'

	# stat1 <- 'schoenerDBg'
	# stat1 <- 'schoenerDPres'
	# stat1 <- 'schoenerDPresWeighted'
	
	# stat1 <- 'rankCorBg'
	# stat1 <- 'rankCorPres'
	# stat1 <- 'rankCorPresWeighted'
	
	# stat1 <- 'corBg'
	# stat1 <- 'corPres'
	# stat1 <- 'corPresWeighted'

say('For each division scheme, create map showing ENM performance OR ENM-based niche overlap against self/other units.')
say('Using ', toupper(algo), ' with ', stat1, '!!!')

stat1Nice <- if (stat1 == 'cbi') {
	'CBI'
} else if (stat1 == 'cbiPresWeight') {
	'CBI (Presence-Weighted)'
} else if (stat1 == 'cbiPresBgWeight') {
	'CBI (Presence- & Absence-Weighted)'

} else if (stat1 == 'tholdMaxSumSS') {
	'Threshold at Maximum Sens + Spec'

} else if (stat1 == 'sensTholdMaxSumSS') {
	'True Positive Rate (Threshold at Max S+S)'
} else if (stat1 == 'specTholdMaxSumSS') {
	'True Negative Rate (Threshold at Max S+S)'

} else if (stat1 == 'sensTholdMinDiffSS') {
	'True Positive Rate (Threshold at Min |S-S|)'
} else if (stat1 == 'specTholdMinDiffSS') {
	'True Negative Rate (Threshold at Min |S-S|)'

} else if (stat1 == 'schoenerDBg') {
	'Schoener\'s D for Background'
} else if (stat1 == 'schoenerDPres') {
	'Schoener\'s D for Presences'
} else if (stat1 == 'schoenerDPresWeighted') {
	'Schoener\'s D for Presences (Weighted)'

} else if (stat1 == 'rankCorBg') {
	'Rank Correlation for Background'
} else if (stat1 == 'rankCorPres') {
	'Rank Correlation for Presences'
} else if (stat1 == 'rankCorPresWeighted') {
	'Rank Correlation for Presences (Weighted)'

} else if (stat1 == 'corBg') {
	'Correlation for Background'
} else if (stat1 == 'corPres') {
	'Correlation for Presences'
} else if (stat1 == 'corPresWeighted') {
	'Correlation for Presences (Weighted)'
}

genreNice <- if (genre == 'performance') {
	'ENM Performance'
} else if (genre == 'overlap') {
	'Niche Overlap'
}


# site for ALL unit on map
allCoords <- cbind(-880018.7, 6640130)
		
# estPerformSig <- function(vsSelf, vsOther, n=1000) {

	# ### estimates significance of performance of model tested against training unit (vsSelf) and another unit (vsOther) using permutation test
	# # vsSelf, vsOther	performance stats for each k-fold of within- and cross-unit evaluation
	# # n					number of null iterations

	# # calculate observed difference between cases
	# obsDiff <- numeric()
	# for (i in seq_along(vsSelf)) obsDiff <- c(obsDiff, mean(vsSelf[i] - vsOther, na.rm=TRUE))
	
	# # calculate null difference between cases, permuting test scores between vsSelf and vsOther
	# nullDiff <- numeric()
	
	# pooled <- c(vsSelf, vsOther)
	
	# # for each iteration
	# for (i in 1:n) {
	
		# # sample from test statistic vs self and vs other giving each (self/other) equal total weight of being drawn
		# pooled <- sample(pooled, length(pooled), prob=c(rep(1, length(vsSelf)), rep(1, length(vsOther)) / length(vsSelf)))
		# nullP1 <- pooled[1:length(vsSelf)]
		# nullP2 <- pooled[(length(vsSelf) + 1):(length(vsSelf) + length(vsSelf))]
		
		# for (i in seq_along(nullP1)) nullDiff <- c(nullDiff, mean(nullP1 - nullP2, na.rm=TRUE))
	
	# }

	# obsQuantiles <- quantile(obsDiff, c(0.025, 0.975))
	# nullQuantiles <- quantile(nullDiff, c(0.025, 0.975))

	# # is the difference significant?
	# sig <- nullQuantiles[1] > obsQuantiles[2] | nullQuantiles[2] < obsQuantiles[1]
	# sig
	
# }

# make polygon representing ALL clade
allPoly <- SpatialPoints(cbind(allCoords), CRS(crsClimateNA))
allPoly <- gBuffer(allPoly, width=150000)

pres <- getPres()

gadm <- readOGR('C:/ecology/Political Geography/GADM/ver2pt8/WGS84', 'USA_adm1', verbose=FALSE)
gadm <- sp::spTransform(gadm, CRS(crsClimateNA))

# graphical layout
if (scheme == 'cladeNonOverlap' & show='byUnit') {

	# figure layout and dimensions
	ea <- 4
	layout <- matrix(
		c(
			rep(c(1, rep(2:4, each=ea)), ea),
			rep(c(1, rep(5:7, each=ea)), ea)
		),
		byrow=FALSE, ncol=2 * ea
	)
	
	0.1 * c(5, 4, 4, 2) + 0

	width <- 1700
	height <- 2200

	unitLabels <- TRUE # labels units with name
	
	statCexOther <- 0.7 # model performance statistic (vs another unit)
	statCexSelf <- 1.2 # model performance statistic (vs self)
	
	unitLabelCex <- 0.5 # unit label size
	cexMain <- 0.9 # title of a panel
	lineMain <- -0.5
	cexBigMain <- 0.7 # title of entire plot
	
	latOffset <- 170000 # distance by which to "raise" performance statistic text on map
	
	fadeFill <- 0.3 # transparancy
	fadeBorder <- 1 # transparancy
	
	lwd <- 0.5 # line width

} else if (scheme == 'ecoregionEpa3Modified' & show=='byUnit') {

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

	mar <- 0.1 * c(5, 4, 4, 2) + 0
	
	width <- 2600
	height <- 1700

	unitLabels <- FALSE # labels units with name

	statCexOther <- 0.5 # model performance statistic (vs another unit)
	statCexSelf <- 0.8 # model performance statistic (vs self)
	
	unitLabelCex <- 0.3 # unit label size
	cexMain <- 0.4 # title of a panel
	lineMain <- -0.3
	cexBigMain <- 0.7 # title of entire plot
	
	latOffset <- 170000 # distance by which to "raise" performance statistic text on map
	
	fadeFill <- 0.7 # transparancy
	fadeBorder <- 1 # transparancy
	
	lwd <- 0.5 # line width

}


### get scheme info and division polygon
########################################
out <- schemeInfo(scheme, poly=TRUE)
schemeNice <- out$schemeNice
divisionFieldPres <- out$divisionFieldPres
divisionFieldPoly <- out$divisionFieldPoly
divisionPoly <- out$divisionPoly
rm(out); gc()

### get polygon units with presences
####################################
unitNames <- as.data.frame(divisionPoly)[ , divisionFieldPoly]
divisionPoly <- divisionPoly[order(unitNames), ]
unitNames <- sort(unitNames)
divisionPoly <- divisionPoly[unitNames %in% getUnits(scheme=scheme, incAll=FALSE), ]
divisionPolyDf <- as.data.frame(divisionPoly)
divisionPolyNames <- divisionPolyDf[ , divisionFieldPoly]
divisionPoly <- sp::spTransform(divisionPoly, CRS(crsClimateNA))

centroids <- gCentroid(divisionPoly, byid=TRUE)

### PME info
pmeNice <- pmeNiceName(pmeVariant)
	
	
### aggregate evaluation data
#############################
if (!is.null(aggBy)) {

	stats <- aggregate(stats, by=aggBy, FUN=median, na.rm=TRUE)
	for (removeName in seq_along(aggBy)) stats[ , removeName] <- NULL
	names(stats)[seq_along(aggBy)] <- names(aggBy)

### units
#########
fromUnits <- getUnits(scheme=scheme, incAll=incAll)
toUnits <- getUnits(scheme=scheme, incAll=FALSE)


### image
#########

png(filename, width=width, height=height, res=600)

	par(layout(layout), mar=mar)

	plot(0, 0, col='white', fg='white', col.main='white', col.lab='white', col.axis='white')
	text(0, 0, labels=paste0(toupper(genreNice), ' - ', schemeNice, ' - ', pmeNice, ' PME - ', valanceShort(fromValance), ' vs ', valanceShort(toValance), '\n', stat1Nice, ' - ', toupper(algo)), srt=0, cex=cexBigMain, xpd=NA)
	title(sub=date(), cex.date=0.2, outer=TRUE)

	### for each unit of analysis
	#############################
	
	for (countFromUnit in seq_along(fromUnits)) {

		### make base map
		
		fromUnit <- fromUnits[countFromUnit]
		theseUnitCols <- getUnitCols(fromUnits)
		
		# plot geography
		plot(divisionPoly, lwd=lwd)
		plot(gadm, add=TRUE, lwd=lwd / 3, border='gray30')
		plot(divisionPoly, add=TRUE, col=alpha(theseUnitCols, fadeFill), border=alpha(theseUnitCols, fadeBorder), lwd=lwd)
		plot(divisionPoly[which(divisionPolyDf[ , divisionFieldPoly] %in% fromUnit), ], col=getUnitCols(fromUnit), border='black', lwd=lwd, add=TRUE)
		title(main=niceUnitName(scheme, fromUnit), line=lineMain, xpd=NA, cex.main=cexMain)

		# plot "all" unit
		if (fromUnit == 'all' & incAll) {
			plot(allPoly, add=TRUE, col=alpha('gray', fadeFill), border=alpha('gray', fadeBorder), lwd=lwd)
			text(allCoords + cbind(0, latOffset), labels='Composite', cex=unitLabelCex, xpd=NA)
		}
		
		# label units
		if (unitLabels) {
			x <- coordinates(centroids)[ , 1]
			y <- coordinates(centroids)[ , 2] + latOffset
			text(x, y, labels=niceUnitName(scheme, divisionPolyDf[ , divisionFieldPoly]), cex=unitLabelCex, xpd=NA)
		}
		
		### label each unit with performance statistic from model trained on "from" unit
		for (countToUnit in seq_along(toUnits)) {
		
			toUnit <- toUnits[countToUnit]
		
			if (fromUnit != toUnit) {
			
				toCent <- centroids[divisionPolyNames == toUnit, ]
				x1 <- coordinates(toCent)[1]
				y1 <- coordinates(toCent)[2]

				# estimate if significant difference between same-unit model and cross-unit model
				if (genre == 'performance') vsSelf <- stat[stat$scheme == scheme & stat$pme == pmeVariant & stat$fromValance == fromValance & stat$fromUnit == fromUnit & stat$toValance == fromValance & stat$toUnit == fromUnit, stat1]
				
				vsOther <- stat[stat$scheme == scheme & stat$pme == pmeVariant & stat$fromValance == fromValance & stat$fromUnit == fromUnit & stat$toValance == toValance & stat$toUnit == toUnit, stat1]
		
				# is difference between self and other significant?
				# sig <- estPerformSig(vsSelf=vsSelf, vsOther=vsOther, n=100)
				
				vsOther <- median(vsOther, na.rm=TRUE)
				vsOther <- round(vsOther, 2)
				vsOther <- format(c(vsOther, 0.123456789), digits=2)[1]
				# if (sig) vsOther <- paste0(vsOther, '*')
				text(toCent, labels=vsOther, cex=statCexOther, xpd=NA)
				
			}
		
		} # next "to" unit
		
		# label performance vs self
		if (vsSelf) {

			fromCent <- if (fromUnit != 'all') {
				centroids[divisionPolyNames == fromUnit, ]
			} else {
				allCoords
			}
		
			vsSelf <- stat[stat$scheme == scheme & stat$pme == pmeVariant & stat$fromValance == fromValance & stat$fromUnit == fromUnit & stat$toValance == fromValance & stat$toUnit == fromUnit, stat1]
			vsSelf <- median(vsSelf, na.rm=TRUE)
			vsSelf <- round(vsSelf, 2)
			vsSelf <- format(c(vsSelf, 0.123456789), digits=2)[1]

			text(fromCent, labels=vsSelf, cex=statCexSelf, xpd=NA)
	
		}
		
		### performance of other units vs "all"
		#######################################
		
		if (fromValance == 'including' & toValance == 'including' & fromUnit != 'all' & incAll) {
		
			plot(allPoly, add=TRUE, col=alpha('gray', fadeFill), border=alpha('gray', fadeBorder), lwd=lwd)
			text(allCoords + cbind(0, latOffset), labels='Composite', cex=unitLabelCex, xpd=NA)
			
			# # estimate if significant difference between same-unit model and cross-unit model
			# if (genre == 'performance') {
				# vsSelf <- stat[stat$scheme == scheme & stat$pme == pmeVariant & stat$fromValance == fromValance & stat$fromUnit == fromUnit & stat$toValance == fromValance & stat$toUnit == fromUnit, stat1]
			# }
			
			vsOther <- stat[stat$scheme == scheme & stat$pme == pmeVariant & stat$fromValance == fromValance & stat$fromUnit == fromUnit & stat$toValance == toValance & stat$toUnit == 'all', stat1]
	
			# is difference between self and other significant?
			# sig <- estPerformSig(vsSelf=vsSelf, vsOther=vsOther, n=100)

			vsOther <- median(vsOther, na.rm=TRUE)
			vsOther <- round(vsOther, 2)
			vsOther <- format(c(vsOther, 0.123456789), digits=2)[1]
			# if (sig) vsOther <- paste0(vsOther, '*')
			text(allCoords, labels=vsOther, cex=statCexOther, xpd=NA)
			
		} # if tested against ALL unit
	
	} # next fromUnit
	
dev.off()
