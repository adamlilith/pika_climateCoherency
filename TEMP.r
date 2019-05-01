	sayHead('visualize multivariate PC ENM analysis - boxplots of within minus among performance')
	
	say('This plot shows the difference between among- and within- unit performance. Each scheme gets a column occupied by two bars, one for the broad background (no PME) and one for the narrow background (min PME). Bars represent within-unit performance minus among-unit performance for ensemble models adjusted for confounding covariates.', breaks=60)

	# generalization
	colMinPme <- 'firebrick'
	colNoPme <- 'darkblue'
	
	testStat <- 'cbi'
	# testStat <- 'cbiPresBgWeight'
	
	say('Using ', toupper(algos), ' with ', testStat, '!!!')
	
	testStatNice <- if (testStat == 'cbi') {
		'CBI'
	} else if (testStat == 'cbiPresWeight') {
		'CBI (Presence-Weighted)'
	} else if (testStat == 'cbiPresBgWeight') {
		'CBI (Presence- & Absence-Weighted)'
	}

	evalFrame <- readRDS(paste0(workDir, 'ENMs - PCs/!Collated Evaluation Results for ENSEMBLE ENMs - Statistically Adjusted for Nuisance Variables.rds'))
	evalFrame <- evalFrame$evalFrame
	
	ylim <- c(-2, 2)
	
	offset <- 0.13

	png(paste0(workDir, './ENMs - PCs/!Within minus Among Performance.png'), width=1000, height=1200, res=300)
		
		# position for each boxplot
		par(mfrow=c(1, 1), mar=1 * c(7, 4, 4, 2) + 0.1, mgp=c(2, 1, 0), cex.main=0.6, cex.axis=0.6, cex.lab=0.8)

		plot(1:4, 1:4, col='white', xlim=c(0.5, 4.5), ylim=ylim, xaxt='n', main='Within - Among CBI (Statistically Adjusted)\none obs = mean(from-unit-given-k-minus to-units-same-k)', xlab='', ylab='Within - Among\nPerformance (CBI)')
		axis(side=1, at=1:4, labels=FALSE)
		lines(c(0.5, 4.5), c(0, 0), col='gray')

		at <- 1
	
		# by SCHEME
		for (scheme in schemes) {

			units <- getUnits(scheme, FALSE)
		
			out <- schemeInfo(scheme)
			schemeNice <- out$schemeNice
			
			# by PME
			for (pme in rev(pmes)) {

				say(scheme, ' ', pme)
			
				# plot settings
				if (pme == 'pmeMin') {
					col <- colMinPme
					off <- -offset
				} else {
					col <- colNoPme
					off <- offset
				}
				
				### get and process evaluation data
				withinMinusAmong <- numeric()
				
				# by K FOLD
				for (k in 1:kFolds) {
				
					# by FROM unit
					for (fromUnit in units) {
					
						thisWithin <- evalFrame[evalFrame$scheme == scheme & evalFrame$pme == pme &evalFrame$fromUnit == evalFrame$toUnit & evalFrame$fromUnit != 'all' & evalFrame$toUnit != 'all' & evalFrame$kFrom == k & evalFrame$kTo == k & evalFrame$fromUnit == fromUnit, ]

						thisAmong <- evalFrame[evalFrame$scheme == scheme & evalFrame$pme == pme &evalFrame$fromUnit != evalFrame$toUnit & evalFrame$fromUnit != 'all' & evalFrame$toUnit != 'all' & evalFrame$kFrom == k & evalFrame$kTo == k & evalFrame$fromUnit == fromUnit, ]
				
						withinMinusAmong <- c(withinMinusAmong, mean(thisWithin$cbiFromToResid - thisAmong$cbiFromToResid))
						
					} # next from unit
				
				} # next k fold
				
				boxplot(withinMinusAmong, at=at + off, col=alpha(col, 0.5), border=col, add=TRUE)
				
			} # next PME
		
			text(x=at, y=ylim[1] - 0.15 * diff(ylim), xpd=NA, labels=schemeNice, srt=90, adj=1, cex=0.8)
		
			at <- at + 1
					
		} # next division scheme
		
		legend('bottomleft', legend=c('Narrow background', 'Broad background'), fill=c(alpha(colMinPme, 0.5), alpha(colNoPme, 0.5)), border=c(colMinPme, colNoPme), ncol=1, inset=0, bty='n', cex=0.6)
		
	dev.off()
