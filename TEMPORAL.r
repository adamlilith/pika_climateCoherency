# source('C:/ecology/Drive/Research/Iconic Species/Scripts - Non-stationarity/TEMPORAL.r')

pres <- readRDS('C:/ecology/Drive/Research/Iconic Species/Species Records - Pika/!Collated Data 2016-06-30 1256/03 Ochotona princeps - Usable - Presences 1990-2015 - PRISM & DayMet Climate Data Extracted.rds')

rand <- readRDS('C:/ecology/Drive/Research/Iconic Species/Background Sites/Random - Western USA/BG Sites 01 Set 01 Selected from IUCN Range Map + 800-km Buffer - 11000 Sites - Extracted Raw Climate Data for 2015-10-07.rds')

### min/max daily temp
window <- 10
cols1 <- paste0('tminDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))
cols2 <- paste0('tmaxDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))

# all presences
pts1 <- pres[ , cols1]
pts2 <- pres[ , cols2]

rand1 <- rand[ , cols1]
rand2 <- rand[ , cols2]

# base plot
xlim <- ylim <- range(pts1, pts2, rand1, rand2, na.rm=TRUE)

plot(xlim, ylim, xlab='tmin', ylab='tmax', col='white')

for (i in 1:ncol(rand1)) {
	points(rand1[ , i], rand2[ , i], col=alpha('orange', 0.02), pch=16)
	points(pts1[ , i], pts2[ , i], col=alpha('darkgreen', 0.01), pch=16)
}

abline(0, 1)

### daily ppt vs tmin
window <- 10
cols1 <- paste0('tminDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))
cols2 <- paste0('pptDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))

# all presences
pts1 <- pres[ , cols1]
pts2 <- pres[ , cols2]

rand1 <- rand[ , cols1]
rand2 <- rand[ , cols2]

# base plot
xlim <- range(pts1, rand1, na.rm=TRUE)
ylim <- range(pts2, rand2, na.rm=TRUE)

plot(xlim, ylim, xlab='tmin', ylab='ppt', col='white')

# presences
for (i in 1:ncol(rand1)) {
	if (i %% 365) say(i, post=0)
	points(pts1[ , i], pts2[ , i], col=alpha('darkgreen', 0.01), pch=16)
}

# bg
for (i in 1:ncol(rand1)) {
	if (i %% 365) say(i, post=0)
	points(rand1[ , i], rand2[ , i], col=alpha('orange', 0.02), pch=16)
}

title('Daily Tmin vs Ppt')
legend('topleft', inset=0.01, pch=16, col=c('orange', 'darkgreen'), legend=c('rand bg', 'presence'))

### daily tmin vs SWE
window <- 10
cols1 <- paste0('tminDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))
cols2 <- paste0('dayMetSweDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))

# all presences
pts1 <- pres[ , cols1]
pts2 <- pres[ , cols2]

rand1 <- rand[ , cols1]
rand2 <- rand[ , cols2]

# base plot
xlim <- range(pts1, rand1, na.rm=TRUE)
ylim <- range(pts2, rand2, na.rm=TRUE)

plot(xlim, ylim, xlab='tmin', ylab='swe', col='white')

# presences
for (i in 1:ncol(rand1)) {
	if (i %% 365 == 0) say(i, post=0)
	points(pts1[ , i], pts2[ , i], col=alpha('darkgreen', 0.01), pch=16)
}

# bg
for (i in 1:ncol(rand1)) {
	if (i %% 365 == 0) say(i, post=0)
	points(rand1[ , i], rand2[ , i], col=alpha('orange', 0.02), pch=16)
}

title('Daily Tmin vs SWE')
legend('topright', inset=0.01, pch=16, col=c('orange', 'darkgreen'), legend=c('rand bg', 'presence'))

### daily tax vs ET0
window <- 10
cols1 <- paste0('tmaxDaily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))
cols2 <- paste0('dobrowskiET0Daily_yearMinus', rep(prefix(window:0, 2), each=366), '_doy', prefix(1:366, 3))

# all presences
pts1 <- pres[ , cols1]
pts2 <- pres[ , cols2]

rand1 <- rand[ , cols1]
rand2 <- rand[ , cols2]

# base plot
xlim <- range(pts1, rand1, na.rm=TRUE)
ylim <- range(pts2, rand2, na.rm=TRUE)

plot(xlim, ylim, xlab='tmax', ylab='ET0', col='white')

# presences
for (i in 1:ncol(rand1)) {
	if (i %% 365 == 0) say(i, post=0)
	points(pts1[ , i], pts2[ , i], col=alpha('darkgreen', 0.01), pch=16)
}

# bg
for (i in 1:ncol(rand1)) {
	if (i %% 365 == 0) say(i, post=0)
	points(rand1[ , i], rand2[ , i], col=alpha('orange', 0.02), pch=16)
}

title('Daily Tmax vs ET0')
legend('topleft', inset=0.01, pch=16, col=c('orange', 'darkgreen'), legend=c('rand bg', 'presence'))








pt1 <- unlist(pres[2136, cols1]) # 1990
pt2 <- unlist(pres[2136, cols2])
points(pt1, pt2, col=alpha('red', 0.5), pch=16)

pt1 <- unlist(pres[13722, cols1]) # 1990
pt2 <- unlist(pres[13722, cols2])

# cols <- heat.colors(length(pt1))
# cols <- terrain.colors(length(pt1))
points(pt1, pt2, col=alpha('black', 0.01), pch=16)


### monthly: all
cols1 <- paste0('tminMonthly_yearMinus', rep(prefix(window:0, 2), each=366), '_month', prefix(1:12, 2))
cols2 <- paste0('tmaxMonthly_yearMinus', rep(prefix(window:0, 2), each=366), '_month', prefix(1:12, 2))

xlim <- ylim <- range(pts1, pts2, na.rm=TRUE)

plot(xlim, ylim, xlab='tmin', ylab='tmax', col='white')

for (i in 1:ncol(pts1)) {
	points(pts1[ , i], pts2[ , i], col=alpha('black', 0.005), pch=16)
}
abline(0, 1)

# monthly: early vs late
pts1early <- pts1[pres$obsYear <= 2000, ]
pts2early <- pts2[pres$obsYear <= 2000, ]

pts1late <- pts1[pres$obsYear >=2015, ]
pts2late <- pts2[pres$obsYear >=2015, ]

plot(xlim, ylim, xlab='tmin', ylab='tmax', col='white')

for (i in 1:ncol(pts1early)) {
	points(pts1early[ , i], pts2early[ , i], col=alpha('blue', 0.005), pch=16)
	points(pts1late[ , i], pts2late[ , i], col=alpha('red', 0.005), pch=16)
}




