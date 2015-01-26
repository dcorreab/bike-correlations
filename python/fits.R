library(sp)
library(rgdal)
library(gstat)
library(automap)
library(ggplot2)
library(gridExtra)
day="total"

# trips to
nyc_tc <- read.csv(paste("../results/cor/nyc_",day,"_to_cols.csv",sep=""), row.names=1)

ntc <- data.matrix(nyc_tc)
row.names(ntc) <- colnames(ntc)

# distance matrix
nyc_d <- read.csv("../data/nyc_d_matrix.csv", skip=1, row.names=1)
nyc_d <- nyc_d[-c(1),] # remove empty start_id row

# transform to matrix
ndm <- data.matrix(nyc_d)
ndm1 <- data.matrix(nyc_d)
ndm1[lower.tri(ndm1)] <- t(ndm1)[lower.tri(ndm1)]

# gaussian distance decay model
get_mod <- function(rvec, d, as, kss, tr) {
	tryCatch(nls (rvec ~ cc + a * exp(-( ( d - min(d) )/k)^2), start=list(cc=0, a=as,k=kss), trace=tr, control = list(maxiter = 500)), error=function(e) NULL)
}

get_fits <- function(cv, dvec, rvec) {
	d <- log10(dvec)

	#plot(rvec ~ d)
	mod <- get_mod(rvec, d,0.21,0.6,F)
	dfit <- seq (min (d), max (d), length.out=100)
	rfit <- predict (mod, newdata=data.frame(d=dfit))

	# exponential - no squares
	expo <- nls (rvec ~ cc + a * exp(-((d - min(d))/k)), start=list(cc=0, a=0.21,k=0.6), trace=T)
	rfit1 <- predict (expo, newdata=data.frame(d=dfit))

	#inverse 1/d
	inverse <- nls (rvec ~ (cc + a * 1/d), start=list(cc=1, a=0.6), trace=T)
	rfit2 <- predict (inverse, list(d=dfit))

	gravity <- nls (rvec ~ cc + a * (1/d^2), start=list(cc=1, a=0.6), trace=T)
	rfit3 <- predict (gravity, list(d=(dfit)))
	as.data.frame(cbind(dfit, rfit, rfit1, rfit2, rfit3))
}

################################
###		PICK FIRST STATION	####
################################
city_r <- ntc
cv = 222
dvec <- as.vector (ndm1[cv,])
rvec <- as.vector (city_r[cv,])
indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
dvec <- dvec [indx]
rvec <- rvec [indx]
rm<-which(dvec<1)
dvec<-dvec[-rm]
rvec<-rvec[-rm]

d <- log10(dvec)

#plot(rvec ~ d)
mod <- get_mod(rvec, d,0.21,0.6,F)
dfit <- seq (min (d), max (d), length.out=100)
rfit <- predict (mod, newdata=data.frame(d=dfit))
ssr_gau <- sum(resid(mod)^2)

# exponential - no squares
expo <- nls (rvec ~ cc + a * exp(-((d - min(d))/k)), start=list(cc=0, a=0.21,k=0.6), trace=F)
rfit1 <- predict (expo, newdata=data.frame(d=dfit))
ssr_exp <- sum(resid(expo)^2)

#inverse 1/d
inverse <- nls (rvec ~ (cc + a * 1/d), start=list(cc=1, a=0.6), trace=F)
rfit2 <- predict (inverse, list(d=dfit))
ssr_inv <- sum(resid(inverse)^2)

gravity <- nls (rvec ~ cc + a * (1/d^2), start=list(cc=1, a=0.6), trace=F)
rfit3 <- predict (gravity, list(d=dfit))
ssr_gra <- sum(resid(gravity)^2)

png (file=paste("../results/models/station_one_points.png",sep=""), width=6, height=6, type="cairo", res=150, units="in")
ggplot() + geom_point(aes(dvec,rvec),colour="grey",guide=FALSE) + 
	scale_x_log10(name="Distance", limits=c(1,11), breaks=seq(1,10,1)) + 
	scale_y_continuous(name="Correlation", limits=c(0, 0.8)) +
	ggtitle(paste("Station One", sep=" ")) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

station1.points<-as.data.frame(cbind(dvec, rvec))
station1.fits <- as.data.frame(cbind(dfit, rfit1,rfit2,rfit3))

png (file=paste("../results/models/station_one_models.png",sep=""), width=6, height=4, type="cairo", res=150, units="in")
gg<-ggplot() + geom_point(aes(dvec,rvec, colour="Stations",guide=FALSE)) + ylim(0, 0.8) + scale_x_log10() +
	geom_line(aes(10^dfit, rfit1, colour="Exponential")) +
	geom_line(aes(10^dfit, rfit2, colour="Inverse")) +
	geom_line(aes(10^dfit, rfit3, colour="Gravity")) +
	geom_line(aes(10^dfit, rfit, colour="Gaussian")) +
	scale_colour_manual(name='Function Fit', values=c('Stations'='grey', 'Gaussian'="black", "Exponential"="#E69F00", "Inverse"="#009E73", "Gravity"="#CC79A7")) +
	ggtitle(paste("Station One", sep=" ")) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
# get model stats
s1 <- as.data.frame(cbind(ssr_gau, ssr_exp, ssr_inv, ssr_gra))
################################
###		PICK Second STATION	####
################################
city_r <- ntc
cv = 72
dvec <- as.vector (ndm1[cv,])
rvec <- as.vector (city_r[cv,])
indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
dvec <- dvec [indx]
rvec <- rvec [indx]
rm<-which(dvec<1)
dvec<-dvec[-rm]
rvec<-rvec[-rm]

d <- log10(dvec)

#plot(rvec ~ d)
mod <- get_mod(rvec, d,0.21,0.6,F)
dfit <- seq (min (d), max (d), length.out=100)
rfit <- predict (mod, newdata=data.frame(d=dfit))
ssr_gau <- sum(resid(mod)^2)

# exponential - no squares
expo <- nls (rvec ~ cc + a * exp(-((d - min(d))/k)), start=list(cc=0, a=0.21,k=0.6), trace=F)
rfit1 <- predict (expo, newdata=data.frame(d=dfit))
ssr_exp <- sum(resid(expo)^2)

#inverse 1/d
inverse <- nls (rvec ~ (cc + a * 1/d), start=list(cc=1, a=0.6), trace=F)
rfit2 <- predict (inverse, list(d=dfit))
ssr_inv <- sum(resid(inverse)^2)

gravity <- nls (rvec ~ cc + a * (1/d^2), start=list(cc=1, a=0.6), trace=F)
rfit3 <- predict (gravity, list(d=dfit))
ssr_gra <- sum(resid(gravity)^2)
seq(min(dvec),max(dvec), 1)
png (file=paste("../results/models/station_two_points.png",sep=""), width=6, height=6, type="cairo", res=150, units="in")
ggplot() + geom_point(aes(dvec,rvec),colour="grey",guide=FALSE) + 
	scale_x_log10(name="Distance", limits=c(1,11), breaks=seq(1,10,1)) + 
	scale_y_continuous(name="Correlation", limits=c(0, 0.8)) +
	ggtitle(paste("Station Two", sep=" ")) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()

station2.points<-as.data.frame(cbind(dvec, rvec))
station2.fits <- as.data.frame(cbind(dfit, rfit1,rfit2,rfit3))

png (file=paste("../results/models/station_two_models.png",sep=""), width=6, height=4, type="cairo", res=150, units="in")
gg1<-ggplot() + geom_point(aes(dvec,rvec, colour="Stations",guide=FALSE)) + ylim(0, 0.8) + scale_x_log10() +
	geom_line(aes(10^dfit, rfit1, colour="Exponential")) +
	geom_line(aes(10^dfit, rfit2, colour="Inverse")) +
	geom_line(aes(10^dfit, rfit3, colour="Gravity")) +
	geom_line(aes(10^dfit, rfit, colour="Gaussian")) +
	scale_colour_manual(name='Function Fit', values=c('Stations'='grey', 'Gaussian'="black", "Exponential"="#E69F00", "Inverse"="#009E73", "Gravity"="#CC79A7")) +
	ggtitle(paste("Station Two", sep=" ")) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
dev.off()
# get model stats
s2 <- as.data.frame(cbind(ssr_gau, ssr_exp, ssr_inv, ssr_gra))

## COMBINE THE STATISTICS OF THE MODELS & OUTPUT TO CSV
ssr <- rbind(s1,s2)
names(ssr) <- c("Gaussian","Exponential", "Inverse", "Gravity")
ssr <- cbind(station=c("S1", "S2"), ssr)
write.csv(file="../results/models/ssr_results.csv",ssr)

#######################
# MAKE COMBINED GGPLOT OF TWO ###
######################
gg1<-ggplot(station1.fits) + geom_point(data=station1.points, aes(dvec,rvec, colour="Stations",guide=FALSE)) + ylim(0, 0.8) + scale_x_log10(limits=c(1,10.55)) +
	geom_line(aes(10^dfit, rfit1, colour="Exponential")) +
	geom_line(aes(10^dfit, rfit2, colour="Inverse")) +
	geom_line(aes(10^dfit, rfit3, colour="Gravity")) +
	geom_line(aes(10^dfit, rfit, colour="Gaussian")) +
	scale_colour_manual(name='Function Fit', values=c('Stations'='grey', 'Gaussian'="black", "Exponential"="#E69F00", "Inverse"="#009E73", "Gravity"="#CC79A7")) +
	ggtitle(paste("Station One", sep=" ")) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

gg2<-ggplot(station2.fits) + geom_point(data=station2.points, aes(dvec,rvec, colour="Stations",guide=FALSE)) + ylim(0, 0.8) + scale_x_log10(limits=c(1,10.55)) +
	geom_line(aes(10^dfit, rfit1, colour="Exponential")) +
	geom_line(aes(10^dfit, rfit2, colour="Inverse")) +
	geom_line(aes(10^dfit, rfit3, colour="Gravity")) +
	geom_line(aes(10^dfit, rfit, colour="Gaussian")) +
	scale_colour_manual(name='Function Fit', values=c('Stations'='grey', 'Gaussian'="black", "Exponential"="#E69F00", "Inverse"="#009E73", "Gravity"="#CC79A7")) +
	ggtitle(paste("Station Two", sep=" ")) + theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

png (file=paste("../results/models/station_fits.png",sep=""), width=8, height=10, type="cairo", res=150, units="in")
grid.arrange(gg1,gg2, nrow=2)
dev.off()