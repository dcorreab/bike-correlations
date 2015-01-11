library(sp)
library(rgdal)
library(gstat)
library(automap)
day=commandArgs(TRUE)[1]

#day="fri"
# trips to
nyc_tc <- read.csv(paste("../results/cor/nyc_",day,"_to_cols.csv",sep=""), row.names=1)
nyc_tr <- read.csv(paste("../results/cor/nyc_",day,"_to_rows.csv",sep=""), row.names=1)

ntc <- data.matrix(nyc_tc)
row.names(ntc) <- colnames(ntc)
ntr <- data.matrix(nyc_tr)
row.names(ntr) <- colnames(ntr)
	
# trips from
nyc_fc <- read.csv(paste("../results/cor/nyc_",day,"_from_cols.csv",sep=""), row.names=1)
nyc_fr <- read.csv(paste("../results/cor/nyc_",day,"_from_rows.csv",sep=""), row.names=1)

nfc <- data.matrix(nyc_fc)
row.names(nfc) <- colnames(nfc)

nfr <- data.matrix(nyc_fr)
row.names(nfr) <- colnames(nfr)

# distance matrix
nyc_d <- read.csv("../data/nyc_d_matrix.csv", skip=1, row.names=1)
nyc_d <- nyc_d[-c(1),] # remove empty start_id row


ndm <- data.matrix(nyc_d)
ndm1 <- data.matrix(nyc_d)
ndm1[lower.tri(ndm1)] <- t(ndm1)[lower.tri(ndm1)]

get_mod <- function(rvec, d, as, kss, tr) {
	tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=as,k=kss), trace=tr), error=function(e) NULL)
}


mod1.nls <- function(city_r,i,ks) {
	#print(i)
	#print(length(ntc[i,]) == length(ldm1[i,]))
	dvec <- as.vector (ndm1[i,])
	rvec <- as.vector (city_r[i,])
	indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
	dvec <- dvec [indx]
	rvec <- rvec [indx]
	d <- log10(dvec)
	#mod <- tryCatch( nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	#mod <- tryCatch( nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	mod <- get_mod(rvec, d, 0.8, ks, F)
	if (is.null(mod)) { mod <- get_mod(rvec, d, 1.76,0.38,F)		}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,0.5,F)}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,0.6,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,0.3,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.2,2,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.2,1,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.1,1,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,1.2,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.2,0.01,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,0.01010101,F)	}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.01,0.2,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.1,0.4,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.3,2,F)			}
	if (is.null(mod)) {	mod <- get_mod(rvec, d, 0.4,2,F)			}
	if (is.null(mod)) {mod <- get_mod(rvec, d, 0.04,2,F)	}
	if (is.null(mod)) {mod <- get_mod(rvec, d, 0.21,2,F)	}
	if (is.null(mod)) {
		print(c(i,0.03,0.5))
		mod <- get_mod(rvec, d, 0.03,0.5,F)	}
	if (is.null(mod)) {
		print(c(i,0.8,2))
		mod <- get_mod(rvec, d, 0.8,2,F)	}
	if (is.null(mod)) {
		print(c(i,0.16,0.01))
		mod <- get_mod(rvec, d, 0.16,0.01,F)	}
	if (is.null(mod)) {
		print(c(i,0.97,0.3))
		mod <- get_mod(rvec, d, 0.97,0.3,F)	}
	if (is.null(mod)) {
		print(c(i,0.9,0.3))
		mod <- get_mod(rvec, d, 0.9,0.3,F)	}
	if (is.null(mod)) {
		print(c(i,0.9,0.01))
		mod <- get_mod(rvec, d, 0.9,0.01,F)	}
		
	k <- 10 ^ summary (mod)$parameters [3]
}


#print(i)
#print(length(nfc[i,]) == length(ldm1[i,]))
#city_r <- nfr
#cv = 112
#dvec <- as.vector (ndm1[cv,])
#rvec <- as.vector (city_r[cv,])
#indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
#dvec <- dvec [indx]
#rvec <- rvec [indx]
#range(rvec)
#99 0.000003136919 0.747528061374
#112 0.000003323166 0.388167814813
#120 0.00000001726077 0.69297663123910

#d <- log10(dvec)
#plot(rvec ~ d)
#mod <- get_mod(rvec, d,0.21,2,F)
#mod <- nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=1.76,k=0.38), trace=T)
#xx<-seq(-0.8,2,0.01)
#for (i in xx) {
#		mod <- tryCatch(nls(rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=i,k=0.38), trace=F), error=function(e) NULL)
#	if (!is.null(mod)){
#		print (i)
#		print(10 ^ summary (mod)$parameters [3])#
#	}
#}

ntc_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(ntc, i, 0.7)
	ntc_k <- c(ntc_k, x)
	if (x >= 22){
		print(c(i,x))
	}
}

ntr_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(ntr, i, 0.7)
	ntr_k <- c(ntr_k, x)
}

nfr_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(nfr, i, 2)
	nfr_k <- c(nfr_k, x)
	if (x >= 30){
		print(c(i,x))
	}
	
}
kvals <- numeric()
for (i in 1:332) {
	
}


nfc_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(nfc, i, 0.7)
	nfc_k <- c(nfc_k, x)
}

#names(nyc_tc)

station_names <- names(nyc_tc)

sn <- gsub("X", "", paste(station_names))

nyc_sts <- read.csv("../data/station_latlons_nyc.txt")
tk <- data.frame(id=sn, kval=ntc_k)
tk1 <- data.frame(id=sn, kval=ntr_k)
fk1 <- data.frame(id=sn, kval=nfr_k)
fk <- data.frame(id=sn, kval=nfc_k)

write.csv(merge(tk, nyc_sts, by ="id"), file = paste("../results/kvals/nyc_",day,"_tc_k.csv",sep=""))
write.csv(merge(tk1, nyc_sts, by ="id"), file = paste("../results/kvals/nyc_",day,"_tr_k.csv",sep=""))
write.csv(merge(fk, nyc_sts, by ="id"), file = paste("../results/kvals/nyc_",day,"_fc_k.csv",sep=""))
write.csv(merge(fk1, nyc_sts, by ="id"), file = paste("../results/kvals/nyc_",day,"_fr_k.csv",sep=""))


make_maps <- function(kv){
	ks <- merge(kv, nyc_sts, by ="id")
	# remove point with the same coordinates
	#ks <- ks[-311,]
	
	coordinates(ks)= ~ long+lat
	latlong = "+init=epsg:4326"
	proj4string(ks) <- CRS(latlong)
	
	NAD83 <- "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
	# transform coords to british national grid
	ks_proj <- spTransform(ks, CRS(NAD83))
	## create a grid onto which we will interpolate:
	## first get the range in data
	xx <- bbexpand(bbox(ks_proj)[1, ], 0.1)
	yy <- bbexpand(bbox(ks_proj)[2, ], 0.1)
	#x.range <- as.integer(range(ks_proj@coords[,1]))
	#y.range <- as.integer(range(ks_proj@coords[,2]))
	
	## now expand to a grid with 500 meter spacing:
	grd <- expand.grid(x=seq(from=xx[1], to=xx[2], by=50), y=seq(from=yy[1], to=yy[2], by=50) )

	## convert to SpatialPixel class
	coordinates(grd) <- ~ x+y
	gridded(grd) <- TRUE

	#proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
	proj4string(grd) <- CRS(NAD83)

	#masking of nyc
	ogrListLayers("../data/shape/nyc1.shp") #will show you available layers for the above dataset
	shape=readOGR("../data/shape/nyc1.shp", layer="nyc1") #will load the shapefile to your dataset.
	shape_p = spTransform(shape, CRS(NAD83))

	# the money query for getting grid.
	clip_grid = grd[!is.na(over(grd, geometry(shape_p))),]
	
	
	kr = autoKrige(log10(kval)~1, ks_proj, clip_grid) 
}


plot_maps <- function(kr, i){
	coordinates(nyc_sts)= ~ long+lat
	latlong = "+init=epsg:4326"
	proj4string(nyc_sts) <- CRS(latlong)
	NAD83 <- "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
	# transform coords to british national grid
	nyc_sts <- spTransform(nyc_sts, CRS(NAD83))
	
	fname = paste("../results/kvals/pdf/nyc_ok_",i,".pdf",sep="")
	
	pts <- list("sp.points", nyc_sts, pch = 4, col = "white", cex=0.5)
	
	rng1 <- round(seq(-0.3,2,.1),digits=1)
	
	
	spplot(kr$krige_output, zcol="var1.pred", col.regions=bpy.colors, at=rng1,
		sp.layout=list(pts), 
		colorkey = list(space = "right", labels=paste(rng1)),
		main=paste("Prediction",i,"",sep=" "),
		names.attr="K Value",
		sub = expression("K Value Distances")
		 )
}

pp1 <- make_maps(tk)
#pp2 <- make_maps(tk1, "to_rows")

pp3<-make_maps(fk)

pp1 <- plot_maps(pp1, "to_cols")
pp3 <- plot_maps(pp3, "from_cols")
#pp4<-make_maps(fk1, "from_rows")

pdf (file=paste("../results/kvals/pdf/nyc_ok_side_by_",day,"_krig.pdf",sep=""), width=12, height=12)
print(pp1, split=c(1,1,2,1), more=T)
print(pp3, split=c(2,1,2,1), more=F)
dev.off()
#print(pp2, split=c(2,1,2,2), more=T)
#print(pp4, split=c(1,2,2,2), more=T)
#print(pp3, split=c(2,2,2,2), more=F)
#dev.off()

jpeg (file=paste("../results/kvals/jpeg/nyc_ok_side_by_",day,"_krig.jpeg",sep=""), width=2000, height=2000)
print(pp1, split=c(1,1,2,1), more=T)
print(pp3, split=c(2,1,2,1), more=F)
dev.off()

#print(pp2, split=c(2,1,2,2), more=T)
#print(pp4, split=c(1,2,2,2), more=T)
#print(pp3, split=c(2,2,2,2), more=F)








#i = "blah"
#pp <- spplot(p, zcol="kval.pred", scales = list(draw = T),at = seq(gmin, gmax, length=100), col.regions=heat.colors(1000),cuts=100, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', 
#	main=paste("OK Prediction",i,"",sep=" "))