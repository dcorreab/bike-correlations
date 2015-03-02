library(sp)
library(rgdal)
library(gstat)
library(automap)
library(ggplot2)
library(gridExtra)
day=commandArgs(TRUE)[1]

#day="mon"
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
	tryCatch(nls (rvec ~ cc + a * exp(-( ( d - min(d) )/k)^2), start=list(cc=0, a=as,k=kss), trace=tr, control = list(maxiter = 500)), error=function(e) NULL)
}

# get_mod with only d values in km rather than log10(d).
#get_mod <- function(rvec, d, as, kss, tr) {
#	#print(c(as, kss))
#	tryCatch(
#		nls(rvec ~ cc + a * exp(-(d/k)^2), start=list(cc=0, a=as,k=kss), trace=tr, control = list(maxiter = 500)), 
#		error=function(e) NULL
#		)
#}


mod1.nls <- function(city_r,i,ks) {
	#print(i)
	#print(length(ntc[i,]) == length(ldm1[i,]))
	dvec <- as.vector (ndm1[i,])
	rvec <- as.vector (city_r[i,])
	indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
	dvec <- dvec [indx]
	rvec <- rvec [indx]
	
	d <- log10(dvec)
	# just do on d
	#d <- dvec
	#mod <- tryCatch( nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	#mod <- tryCatch( nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	mod <- NULL
	
	if (i == 112) {
		if (is.null(mod)) { mod <- get_mod(rvec, d, 0.2,1,F)			}
	}
	if (i == 74) {
		if (is.null(mod)) { mod <- get_mod(rvec, d, 2,1,F)			}
	}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.2,2,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 1.76,0.38,F)		}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,0.5,F)}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,0.6,F)			}
	if (is.null(mod)) { mod <- get_mod(rvec, d, 0.8,0.3,F)			}
	
	if (is.null(mod)) {#print(c("SPECIAL", i,0.18754798,2.50594915))
			mod <- get_mod(rvec, d, 0.18754798,2.50594915,F)	}
	if (is.null(mod)) { #print(c("SPECIAL", i,2,1))
		mod <- get_mod(rvec, d, 2,1,F)		}
	if (is.null(mod)) { #print(c(i,"1 MEAN D"))
		mod <- get_mod(rvec, d, 1,mean(d),F)			}
	if (is.null(mod)) { #print(c(i,"MEAN"))
		mod <- get_mod(rvec, d, mean(rvec),mean(d),F)			}
	#get_mod(rvec, d, 0.9, mean(d), T)
		
			k <- 10 ^ summary (mod)$parameters [3]
			# no longer log10 values
			#k <- summary (mod)$parameters [3]
}


print("I AM NUMBER ONE")
ntc_k <- numeric()
for (i in 1:332) {
	
			#print(i)
			x <- mod1.nls(ntc, i, 2)
			ntc_k <- c(ntc_k, x)
		
		if (x < 0) {
			print(c(i,x))
		}
}
print("I AM NUMBER TWO")
ntr_k <- numeric()
for (i in 1:332) {
			#print(i)
			x <- mod1.nls(ntr, i, 2)
			ntr_k <- c(ntr_k, x)
		
}
print("I AM NUMBER THREE")
nfr_k <- numeric()
for (i in 1:332) {
			#print(i)
			x <- mod1.nls(nfr, i, 2)
			nfr_k <- c(nfr_k, x)
		
		if (x < 0){
			print(c(i,x))
		}	
}


print("I AM NUMBER fours")
nfc_k <- numeric()
for (i in 1:332) {
			#print(i)
			x <- mod1.nls(nfc, i, 2)
			nfc_k <- c(nfc_k, x)
		
		if (x < 0){
			print(c(i,x))
		}
}

#names(nyc_tc)

station_names <- names(nyc_tc)
#if (day %in% c("mon", "sun") ) {
#	station_names <- station_names[-112]
#}

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
	
	
	kr = autoKrige(kval~1, ks_proj, clip_grid) 
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

p1.kr = as.data.frame(pp1$krige_output)
p3.kr = as.data.frame(pp3$krige_output)
lab <- round(seq(-0.3,2,.1), digits=1)
lab <- round(seq(0,29,1), digits=1)

nyc_sts <- read.csv("../data/station_latlons_nyc.txt")
coordinates(nyc_sts)= ~ long+lat
latlong = "+init=epsg:4326"
proj4string(nyc_sts) <- CRS(latlong)
NAD83 <- "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# transform coords to british national grid
nyc_sts.p <- spTransform(nyc_sts, CRS(NAD83))
nyc_sts.d <- as.data.frame(nyc_sts.p)

sts <- geom_point(data=nyc_sts.d, aes(x=long, y=lat),shape=3, colour="white", size=0.8)
plot1 <- ggplot() + geom_raster(data=p1.kr, aes(x=x,y=y, fill=var1.pred)) + sts + 
      scale_fill_gradientn(limits=c(1.4,2.8), colours=bpy.colors(50),na.value = "transparent", name="k-value (km)")  + coord_fixed() + theme(legend.key.height=unit(5,"lines"))
plot2 <- ggplot() + geom_raster(data=p3.kr, aes(x=x,y=y, fill=var1.pred)) + sts +
      scale_fill_gradientn(limits=c(1.4,2.8), colours=bpy.colors(50),na.value = "transparent", name="k-value (km)")  + coord_fixed() + theme(legend.key.height=unit(5,"lines"))

png (file=paste("../results/kvals/jpeg/ggplot_nyc_ok_side_by_",day,"_krig.png",sep=""), width=12, height=10, type="cairo", res=150, units="in")
	 grid.arrange(
		 arrangeGrob(plot1, plot2,
		 ncol=2),
		 main=textGrob(paste("OK Krigging", day, sep=" "), gp=gpar(cex=2), just="top"))
	dev.off()