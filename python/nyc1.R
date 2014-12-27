library(sp)
library(rgdal)
library(gstat)
library(automap)
day = 'wed'
# trips to
nyc_tc <- read.csv(paste("../results/nyc_",day,"_to_cols.csv",sep=""), row.names=1)
nyc_tr <- read.csv(paste("../results/nyc_",day,"_to_rows.csv",sep=""), row.names=1)

ntc <- data.matrix(nyc_tc)
row.names(ntc) <- colnames(ntc)
ntr <- data.matrix(nyc_tr)
row.names(ntr) <- colnames(ntr)

# trips from
nyc_fc <- read.csv(paste("../results/nyc_",day,"_from_cols.csv",sep=""), row.names=1)
nyc_fr <- read.csv(paste("../results/nyc_",day,"_from_rows.csv",sep=""), row.names=1)

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

get_mod <- function(as, kss, tr) {
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
	mod <- tryCatch( nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	if (is.null(mod)) { mod <- get_mod(0.8,0.5,F)			}
	if (is.null(mod)) { mod <- get_mod(0.8,0.6,F)			}
	if (is.null(mod)) { mod <- get_mod(0.8,0.3,F)			}
	if (is.null(mod)) { mod <- get_mod(0.2,2,F)			}
	if (is.null(mod)) { mod <- get_mod(0.2,1,F)			}
	if (is.null(mod)) { mod <- get_mod(0.1,1,F)			}
	if (is.null(mod)) { mod <- get_mod(0.8,1.2,F)			}
	if (is.null(mod)) { mod <- get_mod(0.2,0.01,F)			}
	if (is.null(mod)) { mod <- get_mod(0.8,0.01010101,F)	}
	if (is.null(mod)) { mod <- get_mod(0.01,0.2,F)			}
	if (is.null(mod)) { mod <- get_mod(0.1,0.4,F)			}
	if (is.null(mod)) { mod <- get_mod(0.3,2,F)			}
	if (is.null(mod)) {	mod <- get_mod(0.4,2,F)			}
	if (is.null(mod)) {
		print(i)
		mod <- get_mod(0.04,2,F)	
	}
	k <- 10 ^ summary (mod)$parameters [3]
}


#print(i)
#print(length(nfc[i,]) == length(ldm1[i,]))
#city_r <- ntc
#i = 9
#dvec <- as.vector (ndm1[i,])
#rvec <- as.vector (city_r[i,])
#indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
#dvec <- dvec [indx]
#rvec <- rvec [indx]
#d <- log10(dvec)
#mod <- get_mod(0.8,0.6,F)
#mod <- nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.1,k=0.4), trace=T)

ntc_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(ntc, i, 0.7)
	ntc_k <- c(ntc_k, x)
}

ntr_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(ntr, i, 0.7)
	ntr_k <- c(ntr_k, x)
}

nfr_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(nfr, i, 0.7)
	nfr_k <- c(nfr_k, x)
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

write.csv(tk, file = paste("kvals/",day,"_nyc_tc_k.csv",sep=""))
write.csv(tk1, file = paste("kvals/",day,"_nyc_tr_k.csv",sep=""))
write.csv(fk, file = paste("kvals/",day,"_nyc_fc_k.csv",sep=""))
write.csv(fk1, file = paste("kvals/",day,"_nyc_fr_k.csv",sep=""))


make_maps <- function(kv,i){
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
	x.range <- as.integer(range(ks_proj@coords[,1]))
	y.range <- as.integer(range(ks_proj@coords[,2]))
	
	## now expand to a grid with 500 meter spacing:
	grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=50), y=seq(from=y.range[1], to=y.range[2], by=50) )

	## convert to SpatialPixel class
	coordinates(grd) <- ~ x+y
	gridded(grd) <- TRUE

	#proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
	proj4string(grd) <- CRS(NAD83)

	#masking of nyc
	ogrListLayers("shape/nyc1.shp") #will show you available layers for the above dataset
	shape=readOGR("shape/nyc1.shp", layer="nyc1") #will load the shapefile to your dataset.
	shape_p = spTransform(shape, CRS(NAD83))

	# the money query for getting grid.
	clip_grid = grd[!is.na(over(grd, geometry(shape_p))),]
	
	## make gstat object:ks_proj1
	g <- gstat(id="kval", formula=log(kval) ~ 1, data=ks_proj, maxdist=18000)
	
	#x <- variogram(kval ~ 1, ks_proj, cutoff=30000, width=100)
	#g.vgm3 <- vgm(psill=4, model="Gau", range=3000, nugget=6)
	#plot(x, model=g.vgm3, col='black')
	
	#g.krige <- krige(kval~1, ks_proj, clip_grid, model=g.vgm3, beta=mean(ks_proj$kval))
	
	v <- variogram(g, alpha=c(0,45,90,135))
	v.fit <- fit.variogram(v, model=vgm(model='Gau', range=7000, nugget=6, sill=var(ks$kval)))
	
	
	#plot(v, model=v.fit, as.table=TRUE)
	
	#m <- vgm(var(ks$kval), "Gau", 2000, 50)
	
	
	#plot(v, model=v.fit, as.table=TRUE)
	#g <- gstat(g, id="kval", model=v.fit )
	#p <- predict(g, model=v.fit, newdata=clip_grid)
	#p <- krige(kval~1, ks_proj, clip_grid, model = m)
	
	#g <- gstat(g, id="kval", model=v.fit )
	#p <- predict(g, model=v.fit, newdata=clip_grid)
	kr = autoKrige(log10(kval)~1, ks_proj, clip_grid)
	fname = paste("pres/nyc_ok_",i,".pdf",sep="")
	#jpeg (filename="pres/fname", width=2400, height=1400, type="cairo")
	#pdf (file=fname, width=12, height=8)
	
	pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	#pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	#pp <- spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=heat.colors(30, 0.5),cuts=26, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown',
	#pp <- spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=heat.colors(1000),cuts=100, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', 
	#	main=paste("OK Prediction",i,"",sep=" "))
	pp <- spplot(kr$krige_output, zcol="var1.pred", scales = list(draw = T), col.regions=heat.colors(1000),cuts=1000, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', 
		main=paste("OK Prediction",i,"",sep=" "))
	#ra <- range(ks_proj$kval)
	#b1 <- bubble(ks_proj, "kval", maxsize = 1.5, main=paste("NYC",i,ra[1], ra[2],"",sep=" "), 
	#	key.entries = 2^(-1:4), scales = list(draw = T))
	#print(fname)
	#print(pp)
	#dev.off()
}

pp1 <- make_maps(tk, "to_cols")
pp2 <- make_maps(tk1, "to_rows")

#pdf (file="pres/nyc_ok_side_by.pdf", width=12, height=8)
#print(pp1, split=c(1,1,2,4), more=T)
#print(pp2, split=c(2,1,4,1), more=T)
#dev.off()

pp3<-make_maps(fk, "from_cols")
pp4<-make_maps(fk1, "from_rows")
#pdf (file="pres/nyc_ok_side_by1.pdf", width=12, height=8)
#print(pp4, split=c(1,1,2,1), more=T)
#print(pp3, split=c(2,1,2,1), more=F)
#dev.off()

pdf (file=paste("pres/nyc_ok_side_by_",day,"_krig.pdf",sep=""), width=12, height=12)
print(pp1, split=c(1,1,2,2), more=T)
print(pp2, split=c(2,1,2,2), more=T)
print(pp4, split=c(1,2,2,2), more=T)
print(pp3, split=c(2,2,2,2), more=F)
dev.off()

#i = "blah"
#pp <- spplot(p, zcol="kval.pred", scales = list(draw = T),at = seq(gmin, gmax, length=100), col.regions=heat.colors(1000),cuts=100, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', 
#	main=paste("OK Prediction",i,"",sep=" "))