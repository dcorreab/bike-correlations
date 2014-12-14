library(sp)
library(rgdal)
library(gstat)
# cols
nyc_tc <- read.csv("../results/nyc_to_cols.csv", row.names=1)
nyc_fc <- read.csv("../results/nyc_from_cols.csv", row.names=1)
# rows
nyc_tr <- read.csv("../results/nyc_to_rows.csv", row.names=1)
nyc_fr <- read.csv("../results/nyc_from_rows.csv", row.names=1)
# distance matrix
nyc_d <- read.csv("../data/nyc_d_matrix.csv", skip=1, row.names=1)
nyc_d <- nyc_d[-c(1),] # remove empty start_id row

ntc <- data.matrix(nyc_tc)
ntr <- data.matrix(nyc_tr)

nfc <- data.matrix(nyc_fc)
nfr <- data.matrix(nyc_fr)

ndm <- data.matrix(nyc_d)
ndm1 <- data.matrix(nyc_d)
ndm1[lower.tri(ndm1)] <- t(ndm1)[lower.tri(ndm1)]

mod1.nls <- function(city_r,i,ks) {
	#print(i)
	#print(length(ntc[i,]) == length(ldm1[i,]))
	dvec <- as.vector (ndm1[i,])
	rvec <- as.vector (city_r[i,])
	indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
	dvec <- dvec [indx]
	rvec <- rvec [indx]
	d <- log10(dvec)
	ff <- seq(from=0, to=1, length=100)
	mod <- tryCatch( nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	"if (is.null(mod)) {
		sapply(ff, function(x){
			mod <- tryCatch( nls(rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=x[[x]]), trace=F), error=function(e) NULL)
		})
	}"
	if (is.null(mod)) {
		ks=0.5
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.6
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.3
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=2
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=1
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=1
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.1,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=1.2
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.01
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.01010101
		
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	k <- 10 ^ summary (mod)$parameters [3]
}

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
	grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100) )

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
	g <- gstat(id="kval", formula=kval ~ 1, data=ks_proj)
	
	v <- variogram(g, alpha=c(0,45,90,135))
	v.fit <- fit.variogram(v, model=vgm(model='Gau', range=2000,width=3.38, nugget=2, psill=9.38))
		
	g <- gstat(g, id="kval", model=v.fit )
	p <- predict(g, model=v.fit, newdata=clip_grid)
	
	fname = paste("pres/nyc_ok_",i,".pdf",sep="")
	#jpeg (filename="pres/fname", width=2400, height=1400, type="cairo")
	pdf (file=fname, width=12, height=8)
	
	pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	#pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	pp <- spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=heat.colors(30, 0.5),cuts=26, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', main='London OK Prediction')
	print(fname)
	print(pp)
	dev.off()
}

make_maps(tk, 1)
make_maps(tk1, 2)
make_maps(fk, 3)
make_maps(fk1, 4)