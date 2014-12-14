library(sp)
library(rgdal)
library(gstat)
# cols
lon_tc <- read.csv("../results/london_to_cols.csv", row.names=1)
lon_fc <- read.csv("../results/london_from_cols.csv", row.names=1)

# rows
lon_tr <- read.csv("../results/london_to_rows.csv", row.names=1)
lon_fr <- read.csv("../results/london_from_rows.csv", row.names=1)

# already square
ltc <- data.matrix(lon_tc)
ltr <- data.matrix(lon_tr)

lfc <- data.matrix(lon_fc)
lfr <- data.matrix(lon_fr)

# distance matrix
lon_d <- read.csv("../data/london_d_matrix.csv", skip=1, row.names=1)
lon_d <- lon_d[-c(1),] # remove empty row

ldm1 <- data.matrix(lon_d)

#make into sqaure matrix
ldm1[lower.tri(ldm1)] <- t(ldm1)[lower.tri(ldm1)]

mod1.nls <- function(city_r,i,ks) {
	#print(i)
	#print(length(ltc[i,]) == length(ldm1[i,]))
	dvec <- as.vector (ldm1[i,])
	rvec <- as.vector (city_r[i,])
	indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
	dvec <- dvec [indx]
	rvec <- rvec [indx]
	d <- log10(dvec)
	mod <- tryCatch( nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	if (is.null(mod)) {
		ks=0.5
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.3
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.5
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.5
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.5
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.5,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.02
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.1,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.02
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	k <- 10 ^ summary (mod)$parameters [3]
	#kv <- list(kval = k)
}

ltc_k <- numeric()
for (i in 1:742) {
	x <- mod1.nls(ltc, i, 0.7)
	ltc_k <- c(ltc_k, x)
}

ltr_k <- numeric()
for (i in 1:742) {
	x <- mod1.nls(ltr, i, 0.7)
	ltr_k <- c(ltr_k, x)
}

lfr_k <- numeric()
for (i in 1:742) {
	x <- mod1.nls(lfr, i, 0.7)
	lfr_k <- c(lfr_k, x)
}

lfc_k <- numeric()
for (i in 1:742) {
	x <- mod1.nls(lfc, i, 0.7)
	lfc_k <- c(lfc_k, x)
}

#max(ltc_k)
#max(ltr_k)
#max(lfc_k)
#max(lfr_k)


station_names <- names(lon_tc)
sn <- gsub("X", "", paste(station_names))

lon_sts <- read.csv("../data/station_latlons_london.txt")
tk <- data.frame(id=sn, kval=ltc_k)
tk1 <- data.frame(id=sn, kval=ltr_k)
fk1 <- data.frame(id=sn, kval=lfr_k)
fk <- data.frame(id=sn, kval=lfc_k)	



make_maps <- function(kv,i){
	ks <- merge(kv, lon_sts, by ="id")
	# remove point with the same coordinates
	ks <- ks[-311,]
	
	coordinates(ks)= ~ long+lat
	latlong = "+init=epsg:4326"
	proj4string(ks) <- CRS(latlong)
	
	bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
	# transform coords to british national grid
	ks_proj <- spTransform(ks, CRS(bng))
	
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
	proj4string(grd) <- CRS(bng)

	# Mask off the river thames
	ogrListLayers("shape/lon_bbox.shp") #will show you available layers for the above dataset
	shape=readOGR("shape/lon_bbox.shp", layer="lon_bbox") #will load the shapefile to your dataset.
	shape_p = spTransform(shape, CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"))

	# the money query for getting grid.
	clip_grid = grd[!is.na(over(grd, geometry(shape_p))),]
	
	## make gstat object:ks_proj1
	g <- gstat(id="kval", formula=kval ~ 1, data=ks_proj)
	
	v <- variogram(g, alpha=c(0,45,90,135))
	v.fit <- fit.variogram(v, model=vgm(model='Gau', range=2000, nugget=0.1, psill=0.2))
	
	g <- gstat(g, id="kval", model=v.fit )
	p <- predict(g, model=v.fit, newdata=clip_grid)
	
	fname = paste("pres/london_ok_",i,".pdf",sep="")
	#jpeg (filename="pres/fname", width=2400, height=1400, type="cairo")
	pdf (file=fname, width=12, height=8)
	
	pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	#pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	pp <- spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=heat.colors(30, 0.5),cuts=26, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', main='London OK Prediction')
	print(fname)
	print(pp)
	dev.off()
}

make_maps(tk, "1")
make_maps(tk1, 2)
make_maps(fk, 3)
make_maps(fk1, 4)