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
	g <- gstat(id="kval", formula=log10(kval) ~ 1, data=ks_proj)
	
	v <- variogram(g, alpha=c(0,45,90,135))
	v.fit <- fit.variogram(v, model=vgm(model='Gau', range=2000, nugget=0.1, psill=0.2))
	
	g <- gstat(g, id="kval", model=v.fit )
	p <- predict(g, model=v.fit, newdata=clip_grid)
	
	fn = paste("london_ok_",i,".jpg" ,sep="")
	jpeg (filename=fn, width=2400, height=1400, type="cairo")
	
	pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	#pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
	spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=heat.colors(30, 0.5),cuts=26, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', main='London OK Prediction')
	
	dev.off()
	
}

make_maps(tk, 1)
make_maps(tk1, 2)
make_maps(fk, 3)
make_maps(fk1, 4)

width=12, height=6
pdf (file="lon_to_c_kplots.pdf", width=12, height=6)
ks <- merge(tk, lon_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
b1 <- bubble(ks, "kval", maxsize = 2.5, main=paste ("London To Cols", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()

pdf (file="lon_to_r_kplots.pdf", width=12, height=6)
ks <- merge(tk1, lon_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
latlong = "+init=epsg:4326"
b1 <- bubble(ks, "kval", maxsize = 2.5, main=paste ("London To Rows", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()

pdf (file="lon_from_c_kplots.pdf", width=12, height=6)
latlong = "+init=epsg:4326"
ks <- merge(fk, lon_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)

b1 <- bubble(ks, "kval", maxsize = 2.5, main=paste ("London From Cols", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()

pdf (file="lon_from_r_kplots.pdf", width=12, height=6)
ks <- merge(fk1, lon_sts, by ="id")
latlong = "+init=epsg:4326"
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)

b1 <- bubble(ks, "kval", maxsize = 2.5, main=paste ("London From Rows", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()


#tk <- data.frame(id=sn, kval=ltc_k)
#tks <- merge(tk, lon_sts, by ="id")
#coordinates(tks)= ~ long+lat
#proj4string(tks) <- CRS(latlong)
#cb <- as.geodata(tks)
#lon_tr1 <- lon_tr[10:11,]
#lon_d1 <- lon_d[10:11,]
#ltc <- data.matrix(lon_tc)
#ltr <- data.matrix(lon_tr1)
#ldm <- data.matrix(lon_d1)
#library(rgdal)
#library(gstat)

# remove point with the same coordinates
ks <- ks[-311,]

#######################################################################################################################
# 						MERGE KS
########################################################################################################################
ks <- merge(fk1, lon_sts, by ="id")
latlong = "+init=epsg:4326"
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
# transform coords to british national grid
ks_proj <- spTransform(ks, CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")) #projInfo(type="proj")
#######################################################################################################################
#ks_proj <- spTransform(ks, CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))


## create a grid onto which we will interpolate:
## first get the range in data
x.range <- as.integer(range(ks_proj@coords[,1]))
y.range <- as.integer(range(ks_proj@coords[,2]))



#######################################################################################################################

library(shapefiles)
poli<-read.shapefile("shape/lon_bbox")
poli<-convert.to.simple(poli$shp)
poli<-poli[,-1]
poli <- as.matrix(poli)

## now expand to a grid with 500 meter spacing:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100) )

## convert to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

#proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
proj4string(grd) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")


# Mask off the thames
ogrListLayers("shape/lon_bbox.shp") #will show you available layers for the above dataset
shape=readOGR("shape/lon_bbox.shp", layer="lon_bbox") #will load the shapefile to your dataset.
shape_p = spTransform(shape, CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"))

# the money query for getting grid.
clip_grid = grd[!is.na(over(grd, geometry(shape_p))),]



#transform to WGS84 to match shape
#grd <- spTransform(grd, CRS(latlong))

library(splancs)
# output to dataframe to use inout
grd1 <- as.data.frame(grd)
## data.frame - get only points within the shape
grd1.irr <- grd1[inout(grd1,poli),]
grd1.irr <- SpatialPoints(grd1.irr[,c("x","y")],proj4string=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"))
gridded(grd1.irr) <- TRUE
plot(grd1.irr)
#######################################################################################################################




## test it out:
plot(clip_grid, cex=0.5)
points(ks_proj, pch=1, col='red', cex=0.7)
title("Interpolation Grid and Sample Points")

ks_proj.idw <- idw(ks_proj$kval ~ 1, ks_proj, grd)
spplot(ks_proj.idw, "var1.pred", main = "IDW interpolation kval")

## make gstat object:ks_proj1
g <- gstat(id="kval", formula=log10(kval) ~ 1, data=ks_proj)
plot(variogram(g, map=TRUE, cutoff=3000, width=500), threshold=10)

v <- variogram(g, alpha=c(0,45,90,135))
plot(v, model=v.fit, as.table=TRUE)

# nyc
#v.fit <- fit.variogram(v, model=vgm(model='Gau', range=10000, nugget=12, psill=5))
# london
v <- variogram(g, alpha=c(0,45,90,135))
v.fit <- fit.variogram(v, model=vgm(model='Gau', range=2000, nugget=0.1, psill=0.2))
plot(v, model=v.fit, as.table=TRUE)



g <- gstat(g, id="kval", model=v.fit )
p <- predict(g, model=v.fit, newdata=clip_grid)



#write.csv(p, file="colin_test.csv")


## lattice graphics: thanks for R. Bivand's advice on this
## 
## alternatively plot quantiles with
## ... col.regions=terrain.colors(6), cuts=quantile(p$elev.pred) ...
##
jpeg (filename="lon_ok.jpg", width=1200, height=700, type="cairo")
par(mar=c(2,2,2,2))
pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
#pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=heat.colors(30, 0.5),cuts=26, sp.layout=list(pts), contour=F, labels=FALSE, pretty=F, col='brown', main='London OK Prediction')

## plot the kriging variance as well
spplot(p, zcol='kval.var', col.regions=heat.colors(100), cuts=cus, main='OK Variance',sp.layout=list(pts) )

#xx <- c(0.268494683, 0.019449747)
#yy <- c(52.44038,52.55651)

bbox <- coord_map(xlim = extendrange(ks@bbox[1,]), ylim = extendrange(ks@bbox[2,]))
#bbox <- coord_map(xlim = xx, ylim = yy)

#points <- ks@coords
points <- data.frame(cbind(long=ks@coords[,1], lat=ks@coords[,2], kval=ks$kval))
#base <- with(bbox, get_map(paste(mean(limits$y), mean(limits$x), sep = " "),zoom=13, color = "color",maptype = "roadmap"))
base <- get_map(location = 'London', zoom = 12, color = "color",maptype = "roadmap")
points$bins <- cut(log10(points$kval), breaks=seq(0, 3.3, 0.4), labels=seq(1,8,1))
#range01 <- function(x){(x-min(x))/(max(x)-min(x))}
#range01(points$kval)
#summary(log10(points$kval))
cus <- as.integer(seq(range(p$kval.pred)[1],range(p$kval.pred)[2]+5,5))

as.integer(seq(range(p$kval.pred)[1],range(p$kval.pred)[2],5))

base <- get_map(location = 'London', zoom = 12, color = "color",maptype = "roadmap")
ggmap(base) + 
  #geom_path(data = polygons, aes(x, y, colour = agency_name ), size = 1) +
  geom_point(data = points, aes(x = long, points = ks$lat, size = as.numeric(points$bins),name="Kval", color = points$bins), alpha=0.8) + 
  theme_nothing(legend=TRUE) + bbox
  
  #+ # transit stops in black
  #scale_colour_manual(labels=c("1","2","3","4","5","6","7","8"))
  #bbox