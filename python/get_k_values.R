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

#hhh <- merge(tk, nyc_sts, by ="id")
#b1 = bubble(hhh, "kval", maxsize = 1.5, main = "kvalue per station",
#	key.entries = 2^(-1:4))

#gg <- sapply(station_names, function(x){unlist(strsplit(x, split='X', fixed=TRUE))[2]})
#unlist(strsplit(names(nyc_tc), split='X', fixed=TRUE))[2]

plots <- list ()
plots [[1]] <- tk
plots [[2]] <- tk1
jpeg (filename="r2plots.jpg", width=1200,
      height=700, type="cairo")
par (mfrow=c(1,2))
mts <- c ("from", "to")
for (i in 1:2) {
	hhh <- merge(plots[[i]], nyc_sts, by ="id")
	coordinates(hhh)= ~ long+lat
	proj4string(hhh) <- CRS(latlong)
	latlong = "+init=epsg:4326"
	b1 <- bubble(hhh, "kval", maxsize = 1.5, main=paste (mts[i], sep=""), 
		key.entries = 2^(-1:4), scales = list(draw = T))
	print(b1, split=c(1,2), more=T)
}
dev.off()

pdf (file="nyc_to_c_kplots.pdf", width=8, height=10)
ks <- merge(tk, nyc_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
latlong = "+init=epsg:4326"
b1 <- bubble(ks, "kval", maxsize = 1.5, main=paste ("NYC To Cols", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()

pdf (file="nyc_to_r_kplots.pdf", width=8, height=10)
ks <- merge(tk1, nyc_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
latlong = "+init=epsg:4326"
b1 <- bubble(ks, "kval", maxsize = 1.5, main=paste ("NYC To Rows", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()

pdf (file="nyc_from_c_kplots.pdf", width=8, height=10)
ks <- merge(fk, nyc_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
latlong = "+init=epsg:4326"
b1 <- bubble(ks, "kval", maxsize = 1.5, main=paste ("NYC From Cols", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()

pdf (file="nyc_from_r_kplots.pdf", width=8, height=10)
ks <- merge(fk1, nyc_sts, by ="id")
latlong = "+init=epsg:4326"
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)

b1 <- bubble(ks, "kval", maxsize = 1.5, main=paste ("NYC From Rows", sep=""), 
	key.entries = 2^(-1:4), scales = list(draw = T))
b1
dev.off()



library(rgdal)
library(gstat)
library(splancs)
ks <- merge(fk1, nyc_sts, by ="id")
latlong = "+init=epsg:4326"
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
# transform coords to british national grid
#ks_proj <- spTransform(ks, CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")) #projInfo(type="proj")

ks_proj <- spTransform(ks, CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

## create a grid onto which we will interpolate:
## first get the range in data
x.range <- as.integer(extendrange(ks_proj@coords[,1]))
y.range <- as.integer(extendrange(ks_proj@coords[,2]))

## now expand to a grid with 500 meter spacing:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=50), y=seq(from=y.range[1], to=y.range[2], by=50) )

########################################## SKIP ##########################
########################################## SKIP ##########################
## convert to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
## test it out:
plot(grd, cex=0.5)
#plot(grd.ir, cex=0.1)
points(ks_proj, pch=1, col='red', cex=0.7)
plot(shape_p, cex=0.7, add=T)


#library(shapefiles)
#poli<-read.shapefile("shape/nyc1")
#poli<-convert.to.simple(poli$shp)
#poli<-poli[,-1]
#poli <- as.matrix(poli)

#masking of nyc
ogrListLayers("shape/nyc1.shp") #will show you available layers for the above dataset
shape=readOGR("shape/nyc1.shp", layer="nyc1") #will load the shapefile to your dataset.
shape_p = spTransform(shape, CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# the money query for getting grid.
clip_grid = grd[!is.na(over(grd, geometry(shape_p))),]


## now expand to a grid with 500 meter spacing:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=200), y=seq(from=y.range[1], to=y.range[2], by=200) )

## convert to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

#proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
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
points(poli, pch=1, col='red', cex=0.7)


#title("Interpolation Grid and Sample Points")

#jpeg (filename="nyc_idw.jpg", width=1200,
 #     height=700, type="cairo")
#ks_proj.idw <- idw(ks_proj$kval ~ 1, ks_proj, grd)
#spplot(ks_proj.idw, "var1.pred", main = "IDW interpolation kval")


########################################## SKIP ##########################

library(shapefiles)
poli<-read.shapefile("shape/nyc1")
poli<-convert.to.simple(poli$shp)
poli<-poli[,-1]
poli <- as.matrix(poli)



## now expand to a grid with 100 meter spacing:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100) )

grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100) )

## convert to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#transform to WGS84 to match shape
grd <- spTransform(grd, CRS(latlong))

#ind.grid <- spsample(grd, type="regular", cellsize = c(100,100))

library(splancs)
# output to dataframe to use inout
grd1 <- as.data.frame(grd)
## data.frame - get only points within the shape
grd1.irr <- grd1[inout(grd1,poli),]

#need to remove inf and -inf
grd1.irr <- do.call(data.frame,lapply(grd1.irr, function(x) replace(x, is.infinite(x),NA)))

grd1.irr <- SpatialPoints(grd1.irr[,c("x","y")],proj4string=CRS(latlong))
gridded(grd1.irr) <- TRUE

grd1.irr = spTransform(grd1.irr, CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))


########################################## SKIP ##########################

library(gstat)
## make gstat object:ks_proj1
g <- gstat(id="kval", formula=kval ~ 1, data=ks_proj)
#plot(variogram(g, map=TRUE, cutoff=2000, width=500), threshold=10)

#hscat(kval ~ 1, ks_proj, c(40, 100, 150, 200, 400, 600))


v <- variogram(g, alpha=c(0,45,90,135))
#plot(v, model=v.fit, as.table=TRUE)

# nyc
v.fit <- fit.variogram(v, model=vgm(model='Gau', range=2000,width=3.38, nugget=2, psill=9.38))
#v.fit <- fit.variogram(v, model=vgm(model='Exp', range=2000, width=9.38, nugget=0.001, psill=0.2))

# london
#v.fit <- fit.variogram(v, model=vgm(model='Per', range=20000, nugget=0.1, psill=0.2))
plot(v, model=v.fit, as.table=TRUE)



g <- gstat(g, id="kval", model=v.fit )
p <- predict(g, model=v.fit, newdata=clip_grid)


#proj4string(grd) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")

#par(mar=c(2,2,2,2))
#image(p, col=terrain.colors(20))
#x <- contour(p)
#points(ks_proj1, pch=4, cex=0.5)
#title('OK Prediction')

#write.csv(p, file="colin_test.csv")


## now expand to a grid with 100 meter spacing:
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=100), y=seq(from=y.range[1], to=y.range[2], by=100) )
## convert to SpatialPixel class
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE
proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
spTransform(shape, CRS(latlon))


###########  Create irregular grid to interpolate
ind85grid <- rbind(ind85[,c(1:3)], c(112000, 5000, 0), c(310000, 35000, 0))

sp.ind85 <- ind85grid
coordinates(sp.ind85) = ~x+y

ind85.grid <- spsample(sp.ind85, type="regular", cellsize = c(800,800))

ind85.grid  <- as.data.frame(ind85.grid)
colnames(ind85.grid) <- c("x", "y")

library(shapefiles)
poli<-read.shapefile("shape/nyc")
poli<-convert.to.simple(poli$shp)
poli<-poli[,-1]
poli <- as.matrix(poli)

grd1 <- as.data.frame(grd)
## data.frame
grd.i <- grd1[inout(grd1,poli),]


coordinates(grd.i) <- ~ x+y
gridded(grd.i) <- TRUE
#proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
proj4string(grd) <- CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# Then you can convert it to a sp object:

grd.ir <- SpatialPoints(grd.i[,c("x","y")],proj4string=CRS(latlong))

gridded(grd.irregular) <- TRUE
grid.ir = spTransform(grd.ir, CRS("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

###############


## lattice graphics: thanks for R. Bivand's advice on this
## 
## alternatively plot quantiles with
## ... col.regions=terrain.colors(6), cuts=quantile(p$elev.pred) ...
##
#pts <- list("sp.points", ks_proj1, pch = 4, col = "black", cex=0.5)
jpeg (filename="nyc_ok.jpg", width=1200,
      height=700, type="cairo")
pdf("nyc_ok_to.pdf", width=7, height=12)
pts <- list("sp.points", ks_proj, pch = 4, col = "black", cex=0.5)
#spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=terrain.colors(20), cuts=19, sp.layout=list(pts), contour=TRUE, labels=FALSE, pretty=TRUE, col='brown', main='OK Prediction')
spplot(p, zcol="kval.pred", scales = list(draw = T), col.regions=heat.colors(50, 1),cuts=50, sp.layout=list(pts), contour=F, labels=FALSE, pretty=T, col='brown', main='NYC OK Prediction')


## plot the kriging variance as well
spplot(p, zcol='kval.var', col.regions=heat.colors(100), cuts=99, main='OK Variance',sp.layout=list(pts) )
