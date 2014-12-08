library(sp)
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

max(ltc_k)
max(ltr_k)
max(lfc_k)
max(lfr_k)


station_names <- names(lon_tc)
sn <- gsub("X", "", paste(station_names))

lon_sts <- read.csv("../data/station_latlons_london.txt")
tk <- data.frame(id=sn, kval=ltc_k)
tk1 <- data.frame(id=sn, kval=ltr_k)
fk1 <- data.frame(id=sn, kval=lfr_k)
fk <- data.frame(id=sn, kval=lfc_k)	

width=12, height=6
pdf (file="lon_to_c_kplots.pdf", width=12, height=6)
ks <- merge(tk, lon_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
latlong = "+init=epsg:4326"
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
ks <- merge(fk, lon_sts, by ="id")
coordinates(ks)= ~ long+lat
proj4string(ks) <- CRS(latlong)
latlong = "+init=epsg:4326"
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


tk <- data.frame(id=sn, kval=ltc_k)
tks <- merge(tk, lon_sts, by ="id")
coordinates(tks)= ~ long+lat
proj4string(tks) <- CRS(latlong)

cb <- as.geodata(tks)


lon_tr1 <- lon_tr[10:11,]
lon_d1 <- lon_d[10:11,]


ltc <- data.matrix(lon_tc)
ltr <- data.matrix(lon_tr1)
ldm <- data.matrix(lon_d1)