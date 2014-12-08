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

ntr_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(ntr, i, 0.7)
	ntr_k <- c(ntr_k, x)
}

nfc_k <- numeric()
for (i in 1:332) {
	x <- mod1.nls(nfc, i, 0.7)
	nfc_k <- c(nfc_k, x)
}

names(nyc_tc)

station_names <- names(nyc_tc)

sn <- gsub("X", "", paste(station_names))

nyc_sts <- read.csv("../data/station_latlons_nyc.txt")
tk <- data.frame(id=sn, kval=ntc_k)
hhh <- merge(tk, nyc_sts, by ="id")

b1 = bubble(hhh, "kval", maxsize = 1.5, main = "kvalue per station",
	key.entries = 2^(-1:4))

gg <- sapply(station_names, function(x){unlist(strsplit(x, split='X', fixed=TRUE))[2]})
unlist(strsplit(names(nyc_tc), split='X', fixed=TRUE))[2]


max(ntc_k)
max(ntr_k)
max(nfc_k)
max(ntr_k)

min(ntc_k)
min(ntr_k)
min(nfc_k)
min(ntr_k)


