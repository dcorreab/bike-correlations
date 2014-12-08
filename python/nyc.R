library(ggplot2)
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
	print(i)
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
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.6
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.3
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=2
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=1
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=1
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.1,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=1.2
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.01
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.01010101
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	"if (is.null(mod)) {
		ks=0.5
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.5
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.5
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.5,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.02
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.1,k=ks), trace=F), error=function(e) NULL)
	}
	if (is.null(mod)) {
		ks=0.02
		print(ks)
		mod <- tryCatch(nls (rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.2,k=ks), trace=F), error=function(e) NULL)
	}"
	k <- 10 ^ summary (mod)$parameters [3]
	#kv <- list(kval = k)
}
for (i in 1:332) {x <- mod1.nls(ntr, i, 0.7)}
	mod <- nls(rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=0.0101), trace=T)

mod <- tryCatch( nls(rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=), trace=F), error=function(e) NULL)

sapply(ff, function(x){
	mod <- tryCatch( nls(rvec ~ cc + a * exp(-((d - min(d)/k)^2)), start=list(cc=0, a=0.8,k=x[[x]]), trace=T), error=function(e) NULL)
})
function (x) {
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

max(ntc_k)
max(ntr_k)
max(nfc_k)
max(ntr_k)

min(ntc_k)
min(ntr_k)
min(nfc_k)
min(ntr_k)

r <- list ()
r [[1]] <- ntc
r [[2]] <- ntr
jpeg (filename="nyc_r2plots_to.jpg", width=1200,
      height=700, type="cairo")
par (mfrow=c(1,2))
mts <- c ("cols", "rows")

for (i in 1:2) {
	print ("This is beginning  of model")
	dvec <- as.vector (ndm[upper.tri(ndm)])
	rvec <- as.vector (r [[i]] [upper.tri(r [[i]])])
	indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
	dvec <- dvec [indx]
	rvec <- rvec [indx]
	
    plot (dvec, rvec, pch=1, col="grey", log="x", ylim = range(c(r[[1]],r[[2]]), na.rm=T),
          xlab="Distance (km)", ylab=expression (paste (R^2)))
		  
	d <- log10(dvec)
    mod <- nls (rvec ~ cc + a * exp (-(d-min(d))^2 / k^2), 
                start=list(cc=0,a=0.2,k=1))
    
	dfit <- seq (min (d), max (d), length.out=100)
    rfit <- predict (mod, newdata=data.frame(d=dfit))
    lines (10 ^ dfit, rfit, col="black", lwd=2)
    k <- 10 ^ summary (mod)$parameters [3]
    title (main=paste (mts[i], ": k = ",
                       formatC(k, format="f", digits=2), " km", sep=""))
}
dev.off()