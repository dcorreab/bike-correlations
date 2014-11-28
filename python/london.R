library(ggplot2)
# cols
lon_tc <- read.csv("../results/london_to_cols.csv", row.names=1)
# rows
lon_tr <- read.csv("../results/london_to_rows.csv", row.names=1)
# distance matrix
lon_d <- read.csv("../data/london_d_matrix.csv", skip=1, row.names=1)
lon_d <- lon_d[-c(1),] # remove empty row


ltc <- data.matrix(lon_tc)
ltr <- data.matrix(lon_tr)
ldm <- data.matrix(lon_d)

r <- list ()
r [[1]] <- ltc
r [[2]] <- ltr
jpeg (filename="../results/al/lon_r2plots1.jpg", width=1200,
      height=700, type="cairo")
par (mfrow=c(1,2))
mts <- c ("cols", "rows")

for (i in 1:2) {
	print ("This is beginning  of model")
	dvec <- as.vector (ldm[upper.tri(ldm)])
	rvec <- as.vector (r [[i]] [upper.tri(r [[i]])])
	indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
	dvec <- dvec [indx]
	rvec <- rvec [indx]
	
	d <- log10(dvec)
	print (max(rvec))
	#nls(rvec ~ cc + a * exp(-((d - min(d)/k)^2)
	mod <- nls (rvec ~ cc + a * expm1(-((d - min(d)/k)^2)), start=list(cc=0, a=max(rvec) ,k=-0.9), trace=T)
    #mod <- nls (rvec ~ cc + a * exp (-(d-min(d))^2 / k^2), start=list(cc=0,a=0.3,k=1), trace=T)
    plot (dvec, rvec, pch=1, col="grey", log="x", ylim = range(c(r[[1]],r[[2]]), na.rm=T),
          xlab="Distance (km)", ylab=expression (paste (R^2)))
	dfit <- seq (min (d), max (d), length.out=100)
    rfit <- predict (mod, newdata=data.frame(d=dfit))
    lines (10 ^ dfit, rfit, col="blue", lwd=2)
    k <- 10 ^ summary (mod)$parameters [3]
	print ('k = ', k)
    title (main=paste (mts[i], ": k = ",
                       formatC(k, format="f", digits=2), " km", sep=""))
}
dev.off()
