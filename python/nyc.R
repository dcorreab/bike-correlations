library(ggplot2)
# cols
nyc_tc <- read.csv("../results/nyc_to_cols.csv", row.names=1)
# rows
nyc_tr <- read.csv("../results/nyc_to_rows.csv", row.names=1)
# distance matrix
nyc_d <- read.csv("../data/nyc_d_matrix.csv", skip=1, row.names=1)
nyc_d <- nyc_d[-c(1),] # remove empty start_id row


ntc <- data.matrix(nyc_tc)
ntr <- data.matrix(nyc_tr)
ndm <- data.matrix(nyc_d)

r <- list ()
r [[1]] <- ntc
r [[2]] <- ntr
jpeg (filename="../results/al/nyc_r2plots.jpg", width=1200,
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
                start=list(cc=0,a=0.2,k=-1))
    
	dfit <- seq (min (d), max (d), length.out=100)
    rfit <- predict (mod, newdata=data.frame(d=dfit))
    lines (10 ^ dfit, rfit, col="black", lwd=2)
    k <- 10 ^ summary (mod)$parameters [3]
    title (main=paste (mts[i], ": k = ",
                       formatC(k, format="f", digits=2), " km", sep=""))
}
dev.off()