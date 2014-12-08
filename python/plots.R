plotR2 <- function ()
{
    # Correlations only exist for London at present
    r <- list ()
    r [[1]] <- read.csv ("../results/r2from.csv", header=FALSE)
    r [[2]] <- read.csv ("../results/r2to.csv", header=FALSE)
    stopifnot (dim (r [[1]])[1] == dim (r [[1]])[2])
    stopifnot (dim (r [[2]])[1] == dim (r [[2]]))
    stopifnot (dim (r [[1]])[1] == dim (r [[2]])[1])
    nstns <- dim (r [[1]])[1]
    dists <- read.csv ("../results/station_dists_london.txt", header=FALSE)
    stopifnot (max (dists [,1:2]) <= nstns)

    jpeg (filename="r2plots.jpg", width=1200,
          height=700, type="cairo")
    par (mfrow=c(1,2))
    mts <- c ("from", "to")

    for (i in 1:2) {
        # dists are in flat index columns 
        darr <- array (NA, dim=c(nstns, nstns))
        indx <- (dists [,2] - 1) * nstns + dists [,1]
        darr [indx] <- dists [,3]
        dvec <- as.vector (darr [upper.tri (darr)])
        rvec <- as.vector (r [[i]] [upper.tri (r [[i]])])
        indx <- which (!is.na (dvec) & !is.na (rvec) & dvec > 0)
        dvec <- dvec [indx]
        rvec <- rvec [indx] ^ 2
        
        plot (dvec, rvec, pch=1, col="grey", log="x",
              xlab="Distance (km)", ylab=expression (paste (R^2)))
        d <- log10 (dvec)
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
}
plotR2 ()
