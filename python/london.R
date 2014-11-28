library(ggplot2)
# cols
#lon_fc <- read.csv("../results/london_from_cols.csv", row.names=1)
lon_tc <- read.csv("../results/london_to_cols.csv", row.names=1)
# rows
#lon_fr <- read.csv("../results/london_from_rows.csv", row.names=1)
lon_tr <- read.csv("../results/london_to_rows.csv", row.names=1)
# distance matrix
lon_d <- NULL
lon_d <- read.csv("../data/london_d_matrix.csv", skip=1, row.names=1)
lon_d <- lon_d[-c(1),] # remove empty row

#lfc <- data.matrix(lon_fc)
ltc <- data.matrix(lon_tc)

#lfr <- data.matrix(lon_fr)
ltr <- data.matrix(lon_tr)

ldm <- data.matrix(lon_d)
#ldm[ldm==0] <- NA
#ldm2 <- as.vector(c(ldm))
#ldm2 <- na.omit(ldm2)
#ldm_log <- log10(ldm2)

r <- list ()
r [[1]] <- ltc
r [[2]] <- ltr
jpeg (filename="../results/al/lon_r2plots1.jpg", width=1200,
      height=700, type="cairo")
par (mfrow=c(1,2))
mts <- c ("cols", "rows")

for (i in 1:2) {
	print ("This is begin of model")
	dvec <- as.vector (ldm[upper.tri(ldm)])
	#r <- as.vector (ntr[upper.tri(ntr)])
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




#r <- c(ltc)
#d <- ldm_log
dvec <- as.vector (ldm[upper.tri(ldm)])
r <- as.vector (ltc[upper.tri(ltc)])

#r <- c(ntc)
#d <- ndm_log
indx <- which (!is.na (dvec) & !is.na (r) & dvec > 0)
dvec <- dvec [indx]
r <- r [indx]
#c <- 2
#data7 <- data.frame(r ,d)
	#mod <- nls (rvec ~ cc + a * exp (-(d-min(d))^2 / k^2), 
d <- log10(dvec)

#data7 <- data.frame(r,d)
ltc.mod <- nls(r ~ c + a * exp(-(d-min(d))^2 / k^2), start=list(a=min(d), c=0.2, k=1))
#mod <- nls (r ~ cc + a * exp (-(d-min(d))^2 / k^2), 
#            start=list(cc=0,a=-1.508638,k=1))

jpeg (filename="../results/al/lon_r2_tc_fplots.jpg", width=1200,
      height=700, type="cairo")
plot (dvec, r, pch=1, col="grey", log="x",
      xlab="Distance (km)", ylab=expression (paste (R^2)))
dfit <- seq (min (d, na.rm=TRUE), max (d, na.rm=TRUE), length.out=100)
rfit <- predict (ltc.mod, newdata=data.frame(d=dfit))
lines (10 ^ dfit, rfit, col="red", lwd=2)
k <- 10 ^ summary (ltc.mod)$parameters [3]
title (main=paste ("London ToC: k = ",
                   formatC(k, format="f", digits=2), " km", sep=""))
dev.off()




############
dvec <- as.vector (ldm[upper.tri(ldm)])
r <- as.vector (ltr[upper.tri(ltr)])

#r <- c(ntc)
#d <- ndm_log
indx <- which (!is.na (dvec) & !is.na (r) & dvec > 0)
dvec <- dvec [indx]
r <- r [indx]
#c <- 2
#data7 <- data.frame(r ,d)
	#mod <- nls (rvec ~ cc + a * exp (-(d-min(d))^2 / k^2), 
d <- log10(dvec)

#data7 <- data.frame(r,d)
ltr.mod <- nls(r ~ c + a * exp(-(d-min(d))^2 / k^2), start=list(a=min(d), c=0, k=1))
#mod <- nls (r ~ cc + a * exp (-(d-min(d))^2 / k^2), 
#            start=list(cc=0,a=-1.508638,k=1))

#jpeg (filename="../results/al/lon_r2_tr_fplots.jpg", width=1200,
#      height=700, type="cairo")
plot (dvec, r, pch=1, col="grey", log="x",
      xlab="Distance (km)", ylab=expression (paste (R^2)))
dfit <- seq (min (d, na.rm=TRUE), max (d, na.rm=TRUE), length.out=100)
rfit <- predict (ltc.mod, newdata=data.frame(d=dfit))
lines (10 ^ dfit, rfit, col="red", lwd=2)
k <- 10 ^ summary (ltr.mod)$parameters [3]
title (main=paste ("London To Rows: k = ",
                   formatC(k, format="f", digits=2), " km", sep=""))
dev.off()








r <- c(ltr)
data8 <- data.frame(r,d)
ltr.mod <- nls(r ~ c + exp(-d^2 / k^2), start=list(c=0, k=1), data=data8)

r <- c(lfc)
data9 <- data.frame(r,d)
lfc.mod <- nls(r ~ cc + a * exp(-(d-min(d, na.rm=TRUE))^2 / k^2), start=list(a=min(d, na.rm = TRUE), cc=0, k=1), data=data9)

r <- c(lfr)
data10 <- data.frame(r,d)
lfr.mod <- nls(r ~ c + exp(-d^2 / k^2), start=list(c=0, k=1), data=data10)

coef(ltc.mod)[2]
coef(ltr.mod)[2]
coef(lfc.mod)[2]
coef(lfr.mod)[2]

ltc.mod <- nls(r ~ c + exp(-na.omit(d)^2 / k^2), start=list(c=10, k=1))





plot(r, d)
plot(d,r, main = "nls(*), data, true function and fit, n=100")
lines(d,predict(mod))

ggg + geom_smooth(color="green", method = "nls" ,formula = y ~ c + exp(-(log10(x))^2 / k^2), start=list(c=0, k=1), se=FALSE)
ggg + geom_line(aes(10 ^ dfit, rfit))
dfit1 <- 10^dfit
df <- data.frame(dfit1, rfit)
ggplot(df) + geom_line(aes(dfit1, rfit))

data1 <- data.frame(c(ltc) ,ldm2)
ggg <- ggplot(data1, aes(ldm2, c.ltc.)) +
	geom_point(alpha=0.2, size=1.5) + 
	#geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("London To All Stations - Columns") + theme_bw()

ggg + stat_smooth(color="green", method = "nls" ,formula = y ~ c + exp(-(log10(x))^2 / k^2), start=list(c=0, k=1), se=FALSE)

ggg + stat_smooth(color="red", method = "nls" ,formula = y ~ c + exp(-(log10(x))^2 / k^2), start=list(c=0, k=1), se=FALSE)

ggsave("../results/al/london_to_cols.png", width=6, height=4, dpi=300)
dev.off()

data2 <- data.frame(c(ltr) ,ldm2)
ggplot(data2, aes(ldm2, c.ltr.)) +
	geom_point(alpha=0.2, size=1.5) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("London To All Stations - Rows") + theme_bw()
ggsave("../results/al/london_to_rows.png", width=6, height=4, dpi=300)
dev.off()

data3 <- data.frame(c(lfc) ,ldm2)
ggg <- ggplot(data3, aes(ldm2, c.lfc.)) +
	#geom_point(alpha=0.2, size=1.5) + 
	#geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("London From All Stations - Cols") + theme_bw()
ggsave("../results/al/london_from_cols.png", width=6, height=4, dpi=300)
dev.off()

data4 <- data.frame(c(lfr) ,ldm2)
ggplot(data4, aes(ldm2, c.lfr.)) +
	geom_point(alpha=0.2, size=1.5) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("London From All Stations - Rows") + theme_bw()
ggsave("../results/al/london_from_rows.png", width=6, height=4, dpi=300)
dev.off()

