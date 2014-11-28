library(ggplot2)
# cols
nyc_fc <- read.csv("../results/nyc_from_cols.csv", row.names=1)
nyc_tc <- read.csv("../results/nyc_to_cols.csv", row.names=1)
# rows
nyc_fr <- read.csv("../results/nyc_from_rows.csv", row.names=1)
nyc_tr <- read.csv("../results/nyc_to_rows.csv", row.names=1)
# distance matrix
nyc_d <- read.csv("../data/nyc_d_matrix.csv", skip=1, row.names=1)
nyc_d <- nyc_d[-c(1),] # remove empty start_id row

nfc <- data.matrix(nyc_fc)
ntc <- data.matrix(nyc_tc)

ntr <- data.matrix(nyc_tr)
nfr <- data.matrix(nyc_fr)

ndm <- data.matrix(nyc_d)

#ntc <- data.matrix(nyc_tc1)
#ndm <- data.matrix(nyc_d[-1,-1])

#ndm2 <- log1p(as.vector(c(ndm)))
ndm2 <- as.vector(c(ndm))
ndm_log <- log10(ndm2)
#Ndata <- data.frame(ntc,ndm_log)

dvec <- as.vector (ndm[upper.tri(ndm)])
#r <- as.vector (ntr[upper.tri(ntr)])
r <- as.vector (ntc[upper.tri(ntc)])

#r <- c(ntc)
#d <- ndm_log
indx <- which (!is.na (dvec) & !is.na (r) & dvec > 0)
dvec <- dvec [indx]
r <- r [indx]
#c <- 2
#data7 <- data.frame(r ,d)
	#mod <- nls (rvec ~ cc + a * exp (-(d-min(d))^2 / k^2), 
d <- log10(dvec)


ntc.mod <- nls(r ~ c + a * exp(-(d-min(d))^2 / k^2), start=list(c=0, a=min(d), k=1))
jpeg (filename="../results/al/nyc_r2_tc_plots.jpg", width=1200,
      height=700, type="cairo")
	  
plot (dvec, r, pch=1, col="grey", log="x",
      xlab="Distance (km)", ylab=expression (paste (R^2)))
dfit <- seq (min (d, na.rm=TRUE), max (d, na.rm=TRUE), length.out=100)
rfit <- predict (ntc.mod, newdata=data.frame(d=dfit))

lines (10 ^ dfit, rfit, col="blue", lwd=2)

k <- 10 ^ summary (ntc.mod)$parameters [3]
title (main=paste ("NYC To Cols: k = ",
                   formatC(k, format="f", digits=2), " km", sep=""))
dev.off()



dvec <- as.vector (ndm[upper.tri(ndm)])
r <- as.vector (ntr[upper.tri(ntr)])
indx <- which (!is.na (dvec) & !is.na (r) & dvec > 0)
dvec <- dvec [indx]
r <- r [indx]
d <- log10(dvec)

ntr.mod <- nls(r ~ c + a * exp(-(d-min(d))^2 / k^2), start=list(c=0, a=min(d), k=1))
jpeg (filename="../results/al/nyc_r2_tr_plots.jpg", width=1200,
      height=700, type="cairo")
	  
plot (dvec, r, pch=1, col="grey", log="x",
      xlab="Distance (km)", ylab=expression (paste (R^2)))
dfit <- seq (min (d, na.rm=TRUE), max (d, na.rm=TRUE), length.out=100)
rfit <- predict (ntr.mod, newdata=data.frame(d=dfit))

lines (10 ^ dfit, rfit, col="blue", lwd=2)

k <- 10 ^ summary (ntr.mod)$parameters [3]
title (main=paste ("NYC To Rows: k = ",
                   formatC(k, format="f", digits=2), " km", sep=""))
dev.off()
				   
				   
r <- c(ntr)
data8 <- data.frame(r,d)
ntr.mod <- nls(r ~ c + exp(-d^2 / k^2), start=list(c=0, k=1), data=data8)

r <- c(nfc)
data9 <- data.frame(r,d)
nfc.mod <- nls(r ~ c + exp(-d^2 / k^2), start=list(c=0, k=1), data=data9)

r <- c(nfr)
data10 <- data.frame(r,d)
nfr.mod <- nls(r ~ c + exp(-d^2 / k^2), start=list(c=0, k=1), data=data10)

coef(ntc.mod)[2]
coef(ntr.mod)[2]
coef(nfc.mod)[2]
coef(nfr.mod)[2]


#a<-coef(mod)[1]; b<-coef(mod)[2]
#curve(a*x^b, col='red', add=T)

#x + stat_smooth(method = "nls" ,formula = c.ntc. ~ c + exp(-ndm_log^2 / k^2), start=list(c=0, k=1), se=FALSE)

ggg + stat_smooth(color="green", method = "nls" ,formula = y ~ c + a * exp(-(log10(x) - min(log10(x)))^2 / k^2), start=list(a=0.2,c=0, k=1), se=FALSE)

Ndata1 <- data.frame(c(ntc) ,ndm2)
ggg <- ggplot(Ndata1, aes(ndm2, c.ntc.)) +
	geom_point(alpha=0.2, size=1.5) + 
	geom_smooth(color="red") + 
	#geom_smooth(color="red", method = 'nls', formula = mod, se = FALSE) + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC To All Stations - Columns") + theme_bw()
ggsave("../results/al/nyc_to_cols.png", width=6, height=4, dpi=300)
dev.off()

Ndata2 <- data.frame(c(ntr) ,ndm2)
ggplot(Ndata2, aes(ndm2, c.ntr.)) +
	geom_point(alpha=0.2, size=1.5) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC To All Stations - Rows") + theme_bw()
ggsave("../results/al/nyc_to_rows.png", width=6, height=4, dpi=300)
dev.off()

Ndata3 <- data.frame(c(nfc) ,ndm2)
fff <- ggplot(Ndata3, aes(ndm2, c.nfc.)) +
	geom_point(alpha=0.2, size=1.5) + 
	#geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC From All Stations - Columns") + theme_bw()
fff + stat_smooth(color="green", method = "nls" ,formula = y ~ c + exp(-(log10(x))^2 / k^2), start=list(c=0, k=1), se=FALSE)

ggsave("../results/al/nyc_from_cols.png", width=6, height=4, dpi=300)
dev.off()

Ndata4 <- data.frame(c(nfr) ,ndm2)
ggplot(Ndata4, aes(ndm2, c.nfr.)) +
	geom_point(alpha=0.2, size=1.5) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC From All Stations - Rows") + theme_bw()
ggsave("../results/al/nyc_from_rows.png", width=6, height=4, dpi=300)
dev.off()

#grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
#ggsave("../results/anal/nyc_to.png")
#dev.off()