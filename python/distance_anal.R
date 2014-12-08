# cols
lon_fc <- read.csv("../results/london_from_cols.csv", row.names=1)
lon_tc <- read.csv("../results/london_to_cols.csv", row.names=1)
# rows
lon_fr <- read.csv("../results/london_from_rows.csv", row.names=1)
lon_tr <- read.csv("../results/london_to_rows.csv", row.names=1)
# distance matrix
lon_d <- read.csv("../data/london_d_matrix.csv", skip=1, row.names=1)
lon_d <- lon_d[-c(1),] # remove empty row

xf <- data.matrix(lon_fc)
xt <- data.matrix(lon_tc)

yt <- data.matrix(lon_tr)
yf <- data.matrix(lon_fr)

dm <- data.matrix(lon_d)

Data <- data.frame(xt ,dm)
plot(xt ~ dm, xlab='Correlations - R2', ylab = 'Station Distances (km)', main="NYC CitiBike - Trips To Columns Only")

#make into simple vectors
dm1 <- log10(dm)
xt1 <- c(xt)
dm1 <- c(dm1)
xf1 <- c(xf)
Data <- data.frame(xt1,dm3)
Data1 <- data.frame(xf1,dm3)
ggplot(Data, aes(dm3, xt1)) + geom_point() + geom_smooth() + scale_x_continuous(trans="log1p", limits=c(NA,1))
ggplot(Data, aes(dm3, xt1)) + geom_point() + geom_smooth() + scale_x_continuous()

dm3 <- log1p(as.vector(dm)) # scale distance using log. Zero remains zero.
lon_from_col = ggplot(Data, aes(dm3, xf1)) + geom_point(alpha=0.2) + geom_smooth() + scale_x_continuous()
lon_from_col + scale_y_continuous("Correlations R2") + ggtitle("London From Columns")


print("Plotting London")
print("Plotting to cols")
png("../results/lon_to_cols_dist.png",width=280, height=210, units="mm", res=300)
plot(xt, dm, xlab='Correlations - R2', ylab = 'Station Distances (km)', 
		main="London CitiBike - Trips To Columns Only")
dev.off()

print("Plotting to Rows")
png("../results/lon_to_rows_dist.png",width=280, height=210, units="mm", res=300)
plot(yt, dm, xlab='Correlations - R2', ylab = 'Station Distances (km)', 
		main="London CitiBike - Trips To Rows Only")
dev.off()

print("Plotting from cols")
png("../results/lon_from_cols_dist.png",width=280, height=210, units="mm", res=300)
plot(xf, dm, xlab='Correlations - R2', ylab = 'Station Distances (km)', 
		main="London CitiBike - Trips From Columns Only")
dev.off()

print("Plotting to rows")
png("../results/lon_from_rows_dist.png",width=280, height=210, units="mm", res=300)
plot(yf, dm, xlab='Correlations - R2', ylab = 'Station Distances (km)', 
		main="London CitiBike - Trips From Rows Only")
dev.off()


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

ndm2 <- log1p(as.vector(c(ndm)))
Ndata <- data.frame(c(ntc) ,ndm2)


Ndata1 <- data.frame(c(ntc) ,ndm2)
p1 <- ggplot(Ndata1, aes(ndm2, c.ntc.)) +
	geom_point(alpha=0.2) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC To All Stations - Columns") + theme_bw()
ggsave("../results/nyc_to_cols.pdf")
dev.off()

Ndata2 <- data.frame(c(ntr) ,ndm2)
p2 <- ggplot(Ndata2, aes(ndm2, c.ntr.)) +
	geom_point(alpha=0.2) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC To All Stations - Rows") + theme_bw()
ggsave("../results/nyc_to_rows.pdf")
dev.off()

Ndata3 <- data.frame(c(nfc) ,ndm2)
p3 <- ggplot(Ndata3, aes(ndm2, c.nfc.)) +
	geom_point(alpha=0.2) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC From All Stations - Columns") + theme_bw()

ggsave("../results/nyc_from_cols.pdf")
dev.off()

Ndata4 <- data.frame(c(nfr) ,ndm2)
p4 <- ggplot(Ndata4, aes(ndm2, c.nfr.)) +
	geom_point(alpha=0.2) + 
	geom_smooth(color="red") + 
	scale_x_continuous() + 
	xlab("Distance") + 
	ylab("Correlation R2") + 
	ggtitle("NYC From All Stations - Rows") + theme_bw()
ggsave("../results/nyc_from_rows.pdf")
dev.off()

grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)
ggsave("../results/nyc_to.pdf")
dev.off()

fit = lm(ntc ~ ndm1, Data)

g1<-ggplot(Data, aes(ntc)) + geom_histogram() 
p1<-ggplot(Data, aes(X72)) + geom_density()

fit.loess = loess.smooth(ntc, ndm1, evaluation = eval.length, family=”gaussian”, span=.75, degree=1)
ggplot(Data, aes(x=ntc, y=ndm1)) + geom_point(shape=1)      # Use hollow circles

ggplot(Data, aes(x=X79, y=X303)) + 


plot(xt ~ ndm1, xlab='Correlations - R2', ylab = 'Station Distances (km)', main="NYC CitiBike - Trips To Columns Only")
loess_fit <- loess(ntc ~ ndm)
nls_fit <- nls(ntc ~ a + b * ndm^(-c), Data, start = list(a = 1, b = 0.4, 
    c = 0))


print("Plotting NYC")
print("Plotting to cols")
png("../results/nyc_to_cols_dist.png",width=280, height=210, units="mm", res=300)
plot(ntc, ndm1, xlab='Correlations - R2', ylab = 'Station Distances (km)', main="NYC CitiBike - Trips To Columns Only")
dev.off()

print("Plotting from cols")
png("../results/nyc_from_cols_dist.png",width=280, height=210, units="mm", res=300)
plot(nfc, ndm, xlab='Correlations - R2', ylab = 'Station Distances (km)', 
		main="NYC CitiBike - Trips From Columns Only")
dev.off()

print("Plotting to rows")
png("../results/nyc_to_rows_dist.png",width=280, height=210, units="mm", res=300)
plot(ntr, ndm, xlab='Correlations - R2', ylab = 'Station Distances (km)', main="NYC CitiBike - Trips To Rows Only")
dev.off()

print("Plotting from rows")
png("../results/nyc_from_rows_dist.png",width=280, height=210, units="mm", res=300)
plot(nfr, ndm, xlab='Correlations - R2', ylab = 'Station Distances (km)', main="NYC CitiBike - Trips From Rows Only")
dev.off()
