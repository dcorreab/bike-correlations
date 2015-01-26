library(sp)
library(rgdal)
library(gstat)
library(automap)
library(ggmap)
library(gridExtra)
library(plyr)
day=commandArgs(TRUE)[1]

make_bubbles <- function(day,dayf,i, kvals){
	xr<-bbexpand(range(mon$long), 0.1)
	yr<-bbexpand(range(mon$lat),0.1)
	q = seq(0,1,0.1)
	brks <- quantile(kvals,q)
	brks <- brks[0:10] # remove highest value
	coordinates(day)= ~ long+lat
	latlong = "+init=epsg:4326"
	proj4string(day) <- CRS(latlong)
	
	b1<-bubble(day, "kval", maxsize = 3, do.sqrt = F, 
			key.entries = brks, scales = list(draw = F), xlim=xr, ylim=yr)
			
			
	coordinates(dayf)= ~ long+lat
	latlong = "+init=epsg:4326"
	proj4string(dayf) <- CRS(latlong)
	b2<-bubble(dayf, "kval", maxsize = 3,do.sqrt = F,
			key.entries = (0:10), scales = list(draw = T), xlim=xr, ylim=yr)
	png (file=paste("../results/kvals/jpeg/",i,"_nyc_k.png",sep=""), width=10, height=10, type="cairo", res=100, units="in")
	print(b1, split=c(1,1,2,1), more=T)
	print(b2, split=c(2,1,2,1), more=F)
	dev.off()
}


g_legend<-function(p){
tmp <- ggplotGrob(p)
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]
return(legend)}


#read in the nyc shapes
#ogrListLayers("../data/shape/nyc1.shp") #will show you available layers for the above dataset
#shape=readOGR("../data/shape/nyc1.shp", layer="nyc1") #will load the shapefile to your dataset.
ogrListLayers("../data/study_area/study_area_neighbourhood_boundaries.shp") #will show you available layers for the above dataset
shape=readOGR("../data/study_area/study_area_neighbourhood_boundaries.shp", layer="study_area_neighbourhood_boundaries") #will load the shapefile to your dataset.
latlong = "+init=epsg:4326"
shape_p <- spTransform(shape, CRS(latlong))
shape_p@data$id = rownames(shape_p@data)
shape_p.points = fortify(shape_p, region="id")
names(shape_p.points) <- c("long", "lat","order","hole","piece","group","NTACode")
shape_p.df = join(shape_p.points, shape_p@data, by="NTACode")


make_plot <- function(day,dayf,i,g) {
	yl <- bbexpand(range(day$lat), 0.1)
	xl <- bbexpand(range(day$long), 0.1)
	nyc_boros <- geom_path(data=shape_p.df, aes(long,lat, group=group), colour="grey75", linetype = 2)
	xlim <- xlim(xl[1], xl[2])
	ylim <- ylim(yl[1], yl[2])
	k_size <- scale_size_continuous(name="K-Value (km)",range=c(1,10), limits=c(0,20), breaks = c(0,3, 5, 7, 9, 11,13, 20, 25), labels=c(0,"< 3", 5, 7, 9, 11,13, 20, "25 +"))
	k_colour <- scale_color_gradient(name="K-Value (km)", high="darkorchid3",low = "orange",limits=c(0,20), breaks = c(0,3, 5, 7, 9, 11,13, 20, 25),labels=c(0,"< 3", 5, 7, 9, 11,13, 20, "25 +"))

	xx <- ggplot(data=day, aes(x=long, y=lat)) + xlim + ylim + nyc_boros +
	  		geom_point(aes(size=kval, color=kval),alpha=0.6) +
	  	  	k_size + k_colour +
			theme(legend.direction="horizontal") + ggtitle("To") + theme(panel.background = element_blank()) +
			guides(colour = guide_legend("K-Value (km)"), size = guide_legend("K-Value (km)"))
  	  	
	legend <- g_legend(xx)
	lwidth <- sum(legend$width)

	yy <- ggplot(data=dayf, aes(x=long, y=lat)) + xlim + ylim + nyc_boros +
	  		geom_point(aes(size=kval, color=kval),alpha=0.6) +
	  	  	k_size + k_colour + theme(panel.background = element_blank()) +
			ggtitle("From")

	png (file=paste("../results/kvals/jpeg/t/",i,"_nyc_k.png",sep=""), width=12, height=10, type="cairo", res=150, units="in")
		#grid.arrange(xx,yy,ncol=2, main=paste(unlist(strsplit(i, "_"))[2], "From / To", sep=" "), legend, 
	     #                widths=unit.c(unit(1, "npc") - lwidth, lwidth), )				 
					 
		 grid.arrange(arrangeGrob(xx + theme(legend.position = "none"),
			 yy + theme(legend.position = "none"), ncol=2),
			 arrangeGrob(
			 legend,
			 #widths=c(1,1.2),
			 widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
			 nrow=1),
			 nrow=2, heights=c(6/7,1/7),
			 main=textGrob(paste(unlist(strsplit(i, "_"))[2], sep=" "), gp=gpar(cex=3), just="top")
			 )
		dev.off()
}

make_plot1 <- function(day,i,g) {
yl <- bbexpand(range(day$lat), 0.1)
xl <- bbexpand(range(day$long), 0.1)
nyc_boros <- geom_path(data=shape_p.df, aes(long,lat, group=group), colour="grey75", linetype = 2)
xlim <- xlim(xl[1], xl[2])
ylim <- ylim(yl[1], yl[2])
k_size <- scale_size_continuous(name="K-Value (km)",range=c(1,10), limits=c(0,20), breaks = c(0,3, 5, 7, 9, 11,13, 20, 25), labels=c(0,"< 3", 5, 7, 9, 11,13, 20, "25 +"))
k_colour <- scale_color_gradient(name="K-Value (km)", high="darkorchid3",low = "orange",limits=c(0,20), breaks = c(0,3, 5, 7, 9, 11,13, 20, 25),labels=c(0,"< 3", 5, 7, 9, 11,13, 20, "25 +"))

xx <- ggplot(data=day, aes(x=long, y=lat)) + xlim + ylim + nyc_boros +
  		geom_point(aes(size=kval, color=kval),alpha=0.6) +
  	  	k_size + k_colour +
		theme(legend.direction="horizontal") + theme(panel.background = element_blank()) +
		guides(colour = guide_legend("K-Value (km)"), size = guide_legend("K-Value (km)"))
}

days <- c('sun')
if( day %in% days) {
	mon <- read.csv("../results/kvals/nyc_mon_tc_k.csv")
	tue <- read.csv("../results/kvals/nyc_tue_tc_k.csv")
	wed <- read.csv("../results/kvals/nyc_wed_tc_k.csv")
	thu <- read.csv("../results/kvals/nyc_thu_tc_k.csv")
	# remove station 232 index124
	thu <- thu[-c(124),]
	fri <- read.csv("../results/kvals/nyc_fri_tc_k.csv")
	sat <- read.csv("../results/kvals/nyc_sat_tc_k.csv")
	#sat <- sat[-221,]
	sun <- read.csv("../results/kvals/nyc_sun_tc_k.csv")
	monf <- read.csv("../results/kvals/nyc_mon_fc_k.csv")
	tuef <- read.csv("../results/kvals/nyc_tue_fc_k.csv")
	wedf <- read.csv("../results/kvals/nyc_wed_fc_k.csv")
	thuf <- read.csv("../results/kvals/nyc_thu_fc_k.csv")
	frif <- read.csv("../results/kvals/nyc_fri_fc_k.csv")
	satf <- read.csv("../results/kvals/nyc_sat_fc_k.csv")
	#satf <- satf[-221,] # remove the outliers K = 69
	sunf <- read.csv("../results/kvals/nyc_sun_fc_k.csv")
	#sunf <- sunf[-c(124,205,25,8),]
	sunf <- sunf[-c(25,8),]
	
	kvals = c(mon$kval, tue$kval, wed$kval, thu$kval, fri$kval, sat$kval, sun$kval,
				monf$kval, tuef$kval, wedf$kval, thuf$kval, frif$kval, satf$kval, sunf$kval)
			
		gg <- function(x){
				x[x$kval > 15,]
			}	
			gg(mon)
			gg(tue)
			gg(wed)
			gg(thu)
			gg(fri)
			gg(sat)
			gg(sun)
			
			gg(monf)
			gg(tuef)
			gg(wedf)
			gg(thuf)
			gg(frif)
			gg(satf)
			gg(sunf)
				
	
	m <- make_plot(mon,monf, '1_Monday', kvals)
	t <- make_plot(tue,tuef, '2_Tuesday', kvals)
	w <- make_plot(wed,wedf, '3_Wednesday',kvals)
	th <- make_plot(thu,thuf, '4_Thursday',kvals)
	f <- make_plot(fri,frif, '5_Friday',kvals)
	s <- make_plot(sat,satf, '6_Saturday',kvals)
	su <- make_plot(sun,sunf, '7_Sunday',kvals)
	
	m1 <- make_plot1(mon,'1_Monday', kvals)
	t1 <- make_plot1(tue,'2_Tuesday', kvals)
	w1 <- make_plot1(wed, '3_Wednesday',kvals)
	th1 <- make_plot1(thu, '4_Thursday',kvals)
	f1 <- make_plot1(fri, '5_Friday',kvals)
	s1 <- make_plot1(sat, '6_Saturday',kvals)
	su1 <- make_plot1(sun, '7_Sunday',kvals)
	
	mf1 <- make_plot1(monf,'1_Monday', kvals)
	tf1 <- make_plot1(tuef,'2_Tuesday', kvals)
	wf1 <- make_plot1(wedf, '3_Wednesday',kvals)
	thf1 <- make_plot1(thuf, '4_Thursday',kvals)
	ff1 <- make_plot1(frif, '5_Friday',kvals)
	sf1 <- make_plot1(satf, '6_Saturday',kvals)
	suf1 <- make_plot1(sunf, '7_Sunday',kvals)
	png (file=paste("../results/kvals/jpeg/nyc_weekdays_tok.png",sep=""), width=30, height=10, type="cairo", res=150, units="in")
	 	legend <- g_legend(m1)
	 	lwidth <- sum(legend$width)
		 grid.arrange(
			 arrangeGrob(m1 + theme(legend.position = "none"), t1 + theme(legend.position = "none"),
			 w1 + theme(legend.position = "none"), th1 + theme(legend.position = "none"), f1 + theme(legend.position = "none"),
			 ncol=5),
			 arrangeGrob(legend,
			 widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
			 nrow=1),
			 nrow=2, heights=c(6/7,1/7),
			 main=textGrob("Weekdays To (Mon - Fri)", gp=gpar(cex=2), just="top"))
		dev.off()
	png (file=paste("../results/kvals/jpeg/nyc_weekdays_fromk.png",sep=""), width=30, height=10, type="cairo", res=150, units="in")
	 	legend <- g_legend(m1)
	 	lwidth <- sum(legend$width)
		 grid.arrange(
			 arrangeGrob(mf1 + theme(legend.position = "none"), tf1 + theme(legend.position = "none"),
			 wf1 + theme(legend.position = "none"), thf1 + theme(legend.position = "none"), ff1 + theme(legend.position = "none"),
			 ncol=5),
			 arrangeGrob(legend,
			 widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
			 nrow=1),
			 nrow=2, heights=c(6/7,1/7),
			 main=textGrob("Weekdays From (Mon - Fri)", gp=gpar(cex=2), just="top"))
		dev.off()

	png (file=paste("../results/kvals/jpeg/nyc_weekends_tok.png",sep=""), width=12, height=10, type="cairo", res=150, units="in")
	 	legend <- g_legend(m1)
	 	lwidth <- sum(legend$width)
		 grid.arrange(
			 arrangeGrob(s1 + theme(legend.position = "none"), su1 + theme(legend.position = "none"),
			 ncol=2),
			 arrangeGrob(legend,
			 widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
			 nrow=1),
			 nrow=2, heights=c(6/7,1/7),
			 main=textGrob("Weekends To (Sat - Sun)", gp=gpar(cex=2), just="top"))
		dev.off()
	png (file=paste("../results/kvals/jpeg/nyc_weekends_fromk.png",sep=""), width=12, height=10, type="cairo", res=150, units="in")
	 	legend <- g_legend(m1)
	 	lwidth <- sum(legend$width)
		 grid.arrange(
			 arrangeGrob(sf1 + theme(legend.position = "none"), suf1 + theme(legend.position = "none"),
			 ncol=2),
			 arrangeGrob(legend,
			 widths=unit.c(unit(1, "npc") - lwidth, lwidth), 
			 nrow=1),
			 nrow=2, heights=c(6/7,1/7),
			 main=textGrob("Weekends From (Sat - Sun)", gp=gpar(cex=2), just="top"))
		dev.off()
	
} else {
	total_t <- read.csv("../results/kvals/nyc_total_tc_k.csv")
	total_f <- read.csv("../results/kvals/nyc_total_fc_k.csv")
	kvals = c(total_t$kval, total_f$kval)
	tt <- make_plot(total_t,total_f, "0_Total", kvals)
	#tf <- make_bubbles(total_f, "From", kvals)
	#print(tt, split=c(1,1,2,1), more=T)
	#print(tf, split=c(2,1,2,1), more=F)
}
#rbind(mon[124,], tue[124,], wed[124,], thu[124,], fri[124,],
#				monf[124,], tuef[124,], wedf[124,], thuf[124,], frif[124,])