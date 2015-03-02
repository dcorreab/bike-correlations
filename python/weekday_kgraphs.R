library(ggplot2)
library(gridExtra)
library(scales)

mon <- read.csv("../results/kvals/nyc_mon_tc_k.csv")
mon <- mon[-56,] # REMOVE SAME STATION AS OFF THE WALL VALUE FOR TUESDAY FROM > 40 KM
tue <- read.csv("../results/kvals/nyc_tue_tc_k.csv")
wed <- read.csv("../results/kvals/nyc_wed_tc_k.csv")
thu <- read.csv("../results/kvals/nyc_thu_tc_k.csv")
thu <- thu[-56,]
# REMOVE OFF THE WALL VALUE FOR THURS 115 KM
#thu<-thu[-124,]
fri <- read.csv("../results/kvals/nyc_fri_tc_k.csv")
sat <- read.csv("../results/kvals/nyc_sat_tc_k.csv")
#sat <- sat[-221,]
sun <- read.csv("../results/kvals/nyc_sun_tc_k.csv")
monf <- read.csv("../results/kvals/nyc_mon_fc_k.csv")
tuef <- read.csv("../results/kvals/nyc_tue_fc_k.csv")
#tuef <- tuef[-124,]  # REMOVE OFF THE WALL VALUE FOR TUESDAY > 40 KM
wedf <- read.csv("../results/kvals/nyc_wed_fc_k.csv")
thuf <- read.csv("../results/kvals/nyc_thu_fc_k.csv")
frif <- read.csv("../results/kvals/nyc_fri_fc_k.csv")
satf <- read.csv("../results/kvals/nyc_sat_fc_k.csv")
sunf <- read.csv("../results/kvals/nyc_sun_fc_k.csv")
#gt15 <- which(sunf$kval>19)
#sunf <- sunf[-gt15,]
#sun <- sun[-gt15,]

mon['day'] <- "monday"
tue['day'] <- "tuesday"
wed['day'] <- "wednesday"
thu['day'] <- "thursday"
fri['day'] <- "friday"
sat['day'] <- "saturday"
sun['day'] <- "sunday"

all_to <- rbind(mon, tue,wed,thu,fri,sat,sun)
write.csv(all_to, "../results/kvals/all_to_weekday.csv")

monf['day'] <- "monday"
tuef['day'] <- "tuesday"
wedf['day'] <- "wednesday"
thuf['day'] <- "thursday"
frif['day'] <- "friday"
satf['day'] <- "saturday"
sunf['day'] <- "sunday"
all_from <- rbind(monf, tuef,wedf,thuf,frif,satf,sunf)
write.csv(all_from, "../results/kvals/all_from_weekday.csv")

all_from$day <- factor(all_from$day, levels=c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"), labels=c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), ordered=T)
all_to$day <- factor(all_to$day, levels=c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"), labels=c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), ordered=T)

seq(5,30,2)
c(0,0.5,1,2,3,4,5,10,15,30)

#gt21.f <- which(all_from$kval>21 & all_to$kval>21)
#all_from <- all_from [-gt21.f,]
#all_to <- all_to [-gt21.f,]

k.q <- quantile(all_from$kval, probs = c(0.9, 1))
want <- which(all_from$kval >= k.q[1] & all_from$kval <= k.q[2])
ggplot(all_from, aes(day, kval, group=1)) + geom_point() +
	geom_point(data=all_from[-want,], aes(day, kval, group=1), colour="red")

	k.q1 <- quantile(all_to$kval, probs = c(0.9, 1))
	want1 <- which(all_to$kval >= k.q1[1] & all_to$kval <= k.q1[2])

from <- ggplot(all_from[want,], aes(day, kval, group=1)) + geom_point(alpha=0.5) +
		geom_line(data=all_from, stat='summary', fun.y=mean, aes(colour="Mean"), linetype=4) + 
		geom_point(data=all_from, stat='summary', fun.y=mean, aes(colour="Mean")) + 
		geom_line(stat='summary', fun.y=max, aes(colour="Max"), linetype=4) + 
		geom_point(stat='summary', fun.y=max, aes(colour="Max")) + 
		scale_x_discrete(name="Day of Week")  +
		scale_y_continuous(name="K-Value (km)", labels = comma, breaks=pretty_breaks(n=10), limits=c(0,4.5)) +
		theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
		labs(colour="Lines") + ggtitle("From")


to <- ggplot(all_to[want1,], aes(day, kval, group=1)) + geom_point(alpha=0.5) +
		geom_line(data=all_to,stat='summary', fun.y=mean, aes(colour="Mean"), linetype=4) + 
		geom_point(data=all_to,stat='summary', fun.y=mean, aes(colour="Mean")) + 
		geom_line(stat='summary', fun.y=max, aes(colour="Max"), linetype=4) + 
		geom_point(stat='summary', fun.y=max, aes(colour="Max")) + 
		scale_x_discrete(name="Day of Week")  +
		scale_y_continuous(name="K-Value (km)", labels = comma, breaks=pretty_breaks(n=10), limits=c(0,4.5)) +
		theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
		labs(colour="Lines") + ggtitle("To")
		grid.arrange(to, from, ncol=2, main=textGrob("NYC K-Values 90-100% Quantile", gp=gpar(cex=2), just="top"))

png (file="../results/kvals/jpeg/weekday_kvals_90_100.png", width=8, height=4, type="cairo", res=300, units="in")
	grid.arrange(to, from, ncol=2, main=textGrob("NYC K-Values 90-100% Quantile", gp=gpar(cex=2), just="top"))
dev.off()


