library(ggplot2)
library(gridExtra)
library(scales)

#######################
# PRODUCE GRAPH OF WEEKDAY TOTAL TRIPS
#######################

wk <- read.csv("kvals/weekday_counts.csv", header=F)
names(wk) <- c("day", "num_trips")
wk$day <- factor(wk$day, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered=T)

png (file="../results/weekday_numbers.png", width=6, height=4, type="cairo", res=300, units="in")
xx<-ggplot(wk, aes(day, num_trips, group=1)) + geom_line(colour="red") + geom_point() + 
	scale_x_discrete(name="Day of Week")  +
	scale_y_continuous(name="Number of Trips", labels = comma, limits=c(1200000,1600000)) + 
	theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("NYC Trips Per Weekday")
	print(xx)
dev.off()


#######################
# PRODUCE GRAPH OF DURATION OVER WEEK
#######################

dur1 <- read.csv("../results/weekday_durations1.csv", header=F)
names(dur1) <- c("time", "day", "num_trips")
dur1$day <- factor(dur1$day, levels=c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), ordered=T)
dur1$time <- factor(
	dur1$time, 
	levels=c(levels(dur1$time)), 
	labels=c("0 - 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 +")
	)
png (file="../results/weekday_durations.png", width=6, height=4, type="cairo", res=300, units="in")

yy<-ggplot(dur1, aes(day, num_trips, group=time)) + geom_line(aes(colour=time)) + geom_point() + 
	scale_x_discrete(name="Day of Week")  +
	scale_y_continuous(name="Number of Trips", labels = comma) + 
	theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
	labs(colour="Duration (mins)") + ggtitle("NYC Trip Duration")
	print(yy)
dev.off()

# print the two graphics side by side
png (file="../results/daily_graphs.png", width=12, height=4, type="cairo", res=300, units="in")
	grid.arrange(xx,yy, ncol=2)
dev.off()

