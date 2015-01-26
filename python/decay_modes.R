png (file=paste("../results/models/models.png",sep=""), width=14, height=4, type="cairo", res=150, units="in")
par(mfrow=c(1,4))
x <- seq(from = 0, to = 14, length.out = 200)

# Gaussian Fit
#rvec ~ cc + a * exp(-((d - min(d))/k)^2)

y <- 0.1 + 0.9 * exp(-( ( x - min(x) )/4.1)^2)
plot(x,y, type="l", main="Gaussian Decay", ylim=c(0,1), xlab="Distance", ylab="Correlation")
text(12,1,"rvec ~ cc + a * exp(-((d - min(d))/k)^2)", pos=2)

# Exponential Function 
#rvec ~ cc + a * exp(-( ( d - min(d) )/k))

y <- 0.1 + 0.9 * exp(-( ( x - min(x) )/3))
#lines(x,y, type="l", col="red")
plot(x,y, type="l", main="Exponential Decay", ylim=c(0,1), xlab="Distance", ylab="Correlation")
text(12,1,"rvec ~ cc + a * exp(-( ( d - min(d) )/k))",pos=2)

# Inverse
#rvec ~ cc + a * 1/d
y <- 0.1 + 0.5 * 1/x
#lines(x,y, type="l", col="blue")
plot(x,y, type="l", main="Inverse Decay", ylim=c(0,1), xlab="Distance", ylab="Correlation")
text(12,1,"rvec ~ cc + a * 1/d",pos=2)

# Gravity
#rvec ~ cc + a * 1/d^2
y <- 0.2 + 0.9 * 1/x^2
#lines(x,y, type="l", col="green")
plot(x,y, type="l", main="Gravity Decay", ylim=c(0,1), xlab="Distance", ylab="Correlation")
text(12,1,"rvec ~ cc + a * 1/d^2",pos=2)

dev.off()
library(ggplot2)
par(mfrow=c(2,2))

ggplot() + geom_line(aes(x,y)) +
 	ggtitle("") + ylim(0,1)+
	annotate("text", label = "rvec ~ cc + a %*% -epsilon ((d-min(d))/k)", x = 10, y = 1, size = 3, colour = "red", parse=T)