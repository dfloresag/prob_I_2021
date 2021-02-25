

x1=rbinom(1000,1,0.5)
x1=(x1-mean(x1))/(sd(x1))

x10=rbinom(1000,10,0.5)
x10=(x10-mean(x10))/(sd(x10))

x100=rbinom(1000,100,0.5)
x100=(x100-mean(x100))/(sd(x100))

x1000=rbinom(1000,1000,0.5)
x1000=(x1000-mean(x1000))/(sd(x1000))

Fn.x1<-ecdf(x1)
Fn.x10<-ecdf(x10)
Fn.x100<-ecdf(x100)
Fn.x1000<-ecdf(x1000)

x.ax<-seq(-3,3,0.1)

par(mfrow=c(2,2))
plot(x.ax,Fn.x1(x.ax),col="blue",ylab="",xlab="", main="n = 1",lwd=2)
lines(x.ax,pnorm(x.ax,0,1),col="red",lwd=2)
legend(0.2, 0.4, c("CDF of Yn", "CDF of N(0,1)"), 
       col = c(4, 2), bg = "gray90",lty = c(1,1))


plot(x.ax,Fn.x10(x.ax),col="blue",ylab="",xlab="", main="n = 10",lwd=2)
lines(x.ax,pnorm(x.ax,0,1),col="red",lwd=2)

plot(x.ax,Fn.x100(x.ax),col="blue",ylab="",xlab="", main="n = 100",lwd=2)
lines(x.ax,pnorm(x.ax,0,1),col="red",lwd=2)


plot(x.ax,Fn.x1000(x.ax),col="blue",ylab="",xlab="", main="n = 1000",lwd=2)
lines(x.ax,pnorm(x.ax,0,1),col="red",lwd=2)
