
x<-NULL

n.size=10000

for (i in 1:n.size) {
  set.seed(123)
  x[i]<-mean(rbinom(i,1,0.5))
}

plot(x,col="blue",xlab="sample size", ylab="Pn", ylim=c(0,1),
     type="l", lwd=2)
abline(h=0.5, col="red", lwd=2)



y<-NULL
set.seed(123)
for (i in 1:n.size) {
  y[i]<-mean(rbinom(i,1,0.5))
}

plot(y,col="blue",xlab="sample size", ylab="Pn", ylim=c(0.3,0.7), type = "l")
abline(h=0.5, col="red", lwd=2)


z.0<-NULL
set.seed(123)
for (i in 1:n.size/100) {
  z.0[i]<-mean(rbinom(n.size/100,1,0.5))
}

plot(density(z.0, bw=0.01), col="blue",  lwd=2, main="Pn density for n = 100", xlab="")
abline(v=0.5, lwd=2, col="red")




z<-NULL
set.seed(123)
for (i in 1:n.size) {
  z[i]<-mean(rbinom(n.size,1,0.5))
}

plot(density(z, bw=0.01), col="blue", lwd=2, main="Pn density for n = 10000", xlab="")
abline(v=0.5, lwd=2, col="red")


# Consider the distribution of the mean - p; Lecture 8, slide 12

n.size<-1000

z1<-NULL
set.seed(123)
for (i in 1:100) {
  z1[i]<-mean(rbinom(n.size/100,1,0.5)) - 0.5
}


z2<-NULL
set.seed(123)
for (i in 1:100) {
  z2[i]<-mean(rbinom(n.size/10,1,0.5)) - 0.5
}


z3<-NULL
set.seed(123)
for (i in 1:100) {
  z3[i]<-mean(rbinom(n.size/1,1,0.5)) - 0.5
}



z4<-NULL
set.seed(123)
for (i in 1:100) {
  z4[i]<-mean(rbinom(n.size*10,1,0.5)) - 0.5
}


par(mfrow=c(2,2))

plot(density(z1, bw=0.05), col="blue", lwd=2, main="(Pn-p) density for n = 100", xlab="")
abline(v=0, lwd=2, col="red")

plot(density(z2, bw=0.05), col="blue", lwd=2, main="(Pn-p) density for n = 1000", xlab="")
abline(v=0, lwd=2, col="red")

plot(density(z3, bw=0.05), col="blue", lwd=2, main="(Pn-p) density for n = 10000", xlab="")
abline(v=0, lwd=2, col="red")

plot(density(z4, bw=0.015), col="blue", lwd=2, main="(Pn-p) density for n = 10000", xlab="")
abline(v=0, lwd=2, col="red")




par(mfrow=c(2,2))

hist(z1,col="blue",breaks = 8)
abline(v=0, lwd=2, col="red")

hist(z2,col="blue",breaks = 8)
abline(v=0, lwd=2, col="red")

hist(z3,col="blue",breaks = 8)
abline(v=0, lwd=2, col="red")

hist(z4,col="blue",breaks = 8)
abline(v=0, lwd=2, col="red")



par(mfrow=c(2,2))

hist(z1/sqrt(var(z1)),col="blue",breaks = 8, xlim=c(-3,3),
     main="Hist z1/sd")
abline(v=0, lwd=2, col="red")

hist(z2/sqrt(var(z2)),col="blue",breaks = 8, xlim=c(-3,3),
     main="Hist z2/sd")
abline(v=0, lwd=2, col="red")

hist(z3/sqrt(var(z3)),col="blue",breaks = 8, xlim=c(-3,3),
     main="Hist z3/sd")
abline(v=0, lwd=2, col="red")

hist(z4/sqrt(var(z4)),col="blue",breaks = 8, xlim=c(-3,3),
     main="Hist z4/sd")
abline(v=0, lwd=2, col="red")




