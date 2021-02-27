# Discrete uniforms for Lecture 5
# C Forbes July 2015


par(mfrow=c(2,2))
k<-3
x<-seq(0,k,by=1)
y<-rep(1/k,length(x))
plot(x,y,type="h",ylim=c(0,1),ylab="Pr(X = x)")
abline(h=0,col="lightgrey")
text(x=k/2,y=0.9,"E[X] = 1.5")
text(x=k/2,y=0.8,"Var(X) = 1.25")
title("k = 3")

k<-6
x<-seq(0,k,by=1)
y<-rep(1/k,length(x))
plot(x,y,type="h",ylim=c(0,1),ylab="Pr(X = x)")
abline(h=0,col="lightgrey")
text(x=k/2,y=0.9,"E[X] = 3")
text(x=k/2,y=0.8,"Var(X) = 4")
title("k = 6")

k<-10
x<-seq(0,k,by=1)
y<-rep(1/k,length(x))
plot(x,y,type="h",ylim=c(0,1),ylab="Pr(X = x)")
abline(h=0,col="lightgrey")
text(x=k/2,y=0.9,"E[X] = 5")
text(x=k/2,y=0.8,"Var(X) = 10")
title("k = 10")

k<-50
x<-seq(0,k,by=1)
y<-rep(1/k,length(x))
plot(x,y,type="h",ylim=c(0,1),ylab="Pr(X = x)")
abline(h=0,col="lightgrey")
text(x=k/2,y=0.9,"E[X] = 25")
text(x=k/2,y=0.8,"Var(X) = 216.67")
title("k = 50")
