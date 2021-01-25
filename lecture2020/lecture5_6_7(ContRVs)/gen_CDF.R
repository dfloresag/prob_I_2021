# pdf plot for lecture 
set.seed(746)

p<-c(13)
w<-runif(p,0,1)
w<-w/sum(w)
m<-rnorm(p,0,2)
s<-abs(rnorm(p,1,.6))
xl<-range(m+3*s,m-3*s)

h=0.001
x<-seq(xl[1],xl[2],h)
fx<-w[1]*dnorm(x,m[1],s[1])
for(i in 2:p){
  fx<-fx+w[i]*dnorm(x,m[i],s[i])
}

#plot(x,fx,type="l")
                                    
fx0<-fx
fx0[x>5]<-c(0)
fx0[x<(-3)]<-c(0)

fx0dens<-fx0/(sum(fx0)*h)
xlower<-min(x[fx0>0]) # this is 'a'
xupper<-max(x[fx0>0]) # this is 'b'

fxlower<-fx[x==xlower]
fxupper<-fx[x==xupper]

CDFx<-cumsum(fx0)/sum(fx0)
# now a CDF plot
plot(x,CDFx,type="l",xlab="",ylab="",axes=FALSE,lwd=2,col="red")
text(xlower,-20*h,expression(a))
text(xupper,-20*h,expression(b))

abline(h=0)
abline(v=min(x))
lines(x,rep(1,length(x)),lty=3)
text((min(x)-.2),1,expression(1))
text((min(x)-.2),.03,expression(0))


z<-min(x[x>2.8])
text(z,-20*h,expression(x))
segments(z,0,z,CDFx[x==z],lty=1)

#text((z+2),CDFx[x==z],expression(F[X](x)==Pr(X<=x)))
#arrows((fxlower)+1,CDFx[x==z]+.05,z,CDFx[x==z],code=2,col="red")
#text((fxlower)+1.2,CDFx[x==z],"CDF",col="red")
#points(z,CDFx[x==z],cex=1,pch=19)


zz<-min(x[x>z+.3])
text((z-2),CDFx[x==z],expression(F[X](x)==Pr(X<=x)))
points(z,CDFx[x==z],cex=1,pch=19)
arrows(z+1.5,(CDFx[x==zz]-.1),zz,CDFx[x==zz],code=2,col="red")
text(z+2,CDFx[x==z]-0.1,"CDF",col="red")


