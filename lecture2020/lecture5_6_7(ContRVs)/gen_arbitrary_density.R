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
xlower<-min(x[fx0>0])
xupper<-max(x[fx0>0])

fxlower<-fx[x==xlower]
fxupper<-fx[x==xupper]

plot(x[fx0>0],fx0[fx0>0],xlim=range(x),ylim=range(fx0),type="l",xlab="",ylab="",axes=FALSE,lwd=2,col="blue")
segments(xlower,0, xlower,fxlower,lty=3)
segments(xupper,0, xupper,fxupper,lty=3)
abline(h=0)
abline(v=min(x))

z<-min(x[x>2.8])
segments(z,0,z,fx0[x==z],lty=1)
text(z,-4*h,expression(x))
text(xlower,-4*h,expression(a))
text(xupper,-4*h,expression(b))
text(z,(fx0[x==z]+10*h),expression(f[X](x)),col="blue")

segments(-10,0,xlower,0,lwd=2,col="blue")
segments(xupper,0,20,0,lwd=2,col="blue")

arrows((z+2),(fx0[x==z]+0.01),(z+0.25),(fx0[x==(z+0.25)]),code=2,col="blue")
text((z+2.5),(fx0[x==(z)]+.015),"pdf",col="blue")


# stop here to produce the first graph
# export it before continuing on to produce the second graph
gumx<-x[x>=xlower&x<=z]
gumy<-fx0[x>=xlower&x<=z]

rect(gumx[seq(1,length(gumx),10)],rep(0,length(gumx[seq(1,length(gumx),10)])),gumx[seq(1,length(gumx),10)],gumy[seq(1,length(gumx),10)],border="pink")
par(new=TRUE)
plot(x[fx0>0],fx0[fx0>0],xlim=range(x),ylim=range(fx0),type="l",xlab="",ylab="",axes=FALSE,lwd=2,col="blue")
segments(xlower,0, xlower,fxlower,lty=3)
segments(xupper,0, xupper,fxupper,lty=3)
abline(h=0)
abline(v=min(x))
segments(z,0,z,fx0[x==z],lty=1)
text(1,mean(fx0),expression(F[X](x)==Pr(X<=x)),col="red")


#arrows((z+2),(fx0[x==z]+0.01),(z+0.25),(fx0[x==(z+0.25)]),code=2)
arrows((z+2),(fx0[x==z]-0.03),(z-0.25),(fx0[x==z]-.03),code=2,col="red")
text((z+2.5),(fx0[x==(z)]-0.03),"CDF",col="red")

### Unif(0,1) density

xx<-seq(0,1,.01)
yy<-rep(1,length(xx))
plot(xx,yy,xlim=c(-0.5,2),ylim=c(0,1.2),lwd=2,xlab=expression(x),ylab="",type="l",axes=F)
abline(h=0)
abline(v=0)
segments(1,0,1,1,lty=3)
text(1,-.02,"1")
text(-.02,1,"1")
text(-.02,-0.02,"0")
text(.5,1.1,expression(f[X](x)))

segments(-1,0,0,0,lwd=2)
segments(1,0,10,0,lwd=2)
