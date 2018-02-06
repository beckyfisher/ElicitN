#data=combine.sample.i.CRgenetic

modal <- function(data) {
  # Function for mode estimation of a continuous variable
  # Kernel density estimation by Ted Harding & Douglas Bates (found on RSiteSearch)

    x<-data
    lim.inf=min(x)-1; lim.sup=max(x)+1

    hist(x,freq=FALSE,breaks=seq(lim.inf,lim.sup,0.05),plot=F)
    s<-density(x,from=lim.inf,to=lim.sup,bw=0.2)
    n<-length(s$y)
    v1<-s$y[1:(n-2)];
    v2<-s$y[2:(n-1)];
    v3<-s$y[3:n]
    ix<-1+which((v1<v2)&(v2>v3))

   # lines(s$x,s$y,col="red")
   # points(s$x[ix],s$y[ix],col="blue")

    md <- s$x[which(s$y==max(s$y))]

    return(as.numeric(md))
  }