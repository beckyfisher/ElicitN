#rbeta.number.truncate(1, 2, 5, 0,300)


rnormals.number <- function(n, mu, sig, which.dist,use.lower,use.upper) {
#n=100; mu=data.raw[i,"mu"]; sig=data.raw[i,"sig"]; which.dist=data.raw[i,"which.dist"]
#use.lower=data.raw[i,"use.lower"];use.upper=data.raw[i,"use.upper"]
#n=1000000*as.numeric(data.i[e,"weights"])
#mu=as.numeric(data.i[e,"NallMar.mu"])
#sig=as.numeric(data.i[e,"NallMar.sig"])
#which.dist=data.i[e,"NallMar.d"]
#use.lower=as.numeric(data.i[e,"NallMar.l"])
#use.upper=as.numeric(data.i[e,"NallMar.u"])

#temp <-(((MostUpper - MostLower)* (rbeta(n, alpha, beta) - 1) )+ MostUpper)
#n=1000000, mu=data.raw$mu[i] , sig=data.raw$sig [i],which.dist= data.raw$use.lower[i],data.raw$use.upper[i]

  if(which.dist=="normal"){
    temp=rnorm(n, mu, sig)
    temp=temp[which(temp>=0)]
    temp=sample(temp,n,replace=T)
   # while(length(temp)<n){
   #     tt=rnorm(1, mu, sig)
   #     if(tt>=0){temp=c(temp,tt)}}
   }
  if(which.dist=="LN"){
    temp=rlnorm(n, mu, sig)
    temp=temp[which(temp>=0)]
    temp=sample(temp,n,replace=T)
    #while(length(temp)<n){
    #    tt=rlnorm(n, mu, sig)
    #    if(tt>=0){temp=c(temp,tt)}}
    }
  if(which.dist=="LNleftskew"){
    temp=use.lower - rlnorm(n, mu, sig) + use.upper
    temp=temp[which(temp>=0)]
    temp=sample(temp,n,replace=T)
    #while(length(temp)<n){
    #    tt=use.lower - rlnorm(1, mu, sig) + use.upper
    #    if(tt>=0){temp=c(temp,tt)}}
    }

return(temp)
}
