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
  if(which.dist=="normal"){
    temp=rnorm(n, mu, sig)
    }  
  if(which.dist=="LN"){
    temp=rlnorm(n, mu, sig)
    }   
  if(which.dist=="LNleftskew"){
    temp=use.lower - rlnorm(n, mu, sig) + use.upper
    }
 temp[which(temp<0)]=0   
return(temp)
}