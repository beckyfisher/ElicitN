# BRUTE FORCE ESTIMATION OF mu,sig in a LOG-NORMAL
# compare theoretical to actual lower and upper pihat-percentiles Lhat & Uhat
# and theoretical to actual mode, mean or median Mhat
#

#7 Dec 2009
#previous version works ("expert.K.LN.OLD3.R")
#changes
#1)
##Title from mode for plot 1, mean for plot 2, ...
##NOW "a" for plot 1, "b" for plot 2...
#2)
##just display mode & expert.best guess for plot 1; mean & expert best guess for plot2

#Then plot 3 boxplots based on best guess being mode, mean or median
#Showing feeding back in form of lower & upper bounds for different % confidences

#12 Jan 2010
#previous version works ("expert.K.LN.OLD4.R")
#changes
#1) ssLN to ss
#2) fitLN to fit

#29/6/2012
#ADD OPTIM TO ESIMATE SIGMA PARAMETER FOR NORMAL DISTRIBUTION


######
#TEST: Using data from Trish

#rm(list=ls())
#best.type.status=cbind("ans"=c("N", "P", "M"),"status"=c(0, 0, 0), "Best.type"=c("a", "a", "a"))

#Lhat <- 20000000; Uhat<-30000000; pihat <-.8; Mhat <- 20000000; new.alpha=.95

#Lhat <- 2000; Uhat<-3000; pihat <-.65; Mhat <- 2000

#Lhat <- 2000; Uhat<-3000; pihat <-.85; Mhat <- 2000
#Lhat <- 2000; Uhat<-3000; pihat <-.85; Mhat <- 2200


#source("expert.K.LN.R")
#expert.K.LN.results <- expert.K.LN(Lhat, Uhat, Mhat, pihat, .95)

#Lhat<- 100; Uhat<- 750; pihat<-.6; Mhat<- 250; new.alpha=.95

#Lhat<- 0.1; Uhat <- 5; pihat <-.6; Mhat <- 2; new.alpha=.95;

#Lhat=.1; Uhat=1; Mhat=.2; pihat=.8; new.alpha=.95
#####

expert.K.LN <-function(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type){


eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/modal.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source('modal.R')

fit.score.mode <- function(Lhat, Uhat, Mhat, pihat, w=rep(1/3,3)) {
  
  #possible mu
  ms <- seq(log(Lhat), log(Uhat), length=1000)
  
  #calculate possible sig
  ss1 <- sqrt(abs(ms- log(Lhat)))
  ss2 <- sqrt(abs(log(Uhat) - ms))
  ss3 <- sqrt(abs(ms- log(Mhat)))
  
  ss <- seq(min(ss1,ss2,ss3), max(ss1,ss2,ss3), length=1000)
  
  grid.ms <- expand.grid(mu=ms, sig=ss)
  mu=grid.ms[,1]
  sig=grid.ms[,2]
  
  
  score.bec2 <- ( (((Lhat)-qlnorm((1-pihat)/2, mean=mu, sd=sig))^2)*1/3 +
    (((Uhat) - qlnorm((pihat+(1-pihat)/2), mean=mu, sd=sig))^2 )*1/3+
   # (((Mhat) - exp(mu-sig^2))^2)*1/3) #mode
  #(((Mhat) - exp(mu + (sig^2)/2))^2)*1/3)#mean
 ((Mhat - exp(mu))^2)*1/3)#median
  
  return(as.data.frame(cbind(score.mode=score.bec2, mu=mu, sig=sig)))
}

fit.score.mean <- function(Lhat, Uhat, Mhat, pihat, w=rep(1/3,3)) {
  
  #possible mu
  ms <- seq(log(Lhat), log(Uhat), length=1000)
  
  #calculate possible sig
  ss1 <- (log(Mhat)-log(Lhat))/2
  ss2 <- (log(Uhat)-log(Mhat))/2
  ss <- seq(min(ss1,ss2)/5, max(ss1,ss2)*5, length=1000)
  
  grid.ms <- expand.grid(mu=ms, sig=ss)
  mu=grid.ms[,1]
  sig=grid.ms[,2]
  
  score.bec2 <- ( (((Lhat)-qlnorm((1-pihat)/2, mean=mu, sd=sig))^2)*1/3 +
    (((Uhat) - qlnorm((pihat+(1-pihat)/2), mean=mu, sd=sig))^2 )*1/3+
    (((Mhat) - exp(mu + (sig^2)/2))^2)*1/3)#mean
  
  return(as.data.frame(cbind(score.mean=score.bec2, mu=mu, sig=sig)))
}

fit.score.median <- function(Lhat, Uhat, Mhat, pihat, w=rep(1/3,3)) {
  
  #possible mu
  ms <- seq(log(Lhat), log(Uhat), length=1000)
  
  #calculate possible sig
  ss1 <- (log(Mhat)-log(Lhat))/2
  ss2 <- (log(Uhat)-log(Mhat))/2
  ss <- seq(min(ss1,ss2)/5, max(ss1,ss2)*5, length=1000)
  
  grid.ms <- expand.grid(mu=ms, sig=ss)
  mu=grid.ms[,1]
  sig=grid.ms[,2]
  
  score.bec2 <- ( (((Lhat)-qlnorm((1-pihat)/2, mean=mu, sd=sig))^2)*1/3 +
    (((Uhat) - qlnorm((pihat+(1-pihat)/2), mean=mu, sd=sig))^2 )*1/3+
    ((Mhat - exp(mu))^2)*1/3)#median
  
  return(as.data.frame(cbind(score.median=score.bec2, mu=mu, sig=sig)))
}


######


ssLN <- function(mu, sig, alpha) {
  m <- exp(mu + (sig^2)/2)
  v <- (exp(sig^2)-1)*exp(2*mu+sig^2)
  mo <- exp(mu - (sig^2))
  sk <- (exp(sig^2)+2)*sqrt(exp(sig^2)-1)
  ku <- (exp(4*sig^2)+2*exp(3*sig^2)+3*exp(2*sig^2)-6)
  #ci <- qlnorm(c(alpha/2, .5, 1-alpha/2), mean=mu, sd=sig)
  ci <- qlnorm(c((1-alpha)/2, .5, (alpha+(1-alpha)/2)), mean=mu, sd=sig)
  return(list(mean=m, var=v, mode=ci[2], skew=sk, kurtosis=ku, lower=ci[1],
              median=ci[2], upper=ci[3]))
}


feedbackLN <- function(mu, sig,alpha,low,upp,new.alpha) {
  #mu<-ms[1]; sig<-ms[2];
  m <- exp(mu + (sig^2)/2)
  mo <- exp(mu-sig^2)
  #ci <- qlnorm(c(alpha/2, .5, 1-alpha/2, 
  #	new.alpha/2, 1-new.alpha/2), mean=mu, sd=sig)
  #ci <- qlnorm(c(1-alpha, .5, alpha, 1-new.alpha, new.alpha), mean=mu, sd=sig)
  ci <- qlnorm(c((1-alpha)/2, .5, (alpha+(1-alpha)/2), (1-new.alpha)/2, (new.alpha+(1-new.alpha)/2)), mean=mu, sd=sig)
  
  
  pci <- plnorm(c(upp,low), mean=mu, sd=sig)
  return(list(lower=ci[1], median=ci[2], upper=ci[3], mean=m, mode=ci[2], prob.upper=pci[1], prob.lower=pci[2], 
              new.alpha=new.alpha, new.lower=ci[4], new.upper=ci[5]))
}


################################################################
#function used to estimate (finer scale) mean & sigma for log-Normal distribution
#minimising over ss

#params=c(4.953478, 0.4100269); Lhat=100; Uhat=200; Mhat=120; pihat=0.8
#ss.mu.sig(params,Lhat, Uhat , Mhat,pihat)

ss.mu.sig.LN <- function(params,Lhat, Uhat , Mhat,pihat){
  
  mu.i = params[1]
  sig.i = params[2]
  #print(c(mu.i, sig.i))
  
  sim.Lower = qlnorm((1-pihat)/2, mean=mu.i, sd=sig.i)
  sim.Upper = qlnorm((pihat+(1-pihat)/2), mean=mu.i, sd=sig.i)
  sim.Best =  exp(mu)
  
  diff.lw=((Lhat - sim.Lower)^2)  * 1/3
  diff.up=((Uhat - sim.Upper)^2) * 1/3
  diff.best=((Mhat - sim.Best)^2) * 1/3
  
  ss=(diff.lw + diff.up  + diff.best)
  (diff.lw + diff.up + diff.best )
  
  return(ss)
} # END ss.mu.sig


################################################################
#MEAN
fit.results.mean <-fit.score.mean(Lhat, Uhat, Mhat, pihat,rep(1/3,3)) 

#fit.results.mean[which(fit.results.mean[,1] ==min(fit.results.mean[,1])),]

fit.best.mean.mu <-modal(fit.results.mean[which(fit.results.mean$score.mean 
                                                ==min(fit.results.mean$score.mean)),]$mu)
fit.best.mean.sig <-modal(fit.results.mean[which(fit.results.mean$score.mean 
                                                 ==min(fit.results.mean$score.mean)),]$sig)


ss.mean.results <-ssLN(fit.best.mean.mu, fit.best.mean.sig , pihat)

feedback.mean.results <- feedbackLN(fit.best.mean.mu, fit.best.mean.sig, pihat,Lhat, Uhat,new.alpha=new.alpha)

#################
#MODE
fit.results.mode <-fit.score.mode(Lhat, Uhat, Mhat, pihat,rep(1/3,3)) 

#fit.results.mode[which(fit.results.mode[,1] ==min(fit.results.mode[,1])),]

fit.best.mode.mu <-modal(fit.results.mode[which(fit.results.mode$score.mode 
                                                ==min(fit.results.mode$score.mode)),]$mu)
fit.best.mode.sig <-modal(fit.results.mode[which(fit.results.mode$score.mode 
                                                 ==min(fit.results.mode$score.mode)),]$sig)


ss.mode.results <-ssLN(fit.best.mode.mu, fit.best.mode.sig , pihat)

feedback.mode.results <- feedbackLN(fit.best.mode.mu, fit.best.mode.sig, pihat,Lhat, Uhat,new.alpha=new.alpha)

#USE OPTIM - ESIMTATE (FINER SCALE) SIGMA
estimate.mu.sig=optim(par=c(fit.best.mode.mu,fit.best.mode.sig), 
                      fn=ss.mu.sig.LN, Lhat=Lhat, Uhat=Uhat,Mhat=Mhat,
                      pihat=pihat)#, lower=70, upper=250)

#put in new optim figures
fit.best.mode.mu.optim= estimate.mu.sig$par[1]
fit.best.mode.sig.optim= estimate.mu.sig$par[2]

feedback.mode.results.optim <- feedbackLN(fit.best.mode.mu.optim, fit.best.mode.sig.optim, 
                                          pihat,Lhat, Uhat,new.alpha=new.alpha)



#################
#MEDIAN
fit.results.median <-fit.score.median(Lhat, Uhat, Mhat, pihat,rep(1/3,3)) 

#fit.results.median[which(fit.results.median[,1] ==min(fit.results.median[,1])),]

fit.best.median.mu <-modal(fit.results.median[which(fit.results.median$score.median 
                                                    ==min(fit.results.median$score.median)),]$mu)
fit.best.median.sig <-modal(fit.results.median[which(fit.results.median$score.median 
                                                     ==min(fit.results.median$score.median)),]$sig)


ss.median.results <-ssLN(fit.best.median.mu, fit.best.median.sig , pihat)

feedback.median.results <- feedbackLN(fit.best.median.mu, fit.best.median.sig, pihat,Lhat, Uhat,new.alpha=new.alpha)


####
#plot


par(mfrow=c(1,3))
#MODE
if (ss.mode.results$lower <0) {
ss.mode.results$lower =0
}
if (feedback.mode.results$new.lower<0){
feedback.mode.results$new.lower =0
}

#find min & max for plot

plot.K.min <- min(ss.mode.results$lower,
	feedback.mode.results$new.lower)

plot.K.max <- max(ss.mode.results$upper,
	feedback.mode.results$new.upper)

plot(rep(c(1),2), rep(c(ss.mode.results$lower, ss.mode.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", 
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1), 
	xaxt = "n", main="a",lwd=2)
lines(rep(2,2),c(ss.mode.results$lower, ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(ss.mode.results$upper,2),lwd=2)
#lines(c(1,2),rep(ss.mode.results$mean,2), col=2,lwd=2)
lines(c(1,2), rep(ss.mode.results$mode,2),col=4,lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=3,lwd=2)#best guess
 
#lines(c(1,2), rep(ss.mode.results$median,2), col=4,lwd=2)
legend("topleft", lty=1, col=c(1,1, 4,3, "orange", "orange"), bty="n", legend=c("lower",
	"upper", "maths best guess", "best guess", "new lower","new upper"),cex=.8,lwd=2)
legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")



#plot new lower & upper conf
lines(c(1,2), rep(feedback.mode.results$new.lower,2), col="orange" ,lwd=2)
lines(c(1,2), rep(feedback.mode.results$new.upper,2), col="orange",lwd=2)

#lines(rep(1.5,2), c(feedback.results$new.lower,ss.mode.results$lower) )
#lines(rep(1.5,2),c(feedback.results$new.upper,ss.mode.results$upper) )


#MEAN
if (ss.mean.results$lower <0) {
ss.mean.results$lower =0
}
if (feedback.mean.results$new.lower<0){
feedback.mean.results$new.lower =0
}

#find min & max for plot

plot.K.min <- min(ss.mean.results$lower,
	feedback.mean.results$new.lower)

plot.K.max <- max(ss.mean.results$upper, 
	feedback.mean.results$new.upper)

plot(rep(c(1),2), rep(c(ss.mean.results$lower, ss.mean.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", 
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1)
	,xaxt = "n" , main="b",lwd=2)
lines(rep(2,2),c(ss.mean.results$lower, ss.mean.results$upper),lwd=2)
lines(c(1,2), rep(ss.mean.results$lower,2),lwd=2)
lines(c(1,2), rep(ss.mean.results$upper,2),lwd=2)
lines(c(1,2),rep(ss.mean.results$mean,2), col=4,lwd=2)
#lines(c(1,2), rep(ss.mean.results$mode,2),col=4,lwd=2)
lines(c(1,2), rep(Mhat,2),col=3,lwd=2)#best guess

#lines(c(1,2), rep(ss.mean.results$median,2), col=4,lwd=2)
#legend("topright", lty=1, col=c(1,1, 2,3,4), bty="n", legend=c("lower",
#	"upper", "mean", "mode", "median"))

#plot new lower & upper conf
lines(c(1,2), rep(feedback.mean.results$new.lower,2), col="orange" ,lwd=2)
lines(c(1,2), rep(feedback.mean.results$new.upper,2), col="orange",lwd=2)

#lines(rep(1.5,2), c(feedback.results$new.lower,ss.mean.results$lower) )
#lines(rep(1.5,2),c(feedback.results$new.upper,ss.mean.results$upper) )

#MEDIAN
if (ss.median.results$lower <0) {
ss.median.results$lower =0
}
if (feedback.median.results$new.lower<0){
feedback.median.results$new.lower =0
}

#find min & max for plot

plot.K.min <- min(ss.median.results$lower, 
	feedback.median.results$new.lower)

plot.K.max <- max(ss.median.results$upper,
	feedback.median.results$new.upper)

plot(rep(c(1),2), rep(c(ss.median.results$lower, ss.median.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", 
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1)
	,xaxt = "n" , main="c",lwd=2)
lines(rep(2,2),c(ss.median.results$lower, ss.median.results$upper),lwd=2)
lines(c(1,2), rep(ss.median.results$lower,2),lwd=2)
lines(c(1,2), rep(ss.median.results$upper,2),lwd=2)
#lines(c(1,2),rep(ss.median.results$mean,2), col=2,lwd=2)
#lines(c(1,2), rep(ss.median.results$mode,2),col=3,lwd=2)
lines(c(1,2), rep(ss.median.results$median,2), col=4,lwd=2)
lines(c(1,2), rep(Mhat,2),col=3,lwd=2)#best guess

#plot new lower & upper conf
lines(c(1,2), rep(feedback.median.results$new.lower,2), col="orange" ,lwd=2)
lines(c(1,2), rep(feedback.median.results$new.upper,2), col="orange",lwd=2)

########
#calculate K
#Ksp <-rlnorm(10000,mu,sig)
#Ksp <-rlnorm(10000,fit.best.mode.mu, fit.best.mode.sig)

###
#store selected best type (mode, median, mean)
#best.type = best.type.status[ee.type,3]


return(list(pihat=pihat,
            #ss.mean.results =ss.mean.results , 
            ss.mode.results=ss.mode.results ,
            #ss.median.results =ss.median.results, 
            #fit.best.mode.mu=fit.best.mode.mu,
            #fit.best.mode.sig=fit.best.mode.sig, 
            fit.best.mode.mu=fit.best.mode.mu.optim,
            fit.best.mode.sig=fit.best.mode.sig.optim,
            #fit.best.mean.mu = fit.best.mean.mu,
            #fit.best.mean.sig=fit.best.mean.sig, 
            #fit.best.median.mu=fit.best.median.mu,
            #fit.best.median.sig=fit.best.median.sig,
            feedback.mode.results=feedback.mode.results,
            feedback.mode.results.optim=feedback.mode.results.optim))
#feedback.mean.results=feedback.mean.results,
#  feedback.median.results=feedback.median.results, Ksp=Ksp, MhatK=Mhat))

}
