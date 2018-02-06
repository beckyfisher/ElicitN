# BRUTE FORCE ESTIMATION OF mu,sig in a NORMAL
# compare theoretical to actual lower and upper pihat-percentiles Lhat & Uhat
# and theoretical to actual mode, mean or median Mhat


#12 Jan 2010
#previous version works ("expert.K.NormOLD1.R")
#changes
#1) ssN to ss
#2) fitN to fit


######
#TEST

#rm(list=ls())
#Lhat <- 2000; Uhat<-3000; pihat <-.65; Mhat <- 2500; new.alpha=.95
#Lhat <- 2000; Uhat<-3000; pihat <-.9; Mhat <- 2500
#Lhat <- 5000; Uhat<-12000; pihat <-.9; Mhat <-  8500 #WHY DOESN'T THIS WORK
#Lhat <- 500; Uhat<-1000; pihat <-.9; Mhat <-  750 #THIS WORKS ARRRHHHH
#Lhat <- 5000; Uhat<-10000; pihat <-.9; Mhat <-  7500

#qnorm(pihat, mean=7500, sd=2000)

#Lhat <- 40; Uhat<-100; pihat <-.9; Mhat <- 60
#Lhat <- 200; Uhat<-400; pihat <-.9; Mhat <- 320
#Lhat <- 200; Uhat<-400; pihat <-.9; Mhat <- 330 #STARTING NOT TO WORK
#Lhat <- 200; Uhat<-400; pihat <-.9; Mhat <- 250

#source("expert.K.Norm.optim.R")
#Test.K.Norm <- expert.K.Norm(Lhat, Uhat, Mhat, pihat, .95)
#####

expert.K.Norm <-function(Lhat, Uhat, Mhat, pihat, new.alpha){

source('modal.R')

fit.score.mode <- function(Lhat, Uhat, Mhat, pihat, w=rep(1/3,3)) {
	
	#possible mu
	mu <- Mhat#seq(log(Lhat), log(Uhat), length=1000)

		#calculate possible sig
		#ss1 <- sqrt(abs(ms- Lhat))
		#ss2 <- sqrt(abs(Uhat - ms))
		#ss3 <- sqrt(abs(ms- log(Mhat)))

		#ss <- seq(min(ss1,ss2,ss3), max(ss1,ss2,ss3), length=1000)
		
		sig<-seq(0,10000,by=0.1)

		#grid.ms <- expand.grid(mu=ms, sig=ss)
		#mu=grid.ms[,1]
		#sig=grid.ms[,2]

		score.bec2 <- ( (((Lhat)-qnorm((1-pihat)/2, mean=mu, sd=sig))^2)*1/2 +
			(((Uhat) - qnorm((pihat+(1-pihat)/2), mean=mu, sd=sig))^2 )*1/2)

	return(as.data.frame(cbind(score.mode=score.bec2, mu=mu, sig=sig)))
}

######


ss <- function(mu, sig, alpha) {
	m <- mu
	v <- sd(rnorm(1000000,mu, sig))
	#ci <- qnorm(c(1-alpha, .5, alpha), mean=mu, sd=sig)
	ci <- qnorm(c((1-alpha)/2, .5, (alpha+(1-alpha)/2)), mean=mu, sd=sig)
	return(list(mean=m, mode=m, median=m, var=v,lower=ci[1],
		median=ci[2], upper=ci[3]))
}


feedbackN <- function(mu, sig,alpha,low,upp,new.alpha) {
	m <- mu
	#sd(rnorm(1000000,mu, sig))
	#ci <- qnorm(c(1-alpha, .5, alpha, 1-new.alpha, new.alpha), mean=mu, sd=sig)
	ci <- qnorm(c((1-alpha)/2, .5, (alpha+(1-alpha)/2), (1-new.alpha)/2, (new.alpha+(1-new.alpha)/2)), mean=mu, sd=sig)
	

	pci <- pnorm(c(upp,low), mean=mu, sd=sig)
	return(list(lower=ci[1], median=ci[2], upper=ci[3], mean=m, prob.upper=pci[1], 
		prob.lower=pci[2], new.alpha=new.alpha, new.lower=ci[4], new.upper=ci[5]))
}

################################################################
#function used to estimate (finer scale) sigma for Normal distribution
#minimising over ss

#params=5; Lhat=100; Uhat=200; Mhat=150; pihat=0.8
#ss.mu.sig(params,Lhat, Uhat , Mhat,pihat)



ss.mu.sig <- function(params,Lhat, Uhat , Mhat,pihat){
  
  mu.i = params[1]
  sig.i = params[2]
  #print(c(mu.i,sig.i))
  #mu= Mhat
  
  
  sim.Lower = qnorm((1-pihat)/2, mean=mu.i, sd=sig.i)
  sim.Upper = qnorm((pihat+(1-pihat)/2), mean=mu.i, sd=sig.i)
  
  
  sim.Best = mu.i
  
  diff.lw=((Lhat - sim.Lower)^2)  * 1/3
  diff.up=((Uhat - sim.Upper)^2) * 1/3
  diff.best=((Mhat - sim.Best)^2) * 1/3
  
  ss=(diff.lw + diff.up +diff.best )
  (diff.lw + diff.up +diff.best )
  
  sim.max.lower = qnorm(0.01, mean=mu.i, sd=sig.i)  
  if(as.character(sim.max.lower)!="NaN"){  
    if(sim.max.lower<0){ss=ss+10^10}}
  
  return(ss)
} # END ss.mu.sig

################################################################

#MODE
fit.results.mode <-fit.score.mode(Lhat, Uhat, Mhat, pihat,rep(1/2,2)) 

#fit.results.mode[which(fit.results.mode[,1] ==min(fit.results.mode[,1])),]

fit.best.mode.mu <-modal(fit.results.mode[which(fit.results.mode[,1] 
	==min(fit.results.mode[,1])),]$mu)
fit.best.mode.sig <-modal(fit.results.mode[which(fit.results.mode[,1] 
	==min(fit.results.mode[,1])),]$sig)


ss.mode.results <-ss(fit.best.mode.mu, fit.best.mode.sig , pihat)

feedback.mode.results <- feedbackN(fit.best.mode.mu, fit.best.mode.sig, pihat,Lhat, Uhat,new.alpha=new.alpha)

####
#USE OPTIM - ESIMTATE (FINER SCALE) SIGMA
estimate.mu.sig=optim(c(fit.best.mode.mu, fit.best.mode.sig), 
                      fn=ss.mu.sig, Lhat=Lhat, Uhat=Uhat,Mhat=Mhat,
                      pihat=pihat)
#put in new optim figures
fit.best.mode.mu.optim= estimate.mu.sig$par[1]
fit.best.mode.sig.optim= estimate.mu.sig$par[2]

feedback.mode.results.optim <- feedbackN( fit.best.mode.mu.optim,  fit.best.mode.sig.optim, 
                                          pihat,Lhat, Uhat,new.alpha=new.alpha)


############################
#PLOT

par(mfrow=c(1,3))

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

#PLOT

#MODE
plot(rep(c(1),2), rep(c(ss.mode.results$lower, ss.mode.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", 
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1), 
	xaxt = "n", main="a",lwd=2)
lines(rep(2,2),c(ss.mode.results$lower, ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(ss.mode.results$upper,2),lwd=2)
lines(c(1,2),rep(ss.mode.results$mode,2), col=4,lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=3,lwd=2)#best guess
 
#lines(c(1,2), rep(ss.mode.results$median,2), col=4,lwd=2)
legend("topleft", lty=1, col=c(1,1, 4,3, "orange", "orange"), bty="n", legend=c("lower",
	"upper", "maths best guess", "best guess", "new lower","new upper"),cex=.8,lwd=2)
legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")

#plot new lower & upper conf
lines(c(1,2), rep(feedback.mode.results$new.lower,2), col="orange" ,lwd=2)
lines(c(1,2), rep(feedback.mode.results$new.upper,2), col="orange",lwd=2)

##
#MEAN
if (ss.mode.results$lower <0) {
ss.mode.results$lower =0
}
if (feedback.mode.results$new.lower<0){
feedback.mode.results$new.lower =0
}

plot(rep(c(1),2), rep(c(ss.mode.results$lower, ss.mode.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", 
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1), 
	xaxt = "n", main="b",lwd=2)
lines(rep(2,2),c(ss.mode.results$lower, ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(ss.mode.results$upper,2),lwd=2)
lines(c(1,2),rep(ss.mode.results$mean,2), col=4,lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=3,lwd=2)#best guess
 
#plot new lower & upper conf
lines(c(1,2), rep(feedback.mode.results$new.lower,2), col="orange" ,lwd=2)
lines(c(1,2), rep(feedback.mode.results$new.upper,2), col="orange",lwd=2)

##
#MEDIAN
if (ss.mode.results$lower <0) {
ss.mode.results$lower =0
}
if (feedback.mode.results$new.lower<0){
feedback.mode.results$new.lower =0
}

plot(rep(c(1),2), rep(c(ss.mode.results$lower, ss.mode.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", 
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1), 
	xaxt = "n", main="c",lwd=2)
lines(rep(2,2),c(ss.mode.results$lower, ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(ss.mode.results$upper,2),lwd=2)
lines(c(1,2),rep(ss.mode.results$median,2), col=4,lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=3,lwd=2)#best guess
 
#plot new lower & upper conf
lines(c(1,2), rep(feedback.mode.results$new.lower,2), col="orange" ,lwd=2)
lines(c(1,2), rep(feedback.mode.results$new.upper,2), col="orange",lwd=2)


################
#calculate K
#Ksp <-rnorm(10000,mu,sig)
#Ksp <-rnorm(10000, fit.best.mode.mu, fit.best.mode.sig)


############################################
#when normal distribution selected then set ss.mean.results & ss.median.results to be ss.mode.results
#since mean, mode & median is all the same in normal distribution
#same with fit.best


#return(list(pihat=pihat, ss.mode.results=ss.mode.results ,
#	fit.best.mode.mu=fit.best.mode.mu,
#	fit.best.mode.sig=fit.best.mode.sig, 
#	feedback.mode.results=feedback.mode.results))

###
#store selected best type (mode, median, mean)
#best.type = best.type.status[ee.type,3]
#best.type ="N"


return(list(pihat=pihat,
            #ss.mean.results =ss.mode.results , 
            ss.mode.results=ss.mode.results ,
            #ss.median.results =ss.mode.results, 
            fit.best.mode.mu=fit.best.mode.mu,
            fit.best.mode.sig=fit.best.mode.sig, 
            #fit.best.mode.mu.optim=fit.best.mode.mu.optim,
            #fit.best.mode.sig.optim=fit.best.mode.sig.optim,
            #fit.best.mean.mu = fit.best.mode.mu, 
            #fit.best.mean.sig=fit.best.mode.sig, 
            #fit.best.median.mu=fit.best.mode.mu,
            #  fit.best.median.sig=fit.best.mode.sig,
            feedback.mode.results=feedback.mode.results,
            feedback.mode.results.optim = feedback.mode.results.optim))

#,best.type = best.type))



}
