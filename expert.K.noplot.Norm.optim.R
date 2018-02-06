# BRUTE FORCE ESTIMATION OF mu,sig in a NORMAL
# compare theoretical to actual lower and upper pihat-percentiles Lhat & Uhat
# and theoretical to actual mode, mean or median Mhat


#12 Jan 2010
#previous version works ("expert.K.NormOLD1.R")
#changes
#1) ssN to ss
#2) fitN to fit

#29/6/2012
#ADD OPTIM TO ESIMATE SIGMA PARAMETER FOR NORMAL DISTRIBUTION


######
#TEST

#rm(list=ls())

#Lhat <- 100; Uhat<-200; pihat <-.8; Mhat <- 145

#source("expert.K.noplot.Norm.optim.R")
#Test.K.Norm <- expert.K.noplot.Norm.optim(Lhat, Uhat, Mhat, pihat, .95)

#COMPARE LOWER & UPPER (BASED ON SURENESS OF 80%) BETWEEN USING OLD METHOD VS OPTIM

#SIGMA OLD METHOD
#Test.K.Norm$fit.best.mode.sig
#SIGMA USING OPTIM
#Test.K.Norm$fit.best.mode.sig.optim

#LOWER OLD METHOD
#Test.K.Norm$feedback.mode.results$lower
#LOWER USING OPTIM
#Test.K.Norm$feedback.mode.results.optim$lower

#UPPER OLD METHOD
#Test.K.Norm$feedback.mode.results$upper
#UPPER USING OPTIM
#Test.K.Norm$feedback.mode.results.optim$upper

#SUMS OF SQUARE FOR ESIMATES USING OLD METHOD
#(((Lhat- Test.K.Norm$feedback.mode.results$lower)^2)*1/3 +   ((Lhat- Test.K.Norm$feedback.mode.results$upper)^2)*1/3 +   ((Lhat- Test.K.Norm$feedback.mode.results$mode)^2)*1/3 )

#SUMS OF SQUARE FOR ESTIMATES USING OPTIM
#(((Lhat- Test.K.Norm$feedback.mode.results.optim$lower)^2)*1/3 +   ((Lhat- Test.K.Norm$feedback.mode.results.optim$upper)^2)*1/3 +  ((Lhat- Test.K.Norm$feedback.mode.results.optim$mode)^2)*1/3 )


######################################################################

expert.K.noplot.Norm <-function(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type){

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/modal.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source('modal.R')


fit.score.mode <- function(Lhat, Uhat, Mhat, pihat, w=rep(1/3,3)) {
	
	#possible mu
	mu <- Mhat
  #SINCE SYMMETRIC - THEN MODE AND MEAN ARE EQUAL 
		
  	sig<-seq(0,10000,by=0.1)

	
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
	return(list(lower=ci[1], median=ci[2], upper=ci[3], mean=m, mode=m, prob.upper=pci[1], 
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

#USE OPTIM - ESIMTATE (FINER SCALE) SIGMA
estimate.mu.sig=optim(c(fit.best.mode.mu, fit.best.mode.sig), 
                    fn=ss.mu.sig, Lhat=Lhat, Uhat=Uhat,Mhat=Mhat,
                    pihat=pihat)
#put in new optim figures
fit.best.mode.mu.optim= estimate.mu.sig$par[1]
fit.best.mode.sig.optim= estimate.mu.sig$par[2]

feedback.mode.results.optim <- feedbackN( fit.best.mode.mu.optim,  fit.best.mode.sig.optim, 
                                         pihat,Lhat, Uhat,new.alpha=new.alpha)

################
#calculate K
#Ksp <-rnorm(10000,mu,sig)
Ksp <-rnorm(10000, fit.best.mode.mu, fit.best.mode.sig)


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
            #	fit.best.median.sig=fit.best.mode.sig,
            feedback.mode.results=feedback.mode.results,
            feedback.mode.results.optim=feedback.mode.results.optim))
            #	feedback.mean.results=feedback.mode.results,
            #	feedback.median.results=feedback.mode.results, 
            #Ksp=Ksp))
            #, MhatK=Mhat))
	#,best.type = best.type))


}
