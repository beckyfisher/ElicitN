#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

expert.K.Norm.just1plot <-function(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type){

source('modal.R')

fit.score.mode <- function(Lhat, Uhat, Mhat, pihat, w=rep(1/3,3)) {
	
	#possible mu
	mu <- Mhat
		
		sig<-seq(0,10000,by=0.1)

		score.norm <- ( (((Lhat)-qnorm(1-pihat, mean=mu, sd=sig))^2)*1/2 +
			(((Uhat) - qnorm(pihat, mean=mu, sd=sig))^2 )*1/2)

	return(as.data.frame(cbind(score.mode=score.norm, mu=mu, sig=sig)))
}

######


ss <- function(mu, sig, alpha) {
	m <- mu
	v <- sd(rnorm(1000000,mu, sig))
	ci <- qnorm(c(1-alpha, .5, alpha), mean=mu, sd=sig)
	return(list(mean=m, mode=m, median=m, var=v,lower=ci[1],
		median=ci[2], upper=ci[3]))
}


feedbackN <- function(mu, sig,alpha,low,upp,new.alpha) {
	m <- mu
	#sd(rnorm(1000000,mu, sig))
	ci <- qnorm(c(1-alpha, .5, alpha, 1-new.alpha, new.alpha), mean=mu, sd=sig)

	pci <- pnorm(c(upp,low), mean=mu, sd=sig)
	return(list(lower=ci[1], median=ci[2], upper=ci[3], mean=m, mode=m, prob.upper=pci[1], 
		prob.lower=pci[2], new.alpha=new.alpha, new.lower=ci[4], new.upper=ci[5]))
}

#############################

#MODE
fit.results.mode <-fit.score.mode(Lhat, Uhat, Mhat, pihat,rep(1/2,2)) 

fit.best.mode.mu <-modal(fit.results.mode[which(fit.results.mode[,1] 
	==min(fit.results.mode[,1])),]$mu)
fit.best.mode.sig <-modal(fit.results.mode[which(fit.results.mode[,1] 
	==min(fit.results.mode[,1])),]$sig)


ss.mode.results <-ss(fit.best.mode.mu, fit.best.mode.sig , pihat)

feedback.mode.results <- feedbackN(fit.best.mode.mu, fit.best.mode.sig, pihat,Lhat, Uhat,new.alpha=new.alpha)

############################
#PLOT

X11()
par(mfrow=c(1,1))

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
	xaxt = "n", lwd=2)
lines(rep(2,2),c(ss.mode.results$lower, ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(ss.mode.results$upper,2),lwd=2)
lines(c(1,2),rep(ss.mode.results$mode,2), col="red",lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=1,lwd=2)#best guess
 
legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE

legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")

#plot new lower & upper conf
lines(c(1,2), rep(feedback.mode.results$new.lower,2), col="red" ,lwd=2)
lines(c(1,2), rep(feedback.mode.results$new.upper,2), col="red",lwd=2)


################
#calculate K
Ksp <-rnorm(10000, fit.best.mode.mu, fit.best.mode.sig)


#################
#Store values
ss.mode.results=as.list(ss.mode.results)
fit.best.mode.sig=fit.best.mode.sig
fit.best.mode.mu = fit.best.mode.mu
feedback.mode.results=feedback.mode.results


return(list(pihat=pihat,ss.mode.results=ss.mode.results ,
	fit.best.mode.mu=fit.best.mode.mu,
	fit.best.mode.sig=fit.best.mode.sig, 
	feedback.mode.results=feedback.mode.results,
 Ksp=Ksp, MhatK=Mhat))


}
