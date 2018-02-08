#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

expert.K.noplot.LNleftskew.just1plot <-function(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type){


#store old Mhat - what experts provide
Mhatold <- c(Mhat)
#then change Mhat so that it is right skewed, hence log normal will work
howskew <- c(Uhat-Mhat)
Mhat <- c((Uhat-Mhat)+Lhat)

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/modal.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source('modal.R')

######

ss <- function(mu, sig, alpha) {
	m <- exp(mu + (sig^2)/2)
	v <- (exp(sig^2)-1)*exp(2*mu+sig^2)
	mo <- exp(mu - (sig^2))
	sk <- (exp(sig^2)+2)*sqrt(exp(sig^2)-1)
	ku <- (exp(4*sig^2)+2*exp(3*sig^2)+3*exp(2*sig^2)-6)
	ci <- qlnorm(c(1-alpha, .5, alpha), mean=mu, sd=sig)
	return(list(mean=m, var=v, mode=mo, skew=sk, kurtosis=ku, lower=ci[1],
		median=ci[2], upper=ci[3]))
}


feedbackLN <- function(mu, sig,alpha,low,upp,new.alpha) {
	m <- exp(mu + (sig^2)/2)
	mo <- exp(mu-sig^2)
	ci <- qlnorm(c(1-alpha, .5, alpha, 1-new.alpha, new.alpha), mean=mu, sd=sig)

	pci <- plnorm(c(upp,low), mean=mu, sd=sig)
	return(list(lower=ci[1], median=ci[2], upper=ci[3], mean=m, mode=mo, prob.upper=pci[1], prob.lower=pci[2], 
		new.alpha=new.alpha, new.lower=ci[4], new.upper=ci[5]))
}

#############################

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

		score.LNleftskew <- ( (((Lhat)-qlnorm(1-pihat, mean=mu, sd=sig))^2)*1/3 +
			(((Uhat) - qlnorm(pihat, mean=mu, sd=sig))^2 )*1/3+
			(((Mhat) - exp(mu-sig^2))^2)*1/3) #mode

	return(as.data.frame(cbind(score.mode=score.LNleftskew, mu=mu, sig=sig)))
}
####
#CALCULATE FIT
#FIND BEST MU & SIG
fit.results.mode <-fit.score.mode(Lhat, Uhat, Mhat, pihat,rep(1/3,3)) 

fit.best.mode.mu <-modal(fit.results.mode[which(fit.results.mode$score.mode 
	==min(fit.results.mode$score.mode)),]$mu)
fit.best.mode.sig <-modal(fit.results.mode[which(fit.results.mode$score.mode 
	==min(fit.results.mode$score.mode)),]$sig)


ss.mode.results <-ss(fit.best.mode.mu, fit.best.mode.sig , pihat)

feedback.mode.results <- feedbackLN(fit.best.mode.mu, fit.best.mode.sig, pihat,Lhat, Uhat,new.alpha=new.alpha)

################
#calculate K
Ksp <-Mhatold-(Mhat-rlnorm(10000, fit.best.mode.mu, fit.best.mode.sig))

##
#change mode back so that distribution is left skewed
ss.mode.results$mode <-Mhatold-(Mhat-ss.mode.results$mode)



#return MhatK 
MhatK <-as.data.frame(cbind(Mhat=Mhat, Mhatold=Mhatold))

return(list(pihat=pihat,  ss.mode.results=ss.mode.results ,
	 fit.best.mode.mu=fit.best.mode.mu,
	fit.best.mode.sig=fit.best.mode.sig,
	feedback.mode.results=feedback.mode.results, Ksp=Ksp, MhatK=MhatK))
}
