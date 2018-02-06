#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

expert.P.BETA <-function(Lower.P, Upper.P, Best.P, Sureness.P, new.sure, best.type.status, ee.type){

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/modal.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source('modal.R')


fitBETA.score.mode <- function(Lower.P, Upper.P, Best.P, Sureness.P, w=rep(1/3,3)) {
	
	if (Best.P==0){
	Best.P=0.0000000000001
	}
	#possible alpha & beta
	
	alphas<-seq(.01,50,.01)
	betas <- (alphas - 1 + (2- alphas)*Best.P)/Best.P
   	bad <- betas <0;
    	alphas <- alphas[!bad]; betas <- betas[!bad];

	#beta.rs <- seq(0.001, 100, by=.001)

	#euclid score
	score.beta <- ( ((Lower.P-qbeta(1-Sureness.P, alphas, betas))^2)*1/3 +
			((Upper.P - qbeta(Sureness.P, alphas, betas))^2 )*1/3+
			(Best.P - ((alphas - 1)/(alphas + betas -2))^2)*1/3) #mode

	return(as.data.frame(cbind(score.beta.mode=score.beta, 
	alpha=alphas, beta=betas)))
	
}




######
ssBETA <- function(alpha, beta, Sureness.P) {
	m <- alpha/(alpha + beta) #mean
	v <- (alpha*beta)/((alpha+beta)^2*(alpha+beta+1))
	mo <- (alpha - 1)/(alpha + beta - 2) #mode
	sk <- (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta))
	ku <- 6*((alpha^3)-(alpha^2)*(2*beta-1)+(beta^2)*(beta+1)-2*alpha*beta*(beta+2))/
		 (alpha*beta*(alpha+beta+2)*(alpha+beta+3))
	ci <- qbeta(c(1-Sureness.P, .5, Sureness.P), alpha, beta)
	return(list(mean=m, var=v, mode=mo, skew=sk, kurtosis=ku, lower=ci[1],
		median=ci[2], upper=ci[3]))
}


feedbackBETA <- function(alpha, beta, Sureness.P, LHatC, Upper.P, new.sure) {
	m <- alpha/(alpha + beta) #mean
	mo <- (alpha - 1)/(alpha + beta - 2) #mode
	ci <- qbeta(c(1-Sureness.P, .5, Sureness.P, 1-new.sure, new.sure), alpha, beta)

	pci <- pbeta(c(Upper.P,Lower.P), alpha, beta)
	return(list(lower=ci[1], median=ci[2], upper=ci[3], mean=m, mode=mo, prob.upper=pci[1], prob.lower=pci[2], 
		new.sure=new.sure, new.lower=ci[4], new.upper=ci[5]))
}




#############################


#MODE
fitBETA.results.mode <-fitBETA.score.mode(Lower.P, Upper.P, Best.P, Sureness.P, w=rep(1/3,3))

#fitBETA.results.mode[which(fitBETA.results.mode[,1] ==min(fitBETA.results.mode[,1], na.rm =TRUE)),]

fitBETA.best.mode.alpha <-modal(fitBETA.results.mode[which(abs(fitBETA.results.mode$score.beta.mode) 
	==min(abs(fitBETA.results.mode$score.beta.mode), na.rm=TRUE)),]$alpha)
fitBETA.best.mode.beta <-modal(fitBETA.results.mode[which(abs(fitBETA.results.mode$score.beta.mode) 
	==min(abs(fitBETA.results.mode$score.beta.mode), na.rm=TRUE)),]$beta)


ssBETA.mode.results <-ssBETA(fitBETA.best.mode.alpha, fitBETA.best.mode.beta , Sureness.P)

feedbackBETA.mode.results <- feedbackBETA(fitBETA.best.mode.alpha, fitBETA.best.mode.beta, Sureness.P,Lower.P, Upper.P, new.sure)



return(list(ssBETA.mode.results=ssBETA.mode.results ,
	fitBETA.best.mode.alpha=fitBETA.best.mode.alpha, fitBETA.best.mode.beta=fitBETA.best.mode.beta, 
	feedbackBETA.mode.results =feedbackBETA.mode.results))
}