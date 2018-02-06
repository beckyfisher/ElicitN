#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

expert.just1plot<-function(Lower.N, Upper.N,  Best.N, 
Lower.P, Upper.P, Best.P, 
Lower.M, Upper.M,  Best.M, Sureness,new.alpha,
expert.K.ALLnorm.results,best.type.status){

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.P.BETA.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.P.BETA.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.P.BETA.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.P.BETA.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.ALLNorm.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.ALLNorm.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.ALLNorm.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.ALLNorm.just1plot.R")

#############################################
#IF UHAT = 0 
#e.g. zero number of species are cryptic

#put all Uhat into a matrix
uhat.matrix <- c(Upper.N, Upper.P,Upper.M)

#test which Uhat does not contain NA
uhat.na <- which(c(is.na(Upper.N),is.na(Upper.P), is.na(Upper.M))=="FALSE")

#test if Uhat without NA value is equal to zero
if (uhat.matrix[uhat.na] ==0){

ss.mode.results <- list( mean=0,var=0, mode=0, skew=0, kurtosis=0, lower=0, median=0, upper=0)

feedback.mode.results <- list(lower=0,  median=0,    upper=0,   mean=0,  mode=0, prob.upper=0, 
prob.lower=0, new.alpha=0, new.lower =0, new.upper=0)


output <- list(ss.mode.results=ss.mode.results,
fit.best.mode.mu=0, 
fit.best.mode.sig=0, 
feedback.mode.results=feedback.mode.results,
Ksp=0, 
MhatK=0,
which.dist=0)

best.type =0
Cryp=0

} else {
X11()
##############################################
#IF 1
#EXPERT PROVIDES % OF KNOWN SPECIES (INFLATION)
if (is.na(Best.P) =="FALSE") {

expert.P.BETA.results <- expert.P.BETA.just1plot(Lower.P, Upper.P, Best.P, Sureness, new.alpha, best.type.status, ee.type=2)

###
#INFO FROM ELICITED KNOWN SPP.
#pihat is returned from expert.K.LN function
pihat <- expert.K.ALLnorm.results$pihat

#which distribution was used in expert.K.ALLnorm.results
which.dist <- expert.K.ALLnorm.results$which.dist

MhatK <-expert.K.ALLnorm.results$MhatK

#get values of mu, sig, lower.K & upper.K

mu <- expert.K.ALLnorm.results$fit.best.mode.mu
sig <- expert.K.ALLnorm.results$fit.best.mode.sig



eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/feedback.AllNorm.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("feedback.AllNorm.R")
feedback.AllNorm.results <- feedback.AllNorm(mu, sig,Sureness,new.alpha, MhatK, which.dist) 

lower.K <- feedback.AllNorm.results$lower
upper.K <- feedback.AllNorm.results$upper
mode.K <- feedback.AllNorm.results$mode
new.lower <- feedback.AllNorm.results$new.lower
new.upper <- feedback.AllNorm.results$new.upper

if (new.lower<0){
new.lower=0
}
if (lower.K<0){
lower.K=0
}


#####
#PLOT
#PERCENT of KNOWN SPECIES
####

X11()
par(mfrow=c(2,1), mar=c(1, 4.5, 4,.75)+.5, mgp=c(3, .75,0), omi=c(.1, .5, .1,.1))


if (expert.P.BETA.results$ssBETA.mode.results$lower <0) {
expert.P.BETA.results$ssBETA.mode.results$lower =0
}
if (expert.P.BETA.results$feedbackBETA.mode.results$new.lower<0){
expert.P.BETA.results$feedbackBETA.mode.results$new.lower=0
}

#MODE

plot.P.min <- min(
	expert.P.BETA.results$ssBETA.mode.results$lower,
	expert.P.BETA.results$feedbackBETA.mode.results$new.lower)

plot.P.max <- max(
	expert.P.BETA.results$ssBETA.mode.results$upper,
	expert.P.BETA.results$feedbackBETA.mode.results$new.upper)

#options(scipen=100)

plot(rep(c(1),2), rep(c(expert.P.BETA.results$ssBETA.mode.results$lower*100, 
	expert.P.BETA.results$ssBETA.mode.results$upper*100),1)
	 , xlim=c(-0.5,3), xlab="", ylab="", main="%", type="l", 
	ylim=c((plot.P.min-plot.P.min*.1)*100, (plot.P.max+plot.P.max*.1)*100), 
	xaxt = "n", lwd=2, las=2)


lines(rep(2,2),c(expert.P.BETA.results$ssBETA.mode.results$lower*100, 
	expert.P.BETA.results$ssBETA.mode.results$upper*100), lwd=2)
lines(c(1,2), rep(expert.P.BETA.results$ssBETA.mode.results$lower*100,2), lwd=2)
lines(c(1,2), rep(expert.P.BETA.results$ssBETA.mode.results$upper*100,2), lwd=2)

lines(c(1,2), rep(expert.P.BETA.results$ssBETA.mode.results$mode*100,2),col="red", lwd=2)
lines(c(1,2), rep(Best.P*100,2), lwd=2) #best guess


legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE

legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")

#plot new lower & upper conf
lines(c(1,2), rep(expert.P.BETA.results$feedbackBETA.mode.results$new.lower*100,2), col="red" , lwd=2)
lines(c(1,2), rep(expert.P.BETA.results$feedbackBETA.mode.results$new.upper*100,2), col="red", lwd=2)


###
# USE INFO FROM ELICITED KNOWN SPP. (EXPERT.N.LN.RESULTS
#TO PLOT N (INFLATION (%) * K)
###

#calculate K
Ksp <- expert.K.ALLnorm.results$Ksp

#calculate number of species for cryptic species
Cryp <- rbeta(10000,expert.P.BETA.results$fitBETA.best.mode.alpha, expert.P.BETA.results$fitBETA.best.mode.beta)

#calculate number of species (including cryptic species
#Nsp <- Ksp*(Cryp+1)
#calculate number of cryptic spp.
Cryp<- Ksp*(Cryp)


###
#plot N
###
if (expert.P.BETA.results$ssBETA.mode.results$lower<0) {
expert.P.BETA.results$ssBETA.mode.results$lower =0
}
if (lower.K<0) {
lower.K=0
}
if (expert.P.BETA.results$feedbackBETA.mode.results$new.lower<0){
expert.P.BETA.results$feedbackBETA.mode.results$new.lower =0
}


plot.PN.min <- min(
	expert.P.BETA.results$ssBETA.mode.results$lower * lower.K,
	expert.P.BETA.results$feedbackBETA.mode.results$new.lower * lower.K)

plot.PN.max <- max(
	expert.P.BETA.results$ssBETA.mode.results$upper* upper.K,
	expert.P.BETA.results$feedbackBETA.mode.results$new.upper* new.upper)

options(scipen=100)

plot(rep(c(1),2), rep(c(expert.P.BETA.results$ssBETA.mode.results$lower * lower.K, 
	expert.P.BETA.results$ssBETA.mode.results$upper * upper.K),1)
	 , xlim=c(-0.5,3), xlab="", ylab="", main="N", type="l", 
	ylim=c(plot.PN.min-plot.PN.min*.1, plot.PN.max+plot.PN.max*.1), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),c(expert.P.BETA.results$ssBETA.mode.results$lower * lower.K, 
	expert.P.BETA.results$ssBETA.mode.results$upper * upper.K), lwd=2)
lines(c(1,2), rep(expert.P.BETA.results$ssBETA.mode.results$lower * lower.K,2), lwd=2)
lines(c(1,2), rep(expert.P.BETA.results$ssBETA.mode.results$upper * upper.K,2), lwd=2)

lines(c(1,2), rep(expert.P.BETA.results$ssBETA.mode.results$mode * mode.K,2),col="red", lwd=2)

#plot new lower & upper conf
lines(c(1,2), rep(expert.P.BETA.results$feedbackBETA.mode.results$new.lower * new.lower,2)
	, col="red" , lwd=2)
lines(c(1,2), rep(expert.P.BETA.results$feedbackBETA.mode.results$new.upper * new.upper,2)
	, col="red", lwd=2)


#####
#GET RESULTS FOR OUTPUT

infl.lower=expert.P.BETA.results$ssBETA.mode.results$lower * lower.K 
infl.upper=expert.P.BETA.results$ssBETA.mode.results$upper * upper.K
infl.best =expert.P.BETA.results$ssBETA.mode.results$mode * mode.K
infl.type="p"

###
#store selected best type (mode, median, mean)
best.type <- expert.P.BETA.results$best.type


######################################################################################################
# IF 2
# EXPERT PROVIDES N OF UNKNOWN SPECIES
} else if (is.na(Best.N) =="FALSE") {

expert.N.noplot.ALLnorm.results <- expert.K.noplot.ALLnorm.just1plot(Lower.N, Upper.N, Best.N, Sureness, new.alpha, best.type.status, ee.type=1)

#####
#GET RESULTS FOR OUTPUT

infl.lower=expert.N.noplot.ALLnorm.results$ss.mode.results$lower #%Lower.N
infl.upper=expert.N.noplot.ALLnorm.results$ss.mode.results$upper #%Upper.N
infl.best =expert.N.noplot.ALLnorm.results$ss.mode.results$mode #%Best.N
infl.type="n"
###
#store selected best type (mode, median, mean)
best.type <-expert.N.noplot.ALLnorm.results$best.type

#calculate K
Ksp <- expert.K.ALLnorm.results$Ksp

#calculate number of species for cryptic species
Cryp <- expert.N.noplot.ALLnorm.results$Ksp

#calculate number of species (including cryptic species
Nsp <- Ksp+Cryp


########
#PLOT
#NUMBER OF UNDISCOVER OR UNNAMED


par(mfrow=c(1,1), mar=c(1, 4.5, 4,.75)+.5, mgp=c(3, .75,0), omi=c(.1, .5, .1,.1))


#find min & max for plot
if (expert.N.noplot.ALLnorm.results$ss.mode.results$lower<0) {
expert.N.noplot.ALLnorm.results$ss.mode.results$lower =0
}
if (expert.N.noplot.ALLnorm.results$feedback.mode.results$new.lower<0){
expert.N.noplot.ALLnorm.results$feedback.mode.results$new.lower =0
}


plot.N.min <- min(
	expert.N.noplot.ALLnorm.results$ss.mode.results$lower,
	expert.N.noplot.ALLnorm.results$feedback.mode.results$new.lower)

plot.N.max <- max(
	expert.N.noplot.ALLnorm.results$ss.mode.results$upper,
	expert.N.noplot.ALLnorm.results$feedback.mode.results$new.upper)

##
#PLOT
options(scipen=100)

plot(rep(c(1),2), rep(c(expert.N.noplot.ALLnorm.results$ss.mode.results$lower, 
	expert.N.noplot.ALLnorm.results$ss.mode.results$upper),1)
	 , xlim=c(-0.5,3), xlab="", ylab="", type="l", main="N",
	ylim=c(plot.N.min-plot.N.min*.1, plot.N.max+plot.N.max*.1), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),c(expert.N.noplot.ALLnorm.results$ss.mode.results$lower, 
	expert.N.noplot.ALLnorm.results$ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(expert.N.noplot.ALLnorm.results$ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(expert.N.noplot.ALLnorm.results$ss.mode.results$upper,2),lwd=2)
lines(c(1,2), rep(expert.N.noplot.ALLnorm.results$ss.mode.results$mode,2),col="red",lwd=2)
lines(c(1,2), rep(Best.N,2),col=1, lwd=2) #best guess

legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE

legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")


#plot new lower & upper conf
lines(c(1,2), rep(expert.N.noplot.ALLnorm.results$feedback.mode.results$new.lower,2), col="red" ,lwd=2)
lines(c(1,2), rep(expert.N.noplot.ALLnorm.results$feedback.mode.results$new.upper,2), col="red",lwd=2)



###################################################################
# IF 3
# EXPERT PROVIDES M MULTIPLICAB FACTOR

#e.g CRYPTIC SPP. IS X TIMES KNOWN SPECIES
} else {#Best.M = multiplicab factor

expert.M.noplot.ALLnorm.results <- expert.K.noplot.ALLnorm.just1plot(Lower.M, Upper.M, Best.M, Sureness, new.alpha, best.type.status, ee.type=3)

###
#INFO FROM ELICITED KNOWN SPP.
#pihat is returned from expert.K.LN function
pihat <- expert.K.ALLnorm.results$pihat

#which distribution was used in expert.K.ALLnorm.results
which.dist <- expert.K.ALLnorm.results$which.dist

MhatK <-expert.K.ALLnorm.results$MhatK

#get values of mu, sig, lower.K & upper.K

mu <- expert.K.ALLnorm.results$fit.best.mode.mu
sig <- expert.K.ALLnorm.results$fit.best.mode.sig


eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/feedback.AllNorm.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("feedback.AllNorm.R")
feedback.AllNorm.results <- feedback.AllNorm(mu, sig,Sureness,new.alpha, MhatK, which.dist) 

lower.K <- feedback.AllNorm.results$lower
upper.K <- feedback.AllNorm.results$upper
mean.K <- feedback.AllNorm.results$mean
mode.K <- feedback.AllNorm.results$mode
median.K <- feedback.AllNorm.results$median
new.lower <- feedback.AllNorm.results$new.lower
new.upper <- feedback.AllNorm.results$new.upper

if (new.lower<0){
new.lower=0
}
if (lower.K<0){
lower.K=0
}


####
#PLOT

par(mfrow=c(2,1), mar=c(1, 4.5, 4,.75)+.5, mgp=c(3, .75,0), omi=c(.1, .5, .1,.1))

if (expert.M.noplot.ALLnorm.results$ss.mode.results$lower<0) {
expert.M.noplot.ALLnorm.results$ss.mode.results$lower =0
}
if (expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower<0){
expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower =0
}

###
#find min & max for plot

plot.M.min <- min(
	expert.M.noplot.ALLnorm.results$ss.mode.results$lower,
	expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower)

plot.M.max <- max(
	expert.M.noplot.ALLnorm.results$ss.mode.results$upper,
	expert.M.noplot.ALLnorm.results$feedback.mode.results$new.upper)

###
options(scipen=100)

plot(rep(c(1),2), rep(c(expert.M.noplot.ALLnorm.results$ss.mode.results$lower, 
	expert.M.noplot.ALLnorm.results$ss.mode.results$upper),1)
	 , xlim=c(-0.5,3), xlab="", ylab="", main="x", type="l", 
	ylim=c(plot.M.min-plot.M.min*.1, plot.M.max+plot.M.max*.1), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),c(expert.M.noplot.ALLnorm.results$ss.mode.results$lower, 
	expert.M.noplot.ALLnorm.results$ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$ss.mode.results$upper,2),lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$ss.mode.results$mode,2),col="red",lwd=2)
lines(c(1,2), rep(Best.M,2),col=1, lwd=2) #best guess

legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE
	
legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")


#plot new lower & upper conf
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower,2), col="red" ,lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$feedback.mode.results$new.upper,2), col="red",lwd=2)


###
# USE INFO FROM ELICITED KNOWN SPP. (EXPERT.K.LN.RESULTS
#TO PLOT N (M * K)
#where M is MULTIPLICAB FACTOR
###

###
#plot
###
if (expert.M.noplot.ALLnorm.results$ss.mode.results$lower<0) {
expert.M.noplot.ALLnorm.results$ss.mode.results$lower =0
}
if (lower.K<0) {
lower.K=0
}
if (expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower<0){
expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower =0
}

plot.PM.min <- min(
	expert.M.noplot.ALLnorm.results$ss.mode.results$lower * lower.K,
	expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower * lower.K)

plot.PM.max <- max(
	expert.M.noplot.ALLnorm.results$ss.mode.results$upper* upper.K,
	expert.M.noplot.ALLnorm.results$feedback.mode.results$new.upper* new.upper)

options(scipen=100)

plot(rep(c(1),2), rep(c(expert.M.noplot.ALLnorm.results$ss.mode.results$lower * lower.K, 
	expert.M.noplot.ALLnorm.results$ss.mode.results$upper * upper.K),1)
	 , xlim=c(-0.5,3), xlab="", ylab="", main="N", type="l", 
	ylim=c(plot.PM.min-plot.PM.min*.1, plot.PM.max+plot.PM.max*.1), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),c(expert.M.noplot.ALLnorm.results$ss.mode.results$lower * lower.K, 
	expert.M.noplot.ALLnorm.results$ss.mode.results$upper * upper.K), lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$ss.mode.results$lower * lower.K,2), lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$ss.mode.results$upper * upper.K,2), lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$ss.mode.results$mode * mode.K,2),col="red", lwd=2)

#plot new lower & upper conf
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$feedback.mode.results$new.lower * new.lower,2)
	, col="red" , lwd=2)
lines(c(1,2), rep(expert.M.noplot.ALLnorm.results$feedback.mode.results$new.upper * new.upper,2)
	, col="red", lwd=2)


#####
#GET RESULTS FOR OUTPUT

infl.lower=expert.M.noplot.ALLnorm.results$ss.mode.results$lower * lower.K 
infl.upper=expert.M.noplot.ALLnorm.results$ss.mode.results$upper * upper.K
infl.best =expert.M.noplot.ALLnorm.results$ss.mode.results$mode * mode.K
infl.type="m"



#calculate K
Ksp <- expert.K.ALLnorm.results$Ksp

#calculate number of species for cryptic species
Cryp <- expert.M.noplot.ALLnorm.results$Ksp

#calculate number of species (including cryptic species
Nsp <- Ksp*Cryp

#############################################################
#END IF STATEMENT
	}



#CALCULATE N FOR INFLATION
output <- expert.K.noplot.ALLnorm(Lhat=infl.lower, Uhat=infl.upper, Mhat=infl.best, Sureness, 
		new.alpha, best.type.status, ee.type=1)

} 

return(list(pihat=output$pihat, 
	ss.mode.results=output$ss.mode.results,
	fit.best.mode.mu=output$fit.best.mode.mu,
	fit.best.mode.sig=output$fit.best.mode.sig, 
	feedback.mode.results=output$feedback.mode.results,
	Ksp=output$Ksp, MhatK=output$MhatK,which.dist=output$which.dist,
	Cryp=Cryp, new.alpha=new.alpha))

}

