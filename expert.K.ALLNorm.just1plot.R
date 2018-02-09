#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

expert.K.ALLnorm.just1plot <-function(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type=1){

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.LN.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.LN.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.Norm.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.Norm.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.LNleftskew.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.LNleftskew.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.LN.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.LN.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.Norm.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.Norm.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.LNleftskew.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.LNleftskew.just1plot.R")

expertrange <- Uhat-Lhat

####################################################################################################################################################
#if Lhat & Uhat are zero
#e.g. zero number of species are cryptic

if (Lhat==0 & Uhat==0){

results <- list(
ss.mode.results=0,
fit.best.mode.mu=0, 
fit.best.mode.sig=0, 
feedback.mode.results=0, 
Ksp=0 , 
MhatK=0)

which.dist=0

#ELSE LHAT & UHAT >0
} else {

####################################################################################################################################################
#if less than 25th quartile
#IF 1
if (Mhat<=expertrange*.25+Lhat){
expert.K.LN.results <- expert.K.LN.just1plot(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type)

#STORE RESULTS FOR OUTPUT
results <- expert.K.LN.results
which.dist <- c("LN")

####################################################################################################################################################
#if between 25th and 50th quartile
#IF 2
}else if (Mhat<=expertrange*.5+Lhat){

expert.K.LN.results <- expert.K.noplot.LN.just1plot(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type)
expert.K.Norm.results<- expert.K.noplot.Norm.just1plot(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type)


###
#IDENTIFY WHICH DISTRIBUTION IS BETTER

#for normal, get same value for mean,median & mode
Norm.score <- (
((abs(Lhat-expert.K.Norm.results$ss.mode.results$lower )^2)/1/2) +
((abs(Uhat-expert.K.Norm.results$ss.mode.results$upper)^2)/1/2) )

LN.score.mode <- 
(((abs(Mhat-expert.K.LN.results$ss.mode.results$mode  )^2)/1/3) +
((abs(Lhat-expert.K.LN.results$ss.mode.results$lower )^2)/1/3) +
((abs(Uhat-expert.K.LN.results$ss.mode.results$upper )^2)/1/3) )

######################
#IF 2A.1
# IF LOG-NORMAL IS SMALLER
#SELECT LOG-NORMAL AS BEST
if (LN.score.mode < Norm.score) {

#STORE RESULTS FOR OUTPUT
results <- expert.K.LN.results
which.dist <- c("LN")

###
#plot 1
X11()
par(mfrow=c(1,1), mar=c(1, 4.5, 4,.75)+.5, mgp=c(3, .75,0), omi=c(.1, .5, .1,.1))

if (expert.K.LN.results$ss.mode.results$lower <1) {
expert.K.LN.results$ss.mode.results$lower =1
}
if (expert.K.LN.results$feedback.mode.results$new.lower <1){
expert.K.LN.results$feedback.mode.results$new.lower =1
}

#find min & max for plot

plot.K.min <- min(expert.K.LN.results$ss.mode.results$lower, expert.K.LN.results$feedback.mode.results$new.lower)

plot.K.max <- max(expert.K.LN.results$ss.mode.results$upper,
	expert.K.LN.results$feedback.mode.results$new.upper)

plot(rep(c(1),2), rep(c(expert.K.LN.results$ss.mode.results$lower, expert.K.LN.results$ss.mode.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", main="N",
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),c(expert.K.LN.results$ss.mode.results$lower, expert.K.LN.results$ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(expert.K.LN.results$ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(expert.K.LN.results$ss.mode.results$upper,2),lwd=2)
lines(c(1,2), rep(expert.K.LN.results$ss.mode.results$mode,2),col="red",lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=1,lwd=2)#best guess
 
legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE

legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")



#plot new lower & upper conf
lines(c(1,2), rep(expert.K.LN.results$feedback.mode.results$new.lower,2), col="red" ,lwd=2)
lines(c(1,2), rep(expert.K.LN.results$feedback.mode.results$new.upper,2), col="red",lwd=2)

####################
# ELSE NORMAL IS SMALLER
#IF 2A.2
}else{

#STORE RESULTS FOR OUTPUT
results <- expert.K.Norm.results
which.dist <- c("normal")


####
#plot
X11()
par(mfrow=c(1,1), mar=c(1, 4.5, 4,.75)+.5, mgp=c(3, .75,0), omi=c(.1, .5, .1,.1))

if (expert.K.Norm.results$ss.mode.results$lower <1) {
expert.K.Norm.results$ss.mode.results$lower =1
}
if (expert.K.Norm.results$feedback.mode.results$new.lower <1){
expert.K.Norm.results$feedback.mode.results$new.lower =1
}

#find min & max for plot

plot.K.min <- min(expert.K.Norm.results$ss.mode.results$lower,
	expert.K.Norm.results$feedback.mode.results$new.lower)

plot.K.max <- max(expert.K.Norm.results$ss.mode.results$upper,
	expert.K.Norm.results$feedback.mode.results$new.upper)

plot(rep(c(1),2), rep(c(expert.K.Norm.results$ss.mode.results$lower, expert.K.Norm.results$ss.mode.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", main="N",
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),c(expert.K.Norm.results$ss.mode.results$lower, expert.K.Norm.results$ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(expert.K.Norm.results$ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(expert.K.Norm.results$ss.mode.results$upper,2),lwd=2)

lines(c(1,2), rep(expert.K.Norm.results$ss.mode.results$mode,2),col="red",lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=1,lwd=2)#best guess
 
legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE
	
legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")



#plot new lower & upper conf
lines(c(1,2), rep(expert.K.Norm.results$feedback.mode.results$new.lower,2), col="red" ,lwd=2)
lines(c(1,2), rep(expert.K.Norm.results$feedback.mode.results$new.upper,2), col="red",lwd=2)
}




####################################################################################################################################################
#if greater than 50th quartile but less than 75th quartile
#IF 3
}else if (Mhat>expertrange*.5+Lhat & Mhat<expertrange*.75+Lhat){

expert.K.Norm.results<- expert.K.noplot.Norm.just1plot(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type)
expert.K.LNleftskew.results <- expert.K.noplot.LNleftskew.just1plot(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type)

###########################
#IDENTIFY WHICH DISTRIBUTION IS BETTER

#for normal, get same value for mean,median & mode
Norm.score <- (
((abs(Lhat-expert.K.Norm.results$ss.mode.results$lower )^2)/1/2) +
((abs(Uhat-expert.K.Norm.results$ss.mode.results$upper)^2)/1/2))

LNleftskew.score.mode <- 
(((abs(Mhat-log(expert.K.LNleftskew.results$ss.mode.results$mode  ))^2)/1/3) +
((abs(Lhat-log(expert.K.LNleftskew.results$ss.mode.results$lower ))^2)/1/3) +
((abs(Uhat-log(expert.K.LNleftskew.results$ss.mode.results$upper ))^2)/1/3))


####
#if 3A.1
# IF NORMAL IS SMALLER
#SELECT NORMAL AS BEST
if (Norm.score < LNleftskew.score.mode) {

#STORE RESULTS FOR OUTPUT
results <- expert.K.Norm.results
which.dist <- c("normal")

if (expert.K.Norm.results$ss.mode.results$lower <1) {
expert.K.Norm.results$ss.mode.results$lower=1
}
if (expert.K.Norm.results$feedback.mode.results$new.lower <1){
expert.K.Norm.results$feedback.mode.results$new.lower =1
}

####
#plot

X11()
par(mfrow=c(1,1), mar=c(1, 4.5, 4,.75)+.5, mgp=c(3, .75,0), omi=c(.1, .5, .1,.1))

#find min & max for plot

plot.K.min <- min(expert.K.Norm.results$ss.mode.results$lower,
	expert.K.Norm.results$feedback.mode.results$new.lower)

plot.K.max <- max(expert.K.Norm.results$ss.mode.results$upper,
	expert.K.Norm.results$feedback.mode.results$new.upper)

plot(rep(c(1),2), rep(c(expert.K.Norm.results$ss.mode.results$lower, expert.K.Norm.results$ss.mode.results$upper),1)
	 , xlim=c(0,3), xlab="", ylab="", type="l", main="N",
	ylim=c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),c(expert.K.Norm.results$ss.mode.results$lower, expert.K.Norm.results$ss.mode.results$upper),lwd=2)
lines(c(1,2), rep(expert.K.Norm.results$ss.mode.results$lower,2),lwd=2)
lines(c(1,2), rep(expert.K.Norm.results$ss.mode.results$upper,2),lwd=2)

lines(c(1,2), rep(expert.K.Norm.results$ss.mode.results$mode,2),col="red",lwd=2)#maths
lines(c(1,2), rep(Mhat,2),col=1,lwd=2)#best guess
 
legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE
	
legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")



#plot new lower & upper conf
lines(c(1,2), rep(expert.K.Norm.results$feedback.mode.results$new.lower,2), col="red" ,lwd=2)
lines(c(1,2), rep(expert.K.Norm.results$feedback.mode.results$new.upper,2), col="red",lwd=2)

####################
#3A.2
# ELSE MIRROR LOG-NORMAL IS SMALLER
}else{

#STORE RESULTS FOR OUTPUT
results <- expert.K.LNleftskew.results
which.dist <- c("LNleftskew")

if (expert.K.LNleftskew.results$ss.mode.results$lower <1) {
expert.K.LNleftskew.results$ss.mode.results$lower=1
}
if (expert.K.LNleftskew.results$feedback.mode.results$new.lower<1){
expert.K.LNleftskew.results$feedback.mode.results$new.lower =1
}

####
#plot

X11()
par(mfrow=c(1,1), mar=c(1, 4.5, 4,.75)+.5, mgp=c(3, .75,0), omi=c(.1, .5, .1,.1))

#find min & max for plot

plot.K.min <- min(expert.K.LNleftskew.results$ss.mode.results$lower,
	expert.K.LNleftskew.results$feedback.mode.results$new.lower)

plot.K.max <- max(expert.K.LNleftskew.results$ss.mode.results$upper,
	expert.K.LNleftskew.results$feedback.mode.results$new.upper)

plot(rep(c(1),2), log(rep(c(expert.K.LNleftskew.results$ss.mode.results$lower, expert.K.LNleftskew.results$ss.mode.results$upper),1))
	 , xlim=c(0,3), xlab="", ylab="", type="l", main="N",
	ylim=log(c(plot.K.min-plot.K.min*.1, plot.K.max+plot.K.max*.1)), 
	xaxt = "n", lwd=2, las=2)
lines(rep(2,2),log(c(expert.K.LNleftskew.results$ss.mode.results$lower, expert.K.LNleftskew.results$ss.mode.results$upper)),lwd=2)
lines(c(1,2), log(rep(expert.K.LNleftskew.results$ss.mode.results$lower,2)),lwd=2)
lines(c(1,2), log(rep(expert.K.LNleftskew.results$ss.mode.results$upper,2)),lwd=2)
lines(c(1,2), log(rep(expert.K.LNleftskew.results$ss.mode.results$mode,2)),col="red",lwd=2)#maths
lines(c(1,2), log(rep(Mhat,2)),col=1,lwd=2)#best guess

legend("topleft", lty=1, col=c(1, "red"), bty="n", legend=c("elicited parameters",
	"estimated (fitted) "),cex=.8) #BEST GUESS IS MODE

legend("topright", paste("new 
sureness =", new.alpha), cex=.8, bty="n")

#plot new lower & upper conf
lines(c(1,2), log(rep(expert.K.LNleftskew.results$feedback.mode.results$new.lower,2)), col="red" ,lwd=2)
lines(c(1,2), log(rep(expert.K.LNleftskew.results$feedback.mode.results$new.upper,2)), col="red",lwd=2)

}



####################################################################################################################################################
#if greater than 25th quartile
#IF 4
}else { #(Mhat>=expertrange*.75+Lhat)

expert.K.LNleftskew.results <-  expert.K.LNleftskew.just1plot(Lhat, Uhat, Mhat, pihat, new.alpha,best.type.status, ee.type)

#STORE RESULTS FOR OUTPUT
results <- expert.K.LNleftskew.results
which.dist <- c("LNleftskew")


}

}

return(list(pihat=pihat, 
	ss.mode.results=results$ss.mode.results,
	fit.best.mode.mu=results$fit.best.mode.mu,
	fit.best.mode.sig=results$fit.best.mode.sig, 
	feedback.mode.results=results$feedback.mode.results,
	Ksp=log(results$Ksp) , MhatK=results$MhatK, which.dist=which.dist,
	new.alpha=new.alpha))
}








