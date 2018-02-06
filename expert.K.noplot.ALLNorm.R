#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

expert.K.noplot.ALLnorm <-function(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type){

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.LN.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.LN.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.Norm.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.Norm.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.LNleftskew.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.LNleftskew.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.LN.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.LN.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.Norm.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.Norm.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.LNleftskew.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.LNleftskew.R")

expertrange <- Uhat-Lhat

####################################################################################################################################################
#if less than 25th quartile
#IF 1
if (Mhat<=expertrange*.25+Lhat){
expert.K.LN.results <- expert.K.noplot.LN(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)

#STORE RESULTS FOR OUTPUT
results <- expert.K.LN.results
which.dist <- c("LN")


####################################################################################################################################################
#if between 25th and 50th quartile
#IF 2
}else if (Mhat<=expertrange*.5+Lhat){

expert.K.LN.results <- expert.K.noplot.LN(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)
expert.K.Norm.results<- expert.K.noplot.Norm(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)

###
#IDENTIFY WHICH DISTRIBUTION IS BETTER

#for normal, get same value for mean,median & mode
Norm.score <- (((abs(Mhat-expert.K.Norm.results$ss.mode.results$mean )^2)/1/3) +
((abs(Lhat-expert.K.Norm.results$ss.mode.results$lower )^2)/1/3) +
((abs(Uhat-expert.K.Norm.results$ss.mode.results$upper)^2)/1/3) )

#for normal, get same value for mean,median & mode
Norm.score <- (
((abs(Lhat-expert.K.Norm.results$ss.mode.results$lower )^2)/1/2) +
((abs(Uhat-expert.K.Norm.results$ss.mode.results$upper)^2)/1/2) )

LN.score<- 
(((abs(Mhat-expert.K.LN.results$ss.mode.results$mode  )^2)/1/3) +
((abs(Lhat-expert.K.LN.results$ss.mode.results$lower )^2)/1/3) +
((abs(Uhat-expert.K.LN.results$ss.mode.results$upper )^2)/1/3) )



####
# IF LOG-NORMAL IS SMALLER
#SELECT LOG-NORMAL AS BEST
#%if (LN.score < Norm.score) {
if (max(LN.score) < Norm.score) {

#STORE RESULTS FOR OUTPUT
results <- expert.K.LN.results
which.dist <- c("LN")


####################
# ELSE NORMAL IS SMALLER
}else{

#STORE RESULTS FOR OUTPUT
results <- expert.K.Norm.results
which.dist <- c("normal")


}


####################################################################################################################################################
#if greater than 50th quartile but less than 75th quartile
#IF 3
}else if (Mhat>expertrange*.5+Lhat & Mhat<expertrange*.75+Lhat){

expert.K.Norm.results<- expert.K.noplot.Norm(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)
expert.K.LNleftskew.results <- expert.K.noplot.LNleftskew(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)


###
#IDENTIFY WHICH DISTRIBUTION IS BETTER

#for normal, get same value for mean,median & mode
Norm.score <- (
((abs(Lhat-expert.K.Norm.results$ss.mode.results$lower )^2)/1/2) +
((abs(Uhat-expert.K.Norm.results$ss.mode.results$upper)^2)/1/2))



LNleftskew.score.mode <- 
(((abs(Mhat-expert.K.LNleftskew.results$ss.mode.results$mode  )^2)/1/3) +
((abs(Lhat-expert.K.LNleftskew.results$ss.mode.results$lower )^2)/1/3) +
((abs(Uhat-expert.K.LNleftskew.results$ss.mode.results$upper )^2)/1/3))


####
# IF NORMAL IS SMALLER
#SELECT NORMAL AS BEST
#%if (Norm.score < LNleftskew.score) {
if (Norm.score < LNleftskew.score.mode)  {

#STORE RESULTS FOR OUTPUT
results <- expert.K.Norm.results
which.dist <- c("normal")


####################
# ELSE MIRROR LOG-NORMAL IS SMALLER
}else{

#STORE RESULTS FOR OUTPUT
results <- expert.K.LNleftskew.results
which.dist <- c("LNleftskew")

}



####################################################################################################################################################
#if greater than 25th quartile
#IF 4
}else { 
expert.K.LNleftskew.results <-  expert.K.noplot.LNleftskew(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)

#STORE RESULTS FOR OUTPUT
results <- expert.K.LNleftskew.results
which.dist <- c("LNleftskew")


}


return(list(pihat=pihat, 
	ss.mode.results=results$ss.mode.results,
		fit.best.mode.mu=results$fit.best.mode.mu,
	fit.best.mode.sig=results$fit.best.mode.sig, 
	feedback.mode.results=results$feedback.mode.results,
	Ksp=results$Ksp, MhatK=results$MhatK, which.dist=which.dist))

}








