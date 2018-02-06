#ALL NORMAL DISTRIBUTION FUNCTION
#WHICH INCLUDES LOG-NORMAL, NORMAL & MIRROR (LEFT SKEWED) LOG-NORMAL DISTRIBUTION
#IF MHAT IS <25TH QUARTILE THEN USE LOG NORMAL
#IF MHAT IS >75TH QUARTHILE THEN USE MIRROR LOG-NORM

#ELSE USE BOTH LOG-NORM & NORM IF MHAT IS <50QUARTILE 
#& TEST WHICH GETS ESTIMATE CLOSEST TO EXPERTS MHAT

#ELSE USE BOTH NORM & MIRROR LOG-NORM IF MHAT IS >50QUARTILE 
#& TEST WHICH GETS ESTIMATE CLOSEST TO EXPERTS MHAT

########
#TEST
#rm(list=ls())
#best.type.status=cbind("ans"=c("N", "P", "M"),"status"=c(0, 0, 0), "Best.type"=c("a", "a", "a"));ee.type=1
#best.type.status=cbind("ans"=c("N", "P", "M"),"status"=c(1, 1, 1), "Best.type"=c("a", "a", "a"));ee.type=1
#Lhat <- 2000; Uhat<-3000; pihat <-.65; Mhat <- 2000

#Lhat <- 2000; Uhat<-3000; pihat <-.85; Mhat <- 2000
#Lhat <- 2000; Uhat<-3000; pihat <-.85; Mhat <- 2200

#Lhat <- 2000; Uhat<-3000; pihat <-.9; Mhat <- 2500; new.alpha<-.95

#Lhat <- 5000; Uhat<-12000; pihat <-.85; Mhat <- 6000; new.alpha<-.95 # <25quartile
#Lhat <- 5000; Uhat<-12000; pihat <-.85; Mhat <- 7000; new.alpha<-.95 # 25 to 50 quartile
#Lhat <- 5000; Uhat<-12000; pihat <-.85; Mhat <-  8150; new.alpha<-.95 # 45th quartile = expertrange*.45+5000 = 8150
#Lhat <- 5000; Uhat<-12000; pihat <-.9; Mhat <-  8500; new.alpha<-.95 # 50th quartile 

#Lhat <- 5000; Uhat<-12000; pihat <-.9; Mhat <-  8600; new.alpha<-.95 # 50th quartile 
#Lhat <- 5000; Uhat<-12000; pihat <-.9; Mhat <-  8850; new.alpha<-.95 # 55th quartile = expertrange*.55+5000 = 8150
#Lhat <- 5000; Uhat<-12000; pihat <-.9; Mhat <-  9200; new.alpha<-.95 # 60th quartile  =expertrange*.60+5000
#Lhat <- 5000; Uhat<-12000; pihat <-.85; Mhat <- 10000; new.alpha<-.95 # 50 to 75 quartile
#Lhat <- 5000; Uhat<-12000; pihat <-.85; Mhat <- 10250; new.alpha<-.95 #  expertrange*.75+5000


#Lhat <- 5000; Uhat<-12000; pihat <-.85; Mhat <- 11000; new.alpha<-.95 # > 75 quartile

#source("expert.K.noplot.ALLnorm.R")
#Test.allnorm<- expert.K.noplot.ALLnorm(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)

########


expert.K.noplot.ALLnorm <-function(Lhat, Uhat, Mhat, pihat,  new.alpha=0.95, best.type.status=cbind("ans"=c("N", "P", "M")), ee.type=1){

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.LN.optim.median.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.LN.optim.median.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.Norm.optim.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.Norm.optim.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.LNleftskew.optim.median.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.LNleftskew.optim.median.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.LN.optim.median.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.LN.optim.median.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.Norm.optim.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.Norm.optim.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.LNleftskew.optim.median.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.LNleftskew.optim.median.R")

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
Norm.score <- (((abs(Mhat-expert.K.Norm.results$feedback.mode.results.optim$mean )^2)/1/3) +
((abs(Lhat-expert.K.Norm.results$feedback.mode.results.optim$lower )^2)/1/3) +
((abs(Uhat-expert.K.Norm.results$feedback.mode.results.optim$upper)^2)/1/3) )

#for normal, get same value for mean,median & mode
Norm.score <- (
((abs(Lhat-expert.K.Norm.results$feedback.mode.results.optim$lower )^2)/1/2) +
((abs(Uhat-expert.K.Norm.results$feedback.mode.results.optim$upper)^2)/1/2) )

LN.score<- 
(((abs(Mhat-expert.K.LN.results$ss.mode.results$mode  )^2)/1/9) +
((abs(Lhat-expert.K.LN.results$ss.mode.results$lower )^2)/1/9) +
((abs(Uhat-expert.K.LN.results$ss.mode.results$upper )^2)/1/9) +

((abs(Mhat-expert.K.LN.results$ss.mean.results$mean)^2)/1/9) +
((abs(Lhat-expert.K.LN.results$ss.mean.results$lower)^2)/1/9) +
((abs(Uhat-expert.K.LN.results$ss.mean.results$upper)^2)/1/9) +

((abs(Mhat-expert.K.LN.results$ss.median.results$median)^2)/1/9) +
((abs(Lhat-expert.K.LN.results$ss.median.results$lower)^2)/1/9) +
((abs(Uhat-expert.K.LN.results$ss.median.results$upper)^2)/1/9) )

LN.score.mode <- 
(((abs(Mhat-expert.K.LN.results$feedback.mode.results.optim$mode  )^2)/1/3) +
((abs(Lhat-expert.K.LN.results$feedback.mode.results.optim$lower )^2)/1/3) +
((abs(Uhat-expert.K.LN.results$feedback.mode.results.optim$upper )^2)/1/3) )

LN.score.mean <- 
(((abs(Mhat-expert.K.LN.results$ss.mean.results$mean)^2)/1/3) +
((abs(Lhat-expert.K.LN.results$ss.mean.results$lower)^2)/1/3) +
((abs(Uhat-expert.K.LN.results$ss.mean.results$upper)^2)/1/3))

LN.score.median <-
(((abs(Mhat-expert.K.LN.results$ss.median.results$median)^2)/1/3) +
((abs(Lhat-expert.K.LN.results$ss.median.results$lower)^2)/1/3) +
((abs(Uhat-expert.K.LN.results$ss.median.results$upper)^2)/1/3) )



####
# IF LOG-NORMAL IS SMALLER
#SELECT LOG-NORMAL AS BEST
#%if (LN.score < Norm.score) {
if (max(LN.score.mode,LN.score.mean,LN.score.median) < Norm.score) {

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
((abs(Lhat-expert.K.Norm.results$feedback.mode.results.optim$lower )^2)/1/2) +
((abs(Uhat-expert.K.Norm.results$feedback.mode.results.optim$upper)^2)/1/2))
#((abs(Mhat-expert.K.Norm.results$ss.mode.results$mean )^2)/1/3) +


LNleftskew.score <- 
(((abs(Mhat-expert.K.LNleftskew.results$feedback.mode.results.optim$mode  )^2)/1/9) +
((abs(Lhat-expert.K.LNleftskew.results$feedback.mode.results.optim$lower )^2)/1/9) +
((abs(Uhat-expert.K.LNleftskew.results$feedback.mode.results.optim$upper )^2)/1/9) +

((abs(Mhat-expert.K.LNleftskew.results$ss.mean.results$mean)^2)/1/9) +
((abs(Lhat-expert.K.LNleftskew.results$ss.mean.results$lower)^2)/1/9) +
((abs(Uhat-expert.K.LNleftskew.results$ss.mean.results$upper)^2)/1/9) +

((abs(Mhat-expert.K.LNleftskew.results$ss.median.results$median)^2)/1/9) +
((abs(Lhat-expert.K.LNleftskew.results$ss.median.results$lower)^2)/1/9) +
((abs(Uhat-expert.K.LNleftskew.results$ss.median.results$upper)^2)/1/9) )

LNleftskew.score.mode <- 
(((abs(Mhat-expert.K.LNleftskew.results$feedback.mode.results.optim$mode  )^2)/1/3) +
((abs(Lhat-expert.K.LNleftskew.results$feedback.mode.results.optim$lower )^2)/1/3) +
((abs(Uhat-expert.K.LNleftskew.results$feedback.mode.results.optim$upper )^2)/1/3))

LNleftskew.score.mean <-
(((abs(Mhat-expert.K.LNleftskew.results$ss.mean.results$mean)^2)/1/3) +
((abs(Lhat-expert.K.LNleftskew.results$ss.mean.results$lower)^2)/1/3) +
((abs(Uhat-expert.K.LNleftskew.results$ss.mean.results$upper)^2)/1/3) )


LNleftskew.score.median <-
(((abs(Mhat-expert.K.LNleftskew.results$ss.median.results$median)^2)/1/3) +
((abs(Lhat-expert.K.LNleftskew.results$ss.median.results$lower)^2)/1/3) +
((abs(Uhat-expert.K.LNleftskew.results$ss.median.results$upper)^2)/1/3) )

####
# IF NORMAL IS SMALLER
#SELECT NORMAL AS BEST
#%if (Norm.score < LNleftskew.score) {
if (Norm.score < max(LNleftskew.score.mode, LNleftskew.score.mean, LNleftskew.score.median) ) {

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
}else { #(Mhat>=expertrange*.75+Lhat)

expert.K.LNleftskew.results <-  expert.K.noplot.LNleftskew(Lhat, Uhat, Mhat, pihat, new.alpha, best.type.status, ee.type)

#STORE RESULTS FOR OUTPUT
results <- expert.K.LNleftskew.results
which.dist <- c("LNleftskew")


}

###
#store selected best type (mode, median, mean)
#best.type = best.type.status[ee.type,3]



return(list(pihat=pihat, ss.mean.results =results$ss.mean.results , 
	ss.mode.results=results$ss.mode.results,
	ss.median.results =results$ss.median.results, 
	fit.best.mode.mu=results$fit.best.mode.mu,
	fit.best.mode.sig=results$fit.best.mode.sig, 
	fit.best.mean.mu = results$fit.best.mean.mu,
	fit.best.mean.sig=results$fit.best.mean.sig, 
	fit.best.median.mu=results$fit.best.median.mu,
	fit.best.median.sig=results$fit.best.median.sig ,
	feedback.mode.results=results$feedback.mode.results,
  feedback.mode.results.optim=results$feedback.mode.results.optim,
  feedback.mean.results=results$feedback.mean.results,
	feedback.median.results=results$feedback.median.results, 
  Ksp=results$Ksp, MhatK=results$MhatK, which.dist=which.dist))
	#,best.type = best.type))

}








