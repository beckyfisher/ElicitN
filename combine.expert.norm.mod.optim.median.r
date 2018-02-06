
###########################################
combine.expert.norm.N <-function(combine.expert.sample, new.sure=0.95){ #,best.type.status, ee.type
#combine.expert.sample=combine.sample.i.NallMar; MostUpper=MostUpper.combine.NallMar; MostLower=MostLower.combine.NallMar; new.sure=0.95
#combine.expert.sample=combine.sample.i.CRcryptic; MostUpper=MostUpper.combine.CRcryptic; MostLower=MostLower.combine.CRcryptic, new.sure=0.95
#combine.expert.sample=combine.sample.i.CRdisc; MostUpper=MostUpper.combine.CRdisc;MostLower=MostLower.combine.CRdisc; new.sure=0.95
#combine.expert.sample=combine.sample.i.CRcryptic#expert.e.sample.CRundisc.L

#combine.expert.sample=totalN.taxa.t
#combine.expert.sample=combine.sample.i.CRgenetic

source("modal3.r")
source("expert.K.noplot.ALLnorm.optim.median.r")
Best <- median(combine.expert.sample)
Lower <- quantile(combine.expert.sample, probs = (1-new.sure)/2)
Upper <- quantile(combine.expert.sample, probs = new.sure+ (1-new.sure)/2)
#REMOVE NAME
names(Lower) <-names(Upper) <-NULL

if(Best <0.3){Best =0.3}
if(Lower<0.2){Lower=0.2}
if(Upper<0.4){Upper=0.4}

out=expert.K.noplot.ALLnorm(Lhat=Lower, Uhat=Upper, Mhat=Best , pihat=new.sure)
return(out)}



