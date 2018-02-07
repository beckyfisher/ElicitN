


rm(list=ls())
require(tcltk)

addTclPath("c:/xtra/tcl/lib") # This is how you gain access to the ActiveTcl library!

require(gWidgetstcltk)

fontText <- tkfont.create(family="times",size=13)
fontHeading <- tkfont.create(family="times",size=18,
                             weight="bold",slant="italic")
fontSubText <- tkfont.create(family="times",size=10)
fontBlank <-tkfont.create(family="times",size=2)

## Use tcltk to set the working directory --------------------------------------
dirName <- tclvalue(tkchooseDirectory()) # Very simple, isn't it?
if (!nchar(dirName)) {
    tkmessageBox(message = "No directory was selected!")
} else {
    tkmessageBox(message = paste("The directory selected was", dirName))
}
setwd(dirName)
#setwd('..')

# Source the functions needed to run the scripts -------------------------------
require(RCurl)
eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.ALLNorm.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.ALLNorm.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_default.vars.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_default.vars.r")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_function_plot_routines.tcltk.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_function_plot_routines.tcltk.r")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.ALLNorm.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("expert.K.noplot.ALLNorm.just1plot.R")


#source("feedback.AllNorm.R")
#source("modal.R")
##source("rnormals.number_v2.r")
#source("expert.K.ALLNorm.just1plot.R")
##source("expert.R")
#source("expert.just1plot.R")
##source("ee_default.vars.r")
#source("ee_function_elicitN.tcltk.r")
#source("ee_function_elicitN_training.tcltk.r")
#source("ee_function_elicitInf_stdqst.tcltk.r")
#source("ee_function_elicitInf.tcltk.r")
#source("ee_function_plot_routines.tcltk.r")



# read in the data tables
#_______________ Read the database table files _________________________________
tbl_training=na.omit(as.matrix(read.table(file="tbl_training.csv",header=TRUE,sep=",")))
tbl_component1=na.omit(as.matrix(read.table(file="tbl_component1.csv",header=TRUE,sep=",")))

r=1
fitted.components=list()
for(r in 1:nrow(tbl_component1)){
        Name=tbl_component1[r,"Name"]
        Taxon=tbl_component1[r,"Taxon"]
        Units=as.numeric(tbl_component1[r,"Units"])
        Smallest=as.numeric(tbl_component1[r,"Smallest"])
        Largest=as.numeric(tbl_component1[r,"Largest"])
        Lower.N=as.numeric(tbl_component1[r,"Lower"])
        Upper.N=as.numeric(tbl_component1[r,"Upper"])
        Sureness=as.numeric(tbl_component1[r,"Sureness"])/100
        Best.N=as.numeric(tbl_component1[r,"Best"])
        new.alpha=0.95

   if(Upper.N!=0){
   best.type.status.expert=matrix(c("N","1","a","P", "1","a","M","1","a"),ncol=3,byrow=T)
   names(best.type.status.expert)=c("ans","status","Best.type")
   out<- expert.K.noplot.ALLnorm.just1plot(Lhat=Lower.N, Uhat=Upper.N, Mhat=Best.N,
                                               pihat=Sureness,
                                               new.alpha=new.alpha, best.type.status="best.type.status.expert",
                                               ee.type=1)
   out=c(out,Name,Taxon)
   fitted.components=c(fitted.components,list(out))}

}

names(out)
# note can get value for each person and task straight from here

n=length(out$Ksp)

# now sum for all people and tasks
complete.sample=rowSums(do.call("cbind",lapply(fitted.components, FUN=function(x){x$Ksp})))
dev.off()
boxplot(complete.sample)

#source("modal2.R")
#source("rnormals.number_v2.R")
#source("expert.K.noplot.ALLnorm.optim.median.R")
#source("combine.expert.norm.mod.optim.median.r")
#total.time=combine.expert.norm.N(combine.expert.sample=complete.sample)


# now sum for each task
dev.off()
tasks=na.omit(unique(tbl_component1[,"Taxon"]))
t=1
task.sums=list()
for(t in 1:length(tasks)){
 task.sample=rowSums(do.call("cbind",lapply(fitted.components,
                FUN=function(x){if(x$Taxon==tasks[t]){
                        return(x$Ksp)}else{return(rep(0,n))}})))
 task.sums=c(task.sums,list(task.sample))}

names(task.sums)=tasks
par(mfrow=c(ceiling(length(tasks)/2),2))
for(t in 1:length(tasks)){
     boxplot(task.sums[tasks[t]],main=tasks[t])

}



