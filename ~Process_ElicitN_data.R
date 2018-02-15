


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

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.ALLNorm.just1plot.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/rnormals.number.R", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))

#eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.R", ssl.verifypeer = FALSE)
#eval(parse(text = eval.text))
#source("expert.R")

#eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.just1plot.R", ssl.verifypeer = FALSE)
#eval(parse(text = eval.text))
#source("expert.just1plot.R")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_default.vars.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_default.vars.r")

eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_function_plot_routines.tcltk.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_function_plot_routines.tcltk.r")

#eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/expert.K.noplot.ALLNorm.just1plot.R", ssl.verifypeer = FALSE)
#eval(parse(text = eval.text))
#source("expert.K.noplot.ALLNorm.just1plot.R")


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
        Smallest=as.numeric(tbl_component1[r,"Smallest"])#*Units
        Largest=as.numeric(tbl_component1[r,"Largest"])#*Units
        Lower.N=as.numeric(tbl_component1[r,"Lower"])#*Units
        Upper.N=as.numeric(tbl_component1[r,"Upper"])#*Units
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
   fitted.components=c(fitted.components,list(c(tbl_component1[r,],out)))}

}

names(out)
# note can get value for each person and task straight from here

n=length(out$Ksp)

# now sum for all experts and questions
complete.sample=rowSums(do.call("cbind",lapply(fitted.components, FUN=function(x){
           #x$Ksp
           rnormals.number(n, x$fit.best.mode.mu, x$fit.best.mode.sig,
                                 x$which.dist,
                                 x$feedback.mode.results$lower,x$feedback.mode.results$upper)*as.numeric(x$Units)
           })))

# now sum for each task
tasks=na.omit(unique(tbl_component1[,"Taxon"]))
t=1
task.sums=list()
for(t in 1:length(tasks)){
 task.sample=rowSums(do.call("cbind",lapply(fitted.components,
                FUN=function(x){
                        out.x=rep(0,n)
                        if(x$Taxon==tasks[t]){
                             out.x=rnormals.number(n, x$fit.best.mode.mu, x$fit.best.mode.sig,
                                 x$which.dist,
                                 x$feedback.mode.results$lower,x$feedback.mode.results$upper)*as.numeric(x$Units)
                        }
                return(out.x)})))



 task.sums=c(task.sums,list(task.sample))}
names(task.sums)=tasks

# now sum by experts
names=na.omit(unique(tbl_component1[,"Name"]))
t=1
name.sums=list()
for(t in 1:length(names)){
 name.sample=rowSums(do.call("cbind",lapply(fitted.components,
                FUN=function(x){
                        out.x=rep(0,n)
                        if(x$Name==names[t]){
                             out.x=rnormals.number(n, x$fit.best.mode.mu, x$fit.best.mode.sig,
                                 x$which.dist,
                                 x$feedback.mode.results$lower,x$feedback.mode.results$upper)*as.numeric(x$Units)
                        }
                return(out.x)})))



 name.sums=c(name.sums,list(name.sample))}
names(name.sums)=names

pdf("Total_sum.pdf")
plot(density(complete.sample),main="Summed total")
abline(v=quantile(complete.sample, probs=c(0.025,0.5,0.975)),col="red",lty=c(3,1,3))
dev.off()

pdf("Total_sum_by_components.pdf",onefile=T)
for(t in 1:length(tasks)){
 plot(density(task.sums[[tasks[t]]]),main=paste("Summed total -",tasks[t]))
 abline(v=quantile(task.sums[[tasks[t]]], probs=c(0.025,0.5,0.975)),col="red",lty=c(3,1,3))
}
dev.off()

pdf("Total_sum_by_names.pdf",onefile=T)
for(t in 1:length(names)){
 plot(density(name.sums[[names[t]]]),main=paste("Summed total -",names[t]))
 abline(v=quantile(name.sums[[tasks[t]]], probs=c(0.025,0.5,0.975)),col="red",lty=c(3,1,3))
}
dev.off()


save(list=ls(),file="Collated_results_R_workspace.Rdata")

















































