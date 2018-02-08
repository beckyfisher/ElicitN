################################################################################
#Software name: ElicitN

#Developers: Rebecca Fisher (GUI, data, visualization), Rebecca O'Leary 
#(statistical computation). 
#Other contributors towards code and survey design: Sama Low Choy, Julian Caley and Kerrie Mengersen.

#Contact Address: Australian Institute of Marine Science, 
#UWA Oceans Institute (M096), 35 Stirling Hwy, Crawley, WA 6009, Australia.
#E-mail: r.fisher@aims.gov.au; rebecca_fisher76@yahoo.com.au

#Date development began: November 12, 2009
#Date final version: October 13, 2011

#Software requirements: R version 2.12 with packages gWidgetstcltk, digest and gWidgets installed.

#Description:
#ElicitN uses R and a tcltk gui interface for eliciting a number from an appropriate expert. 
#The code has been modified from it's original version designed specifically for 
#eliciting the number of species on coral reefs from expert taxonomists. 

#Licence and copyright:
#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#This program is free software: you can redistribute it and/or modify it under the 
#terms of the GNU General Public License as published by the Free Software Foundation, 
#either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
#without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#See the GNU General Public License (http://www.gnu.org/licenses/) for more details.

#If you use this software or any modification of it in your research please cite 
#the following in all resulting works: 
#Fisher, R., O'Leary, R.A., Low Choy, S., Mengersen, K., Caley, M.J., 2012.
#A software template for elicitation of expert knowledge about species richness or similar counts. 
#Environmental Modeling and Software. 30: 1-14
#-------------------------------------------------------------------------------
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

all.files=c("111",list.files())
if(length(na.omit(match(all.files,"results")))==0){
      dir.create(file.path(paste(dirName,"results",sep="/")))
}


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

#_______________ Assign parameter type status __________________________________
best.type.status=cbind("ans"=c("N", "P", "M"),"status"=c(1, 1, 1), "Best.type"=c("a", "a", "a"))
best.type.status.training=cbind("ans"=c("N", "P", "M"),"status"=c(1, 1, 1), "Best.type"=c("a", "a", "a"))
best.type.status.expert=cbind("ans"=c("N", "P", "M"),"status"=c(1, 1, 1), "Best.type"=c("a", "a", "a"))
survey.stage=""   # Note that this is currently set so the form DOES NOT ask the expert, and assumes mode.
                  # to edit this change all "status" values from 1 to 0.

#_______________ Read the database table files _________________________________
all.files=c("111",list.files())
if(length(na.omit(match(all.files,"tbl_training.csv")))==1){
  tbl_training=as.matrix(read.table(file="tbl_training.csv",header=TRUE,sep=","))}else{
  tbl_training=matrix(rep(NA,10),nrow=1)
  colnames(tbl_training)=c("Name","City","Units","Smallest","Largest","Lower","Upper",
                           "Sureness","Best","New.alpha")
  write.table(tbl_training,file="tbl_training.csv",row.names=FALSE,sep=",")
}
if(length(na.omit(match(all.files,"tbl_component1.csv")))==1){
  tbl_component1=as.matrix(read.table(file="tbl_component1.csv",header=TRUE,sep=","))}else{
  tbl_component1=matrix(rep(NA,10),nrow=1)
  colnames(tbl_component1)=c("Name","Taxon","Units","Smallest","Largest","Lower","Upper",
                           "Sureness","Best","New.alpha")
  write.table(tbl_component1,file="tbl_component1.csv",row.names=FALSE,sep=",")

}

#______________ Start page ____________________________________________________
ttStart.Page <- tktoplevel()
tkwm.title(ttStart.Page,"expert elicitation survey")
tkgrid(tklabel(ttStart.Page,text="    ",font=fontBlank)) # Blank line

# ---- list box containing list of previous interviews (= folders in the results 
# file of the selected file.

selPrevious <- function()
{
    folderChoice <- folders[as.numeric(tkcurselection(tl))+1]
    name.respondant<<-folderChoice 
    respondant.folders<<-list.files(path=paste(dirName,"results",name.respondant,sep="/"))
    expert.data<<-respondant.folders[-c(which(respondant.folders=="personal_details"),which(respondant.folders=="training"))]
    data.wait.var <<- tclVar(0)
    # Check to see if more than one expert data has been answered
    if(length(expert.data)==1){
    expert.data <<- expert.data
    tclvalue(data.wait.var)<<-2
    }else if(length(expert.data)>1){
    ttSelData <- tktoplevel()
    tkwm.title(ttSelData,"Expert elicitation survey")
    tkgrid(tklabel(ttSelData,text="    ",font=fontBlank)) # Blank line
    selData <- function()
    {   dataChoice <- expert.data[as.numeric(tkcurselection(tl.data))+1]
    expert.data<<-dataChoice
    tkmessageBox(message=paste("You have selected",dataChoice,sep=" "))
    tclvalue(data.wait.var)<<-2}
    selData.but <-tkbutton(ttSelData,text="   Select previous survey results  ", font=fontText,command=selData)
    tkgrid(tklabel(ttSelData,text="    ",font=fontBlank)) # Blank line
    tkgrid(selData.but)
    tl.data<-tklistbox(ttSelData,height=4,selectmode="single",background="white",font=fontText)
    tkgrid(tl.data)

    for (i in (1:length(expert.data)))
    {tkinsert(tl.data,"end",expert.data[i])}
    tkselection.set(tl.data,0)
    tkgrid(tklabel(ttSelData,text="    ",font=fontBlank)) # Blank line
    tkwait.variable(data.wait.var)
    tkdestroy(ttSelData)
    }


training.files<<-list.files(path=paste(dirName,"results",name.respondant,"training",sep="/"))

setwd(paste(dirName,"results",name.respondant,"training",sep="/"))
for(l in 1:length(training.files)){load(file=training.files[l], .GlobalEnv)}
setwd(dirName)

if(max(ls(.GlobalEnv)=="expert.data")==1){
    expert.files<<-list.files(path=paste(dirName,"results",name.respondant,expert.data,sep="/"))
    setwd(paste(dirName,"results",name.respondant,expert.data,sep="/"))
    for(m in 1:length(expert.files)){load(file=expert.files[m], .GlobalEnv)}}
    
setwd(dirName)
eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_component1.tcltk.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source(paste(dirName,"ee_component1.tcltk.r",sep="/"))

}

selPrevious.but <-tkbutton(ttStart.Page,text="   Select previous survey results  ", font=fontText,command=selPrevious)
tkgrid(tklabel(ttStart.Page,text="    ",font=fontBlank)) # Blank line
#tkgrid(selPrevious.but)
tl<-tklistbox(ttStart.Page,height=4,selectmode="single",background="white",font=fontText)    
#tkgrid(tklabel(ttStart.Page,text="Previous surveys", font=fontText))
tkgrid(tl)
tkgrid(selPrevious.but)
folders=list.files(path=paste(dirName,"results",sep="/"))
for (i in (1:length(folders)))
{
    tkinsert(tl,"end",folders[i])
}
tkselection.set(tl,0)  # Default is the top row.  Indexing starts at zero.
tkgrid(tklabel(ttStart.Page,text="    ",font=fontBlank)) # Blank line

#---------- set up boxes for entering details of a new respondant --------------
Name.respondant <<- tclVar("")
entry.Name.respondant <-tkentry(ttStart.Page,width="60",font=fontText,textvariable=Name.respondant)
tkgrid(tklabel(ttStart.Page,text="Type in the name of a new expert ",font=fontText),entry.Name.respondant)
tkgrid(tklabel(ttStart.Page,text="    ",font=fontBlank)) # Blank line

tkgrid(tklabel(ttStart.Page,text=
" If this is the first time this expert has been interviewed, 
please start the survey from the beginning
otherwise you may go straight to the elicitation 
of new data  ",font=fontSubText))
tkgrid(tklabel(ttStart.Page,text="    ",font=fontBlank)) # Blank line

All.but <- tkbutton(ttStart.Page,text="  Start survey from beginning  ",font=fontText,
                   command=function() {
                   name.respondant<<-tclvalue(Name.respondant)
                   default.vars["Name.respondant"]=name.respondant
                   default.vars["Name"]=name.respondant
                   dir.create(paste(dirName,"results",name.respondant,sep="/"))
 ## Run the introduction widget -------------------------------------------------
eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_introduction.tcltk.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_introduction.tcltk.r")
})

Expert.data.but  <- tkbutton(ttStart.Page,text="  Go straight to elicitation of data  ",font=fontText,
                   command=function() {
                   name.respondant<<-tclvalue(Name.respondant)
                   dir.create(paste(dirName,"results",name.respondant,sep="/"))
                   #dir.create(paste(dirName,"results",name.respondant,"personal_details",sep="/")) 
 ## Run the expert data widget -------------------------------------------------
eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_component1.tcltk.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_component1.tcltk.r")
})


tkgrid(All.but)
tkgrid(tklabel(ttStart.Page,text="    ",font=fontBlank)) # Blank line
tkgrid(Expert.data.but)
tkgrid(tklabel(ttStart.Page,text="    ",font=fontBlank)) # Blank line
