#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

survey.stage="E"
tt <- tktoplevel()
tkwm.title(tt,"Expert elicitation survey - Part E, Parameter elicitation.")
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

source.file.component1= getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_database.r", ssl.verifypeer = FALSE)#"ee_database.r"


## Place a text message first.
tkgrid(tklabel(tt,text=
"We are attempting to estimate the total time spent.
We would like to get an estimate from you for the different tasks you were involved in.
What task can you provide an estimate for?",font=fontText))

#_________________ set default Data variable _________________________________

if(nchar(default.vars.component1["Data"])>0){                            
Data <<- tclVar(default.vars.component1["Data"])}else if (max(ls()=="expert.data")>0){
Data <<- tclVar(expert.data)}else{Data <<- tclVar(default.vars.component1["Data"])}

entry.Data <-tkentry(tt,width="30",font=fontText,textvariable=Data)

tkgrid(entry.Data)


tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line
tkgrid(tklabel(tt,text=
"We are interested in how much time you spent on this task.
We will start by thinking how big or how small you think this number could be.",font=fontText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

elicitN(default.vars.component1)
default.vars.component1[names(temp.out)]=temp.out
expert.data<<-tclvalue(Data)
default.vars.component1["Data"]<-expert.data
rm(temp.out)
               
#--- Save files --------------
if(tclvalue(done.var)==2){
if(tclvalue(plot.var)==2){
expert.component1.K.LN.results=default.vars.component1#expert.K.LN.results
#rm(expert.K.LN.results)
}
dir.create(paste(dirName,"results",name.respondant,expert.data,sep="/"))
setwd(paste(dirName,"results",name.respondant,expert.data,sep="/"))
save(list="default.vars.component1",file="default.vars.component1")
save(list="expert.component1.K.LN.results",file="expert.component1.K.LN.results")
setwd(dirName)
tkdestroy(tt)
eval(parse(text = source.file.component1))
#source(source.file.component1)
}

if(tclvalue(done.var)==3){
more.components<<-0
if(tclvalue(plot.var)==2){
expert.component1.K.LN.results=default.vars.component1#expert.K.LN.results
#rm(expert.K.LN.results)
}
dir.create(paste(dirName,"results",name.respondant,expert.data,sep="/"))
setwd(paste(dirName,"results",name.respondant,expert.data,sep="/"))
save(list="default.vars.component1",file="default.vars.component1")
save(list="expert.component1.K.LN.results",file="expert.component1.K.LN.results")
setwd(dirName)
tkdestroy(tt)
eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_database.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_database.r")

}

#if(tclvalue(done.var)==1){
#tkdestroy(tt)
#source(source.file.component1)  }

