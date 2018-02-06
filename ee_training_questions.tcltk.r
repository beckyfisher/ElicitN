#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

################################################################################
# part C: Training questions - their city
survey.stage="T" 
dir.create(paste(dirName,"results",name.respondant,"training",sep="/"))
setwd(dirName)
# create an index to navigate through additional city information
o=1
################################################################################

tt <- tktoplevel()
tkwm.title(tt,"Expert elicitation survey - Part C, training questions")
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line
## Place a text message first.
tkgrid(tklabel(tt,text=
"The following questions will help us develop a baseline and familiarise you with the format of
how we are going to ask the questions. We will focus on estimates of human populations.
We do not expect you to be an expert in this topic",font=fontText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

source.file.train=getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_component1.tcltk.r", ssl.verifypeer = FALSE)#"ee_component1.tcltk.r"


Back.but <- tkbutton(tt,text="  Back  ",font=fontText,
                    command=function(){
                              temp.out<<-c(
                                            "Units"=tclvalue(Units),
                                            "Smallest"=tclvalue(Smallest),
                                            "Largest"=tclvalue(Largest),
                                            "Lower"=tclvalue(Lower),
                                            "Upper"=tclvalue(Upper),
                                            "Sureness"=tclvalue(Sureness),
                                            "Best"=tclvalue(Best),
                                            "New.alpha"=tclvalue(New.alpha))              
                            tclvalue(done.var)<<-1 
                            source.file.train<<- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_personal.tcltk.r", ssl.verifypeer = FALSE)#"ee_personal.tcltk.r"

                            } )
tkgrid(Back.but)
                            
City <- tclVar(default.vars.training["City"])
entry.City <-tkentry(tt,width="30",font=fontText,textvariable=City)
tkgrid(tklabel(tt,text=
"What city do you live in?",font=fontText),entry.City)
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

tkgrid(tklabel(tt,text=
"We're interested in what you think the population of this city is.
We'll start by thinking how big or how small you think this number could be.",font=fontText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

elicitN.train(default.vars.training)
training.out.temp=c("City"=as.character(tclvalue(City)),temp.out)
default.vars.training=training.out.temp


rm(temp.out)
#--- Save files --------------
if(tclvalue(done.var)==2){
#if(max(ls()=="training.expert.K.LN.results")==0){
if(tclvalue(plot.var)==2){
training.expert.K.LN.results=default.vars.training
#rm(expert.K.LN.results)
}
setwd(paste(dirName,"results",name.respondant,"training",sep="/"))
save(list="training.expert.K.LN.results",file="training.expert.K.LN.results")
save(list="default.vars.training",file="default.vars.training")
setwd(dirName)
tkdestroy(tt)
eval(parse(text = source.file.train))
#source(source.file.train)

}

if(tclvalue(done.var)==1){
tkdestroy(tt)
eval(parse(text = source.file.train))
#source(source.file.train)

}
