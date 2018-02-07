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
tkwm.title(tt,"Expert elicitation survey - Part B, training questions")
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

#elicitN.train(default.vars.training)
def.vars=default.vars.training
done.var <<- tclVar(0)
plot.var <<- tclVar(0)
change.var <<- tclVar(0)

Units <<- tclVar(def.vars["Units"])
entry.Units <-tkentry(tt,width="30",font=fontText,textvariable=Units)
tkgrid(tklabel(tt,text=
"Please type the units of magnitude you would like to use for your estimates (1's,10's,100's,1000's,etc)",font=fontSubText),entry.Units)
entry.Units <-tkentry(tt,width="30",font=fontText,textvariable=Units)

Smallest <<- tclVar(def.vars["Smallest"])
entry.Smallest <-tkentry(tt,width="30",font=fontText,textvariable=Smallest)
tkgrid(tklabel(tt,text=
"In the units supplied above, what is the smallest this number could be?", font=fontText),entry.Smallest)
tkgrid(tklabel(tt,text="(You would be really surprised if the number was smaller than this.)",font=fontSubText))

Largest <<- tclVar(def.vars["Largest"])
entry.Largest <-tkentry(tt,width="30",font=fontText,textvariable=Largest)
tkgrid(tklabel(tt,text=
"What is the largest this number could be?",font=fontText),entry.Largest)
tkgrid(tklabel(tt,text="(You would be really surprised if the number was larger than this.)",font=fontSubText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

# Lower and Upper --------------------------------------------------------------
tkgrid(tklabel(tt,text=
"This means you are nearly 100% sure that the number is in this range. We need to bring these bounds in a bit,
so you won't be 100% sure the number falls inside these bounds.",font=fontSubText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

Lower <<- tclVar(def.vars["Lower"])
entry.Lower <-tkentry(tt,width="30",font=fontText,textvariable=Lower)
tkgrid(tklabel(tt,text=
"What is a more realistic lower bound?",font=fontText),entry.Lower)

Upper <<- tclVar(def.vars["Upper"])
entry.Upper <-tkentry(tt,width="30",font=fontText,textvariable=Upper)
tkgrid(tklabel(tt,text=
"What is a more realistic upper bound?",font=fontText),entry.Upper)

tkgrid(tklabel(tt,text=
"Just so we are all on the same page, how much rounding are you doing here?
Eg Does [2,000] mean the same as 1,750-2,250, or 1,900-2,100 or 1,999-2,001?",font=fontSubText))
#[Change the number to be the same as the lower bound = answer to Q3.]

# ------------------------------------------------------------------------------
Sureness <<- tclVar(def.vars["Sureness"])
entry.Sureness <-tkentry(tt,width="10",font=fontText,textvariable=Sureness)
tkgrid(tklabel(tt,text=
"How sure are you that the real number lies in these bounds? (%)",font=fontText),entry.Sureness)

Best <<- tclVar(def.vars["Best"])
entry.Best<-tkentry(tt,width="30",font=fontText,textvariable=Best)
tkgrid(tklabel(tt,text=
"What is your best guess, the most likely value?",font=fontText),entry.Best)
tkgrid(tklabel(tt,text="(the one where you think you are most likely to be right with this estimate compared with any other)",font=fontSubText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

tkgrid(tklabel(tt,text=
"To help you get your numbers right, we can now give some feed back on the implications of what you've just said:
how sure you are, and the corresponding lower and upper bounds. We can feedback some bounds for a different level
of certainty.", font=fontText))
tkgrid(tklabel(tt,text=
"Eg 50% chance (even odds) that the true number is in the interval, 67% chance (2-1 odds), 80% chance (4-1 odds), 95% chance (19-1 odds).",font=fontSubText))

New.alpha <<- tclVar(def.vars["New.alpha"])
entry.New.alpha <-tkentry(tt,width="30",font=fontText,textvariable=New.alpha)
tkgrid(tklabel(tt,text=
"Which would you prefer?",font=fontText),entry.New.alpha)

# Examine plot:
#Then the bounds corresponding to this level of certainty would be...
#Do you think these bounds are too narrow or too wide or just right?

# Create "calculate and plot", "finished" and "back" buttons
Finished.but <- tkbutton(tt,text="  Next  ",font=fontText,
                  command=function() {
                  #Output the information as a list
                  temp.out<<-c(
                  "Units"=tclvalue(Units),
                  "Smallest"=tclvalue(Smallest),
                  "Largest"=tclvalue(Largest),
                  "Lower"=tclvalue(Lower),
                  "Upper"=tclvalue(Upper),
                  "Sureness"=tclvalue(Sureness),
                  "Best"=tclvalue(Best),
                  "New.alpha"=tclvalue(New.alpha))

               if(tclvalue(plot.var)<2){
                    tkmessageBox(message = "You must get feedback before you may proceed")}else{
                  tclvalue(done.var)<<-2}
                   })
tkbind(tt,"<Destroy>",function() tclvalue(done.var)<<-3)

Plot.but <- tkbutton(tt,text="  Get feedback  ",font=fontText,
                     command=plot_routine)

tkgrid(Plot.but,Finished.but)
tkwait.variable(done.var)
try(dev.off(),silent=TRUE)
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
