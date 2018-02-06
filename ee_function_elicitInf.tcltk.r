#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

###### This function elcicits an inflation factor for a number, based either on
# a number (how many more there arectually are), as a percentage of a reference
# ie Best guess N from elicitN above, or as a multiplicative factor of reference

elicitInf=function(def.vars,expert.K.LN.results){
             expert.K.LN.results.temp<<-expert.K.LN.results
elicitStdQst(def.vars,tt)
setwd(dirName)
## Create "calculate and plot" and "finished" buttons
Finished.but <- tkbutton(tt,text="  Next  ",font=fontText,
                   command=function() {
                   #Output the information as a list
                   temp.out.inf<<-c( 
                   "Units"=tclvalue(Units),
                   "Smallest.N"=tclvalue(Smallest.N),
                   "Largest.N"=tclvalue(Largest.N),
                   "Lower.N"=tclvalue(Lower.N),
                   "Upper.N"=tclvalue(Upper.N),
                   "Sureness"=tclvalue(Sureness),
                   "Best.N"=tclvalue(Best.N),
                   "Smallest.P"=tclvalue(Smallest.P),
                   "Largest.P"=tclvalue(Largest.P),
                   "Lower.P"=tclvalue(Lower.P),
                   "Upper.P"=tclvalue(Upper.P),
                   "Best.P"=tclvalue(Best.P),
                   "Smallest.M"=tclvalue(Smallest.M),
                   "Largest.M"=tclvalue(Largest.M),
                   "Lower.M"=tclvalue(Lower.M),
                   "Upper.M"=tclvalue(Upper.M),
                   "Best.M"=tclvalue(Best.M),
                   "New.alpha"=tclvalue(New.alpha))
       non.zero=max(tclvalue(Best.N),tclvalue(Best.P),tclvalue(Best.M))>=0                                       
       check.values=c(
                   "Units"=tclvalue(Units),
                   "Lower.N"=tclvalue(Lower.N),
                   "Upper.N"=tclvalue(Upper.N),
                   "Sureness"=tclvalue(Sureness),
                   "Best.N"=tclvalue(Best.N),
                   "Lower.P"=tclvalue(Lower.P),
                   "Upper.P"=tclvalue(Upper.P),
                   "Best.P"=tclvalue(Best.P),
                   "Lower.M"=tclvalue(Lower.M),
                   "Upper.M"=tclvalue(Upper.M),
                   "Best.M"=tclvalue(Best.M),
                   "New.alpha"=tclvalue(New.alpha))               

               if(tclvalue(plot.var)<2){
                    tkmessageBox(message = "You must get feedback before you may proceed"
                    )}else{
                   tclvalue(done.var)<<-2}
                   })
tkbind(tt,"<Destroy>",function() tclvalue(done.var)<<-3)

Plot.but <- tkbutton(tt,text="  Get feedback  ",font=fontText,
                     command=plot_routine.Inf)
                                         
                     
tkgrid(Plot.but,Finished.but)
tkwait.variable(done.var)
try(dev.off(),silent=TRUE)
}

