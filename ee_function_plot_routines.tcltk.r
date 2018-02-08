#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.


plot_routine <- function() {
  Lhat <- as.numeric(tclvalue(Lower))*as.numeric(tclvalue(Units))
  Uhat <- as.numeric(tclvalue(Upper))*as.numeric(tclvalue(Units))
  Mhat <- as.numeric(tclvalue(Best))*as.numeric(tclvalue(Units))
  pihat <- as.numeric(tclvalue(Sureness))/100                               
  new.alpha <- as.numeric(tclvalue(New.alpha))/100
  input<<-c(Lhat,Uhat,Mhat,pihat,new.alpha)
setwd(dirName)  
  # Check to see if we are on training questions or on expert questions and set 
  # best.type.status accordingly
  if(survey.stage=="T"){best.type.status<<-best.type.status.training}else if(survey.stage=="E"){
    best.type.status<<-best.type.status.expert}
    
  if((max(is.na(input)))==1){
  tkmessageBox(message="A value appears to be missing.
Please make sure you have entered a realistic lower and upper value,
a best guess value, and you have indicated your level of sureness  ")
  tclvalue(plot.var)<<-1
  }else if(pihat<0.5){
  tkmessageBox(message="Your sureness value needs to be more than 50%.
Please adjust your realistic upper and lower bounds such that you are more
than 50% sure they will encompass the real value (ie. make them wider)  ")
  tclvalue(plot.var)<<-1
    }else{

      expert.K.LN.results <<- expert.K.ALLnorm.just1plot(Lhat,Uhat,Mhat,pihat,new.alpha,best.type.status,ee.type=1)
      tclvalue(plot.var)<<-2
      }
      #tkfocus(tt)
}



