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
    
    if (best.type.status[1,2]==0){
      expert.K.LN.results <<- expert.K.ALLnorm(Lhat,Uhat,Mhat,pihat,new.alpha,best.type.status,ee.type=1)
      tclvalue(plot.var)<<-2
      current.answer.type <<-"N"
      best.answer()
      }else{
      expert.K.LN.results <<- expert.K.ALLnorm.just1plot(Lhat,Uhat,Mhat,pihat,new.alpha,best.type.status,ee.type=1)
      tclvalue(plot.var)<<-2
      }}
      #tkfocus(tt)
      }

################################################################################
plot_routine.Inf <- function() {
setwd(dirName)  
        N=  c(Lower.N=as.numeric(tclvalue(Lower.N))*as.numeric(tclvalue(Units)),
              Upper.N=as.numeric(tclvalue(Upper.N))*as.numeric(tclvalue(Units)),
              Best.N=as.numeric(tclvalue(Best.N))*as.numeric(tclvalue(Units)))
        P=  c(Lower.P=as.numeric(tclvalue(Lower.P))/100,
              Upper.P=as.numeric(tclvalue(Upper.P))/100,
              Best.P=as.numeric(tclvalue(Best.P))/100)
        M=  c(Lower.M=as.numeric(tclvalue(Lower.M)),
              Upper.M=as.numeric(tclvalue(Upper.M)),
              Best.M=as.numeric(tclvalue(Best.M)))
        if(min(is.na(P))==0 ){p.2=P}else{p.2=c(0,0,0)}

        all.input=cbind(N,P,M)
        input<<-c(all.input[,which.min(c(sum.N=sum(is.na(N)),
                                       sum.P=sum(is.na(P)),
                                       sum.M=sum(is.na(M))))],
                                       Sureness=as.numeric(tclvalue(Sureness))/100,
                                       new.alpha=as.numeric(tclvalue(New.alpha))/100)
                                       
  # Check to see if we are on training questions or on expert questions and set 
  # best.type.status accordingly
  if(survey.stage=="T"){best.type.status<<-best.type.status.training}else if(survey.stage=="E"){
    best.type.status<<-best.type.status.expert}

  # Check to make sure only one type of answer is used            
  if((min(is.na(N))+min(is.na(P))+min(is.na(M)))<2){
  tkmessageBox(message="You must choose only one of N, % or X for your answers  ")
  tclvalue(plot.var)<<-1

  # Check to make sure all necessary values have been entered
  }else if((max(is.na(input)))==1){
  tkmessageBox(message="A value appears to be missing.
Please make sure you have entered a realistic lower and upper value,
a best guess value, and you have indicated your level of sureness  ")
  tclvalue(plot.var)<<-1

  # check to make sure that, if entered, % values are less than 100
  }else if(max(p.2)>1){
  tkmessageBox(message=paste("You have entered a value greater than %100 percent.

If you think that there are more species that fit in this
category than are currently named you will need to use a
multiplicative factor for your answer.

For example, if you think the number of species in this
category is %",max(P)*100," of the number of named species,
please enter",max(P), " under the X column.

If the number is equivalent to %",min(P)*100, " please enter" ,min(P), "
under the X column."))
   tclvalue(plot.var)<<-1
   
  # Check to make sure the sureness value is >50%
  }else if(input["Sureness"]<0.5){
  tkmessageBox(message="Your sureness value needs to be more than 50%.
Please adjust your realistic upper and lower bounds such that you are more
than 50% sure they will encompass the real value (ie. make them wider)  ")
  tclvalue(plot.var)<<-1
  }else{ 
    # Now check if the expert has examined all three plots for the type of answer
    # variable that was used
    best.values.all=c(as.numeric(tclvalue(Best.N))*as.numeric(tclvalue(Units)), 
                      as.numeric(tclvalue(Best.P))/100, 
                      as.numeric(tclvalue(Best.M)))
    best.type.status[which(best.values.all>=0),2]==0
        # If not, then call the function which uses all three plots
    if(best.type.status[which(best.values.all>=0),2]==0){
    infl.expert.K.LN.results <<- expert.just1plot(
              Lower.N=as.numeric(tclvalue(Lower.N))*as.numeric(tclvalue(Units)),
              Upper.N=as.numeric(tclvalue(Upper.N))*as.numeric(tclvalue(Units)),
              Best.N=as.numeric(tclvalue(Best.N))*as.numeric(tclvalue(Units)),
              Lower.P=as.numeric(tclvalue(Lower.P))/100,
              Upper.P=as.numeric(tclvalue(Upper.P))/100,
              Best.P=as.numeric(tclvalue(Best.P))/100,
              Lower.M=as.numeric(tclvalue(Lower.M)),
              Upper.M=as.numeric(tclvalue(Upper.M)),
              Best.M=as.numeric(tclvalue(Best.M)),
              Sureness=as.numeric(tclvalue(Sureness))/100,
              new.alpha=as.numeric(tclvalue(New.alpha))/100,
              expert.K.LN.results.temp,
              best.type.status)
    tclvalue(plot.var)<<-2
    current.answer.type<<-best.type.status[which(best.values.all>=0),1]
    best.answer()
    }else if(best.type.status[which(best.values.all>=0),2]==1){
      # If yes, then call the function that uses only 1 plot
    infl.expert.K.LN.results <<- expert.just1plot(
              Lower.N=as.numeric(tclvalue(Lower.N))*as.numeric(tclvalue(Units)),
              Upper.N=as.numeric(tclvalue(Upper.N))*as.numeric(tclvalue(Units)),
              Best.N=as.numeric(tclvalue(Best.N))*as.numeric(tclvalue(Units)),
              Lower.P=as.numeric(tclvalue(Lower.P))/100,
              Upper.P=as.numeric(tclvalue(Upper.P))/100,
              Best.P=as.numeric(tclvalue(Best.P))/100,
              Lower.M=as.numeric(tclvalue(Lower.M)),
              Upper.M=as.numeric(tclvalue(Upper.M)),
              Best.M=as.numeric(tclvalue(Best.M)),
              Sureness=as.numeric(tclvalue(Sureness))/100,
              new.alpha=as.numeric(tclvalue(New.alpha))/100,
              expert.K.LN.results.temp,
              best.type.status)
    tclvalue(plot.var)<<-2}                        
    }}

