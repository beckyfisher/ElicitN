#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

elicitStdQst=function(def.vars,tt){

done.var <<- tclVar(0)
plot.var <<- tclVar(0)
change.var <<- tclVar(0)

Units <<- tclVar(def.vars["Units"])
entry.Units <-tkentry(tt,width="10",font=fontText,textvariable=Units)
tkgrid(tklabel(tt,text=
"If you think you would like to answer in actual numbers, please type the multiplicative factor you would like to use for your estimates (1's,10's,100's,1000's,etc)",font=fontSubText),entry.Units) 
#tkgrid(tklabel(tt,text=
#"",font=fontSubText))

# create labels for the N and & options
tkgrid(tklabel(tt,text="   "),tklabel(tt,text=" N ",font=fontText),
                              tklabel(tt,text=" % ",font=fontText),
                              tklabel(tt,text=" x ",font=fontText))

Smallest.N <<- tclVar(def.vars["Smallest.N"])
entry.Smallest.N <-tkentry(tt,width="10",font=fontText,textvariable=Smallest.N)
Smallest.P <<- tclVar(def.vars["Smallest.P"])
entry.Smallest.P <-tkentry(tt,width="10",font=fontText,textvariable=Smallest.P)
Smallest.M <<- tclVar(def.vars["Smallest.M"])
entry.Smallest.M <-tkentry(tt,width="10",font=fontText,textvariable=Smallest.M)
tkgrid(tklabel(tt,text=
"What is the smallest this number could be?", font=fontText),entry.Smallest.N, entry.Smallest.P, entry.Smallest.M)
tkgrid(tklabel(tt,text="(You would be really surprised if the number was smaller than this.)",font=fontSubText))

Largest.N <<- tclVar(def.vars["Largest.N"])
entry.Largest.N <-tkentry(tt,width="10",font=fontText,textvariable=Largest.N)
Largest.P <<- tclVar(def.vars["Largest.P"])
entry.Largest.P <-tkentry(tt,width="10",font=fontText,textvariable=Largest.P)
Largest.M <<- tclVar(def.vars["Largest.M"])
entry.Largest.M <-tkentry(tt,width="10",font=fontText,textvariable=Largest.M)
tkgrid(tklabel(tt,text=
"What is the largest this number could be?",font=fontText),entry.Largest.N, entry.Largest.P, entry.Largest.M)
tkgrid(tklabel(tt,text="(You would be really surprised if the number was larger than this.)",font=fontSubText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

# Lower and Upper --------------------------------------------------------------
tkgrid(tklabel(tt,text=
"This means you are nearly 100% sure that the number is in this range. We need to bring these bounds in a bit, 
so you won't be 100% sure the number falls inside these bounds.",font=fontSubText))

# create labels for the N and & options
tkgrid(tklabel(tt,text="   "),tklabel(tt,text=" N ",font=fontText),
                              tklabel(tt,text=" % ",font=fontText),
                              tklabel(tt,text=" x ",font=fontText))
Lower.N <<- tclVar(def.vars["Lower.N"])
entry.Lower.N <-tkentry(tt,width="10",font=fontText,textvariable=Lower.N)
Lower.P <<- tclVar(def.vars["Lower.P"])
entry.Lower.P <-tkentry(tt,width="10",font=fontText,textvariable=Lower.P)
Lower.M <<- tclVar(def.vars["Lower.M"])
entry.Lower.M <-tkentry(tt,width="10",font=fontText,textvariable=Lower.M)
tkgrid(tklabel(tt,text=
"What is a more realistic lower bound?",font=fontText),entry.Lower.N,entry.Lower.P, entry.Lower.M)

Upper.N <<- tclVar(def.vars["Upper.N"])
entry.Upper.N <-tkentry(tt,width="10",font=fontText,textvariable=Upper.N)
Upper.P <<- tclVar(def.vars["Upper.P"])
entry.Upper.P <-tkentry(tt,width="10",font=fontText,textvariable=Upper.P)
Upper.M <<- tclVar(def.vars["Upper.M"])
entry.Upper.M <-tkentry(tt,width="10",font=fontText,textvariable=Upper.M)
tkgrid(tklabel(tt,text=
"What is a more realistic upper bound?",font=fontText),entry.Upper.N, entry.Upper.P, entry.Upper.M)

tkgrid(tklabel(tt,text=
"Just so we are all on the same page, how much rounding are you doing here?
Eg Does [2,000] mean the same as 1,750-2,250, or 1,900-2,100 or 1,999-2,001?",font=fontSubText))
#[Change the number to be the same as the lower bound = answer to Q3.]

## ------------------------------------------------------------------------------
Sureness <<- tclVar(def.vars["Sureness"])
entry.Sureness <-tkentry(tt,width="10",font=fontText,textvariable=Sureness)
tkgrid(tklabel(tt,text=
"How sure are you that the real number lies in these bounds? (%)",font=fontText),entry.Sureness)

# create labels for the different answer options
tkgrid(tklabel(tt,text="   "),tklabel(tt,text=" N ",font=fontText),
                              tklabel(tt,text=" % ",font=fontText),
                              tklabel(tt,text=" x ",font=fontText))
Best.N <<- tclVar(def.vars["Best.N"])
entry.Best.N<-tkentry(tt,width="10",font=fontText,textvariable=Best.N)
Best.P <<- tclVar(def.vars["Best.P"])
entry.Best.P<-tkentry(tt,width="10",font=fontText,textvariable=Best.P)
Best.M <<- tclVar(def.vars["Best.M"])
entry.Best.M<-tkentry(tt,width="10",font=fontText,textvariable=Best.M)
tkgrid(tklabel(tt,text=
"What is your best guess, the most likely value?",font=fontText),entry.Best.N, entry.Best.P, entry.Best.M)
tkgrid(tklabel(tt,text="(the one where you think you are most likely to be right with this estimate compared with any other)",font=fontSubText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line
tkgrid(tklabel(tt,text=
"To help you get your numbers right, we can now give some feed back on the implications of what you've just said. How sure 
you are, and the corresponding lower and upper bounds. We can feedback some bounds for a different level of certainty.", font=fontText)) 
tkgrid(tklabel(tt,text=
"Eg 50% chance (even odds) that the true number's in the interval, 67% chance (2-1 odds), 80% chance (4-1 odds), 95% chance (19-1 odds).",font=fontSubText))  

New.alpha <<- tclVar(def.vars["New.alpha"])
entry.New.alpha <-tkentry(tt,width="10",font=fontText,textvariable=New.alpha)
tkgrid(tklabel(tt,text=
"Which would you prefer?",font=fontText),entry.New.alpha)
}