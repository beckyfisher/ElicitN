#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

# part A: Introduction
tt <- tktoplevel()
done.var <- tclVar(0)
tkwm.title(tt,"Expert elicitation survey - Part A, Introduction")

tkgrid(tklabel(tt, text="Overall goal: Hindcast time spent on the 2015 Conoco Barossa project to help AIMS determine how well actual time spent aligns with that budgeted ",font=fontHeading))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line


tkgrid(tklabel(tt, text="What we would like from you:",font=fontHeading))
tkgrid(tklabel(tt, text=
"An estimate of the time you spent on varous components of the project",font=fontText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

tkgrid(tklabel(tt, text="Have you reshreshed your memory of the project by looking through the field trip report, interim report and final report?",font=fontHeading))
tkgrid(tklabel(tt, text=
"If not please take the time to do so now, and/or spend some time reviewing your own files to refresh your memory of the project",font=fontText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

tkgrid(tklabel(tt, text="",font=fontText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

# Create a next button and set the value of the done varible
Next.but <- tkbutton(tt,text="  Next  ",font=fontText,
                   command=function() tclvalue(done.var)<-1)
tkbind(tt,"<Destroy>",function() tclvalue(done.var)<-2)

tkgrid(Next.but)
tkfocus(tt)
tkwait.variable(done.var)
tkdestroy(tt)
## 
eval.text <- getURL("https://raw.githubusercontent.com/beckyfisher/ElicitN/master/ee_training_questions.tcltk.r", ssl.verifypeer = FALSE)
eval(parse(text = eval.text))
#source("ee_training_questions.tcltk.r")