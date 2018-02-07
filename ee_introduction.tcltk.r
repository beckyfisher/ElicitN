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

tkgrid(tklabel(tt, text="Overall goal: Develop an expert based estimate of the value of interest that properly captures uncertainty around that estimate ",font=fontHeading))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line


tkgrid(tklabel(tt, text="What we would like from you:",font=fontHeading))
tkgrid(tklabel(tt, text=
"An estimate of the the value of interest",font=fontText))
tkgrid(tklabel(tt,text="    ",font=fontBlank)) # Blank line

tkgrid(tklabel(tt, text="Have you had a chance to revise the information provided in order to refresh your memory around the topic of interest?",font=fontHeading))
tkgrid(tklabel(tt, text=
"If not please take the time to do so now, and/or spend some time reviewing your own files to refresh
your memory. Please think about the problem from a broader context, focusing on elements that might
contribute to your uncertainty. We are not really all that interested in a single number,
but rather the full range of possibilities this number may take.",font=fontText))
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

