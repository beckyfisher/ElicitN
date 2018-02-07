#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

#default.vars.training
tbl_training=unique(rbind(tbl_training,c("Name"=name.respondant,default.vars.training)))

#default.vars.component1
tbl_component1=unique(rbind(tbl_component1,c(name.respondant,default.vars.component1)))
#out.component1=c("Estimate"="component 1",
#              "unit_type"="N",
#              as.numeric(default.vars.component1[c("Lower","Upper","Best")])*as.numeric(default.vars.component1["Units"]),
#              default.vars.component1["Sureness"])
              

# Write the new tables to the output files
write.table(tbl_training,file="tbl_training.csv",row.names=FALSE,sep=",")
write.table(tbl_component1,file="tbl_component1.csv",row.names=FALSE,sep=",")


tkmessageBox(message="All answers have been saved")

  