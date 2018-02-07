Software name: ElicitN

Developers: Rebecca Fisher (GUI, data, visualization), Rebecca O’Leary (statistical computation). Other contributors towards code and survey design: Sama Low Choy, Julian Caley and Kerrie Mengersen.

Contact Address: Australian Institute of Marine Science, UWA Oceans Institute (M096), 35 Stirling Hwy, Crawley, WA 6009, Australia.
E-mail: r.fisher@aims.gov.au; rebecca_fisher76@yahoo.com.au

Date development began: November 12, 2009
Date final version: October 13, 2011
Date simplified version released on github: February 7, 2018

Software requirements: R version 2.12 with packages gWidgetstcltk, digest and gWidgets installed.

Description:
ElicitN uses R and a tcltk gui interface for eliciting a number from an appropriate expert. The code has been modified from it's original version designed specifically for eliciting the number of species on coral reefs from expert taxonomists, and further simplified from the code provided at https://sourceforge.net/projects/elicitn/?source=directory

Licence and copyright:
Copyright 2011 Rebecca Fisher and Rebecca O’Leary.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License (http://www.gnu.org/licenses/) for more details.

If you use this software or any modification of it in your research please cite the following in all resulting works: Fisher, R., O'Leary, R.A., Low Choy, S., Mengersen, K., Caley, M.J., 2012. A software template for elicitation of expert knowledge about species richness or similar counts. Environmental Modeling and Software. 30: 1-14.

Some general instructions on use:
A detail description of the original software is contained in Fisher, R., O'Leary, R.A., Low Choy, S., Mengersen, K., Caley, M.J., in press A software template for elicitation of expert knowledge about species richness or similar counts. Environmental Modeling and Softwar.

Start the code by opening R and sourcing “~ElicitN.r”.

This simpler version allows the elicitation of a single number(s). Use of the function will result in the creation of a new “results” directory within the selected directory. This will contain sub-folers labelled using the information entered under “Enter the name of a new expert”.  Inside this directory, the results of the training exercise will be saved in a folder, and additional folders will be labelled for each “specific area of expertise” entered at component 1.

Note: If you wish to elicit more than one “specific area of expertise” from the same expert, it is possible to by-pass the training excersie and go straight to the elicitation of additional data. Do not select an existing expert for the purposes of entering new data using “Select previous survey results”, as this may cause unexpected behaviour. “Select previous survey results” is purely for viewing and modifying existing data.

All elicited data are saved in the “.csv” file: tbl_component1.csv, as well as R objects within the expert’s results folder. 

Please note that while the software contains some internal checks with respect to entered values, expert answer’s need to be carefully checked by the facilitator. Please make use of feedback graphs and ensure that these represent the opinion of the expert adequately.



