#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

feedbackLN.short <- function(mu, sig,alpha,new.alpha) {
	m <- exp(mu + (sig^2)/2)
	mo <- exp(mu-sig^2)
	ci <- qlnorm(c(1-alpha, .5, alpha, 1-new.alpha, new.alpha), mean=mu, sd=sig)

	return(list(lower=ci[1], median=ci[2], upper=ci[3], mean=m, mode=mo, #prob.upper=pci[1], prob.lower=pci[2], 
		new.alpha=new.alpha, new.lower=ci[4], new.upper=ci[5]))
}
