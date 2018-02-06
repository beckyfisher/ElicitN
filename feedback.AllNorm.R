#This file is part of ElicitN.

#Copyright 2011 Rebecca Fisher and Rebecca O'Leary.

#ElicitN is free software: you can redistribute it and/or modify it under 
#the terms of the GNU General Public License as published by the Free Software 
#Foundation, either version 3 of the License, or any later version.

#This program is distributed in the hope that it will be useful, but WITHOUT ANY 
#WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
#PARTICULAR PURPOSE.  See the GNU General Public License 
#(http://www.gnu.org/licenses/) for more details.

feedback.AllNorm <- function(mu, sig,Sureness,new.alpha, MhatK, which.dist) {

#if for say cryptic have no info
if (which.dist==0){
lower=0
upper=0
mode=0
new.lower=0
new.upper=0
Ksp=0

} else {

#IF LOG-NORMAL
if (which.dist=="LN") {
	mode <- exp(mu-sig^2)
	ci <- qlnorm(c(1-Sureness, .5, Sureness, 1-new.alpha, new.alpha), mean=mu, sd=sig)

	Ksp <-rlnorm(10000,mu,sig)

#IF NORMAL
} else if (which.dist=="normal"){
	mode <- mu
	ci <- qnorm(c(1-Sureness, .5, Sureness, 1-new.alpha, new.alpha), mean=mu, sd=sig)

	Ksp <-rnorm(10000,mu,sig)


#ELSE MIRROR LOG-NORMAL
} else {
	
	modeLN <- exp(mu-sig^2)
	mode <- MhatK$Mhatold-(MhatK$Mhat-modeLN)

	Ksp <-rlnorm(10000,mu,sig)


	ci <- qlnorm(c(1-Sureness, .5, Sureness, 1-new.alpha, new.alpha), mean=mu, sd=sig)


}

lower=ci[1]
upper=ci[3]
new.lower=ci[4]
new.upper=ci[5]


}


return(list(lower=lower, upper=upper,  mode=mode,
		new.alpha=new.alpha, new.lower=new.lower, new.upper=new.upper, Ksp=Ksp))



}
