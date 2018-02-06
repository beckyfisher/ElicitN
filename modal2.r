
#x=combine.sample.i.NallMar
#

modal <<- function(x) {
dd <- density(x)
mode.dd=as.numeric(which.max(dd$y))
return(mode.dd)
}