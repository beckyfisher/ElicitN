modal <-function(x) {
	mode=as.numeric(names(which.max(table(x))))
return(mode)
}