`TestPattern` <-
function()
{
	x <- seq(-1,1,length=251)
	y <- x
	x <- rep(x, 251)
	y <- rep(y, ea=251)
	dir <- atan2(y,x)
	# dir[x == 0] <- NA
	# dir[y == 0] <- NA
	u <- cos(dir)
	v <- sin(dir)
	return(as.data.frame(list(x=x,y=y,u=u,v=v)))
}

