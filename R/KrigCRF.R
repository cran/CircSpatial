`KrigCRF` <-
function(krig.x, krig.y, resid.x, resid.y, resid.direction, Model, Nugget=0, Range, sill,
	Smooth=FALSE, bandwidth, Plot=FALSE, Xlim=NULL, Ylim=NULL, PlotVar=FALSE, ...)
{
	# 2008-11-11.1213
	# select model from covariance models in R package Random Fields function CovarianceFct
	# resid.x and resid.y have no NAs

	if( (length(krig.x) != length(krig.y)) | (length(resid.x) != length(resid.y)) | (length(resid.x) != length(resid.direction)) |
		(length(resid.y) != length(resid.direction)) ) stop("lengths of vector inputs unequal")
	if( (Nugget < 0) | (Nugget > 1) ) stop("Nugget invalid")
	
	# fix the order of the kriging coordinates
	xx <- sort(unique(krig.x)); yy <- sort(unique(krig.y))
	nx <- length(xx); ny <- length(yy) # rectangular or square grid
	krig.y <- rep(yy, nx); krig.x <- rep(xx, each=ny)

	# require(fields)
	# require(RandomFields)

	Distances <- as.matrix(dist(cbind(resid.x, resid.y)))
	Ncol <- ncol(Distances)
	K <- c()
	for (i in 1:Ncol)
	{
		K <- cbind(K, sill + (1 - Nugget - sill)*CovarianceFct(x=Distances[, i]/Range, model=Model,
			param=c(mean=0,variance=1,nugget=0,scale=1, ...), dim=1, fctcall="Covariance"))
	}
	diag(K) <- 1 # TRUE even if nugget > 0 for any model
	Kinv <- solve(K)

	U <- t(cbind(cos(resid.direction), sin(resid.direction)))
	# V <- t(U) %*% U

	n <- length(krig.x) # krig.x=krig.y for square or rect grid	
	krig.direction <- vector(mode="numeric", length=n)
	krig.variance <- krig.direction
	
	for(i in 1:n)
	{
		distances <- sqrt((krig.x[i]-resid.x)^2 + (krig.y[i]-resid.y)^2)
		c <- sill + (1 - Nugget - sill)*CovarianceFct(x=distances/Range, model=Model,
			param=c(mean=0,variance=1,nugget=0,scale=1, ...), dim=1, fctcall="Covariance")
		c[distances == 0] <- 1 # TRUE even if nugget > 0 for any model
		w <- Kinv %*% c

		# w <- (Kinv %*% c)/sqrt(as.numeric(t(c) %*% Kinv %*% V %*% Kinv %*% c)) # gives same directions
		u <- U %*% w
		krig.direction[i] <- atan2(u[2],u[1])
		krig.variance[i] <- 2 - 2*sqrt(as.numeric(t(c) %*% Kinv %*% c))
	}

	if(Smooth)
	{
		xx.dx <- xx[2] - xx[1]; yy.dy <- yy[2] - yy[1]
		# as.image loads the matrix by row
		ImageList.x <- as.image(cos(krig.direction), x=data.frame(krig.x, krig.y), nrow=nx, ncol=ny, boundary.grid=FALSE)
		smooth.x <- image.smooth( ImageList.x, theta = bandwidth)
		ImageList.y <- as.image(sin(krig.direction), x=data.frame(krig.x, krig.y), nrow=nx, ncol=ny, boundary.grid=FALSE)
		smooth.y <- image.smooth( ImageList.y, theta = bandwidth)
		krig.direction <- as.vector(t(atan2(smooth.y$z, smooth.x$z)))
	}

	if(Plot)
	{
		if(!PlotVar)
		{
			plot(krig.x, krig.y, ty="n", xlab="", ylab="", asp=1, xlim=Xlim, ylim=Ylim)
			arrow.plot(a1=krig.x, a2=krig.y, u=cos(krig.direction), v=sin(krig.direction), arrow.ex=0.06,
				xpd=FALSE, true.angle=TRUE, length=.05, col=1)
		} else
		{
			krig.variance.matrix <- matrix(data=krig.variance, nrow=nx, ncol=ny, byrow=TRUE)
			if(!is.null(Xlim))
			{
				filled.contour(x = xx, y = yy, z = krig.variance.matrix, xlim=Xlim, ylim=Ylim,
				color = terrain.colors, key.title = title(main="Circ Krig \nVariance", cex.main=0.8),
				asp = 1, plot.axes={axis(1); axis(2); points(resid.x, resid.y, pch=20, cex=.65)})
			} else {
				filled.contour(x = xx, y = yy, z = krig.variance.matrix,
				color = terrain.colors, key.title = title(main="Circ Krig \nVariance", cex.main=0.8),
				asp = 1, plot.axes={axis(1); axis(2); points(resid.x, resid.y, pch=20, cex=.65)})
			}
		}
	} else return(list(x=krig.x, y=krig.y, direction=krig.direction, variance=krig.variance))
}
