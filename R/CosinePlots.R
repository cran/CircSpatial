`CosinePlots` <-
function(x, y, directions, Lag=NULL, Lag.n.Adj = 1, BinWAdj=1, Plot = TRUE,
	Cloud = FALSE, Model=FALSE, nugget=0, Range=NULL, sill=NULL, x.legend=0.6, y.legend=1.0, TrimMean=0.1, ...)
{
	# 2008-11-11.1050
	# Assumption: Isotropic circular random field
	# x, y are vectors of location coordinates, directions is a vector of directions in radians.
	# Lag is a vector of lag points.  Lag.n.Adj > 0 multiplies the number of lag points.
	# BinWAdj >= 1 multiplies bin width (to make bins narrower increase Lag.n.adj).  Sturges rule determines nBins.
	# nBins and Lag.n.Adj determine Lag.n.  Lag.n adjusts nBins.  nBins and BinWAdj determine bin width.
	# Plot = TRUE plot cosineocloud or cosineogram, else ouput list of points.  Cloud = TRUE plots cosineocloud, else cosineogram.
	# Model = TRUE overplots exponential, gaussian, and spherical models with nugget, Range, and sill parameters.
	# x.legend and y.legend adjust legend location.
	# TrimMean = 0.1 applies trimmed mean in computing mean cosine.

	if( (length(x) != length(y)) | (length(x) != length(directions)) | (length(y) != length(directions)) )
		stop("lengths of vector inputs unequal")
	if(Lag.n.Adj <= 0) stop("Lag.n.Adj invalid")
	if( (nugget < 0) | (nugget > 1) ) stop("nugget invalid")
	if(!is.null(Range)) {if(Range <= 0) stop("Range negative")}
	if(!is.null(sill)) {
		if( (sill < 0) | (sill >=1) ) stop("sill invalid")
		if(1-nugget < sill) stop("1-nugget < sill")}

	# Repair Input and Remove missings
	if(BinWAdj < 1) BinWAdj <- 1 # points will fall out of bins if adjust < 1
	filter <- !is.na(directions)
	x <- x[filter]; y <- y[filter]; directions <- directions[filter]

	# Pairwise cosines
	# Subroutine to compute circular distances in radians
	CircDist <- function(alpha,beta)
	{
		alpha[alpha < 0] <- 2*pi + alpha[alpha < 0]
		beta[beta < 0] <- 2*pi + beta[beta < 0]
		theta <- abs(alpha - beta)
		theta[theta > pi] <- 2*pi - theta[theta > pi]
		return(theta)
	}
	Cosines <- cos(outer(directions, directions, FUN="CircDist"))
	filter.tri <- upper.tri(Cosines)
	Cosines <- Cosines[filter.tri]

	# Pairwise distances
	Distances <- as.matrix(dist(cbind(x, y))) # Diagonal of zero distances
	X <- Distances[filter.tri] # vector of distances corresponding to vector of cosines Cosines

	if(Cloud) {Y <- Cosines} else
	{
		if(!is.null(Lag))
		{
			# Assumes equally spaced lags, except for first lag point
			HalfBinWidth <- BinWAdj * 0.5 * (Lag[2] - Lag[1])
			Lag.n <- length(Lag)
		} else
		{
			nBins <- trunc(log2(sum(filter.tri)) + 1) # Sturges rule
			Lag.n <- trunc(Lag.n.Adj*(nBins + 1))
			nBins <- Lag.n - 1
			distance.max <- max(X)
			HalfBinWidth <- BinWAdj * 0.5 * distance.max/nBins
			Lag <- seq(0, distance.max, length.out= Lag.n)
		}

		Y <- vector(mode = "numeric", length = Lag.n)
		if(Lag[1] == 0) {Y[1] <- 1; i1 <- 2} else i1 <- 1
		for (i in i1:Lag.n)
		{
			filter <- abs(X - Lag[i]) <= HalfBinWidth
			Y[i] <- mean(Cosines[filter], trim=TrimMean)
		}

		X <- Lag # Cloud=FALSE filtered distances replaced by lag vector
	}
	if(Plot)
	{
		plot(X, Y, xlab = "Distance", ylab = "Cosine", cex.main=1, ...)
		if(Cloud == FALSE & Model == TRUE)
		{
			xx <- seq(min(X), max(X), length.out=101)
			c.e <- 1-nugget -(1-nugget-sill)*(1-exp(-3*xx/Range))
			c.g <- 1-nugget -(1-nugget-sill)*(1-exp(-3*(xx/Range)^2))
			X1 <- xx[xx <= Range]
			c.s <- 1-nugget -(1-nugget-sill)*(1.5*X1/Range-0.5*(X1/Range)^3)
			X2 <- xx[xx > Range]
			c.s <- c(c.s, rep(sill, length(X2)))
			lines(xx, c.e, col=2,     lty=1, lwd=1)
			lines(xx, c.g, col="tan", lty=1, lwd=3)
			lines(xx, c.s, col=4,     lty=2, lwd=1)

			legend(x=x.legend*max(X), y=y.legend, c("Exponential","Gaussian","Spherical"),
			lty = c(1, 1, 2), col=c(2, "tan", 4), lwd=c(1, 3, 1), cex=1.1)
		}
	} else {return(list(distance = X, cosine = Y))}
}
