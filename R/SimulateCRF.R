`SimulateCRF` <-
function(N=100, CircDistr, Rho, Mu=0, Range, Ext=1, CovModel, Grid=NULL, Anisotropy=NULL, OverFit=FALSE, Resolution=0.01)
{
	# 2008-11-10.2001
	# Simulate CRF ~ (Range, CircDistr, Rho, mu=0)

	# Input Arguments
	# N: Number of spatial locations to simulate
	# CircDistr: Circular distribution in {U, vM, WrC, Tri, Card},
	# Rho: Mean resultant length parameter
	#        For triangular, 0 < Rho <= 4/pi^2
	#        For cardioid, 0 < Rho <= 0.5
	#        For vM and wrapped Cauchy, 0 < Rho < 1, 1== degenerate
	#        For uniform, Rho = 0
	# Range: Distance at which CRV independent
	# Ext: Range*Ext is horizontal and vertical length of sample space
	# CovModel: Name of spatial correlation function, see package geoR Help cov.spatial
	# Grid: Regular or irregular N x 2 matrix of simulation locations, overides N and Ext
	# Anisotropy: Vector of geometric anisotropy angle in radians, ratio > 1.
	# OverFit=TRUE, or standardization (centering and rescaling realization of the GRV to mean 0 sd 1) results in closer fit
	#   for qualitative evaluation of the CRV.  Undesirable effects are loss of independence of the marginal GRVs, biased GRF
	#   covariance, and biased testing.  Standardization is suitable for demonstration with closer fit, visualization, and
	#   illustrations.  Do not standardize for the purposes of simulation and testing.  OverFit=FALSE, or non-standardization (default)
	#   includes expected variation from transformation of variation in mean and sd of sample of GRV.

	# Values
	# x,y: Vectors of location coordinates
	# direction: Vector of directions
	# Z: Vector of simulated observations of the associated GRV

	# Note:
	# At n > 500, geoR transfers processing the the package Random Fields because the option RF is set.

	if(CircDistr !="U" & CircDistr !="vM" & CircDistr !="WrC" & CircDistr !="Tri" & CircDistr !="Card")
		stop("CRF not implemented for input CircDistr")

	if(CircDistr =="U") Rho = 0
	if(abs(Mu) > pi) stop("abs(Mu) <= pi")

	if(!is.null(Grid)) {
		if(class(Grid) != "matrix") stop("Grid not a matrix")
		if(dim(Grid)[2] !=2) stop("Grid not a N x 2 matrix")
		N <- dim(Grid)[1]}
	if(!is.null(Anisotropy)) {
		if(length(Anisotropy) != 2) stop("Anisotropy is not a 2 element vector.  See geoR Help")
	}

	if(N <=0 | Rho < 0 | Range < 0| Ext <=0 | Resolution <= 0) stop("Improper numeric input")

	direction <- vector(mode="numeric", length=N)

	# require(CircStats)
	# require(geoR)
	# Standard normal GRF, see Help geoR grf
	if(is.null(Grid)) {
		GRF <- grf(n=N, xlims=c(0, Range*Ext), ylims=c(0, Range*Ext), cov.model=CovModel,
			nugget=0, cov.pars=c(1, Range), aniso.pars=Anisotropy, RF=TRUE, messages=FALSE) } else {
		GRF <- grf(grid=Grid, cov.model=CovModel,
			nugget=0, cov.pars=c(1, Range), aniso.pars=Anisotropy, RF=TRUE, messages=FALSE)}

	XY <- GRF$coords # N x 2 matrix
	x <- XY[,1]; y <- XY[,2]
	Z <- GRF$data # Vector of GRV
	if(OverFit) {Z <- (Z - mean(Z))/sd(Z); GRF$data <- Z}
	CumProbZ <- pnorm(Z, mean=0, sd=1, lower.tail = TRUE)

	if(CircDistr=="U") {direction <- -pi + 2*pi*CumProbZ} else
	if(CircDistr == "Tri")
	{
		if(Rho==0 | Rho > 4/pi^2) stop("Tri: 0 < Rho <= 4/pi^2")
		filter <- CumProbZ < 0.5
		u1 <- CumProbZ[filter]
		a <- Rho/8
		b <- (4+pi^2*Rho)/(8*pi)
		c <- 0.5 - u1
		q <- -.5*(b+sqrt(b^2-4*a*c))
		direction[filter] <- c/q	

		u2 <- CumProbZ[!filter]
		a <- -Rho/8
		b <- (4+pi^2*Rho)/(8*pi)
		c <- 0.5 - u2
		q <- -.5*(b+sqrt(b^2-4*a*c))
		direction[!filter]<- c/q
	} else
	{
		# For OTHER circular distributions compute table of circular CDF and interpolate
		CircScale <- seq(-pi, pi, length=2*pi/Resolution)
		# With resolution=.01, circular support from -pi to +pi has 629 elements, delta ~0.01000507, CircScale[315] = 0
		n <- length(CircScale)
		if(CircDistr == "vM")
		{
			if(Rho==0 | Rho >= 1) stop("vM: 0 < Rho < 1")
			CircProb <- rep(-1, n)
			Kappa=A1inv(Rho) # N. I Fisher, Statistical Analysis of Circular Data, 2000 p. 49
			# As direction increases from -pi, pvm increases from .5
			for(i in 1:length(CircScale)) CircProb[i] <- pvm(CircScale[i], mu=0, kappa=Kappa)
			filter <- CircScale < 0
			CircProb[filter] <- CircProb[filter] - 0.5
			CircProb[!filter] <- CircProb[!filter] + 0.5 
		} else
		if(CircDistr == "Card") 
		{
			if(Rho==0 | Rho > 0.5) stop("Cardioid: 0 < Rho <= 0.5") 
			CircProb <- (CircScale + pi + 2*Rho*sin(CircScale))/(2*pi)
		} else
		if(CircDistr == "WrC") 
		{
			if(Rho==0 | Rho >= 1) stop("Wrapped Cauchy: 0 < Rho < 1 ")
			Angles1 <- CircScale[CircScale < 0]
			Angles2 <- CircScale[CircScale >= 0]
			prob1 <- 0.5 - acos(((1+Rho^2)*cos(Angles1) - 2*Rho)/(1 + Rho^2 - 2*Rho * cos(Angles1)))/(2*pi)
			prob2 <- 0.5 + acos(((1+Rho^2)*cos(Angles2) - 2*Rho)/(1 + Rho^2 - 2*Rho * cos(Angles2)))/(2*pi)
			CircProb <-c(prob1, prob2)
		}
		CircProb[1] <- 0; CircProb[n] <- 1

		# Interpolation
		DeltaTh <- CircScale[2] + pi
		for(i in 1:N)
		{
			p <- CumProbZ[i]  # Cumulative prob of GRV
			a <- max((1:n)[CircProb <= p]) # Index
			if(a==n) {r <- 0} else
			{
				if(CircProb[a]==p) {r <- 0} else {r <- (p -CircProb[a])/( CircProb[a+1] -CircProb[a])}
			}
			direction[i] <- CircScale[a] + r*DeltaTh
		}
	}

	direction <- direction + Mu
	return(list(x=x, y=y, direction=direction, Z=Z))
}
