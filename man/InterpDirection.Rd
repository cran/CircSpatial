\name{InterpDirection}
\alias{InterpDirection}
\title{Interpolate Circular-Spatial Model}
\description{Interpolate trend model direction.}

\details{
In the installed folder doc, Section J.6 in Appendices.J.PackageDocumentation provides additional detail and illustrations.\cr\cr

InterpDirection can also be used to estimate fitted model at data locations in order to compute residuals.  The interpolated model direction is added to the kriged or smoothed kriged residuals to obtain the estimated direction.  To avoid cross over, the interpolated model direction is obtained by separately interpolating the cosines and sines of model direction.  A plane is fitted to the three cosine values over the triangular partition of the model grid cell in which an interpolation location occurs.  The interpolated cosine value is the elevation of the plane at the interpolation location.  The sine is interpolated by the same method.  The interpolated direction is obtained by applying the quadrant specific inverse tangent (atan2) to the interpolated sine and cosine components.
}

\usage{InterpDirection(in.x, in.y, in.direction, out.x, out.y)
}

\arguments{
  \item{in.x}{Vector of input horizontal coordinates}
  \item{in.y}{Vector of input vertical coordinates}
  \item{in.direction}{Vector of input direction in radians}
  \item{out.x}{Vector of output horizontal coordinates}
  \item{out.y}{Vector of output vertical coordinates}
}

\note{
Locations to interpolate must be within range of inputs (in.x, in.y), or an error message is displayed.  NAs not allowed.
}

\value{
List of
  \item{x}{out.x}
  \item{y}{out.y}
  \item{direction}{Vector of interpolated direction in radians}
}

\author{Bill Morphet}

\seealso{
  \code{\link{KrigCRF}}
}

\examples{
## Construct Trend Model of 121 locations
x1<- 1:11; y1 <- 1:11; y1 <- rep(y1, 11); x1 <- rep(x1, each=11)
model.direction1 <- matrix(data=c(
  157, 141, 126, 113, 101, 90, 79, 67, 54, 40, 25, 152, 137, 123, 111,
  100, 90, 80, 69, 57, 44, 30, 147, 133, 120, 109,  99, 90, 81, 71, 60,
  48, 35, 142, 129, 117, 107,  98, 90, 82, 73, 63, 52, 40, 137, 125, 114,
  105,  97, 90, 83, 75, 66, 56, 45, 132, 121, 111, 103,  96, 90, 84, 77,
  69, 60, 50, 127, 117, 108, 101,  95, 90, 85, 79, 72, 64, 55, 122, 113,
  105,  99,  94, 90, 86, 81, 75, 68, 60, 117, 109, 102,  97,  93, 90, 87,
  83, 78, 72, 65, 112, 105,  99,  95,  92, 90, 88, 85, 81, 76, 70, 107,
  101,  96,  93,  91, 90, 89, 87, 84, 80, 75), ncol=11, byrow=TRUE)
model.direction1 <- as.vector(model.direction1)*pi/180

## Compute vM CRF of 121 observations, Rho=sqrt(0.5) so sill about 0.5,
## from GRF (Range=4, spherical covariance).
set.seed(666)
crf1<- SimulateCRF(CircDistr="vM", Rho=sqrt(0.5), Range=4,
  CovModel="spherical", Grid=cbind(x1, y1), OverFit=TRUE)

# Make sample
sample.direction1 <- model.direction1 + crf1$direction

## Fit An Appropriate Model
FitHoriz1 <- lm(cos(sample.direction1) ~ (x1 + y1))
FitVert1 <- lm(sin(sample.direction1)  ~ (x1 + y1))
fitted.direction1 <- atan2(FitVert1$fitted.values,
  FitHoriz1$fitted.values)

## Compute Residuals
resids1 <- CircResidual(X=x1, Y=y1, Raw=sample.direction1,
  Trend=fitted.direction1, Plot=FALSE)

## Fit cosine Models
## Fit of exponential with range=4 and sill=.56 adequate
CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,
  Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=TRUE, nugget=0,
  Range=4.0,  sill=0.56, x.legend=.2, y.legend=0.4, xlim=c(0,8),
  ylim=c(0,1))

## Krig to residuals using cosine model
x2 <- seq(3,6, by=0.1); n <- length(x2)
y2 <- x2; y2 <- rep(y2, n)
x2 <- rep(x2, each=n); rm(n)
krig2 <- KrigCRF(krig.x=x2, krig.y=y2, resid.x=resids1$x,
  resid.y=resids1$y, resid.direction=resids1$direction, Model = 
  "exponential", Nugget=0.0, Range=4, sill=0.56, Plot=FALSE)
## Using previous range and sill estimate

## Interpolate Fitted Model
interp2 <- InterpDirection(in.x=x1, in.y=y1, in.direction=
  fitted.direction1, out.x=krig2$x, out.y=krig2$y)

## Plot Estimate Of Direction And Overplot Sample.
estimate2=interp2$direction + krig2$direction
plot(interp2$x, interp2$y, type="n", xlab="", ylab="", asp=1)
arrow.plot(interp2$x, interp2$y, u=cos(estimate2), v= sin(estimate2),
  arrow.ex=0.05, xpd=FALSE, true.angle=TRUE, length=.05, col="tan")
arrow.plot(x1, y1, u=cos(sample.direction1), v=sin(sample.direction1),
  arrow.ex=0.05, xpd=FALSE, true.angle=TRUE, length=.05, col=1)
}

\keyword{arith}
\keyword{spatial}
