\name{CircResidual}
\alias{CircResidual}
\alias{Circular Residuals}
\title{Compute or Plot Circular Residuals}

\description{
Returns the residuals or plots data, model, and residuals with black, tan, and dashed black arrows, respectively.
}

\usage{CircResidual(X, Y, Raw, Trend, Plot=FALSE, AdjArrowLength=1, \dots)}

\arguments{
  \item{X}{Vector of horizontal coordinates of observations and trend locations}
  \item{Y}{Vector of vertical coordinates of observations and trend locations}
  \item{Raw}{Vector of direction of observations in radians}
  \item{Trend}{Vector of fitted model direction in radians, NAs not allowed}
  \item{Plot}{If FALSE return value.  If TRUE, plot data (black), model(tan), and residuals(dashed black) with asp=1.}
  \item{AdjArrowLength}{Multiplies length of arrows in plots}
  \item{\dots}{Additional parameters for plotting}
}

\note{
To characterize the spatial cosine structure, the first order trend, if any, must be removed via an appropriate fitted model (See Examples).
}

\details{
In the installed folder doc, Section J.3 in Appendices.J.PackageDocumentation provides additional detail and illustrations.\cr\cr

Spatial dependence is encoded in the residual rotations=the rotation in radians from the fitted mean direction to the data direction.  The first order trend, if any, must be removed from the data via an appropriate fitted model.  Separately fit the cosine and sine components of direction to functions of the spatial coordinates to avoid the cross over problem (direction of 0 deg equals direction of 360 deg).  Then, the fitted direction is obtained using R function atan2(fitted sines, fitted cosines).  A positive residual rotation indicates that counter clockwise (CCW) rotation is required to rotate the fitted model direction to the data direction.  A negative residual rotation indicates that clockwise (CW) rotation is required.  CircResidual returns the residuals or plots data, model, and residuals with black, thick tan, and dashed red arrows, respectively.
}

\value{
If Plot=FALSE, value is list of
  \item{x}{Vector of horizontal coordinates of residuals}
  \item{y}{Vector of vertical coordinates of residuals}
  \item{direction}{Vector of direction residuals in radians}
}

\author{Bill Morphet}

\seealso{
  \code{\link{CosinePlots}}\cr
  \code{\link{KrigCRF}}
}

\examples{
## Model
x1<- 1:11; y1 <- 1:11; y1 <- rep(y1, 11); x1 <- rep(x1, each=11)
model.direction1 <- matrix(data=c(
  157, 141, 126, 113, 101, 90, 79, 67, 54, 40, 25, 152, 137, 123, 111,
  100, 90, 80, 69, 57, 44, 30, 147, 133, 120, 109,  99, 90, 81, 71, 60,
  48, 35, 142, 129, 117, 107,  98, 90, 82, 73, 63, 52, 40, 137, 125,
  114, 105,  97, 90, 83, 75, 66, 56, 45, 132, 121, 111, 103,  96, 90,
  84, 77, 69, 60, 50, 127, 117, 108, 101,  95, 90, 85, 79, 72, 64, 55,
  122, 113, 105,  99,  94, 90, 86, 81, 75, 68, 60, 117, 109, 102,  97,
  93, 90, 87, 83, 78, 72, 65, 112, 105,  99,  95,  92, 90, 88, 85, 81,
  76, 70, 107, 101,  96,  93,  91, 90, 89, 87, 84, 80, 75), ncol=11,
  byrow=TRUE)
model.direction1 <- as.vector(model.direction1)*pi/180

## Plot Trend Model
plot(x1, y1, type="n", xlab="", ylab="", asp=1)
arrow.plot(x1, y1, u=cos(model.direction1), v=sin(model.direction1),
  arrow.ex=0.1, xpd=TRUE, true.angle=TRUE, length=.1)

## Compute vM CRF of 121 observations, Rho=sqrt(0.5) so sill about 0.5,
## from GRF (Range=4, spherical covariance).
set.seed(666)
crf1<- SimulateCRF(CircDistr="vM", Rho=sqrt(0.5), Range=4, CovModel=
  "spherical", Grid=cbind(x1, y1), OverFit=TRUE)

## Plot CRF
par(mai=c(0.4, 0.35, .25, 0.25))
plot(crf1$x, crf1$y, type="n", xlab="", ylab="", asp=1)
arrow.plot(a1=crf1$x, a2=crf1$y, u=cos(crf1$direction), v=
  sin(crf1$direction), arrow.ex=0.1, xpd=TRUE, true.angle=TRUE,
  length=.1)

# Make sample
sample.direction1 <- model.direction1 + crf1$direction

## Plot Sample
sample.direction1 <- model.direction1 + crf1$direction
plot(x1, y1, type="n", asp=1)
arrow.plot(a1=x1, a2=y1, u=cos(sample.direction1), v=
  sin(sample.direction1), arrow.ex=0.125, xpd=TRUE, true.angle=TRUE,
  length=.1)

## Fit An Appropriate Model
FitHoriz1 <- lm(cos(sample.direction1) ~ (x1 + y1))
FitVert1 <- lm(sin(sample.direction1)  ~ (x1 + y1))
fitted.direction1 <- atan2(FitVert1$fitted.values,
  FitHoriz1$fitted.values)

## Plot Fitted Model
plot(x1, y1, type="n", asp=1, xlab="", ylab="")
arrow.plot(x1, y1, u=cos(fitted.direction1), v=sin(fitted.direction1),
  arrow.ex=0.1, xpd=TRUE, true.angle=TRUE, length=.1)

## Compute Residuals
resids1 <- CircResidual(X=x1, Y=y1, Raw=sample.direction1,
  Trend=fitted.direction1, Plot=FALSE)

## Plot Sample, Fitted Model, and Residual Rotations
CircResidual(X=x1, Y=y1, Raw=sample.direction1, Trend=fitted.direction1,
  Plot=TRUE, xlim=c(3,7), ylim=c(3,7))
}

\keyword{hplot}
\keyword{graphs}
\keyword{spatial}
