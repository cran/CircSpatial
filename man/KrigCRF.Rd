\name{KrigCRF}
\alias{KrigCRF}
\title{Krig Circular-Spatial Data}

\description{
Estimate circular-spatial direction and variability of estimate using spatial dependence, and smooth circular-spatial data
}

\usage{
KrigCRF(krig.x, krig.y, resid.x, resid.y, resid.direction, Model,
  Nugget=0, Range, sill, Smooth=FALSE, bandwidth, Plot=FALSE,
  Xlim=NULL, Ylim=NULL, PlotVar=FALSE, \dots)
}

\arguments{
  \item{krig.x}{Vector of horizontal coordinates of kriging locations}
  \item{krig.y}{Vector of vertical coordinates of kriging locations corresponding to krig.x}
  \item{resid.x}{Vector of horizontal coordinates of rotational residuals or data}
  \item{resid.y}{Vector of vertical coordinates of rotational residuals or data}
  \item{resid.direction}{Vector of direction in radians of rotational residuals or data}
  \item{Model}{Model from package RandomFields function CovarianceFct best fitting the empirical cosineogram}
  \item{Nugget}{1 minus the mean cosine at distance close to zero due to measurement error, micro scale variation or sampling}
  \item{Range}{Distance at which spatially correlated circular random variables (CRV) are not correlated if spherical, or scale parameter}
  \item{sill}{Mean cosine at distances where CRV are uncorrelated}
  \item{Smooth}{If TRUE apply smoothing bandwidth to components of kriged direction}
  \item{bandwidth}{Kernel smoothing bandwidth (>0)}
  \item{Plot}{If TRUE plot kriged direction vs. location, else output location and kriged direction in radian units}
  \item{PlotVar}{If TRUE plot circular kriging variance}
  \item{Xlim}{Horizontal plot limits}
  \item{Ylim}{Vertical plot limits}
  \item{\dots}{Additional model parameters}
}

\details{
In the installed folder doc, Chapter4.CircularKriging and Section J.5\cr
in Appendices.J.PackageDocumentation provide additional detail and illustrations.\cr\cr

KrigCRF estimates circular-spatial data of an isotropic CRF with \dQuote{exact interpolation} (estimate direction equals measured direction at observation locations).  The solution is a linear combination of observations of direction or residual rotations (when the first order trend is removed) that minimizes the squared length of the error vector.  This is accomplished by incorporation of the spatial correlation (nugget, range, sill, and cosine model) as estimated by the model fitted to the cosineogram.  The circular kriging variance is an estimate of the expected mean squared length of the error vector.  To avoid cross over, KrigCRF also separately applies image.smooth of package fields to the horizontal and vertical components of kriged direction.  The smoothed kriged direction is the quadrant specific inverse tangent of the smoothed components (atan2).  The circular kriging estimate can be combined with an interpolation of the fitted model.
}

\value{List of
  \item{x}{Vector of kriging horizontal coordinates}
  \item{y}{Vector of kriging vertical coordinates}
  \item{direction}{Vector of kriging direction in radians}
}

\author{Bill Morphet}

\seealso{
  \code{\link{SimulateCRF}}\cr
  \code{\link{CircResidual}}\cr
  \code{\link{CosinePlots}}\cr
  \code{\link{InterpDirection}}
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

## Cosineogram
CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,
  Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=FALSE)
abline(h=0.56, col=2); abline(v=4, col=2)

## Fit cosine Models
CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,
  Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=TRUE, nugget=0,
  Range=4.0,  sill=0.56, x.legend=.2, y.legend=0.3, xlim=c(0,8),
  ylim=c(0,1))

## Krig to residuals using cosine model
x2 <- seq(1,11, by=0.2)
y2 <- x2
krig2 <- KrigCRF(krig.x=x2, krig.y=y2, resid.x=resids1$x,
  resid.y=resids1$y, resid.direction=resids1$direction, Model=
  "exponential", Nugget=0.0, Range=4, sill=0.56, Plot=FALSE)

## Plot Kriging, Overplot Residuals In Black
plot(krig2$x, krig2$y, ty="n", xlab="", ylab="", xlim=c(5, 8),
  ylim=c(5, 8), asp=1)
arrow.plot(krig2$x, krig2$y, u=cos(krig2$direction), v=
  sin(krig2$direction), arrow.ex=0.06, xpd=FALSE, true.angle=TRUE,
  length=.05, col="tan")
arrow.plot(resids1$x, resids1$y, u=cos(resids1$direction), v=
  sin(resids1$direction), arrow.ex=0.09, xpd=FALSE, true.angle=TRUE,
  length=.05, col=1)

## Smooth via Bandwidth & plot kriging
KrigCRF(krig.x = x2, krig.y = y2, resid.x= resids1$x, resid.y= resids1$y,
  resid.direction=resids1$direction, Model="exponential", Nugget=0.0,
  Range=4, sill=0.56, Plot=TRUE, Xlim=c(7,10), Ylim=c(7,10), Smooth=TRUE,
  bandwidth=2)  

## Plot Kriging estimate variability at sample locations on a regular
## grid
KrigCRF(krig.x=x2, krig.y=y2, resid.x=resids1$x, resid.y=resids1$y,
  resid.direction=resids1$direction, Model="exponential", Nugget=0.0,
  Range=4, sill=0.56, Plot=TRUE, Smooth=FALSE, PlotVar=TRUE)

## CRF with random locations
set.seed(13)
crf6 <- SimulateCRF(N=400, CircDistr="Card", Rho=0.4, Range=4, Ext=3,
  CovModel="spherical")

## Best fit is spherical with range=2.85 and sill=0.15
CosinePlots(x=crf6$x, y=crf6$y, directions=crf6$direction, Lag.n.Adj=1.5,
  BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=TRUE, nugget=0, Range=2.85,
  sill=0.15, x.legend=.14, y.legend=0.75, xlim=c(0,6), ylim=c(0,1))

## Plot kriging estimate variability with random sample locations.
## This may take significant time depending on your computer.
x6 <- seq(4,7, by=0.02); y6 <- x6
y6 <- x6
KrigCRF(krig.x =x6, krig.y =y6, resid.x=crf6$x, resid.y=crf6$y,
  resid.direction=crf6$direction, Model="spherical", Nugget=0.0,
  Range=2.85, sill=0.15, Plot=TRUE, PlotVar=TRUE)
}

\keyword{hplot}
\keyword{graphs}
\keyword{spatial}
