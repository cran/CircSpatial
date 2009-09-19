\name{CosinePlots}
\alias{CosinePlots}
\alias{Cosineocloud}
\alias{Cosineogram}
\alias{Cosine Model}
\title{Plot Cosine Plots}

\description{
Functions for plotting cosineocloud, cosineogram, and cosine models for fitting cosineograms.
}

\usage{
CosinePlots(x, y, directions, Lag=NULL, Lag.n.Adj=1, BinWAdj=1,
  Plot=TRUE, Cloud=FALSE, Model=FALSE, nugget=0, Range=NULL,
  sill=NULL, x.legend=0.6, y.legend=1.0, TrimMean=0.1, \dots)
}

\arguments{
  \item{x}{Vector of horizontal coordinates of observations}
  \item{y}{Vector of vertical coordinates of observations}
  \item{directions}{Vector of direction of observations or residual rotations in radians}
  \item{Lag}{Vector of ascending distances, beginning with 0, where mean cosine is to be computed}
  \item{Lag.n.Adj}{Multiplier (> 0) of the number of lag points.  Value > 1 increases the number of points for more detail}
  \item{BinWAdj}{Multiplier (>= 1) of bin width. Value > 1 has a smoothing effect}
  \item{Plot}{See Details}
  \item{Cloud}{See Details}
  \item{Model}{See Details}
  \item{nugget}{Model nugget.  0<=nugget<=1}
  \item{Range}{Model range}
  \item{sill}{Model sill}
  \item{x.legend}{Model plot legend horizontal location as fraction of horizontal maximum coordinate}
  \item{y.legend}{Model plot legend vertical coordinate}
  \item{TrimMean}{Compute mean cosine using trimmed mean (0.0 to 0.5).  See R Help for mean}
  \item{\dots}{Additional parameters for plotting}
}

\details{
In the installed folder doc, Chapter3.Cosineogram and Section J.4 in\cr
Appendices.J.PackageDocumentation provide additional detail and illustrations.\cr\cr

The empirical omnidirectional cosineogram expresses the spatial dependence of an isotropic CRF (spatial dependence depends on distance not direction) as the mean cosine of the circular distances between pairs of rotations from the mean direction as a function of the distance between measurement locations.  Fitted cosineogram model parameters are \var{Range}, \var{sill}, and \var{nugget}.  \var{Range} is the distance at which observations are no longer spatially correlated for the spherical cosine model, and otherwise a scale parameter.  \var{sill} is the mean cosine at distances where CRV are uncorrelated.  \var{nugget} is 1 minus the mean cosine at approx 0 distance.  It indicates measurement error and/or variation on a micro scale.  The nugget, range, sill, and cosine model (see below) of the fitted cosineogram are incorporated into the circular kriging solution.\cr\cr

The cosineocloud plot, which derives its name from the variocloud plot for linear kriging, is useful to identify outliers that may be excluded from subsequent calculations.  It shows the cosine values computed from all pairs of directional observations vs. distance between observations.\cr\cr

The number of lag points and bin width of the cosineogram are determined in a sequence:\cr
1) Sturges rule determines nBins, the number of bins.\cr
2) nBins and \var{Lag.n.Adj} determine Lag.n, the number of distances (lag points) to evaluate the mean cosine.\cr
3) nBins is adjusted to Lag.n minus 1 (To make bins narrower increase Lag.n.adj.).\cr
4) nBins and \var{BinWAdj} determine bin width.\cr\cr

The empirical cosineogram may be overplotted with exponential (red), gaussian (thick tan), and spherical (dashed blue) cosine models to help fit a model.  The cosine model is monotonic decreasing with increasing distance between measurement locations up to the range, and produces a positive definite matrix of cosines when applied to the pairwise distances between measurement locations.  The positive definite matrix is required by circular kriging for an optimum solution.  The cosineogram maximum is 1 and the minimun is approximately the square of the mean resultant length of the CRV.  Typically, range and sill are determined visually.  When Plot=Model=TRUE, vary the \var{Range} and \var{sill} parameters to fit.  The \var{nugget} may be determined by linear regression of initial empirical cosineogram points.
}

\value{
  \tabular{lllllll}{
  \bold{Plot}  \tab .....\tab \bold{Cloud} \tab .....\tab \bold{Model} \tab .....\tab \bold{Output}\cr
        FALSE  \tab .....\tab       TRUE   \tab .....\tab       FALSE  \tab .....\tab       List of cosineocloud coordinates\cr
        FALSE  \tab .....\tab       FALSE  \tab .....\tab       FALSE  \tab .....\tab       List of cosineogram coordinates\cr
        TRUE   \tab .....\tab       TRUE   \tab .....\tab       FALSE  \tab .....\tab       Cosineocloud Plot\cr
        TRUE   \tab .....\tab       FALSE  \tab .....\tab       FALSE  \tab .....\tab       Cosineogram Plot\cr
        TRUE   \tab .....\tab       FALSE  \tab .....\tab       TRUE   \tab .....\tab       Cosineogram overplotted with cosine models}
}

\author{Bill Morphet}

\seealso{
  \code{\link{SimulateCRF}}\cr
  \code{\link{CircResidual}}\cr
  \code{\link{KrigCRF}}
}

\examples{
## Model
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

## Output list of cosineogram coordinates for fitting analytically
cosineogram.out <- CosinePlots(x=resids1$x, y=resids1$y,
  directions=resids1$direction, Lag.n.Adj=1, BinWAdj=1, Plot=FALSE,
  Cloud=FALSE, Model=FALSE)

## Cosineocloud
CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,
  Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=TRUE)

## Cosineogram
CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,
  Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=FALSE)
abline(h=0.56, col=2); abline(v=4, col=2)

## Fit cosine Models
## Fit of exponential with range=4 and sill=.56 adequate
CosinePlots(x=resids1$x, y=resids1$y, directions=resids1$direction,
  Lag.n.Adj=1, BinWAdj=1, Plot=TRUE, Cloud=FALSE, Model=TRUE, nugget=0,
  Range=4.0, sill=0.56, x.legend=.2, y.legend=0.3, xlim=c(0,8),
  ylim=c(0,1))
}

\keyword{models}
\keyword{univar}
\keyword{graphs}
\keyword{hplot}
\keyword{spatial}
