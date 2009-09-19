\name{PlotVectors}
\alias{PlotVectors}
\title{Vector Plots}

\description{
PlotVectors plots vector-spatial data as unit vectors, vectors, or triangle icons with or without jittering.  Triangle icons have area proportional to vector magnitude.  Jittering can be help clarify structure when vectors overlap.
}

\usage{
PlotVectors(x, y, h, v, UnitVector=TRUE, TriIcon=FALSE,
  AdjArrowLength=1, AdjHeadLength=1, TriIconAdj=1, TriRatio=4,
  JitterPlot=FALSE, Jitter=1, \dots)
}

\arguments{
	\item{x}{Vector of x coordinates}
	\item{y}{Vector of y coordinates}
	\item{h}{Vector of horizontal component}
	\item{v}{Vector of vertical comonent}
	\item{UnitVector}{TRUE or FALSE, see Details}
	\item{TriIcon}{TRUE or FALSE, see Details}
	\item{AdjArrowLength}{Arrow length multiplier}
	\item{AdjHeadLength}{Arrow head length multiplier}
	\item{TriIconAdj}{Multiplies size of icons}
	\item{TriRatio}{Length to width ratio of triangle icon}
	\item{JitterPlot}{If TRUE add jitter to location coordinates}
	\item{Jitter}{Amount of jitter=Jitter x runif value}
	\item{\dots}{Additional parameters for plotting}
}

\details{
Section J.11 in Appendices.J.PackageDocumentation in the installed folder doc provides\cr
additional detail and illustrations.\cr\cr

Missing values are permitted in \var{h} and/or \var{v}.  u=v=0 are filted and not plotted.\cr

\tabular{lll}{
	\bold{\var{UnitVector}} \tab \bold{\var{TriIcon}} \tab \bold{Plot Output}\cr
	FALSE \tab FALSE  \tab Direction and magnitude as variable length arrow\cr
	TRUE  \tab FALSE  \tab Direction as constant length arrow\cr
	FALSE \tab TRUE   \tab Triangle icons with area proportional to magnitude\cr
	TRUE  \tab TRUE   \tab Direction as constant length arrow}
}

\value{
Plot of vector-spatial data.  No values are returned.
}

\section{Warning}{Excessive jitter can mask directional structure.  The arrow.plot function produces warning\cr
messages about zero-length vectors for non-zero vectors.
}

\references{
Ware, C. (2004), Information Visualization, 2nd ed., Morgan Kauffman, San Francisco,\cr
p. 201-205.
}

\author{Bill Morphet}

\seealso{\code{\link{OceanWind}}}

\examples{
data(OceanWind)
wind.1997.Jan <- OceanWind[OceanWind$year>1997 & OceanWind$year<1997.1,
  -1]

## Direction Only
PlotVectors(x=wind.1997.Jan$x, y=wind.1997.Jan$y, h=wind.1997.Jan$u,
  v=wind.1997.Jan$v, UnitVector=TRUE, AdjArrowLength=0.75,
  AdjHeadLength=0.75, xlim=c(320,350), ylim=c(0,30))

## Direction and Magnitude
PlotVectors(x=wind.1997.Jan$x, y=wind.1997.Jan$y, h=wind.1997.Jan$u,
  v=wind.1997.Jan$v, UnitVector=FALSE, TriIcon=FALSE, AdjArrowLength=3,
  AdjHeadLength=0.4, xlim=c(320,350), ylim=c(0,30))

## Triangle Icons
PlotVectors(x=wind.1997.Jan$x, y=wind.1997.Jan$y, h=wind.1997.Jan$u,
  v=wind.1997.Jan$v, UnitVector=FALSE, TriIcon=TRUE, TriIconAdj=0.25,
  TriRatio=4, xlim=c(320,350), ylim=c(0,30))
}

\keyword{hplot}
\keyword{graphs}
\keyword{spatial}
