\name{OceanWind}
\alias{OceanWind}
\docType{data}
\title{Ocean Wind Data}

\description{
Ocean wind data from ICOADS for the El Nino years 1972, 1976, 1982, 1987, 1991, 1994, and 1997, January through April, in 1 deg increments for the area of longitude 0.5 deg E to +359.5 deg E by latitude -59.5 deg N to +60.5 deg N.  Time of observation = year + month/12. East and north components of wind velocity are in units of 0.01 meters/second.
}

\usage{data(OceanWind)}

\format{
	A data frame with 495,688 observations on the following 5 variables.
	\describe{
	\item{\code{year}:}{a numeric vector of time of observation}
	\item{\code{x}:}{a numeric vector of longitude}
	\item{\code{y}:}{a numeric vector of latitude}
	\item{\code{u}:}{a numeric vector of east component of wind}
	\item{\code{v}:}{a numeric vector of north component of wind}
	}
}

\details{
Section J.8 in Appendices.J.PackageDocumentation in the installed folder doc provides additional detail and illustrations.\cr\cr

Subset the data and remove the column "year" prior to imaging by \code{\link{CircDataimage}}.
}

\source{\url{http://dss.ucar.edu/pub/coads/forms/msg/msga.form.html}
}

\seealso{
	\var{\link{WorldMask}}\cr
	\code{\link{CircDataimage}}\cr
	\code{\link{PlotVectors}}
}

\examples{
data(OceanWind)

## Remove year column
wind.subset <- OceanWind[, -1]
}

\keyword{datasets}
