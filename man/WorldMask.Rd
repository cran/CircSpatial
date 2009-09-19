\name{WorldMask}
\alias{WorldMask}
\docType{data}
\title{World Land Mask}

\description{
Restores land contours to the circular dataimage of smoothed OceanWind.
}

\usage{data(WorldMask)}

\format{
360 x 121 matrix suited to \var{\link{OceanWind}} and consisting of NA where wind data is not missing and 1 where data is missing.
}

\details{
Section J.9 in Appendices.J.PackageDocumentation in the installed folder doc provides additional detail and illustrations.\cr\cr

Smoothing circular-spatial data creates values where data is missing.  \code{\link{CircDataimage}} overplots \var{WorldMask} (or other suitable mask matrix) entered in \strong{Mask Matrix} entry of the GUI on top of the circular dataimage.
}

\source{
Derived from R package fields dataset world.dat.}

\seealso{
  \var{\link{OceanWind}}\cr
  \code{\link{CircDataimage}}
}

\examples{
data(WorldMask)
image(WorldMask)
}

\keyword{datasets}
