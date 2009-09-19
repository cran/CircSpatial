\name{TestPattern}
\alias{TestPattern}
\title{Make Test Pattern}

\description{
Make a simple test pattern to explore the function \code{\link{CircDataimage}} for interactive imaging of circular-spatial data.
}

\usage{
TestPattern()
}

\details{
Section J.7 in Appendices.J.PackageDocumentation in the installed folder doc provides\cr
additional detail and illustrations.\cr\cr

Direction at location (x,y) = atan2(y, x).
}

\value{
  A data.frame of vectors x, y, u, v is returned.\cr
  x and y are location coordinates.\cr
  u and v are the horizontal and vertical components, respectively, of direction as a unit vector.
}

\author{Bill Morphet}

\seealso{
  \code{\link{OceanWind}}\cr
  \code{\link{WorldMask}}\cr
  \code{\link{CircDataimage}}
}

\examples{
## Make Test Pattern
testpattern <- TestPattern()
}

\keyword{datagen}
