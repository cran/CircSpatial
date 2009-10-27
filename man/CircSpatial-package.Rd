\name{CircSpatial-package}
\alias{CircSpatial-package}
\docType{package}
\title{Simulation, Kriging, and Imaging of Circular-Spatial Data}

\description{Simulation of circular random fields (a 2D space of circular random variables of a circular distribution with spatial correlation) is implemented for 5 circular distributions. Circular-spatial kriging functions include residual computation (observed direction minus mean direction), empirical and fitted spatial correlation models, kriging, spatial-trend model interpolation, and smoothing.  A GUI is implemented for interactive display of circular-spatial data.
}

\references{
Morphet, W.J., 2009.  \emph{Simulation, Kriging, and Visualization of Circular-Spatial Data}.  Dissertation, Utah State University, Logan, Utah.\cr\cr
Morphet, W.J. and Symanzik, J., (2009).  The Circular Dataimage, a Graph for High Resolution Circular-Spatial data.  \emph{International Journal of Digital Earth}, DOI:10.1080/17538940903277657.\cr
} 

\details{
\tabular{ll}{
Package: \tab CircSpatial\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-09-19\cr
License: \tab GPL-2 or GPL-3\cr
}

Overview:\cr
This package is part of the disertation Morphet (2009).  Chapters and appendices are located in the doc folder of the installed package.  The following references to chapters and appendices are to the chapters and appendices of Morphet (2009).  For a flowchart of the processes of the CircSpatial package, see Chapter1.Introduction, Section 1.5 and Figure 1-7.  A running example is given in Chapter6.ComprehensiveExample.  Appendices.J.PackageDocumentation is an illustrated user's guide to the CircSpatial package.  Appendices.K.to.L contains the package function source code and the code used to generate the figures of Morphet (2009).\cr\cr

Circular random variables (CRV), vectors, axes, and periodic phenomena (e.g., traffic concentration vs. time) take a direction on a circular scale of 360 deg (2 pi radians, or a period in time units).  A CRV takes random directions with the total probability of all possible directions distributed on the unit circle with support [0, 2pi) or [-pi, pi).  The main feature of circular data (as opposed to linear data) is its cyclic nature, i.e., the starting point at 0 deg (0 hours) equals the ending point at 360 deg (24 hours).  A CRV is expressed as either a scaler in units of radians or degrees, or as a unit vector in units of the space with horizontal and vertical direction components.\cr\cr

A circular random field (CRF) is a 2D space containing CRV with spatial correlation depending on distance and orientation between sample locations.  Spatial correlation means that as the distance between measurement locations decreases, random deviations of direction from the mean direction are more similar.  In the form required by the circular kriging function of the CircSpatial package, spatial correlation is defined as the mean cosine of the angle between random components of directions (non-random component or mean direction removed) vs. distance between measurement locations.  The mean cosine tends toward one as distance between sample locations decreases.  The mean cosine at any distance where CRV are uncorrelated equals the square of the mean resultant length parameter of the circular distribution (Chapter3.Cosineogram).  The CircSpatial package implements a new method for generating an isotropic CRF (spatial correlation depends on distance, and is independent of orientation of sample locations) or anisotropic CRF.  The CRF is computed by transforming a gaussian linear random field via the Gaussian cumulative probability function and the inverse cumulative probability function of the desired circular distribution.  Circular random fields are implemented for the uniform, von Mises, cardioid, triangular, and wrapped Cauchy distributions with any valid mean resultant length parameter.  The CRF simulation is detailed Chapter5.CircularRandomFieldSimulation.\cr\cr

Circular-spatial data consists of direction and location, and have the spatial correlation of the underlying CRF.  Circular-spatial statistics treats both observed direction and location of the observed direction.  The empirical spatial correlation is graphed as the cosineogram (Chapter3.Cosineogram).  The cosineogram is the mean cosine of the angle between random components of direction depending on distance between sample locations.  To add detail to circular-spatial data of an isotropic circular random field, or to estimate missing data, a circular kriging solution is implemented which estimates the second order or residual variation (Chapter4.CircularKriging).  Circular kriging uses the best fit postive definite cosineogram model, which is analogous to the covariogram model for linear kriging (Chapter3.Cosineogram and Appendix M of Appendices.M.to.N).  At an observed location, the \dQuote{interpolation is exact}, i.e., the estimated direction is the observed direction.  The circular kriging estimate of the residual direction is combined with an interpolation of the fitted trend model.  The interpolation is computed as the elevation on a plane fitted to the cosines and sines, respectively, of the three model locations bounding the estimation location.\cr\cr

The circular dataimage (Chapter2.CircularDataimage) provides a high resolution color-continuous display of circular-spatial data or circular kriging.  Color continuous means that there is no color discontinuity relative to direction.  The circular dataimage is constructed by displaying direction as color from a color wheel in the same direction.  The color wheel is constructed from a sequence of color gradients with continuity between connecting color gradients at the connections.  For example, the 0 deg direction has the same color as the color of the 360 deg direction.  \dQuote{Cross over}, which occurred in automating the summarization of historical wind data, and image discontinuity, which results from using a single color gradient for visualization of directional-spatial data, are eliminated.  Circular dataimages provide for simultaneous recognition of fine structure on a small scale and large-scale structure and trends on a global scale over a broad range of spatial scales and in the presence of noise.  The circular dataimage is refined and expanded in Morphet and Symanzik (2009).  The CircDataimage function creates a GUI for image manipulation.  The GUI requires Active State TCL to be installed.  See Help for CircDataimage for details.\cr\cr

Main Functions:\cr
\code{\link{CircDataimage}}: Generate a GUI for interaction with a circular dataimage.\cr
\code{\link{SimulateCRF}}: Compute circular random fields from 5 circular distributions.\cr
\code{\link{PlotVectors}}: Standard plots of vector-spatial data.\cr
\code{\link{CircResidual}}: Compute rotations (deviations, residuals) from the mean direction.\cr
\code{\link{CosinePlots}}: Plot the empirical cosineogram expression of spatial correlation of circular-spatial data.\cr
\code{\link{KrigCRF}}: Krig the circular-spatial residuals using the fitted cosineogram model.\cr
\code{\link{InterpDirection}}: Interpolate the model of mean direction at each kriging location.\cr\cr
}

\author{Bill Morphet\cr
Maintainer: Bill Morphet <morphwj@comcast.net>
}

\keyword{package}
