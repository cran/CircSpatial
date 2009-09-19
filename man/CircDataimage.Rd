\name{CircDataimage}
\alias{CircDataimage}
\title{Graphical Interface for Interactive Circular Dataimages}

\description{
CircDataimage generates a GUI for interaction with circular dataimages to facilitate the discovery of structure in circular-spatial data.
}

\usage{CircDataimage()
}

\details{
In the installed folder doc, Chapter2.CircularDataimage and Section J.10 in\cr Appendices.J.PackageDocumentation provide additional detail and illustrations.  Morphet and\cr
Symanzik (2009) refines Chapter2.CircularDataimage and adds a usability study.\cr\cr

The circular dataimage is useful for visualization of random, model, and kriged circular-spatial data.  The circular dataimage is constructed by displaying direction as the color in a color wheel at the same angle.  This implementation of color provides for simultaneous recognition of fine circular-spatial structure on a small scale and large-scale structure on a global scale.  In general, the color wheel is composed of a sequence of color gradients with continuity between connecting color gradients such that the color coding the 0 deg direction is the same as the color coding the 360 deg direction.  Thus, image discontinuity from using a single color gradient for visualization of circular-spatial data is eliminated./cr/cr

The dataframe entered into the GUI Input Dataframe entry box contains:\cr
\tabular{lrl}{
\tab x: \tab   Horizontal measurement location coordinate\cr
\tab y: \tab   Vertical measurement location coordinate\cr
\tab u: \tab   Horizontal component of vector of direction and magnitude.\cr
\tab v: \tab   Vertical component of vector of direction and magnitude.
}

The input data are measured on a regular grid.  The vertical and horizontal grid spacings may be different.  Spacings are computed from the data as the second smallest location coordinate - minimum location coordinate.  Multiple observations at the same location will be replaced with the mean resultant = vector resultant/number of observations.  Missing data is permitted, but data.frame rows of (x, y, NA, NA) are not allowed.  u=v=0 OK.\cr\cr

The mask matrix (\code{\link{WorldMask}}) entered into the GUI Mask Matrix entry box is optional.  It must have rows and columns equal to the rows and columns of the measurement grid, respectively.  Mask cell values are NA where pixel is not to be masked and 1 where a pixel is to be masked.\cr\cr
}

\value{
The circular dataimage is displayed in windows device 2 with corresponding circular color wheel in windows device 3.  Arrange the windows suitably when they appear.  The RGUI acts as a status window to display messages.  Arrows (optional) overplot the mask (optional) which overplots the data.  The list object CircDataimageGlobals, which contains the GUI inputs and results of computations, is written into the R workspace.\cr\cr
}

\references{
Morphet, W.J. and Symanzik, J., (2009).  The Circular Dataimage, a Graph for High Resolution Circular-Spatial data.  \emph{International Journal of Digital Earth}, DOI:10.1080/17538940903277657.
}

\author{Bill Morphet}

\seealso{
  \code{\link{TestPattern}}\cr
  \code{\link{OceanWind}}\cr
  \code{\link{CircSpatial-package}}
}

\examples{
# data(OceanWind) 
# data(WorldMask)
# wind.subset <- OceanWind[, -1] # Using all data, ~ 500k records
# CircDataimage()

## 1: The initial display appears with "unknown" in the Input Dataframe
##    entry box and Mask Matrix entry box.

## 2: Enter the name of the example input dataframe wind.subset in the
##    Input Dataframe entry box.  Enter the example optional mask matrix
##    WorldMask in the Mask Matrix entry box.  N.B.: The input dataframe
##    must be input before other GUI controls will function correctly.
##    After data is input, the controls may be operated in any order.

## 3: Check the Obs Per Cell > 1 checkbox because wind.subset has
##    more than one observation per location.  When the observations
##    per cell <= 1, the looping structure is replaced with
##    vector-matrix expression to reduce computation time.

## 4: To accept the input data and optional mask, click the OK button
##    at the right of the mask entry box.  The OK button will remain
##    depressed until processing is complete.  If the Obs Per Cell
##    > 1 checkbox is checked, the message "The computations necessarily
##    may take significant time" is sent to the R GUI.  The computation
##    of average wind direction over the 500k observations of
##    wind.subset is relatively slow.  However, testpattern is imaged
##    instantaneously (Obs Per Cell > 1 checkbox is unchecked).

## 5: The circular dataimage appears in R Graphics: Device 2 and the
##    current color wheel in R Graphics: Device 3.  The Display
##    Coordinates and reference global (extreme) coordinates to the
##    right are updated.  The Smooth Bandwidth is set to 0, and the
##    Arrows and Mask check buttons are unchecked.  Arrange the windows
##    as necessary.

## 6: Select any color wheel scale by clicking the radio button next to
##    the color wheel label and graphic, and the circular dataimage and
##    color wheel (R Graphics: Device 3) are rapidly updated.  Package
##    provides 7 continuous color wheels with 1 for red-green color
##    impaired (ROYBgBPb), 1 HSV, and 2 divergent, and 6 discrete color
##    wheels with 3 divergent.

## 7: Rotation is used to highlight structure in the displayed area of
##    interest.  The selected color wheel may be varied by moving the
##    Color Scale Rotation slider.  As the slider is moved, the label
##    above the slider updates.  When the mouse button (left) is
##    released, the displayed color wheel rotation angle is processed
##    with a positive value resulting in a counter clockwise rotation of
##    the color wheel by value degrees.  The circular dataimage and
##    color wheel are updated, but the data are unchanged. 

## 8: Coordinates are entered to pan and zoom.  Display coordinates
##    reference the center of a pixel.  Enter display coordinates into
##    the Display Coordinates entry box.  The user may enter any value,
##    but entry box coordinates will snap to the coordinates of the
##    nearest datum.  Thus, coordinates outside the range of data are
##    adjusted to the extremes of the data.  A zoomed display may be
##    reduced to as few as 2 x 2 pixels.  2x1, 1x1, and 1x2 pixel
##    displays result in non-matrix input to the image plot function
##    and an image plot error message.  Enter Min X=100 and Max X=275.
##    Click OK to process the entered coordinates.  The displayed
##    coordinates in the entry boxes snap to Min X=99.5 and Max X=274.5
##    because the input dataframe has longitude from 0.5 deg to 359.5
##    deg in 1 deg increments.

## 9: Smoothing is useful to reduce visual noise.  Smoothing applies
##    image.smooth of package fields separately to the horizontal and
##    vertical components of the mean vector resultant to avoid "cross
##    over".  The smoothed direction is obtained by applying the R
##    function atan2 to the smoothed components.  Enter a smooth
##    bandwidth of 2.5 in the Smooth Bandwidth entry box and click OK.
##    The OK button will remain depressed until processing is complete.

## 10: Color scale gap increases the contrast at the boundaries of areas
##     of similar direction (in the same color gradient component of the
##     color wheel) by truncating the upper fraction of the gradient
##     (stretching out the lower fraction or retarding the terminal
##     color).  For example, a 4 color-gradient, e.g., GBYR with Color
##     Scale Gap=1 codes all directions in the range 0 deg to 90 deg as
##     pure green.  Move the slider of Color Scale Gap and the label
##     above the slider updates.  When the mouse button is released, the
##     displayed color wheel gap is processed, and the color wheel and
##     circular dataimage are updated.

## 11: Now select the HSV (Hue, Saturation, and Value) color wheel.

## 12: To add arrows check the Arrows check box.  An arrow is plotted if
##     the value of direction at the arrow location is not NA, or the NA
##     value is replaced by smoothing and the location is not a masked.
##     Arrow Spacing in Pixels is the horizontal and vertical spacing
##     between arrows.  Valid values of Arrow Spacing in Pixels are
##     indicated by the label to the right of the entry box, i.e., 1, 2,
##     3, ... .  The minimum value of Arrow Spacing in Pixels of 1
##     results in the maximum possible arrow density with one arrow per
##     datum.  At some "large" spacing and with missing data, no arrows
##     may be displayed.  If no arrows are displayed, the message "No
##     arrows can be displayed at current spacing" is displayed in the
##     R GUI.  Increase arrow density by entering 5 in the Arrow Spacing
##     in Pixels entry box and click OK.  As arrow density is increased,
##     arrow length can be decreased by decreasing the Arrow Length
##     Multiplier to a value > 0.  Enter 0.5 in the Arrow Length
##     Multiplier entry box and click OK.

## 13: Smoothing replaces missing data (land in the wind.subset).  The
##     function of mask WorldMask is to restore the outlines of land
##     masses.  Pixels corresponding to matrix cells of 1 are
##     overplotted in tan, and pixels corresponding to matrix cells of
##     NA remain unchanged.  If a mask matrix was not entered and the
##     mask checkbox is clicked, nothing happens.
##     Now check the Mask check box.  The land masses should reappear.

## 14: Click the X in the upper right corners to exit.
dev.set(which = 2)
}

\keyword{color}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
\keyword{graphs}