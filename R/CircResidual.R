`CircResidual` <-
function(X, Y, Raw, Trend, Plot = FALSE, AdjArrowLength = 1, ...)
{
	# 2008-11-10.2053
	# Assumptions: Raw may have NAs, trend has no NAs.  Trend locations and Raw locations are identical to compute residuals.
	# require(fields)
	if((length(X) != length(Y)) | (length(X) != length(Raw)) | (length(X) != length(Trend)) | (length(Y) != length(Raw)) |
		(length(Y) != length(Trend)) | (length(Raw) != length(Trend))) stop("lengths of vector inputs unequal")
	if(AdjArrowLength <= 0) stop("AdjArrowLength invalid")
	if(sum(is.na(Trend)) > 0) stop("NAs not allowed in Trend")

	FilterNA <- is.na(Raw)
	x <- X[!FilterNA]; y <- Y[!FilterNA]; raw <- Raw[!FilterNA]; trend <- Trend[!FilterNA]
	raw[raw < 0] <- raw[raw < 0] + 2*pi # Like R1.Standardize in CircDataimage
	trend[trend < 0] <- trend[trend < 0] + 2*pi
	circdist <- abs(raw - trend) # Linear distance in radians with NAs where raw has NAs
	circdist[circdist > pi] <- 2*pi - circdist[circdist > pi] # Circular distance in radians
	resids <- circdist
	filter <- (trend>raw) & (trend-raw)<pi | (raw >trend) & (raw-trend)>pi
	resids[filter] <- -1* circdist[filter]
	if(Plot==TRUE)
	{
		plot(X, Y, type="n", xlab="", ylab="", asp=1, ...)
		arrow.plot(x, y, u=cos(raw), v=sin(raw), xpd=FALSE, true.angle=TRUE, arrow.ex=.15*AdjArrowLength, length=.1,col=1)
		arrow.plot(X, Y, u=cos(Trend), v=sin(Trend), xpd=FALSE, true.angle=TRUE, arrow.ex=.15*AdjArrowLength, length=.1,col="tan", lwd=3)
		arrow.plot(x, y, u=cos(resids), v=sin(resids), xpd=FALSE, true.angle=TRUE, arrow.ex=.15*AdjArrowLength, length=.1,col=2, lty=2)
	} else return(list(x=x, y=y, direction=resids))
}
