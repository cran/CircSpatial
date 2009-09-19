`PlotVectors` <-
function(x, y, h, v, UnitVector=TRUE, TriIcon=FALSE, AdjArrowLength=1, AdjHeadLength=1, TriIconAdj=1,
	TriRatio=4, JitterPlot=FALSE, Jitter=1, ...)
{
	# 2008-11-11.1535
	# Arrows do not plot where data is missing.
	# require(fields)

	if( (length(x) != length(y)) | (length(h) != length(v)) | (length(x) != length(h)) ) stop("lengths of vector inputs unequal")

	filter <- is.na(h) | is.na(v) | (h==0 & v==0)
	x <- x[!filter]; y <- y[!filter]; h <- h[!filter]; v <- v[!filter]
	# fields function arrows omits arrowheads with a warning on any arrow of length less than 1/1000 inch.

	Dir <- atan2(v, h)
	Dir[Dir<0] <- Dir[Dir<0]+2*pi
	if(JitterPlot==TRUE)
	{
		x <- x + Jitter*runif(length(x))
		y <- y + Jitter*runif(length(y))
	}
	plot(x, y, ty="n", asp=1, ...)

	if(UnitVector)
	{ arrow.plot(x, y, cos(Dir), sin(Dir), true.angle=TRUE, arrow.ex=AdjArrowLength*0.05, length=AdjHeadLength*0.125, angle=20, xpd=FALSE)
	} else
	{
		if(TriIcon)
		{
			m <- sqrt(h^2 + v^2) # magnitude
			w = sqrt(m/TriRatio)
			n <- length(x)
			xa <- x + TriIconAdj*      w*cos(Dir+pi/2)
			ya <- y + TriIconAdj*      w*sin(Dir+pi/2)
			xb <- x + TriIconAdj*TriRatio*w*cos(Dir)
			yb <- y + TriIconAdj*TriRatio*w*sin(Dir)
			xc <- x + TriIconAdj*      w*cos(Dir-pi/2)
			yc <- y + TriIconAdj*      w*sin(Dir-pi/2)

			for(i in 1:n) polygon(x=c(xa[i], xb[i], xc[i]), y=c(ya[i], yb[i], yc[i]), density=-1, col=1)
		} else arrow.plot(x, y, h, v, true.angle=TRUE, arrow.ex=AdjArrowLength*0.05, length=AdjHeadLength*0.125, angle=20, xpd=FALSE)
	}
}
