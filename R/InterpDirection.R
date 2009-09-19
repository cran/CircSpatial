`InterpDirection` <-
function(in.x, in.y, in.direction, out.x, out.y)
{
	# 2008-11-11.1444
	# Interpolate models of direction cosines and sines, separately to avoid cross over.  Fit plane to triangular half of cell
	# (rectangular element of regular grid of measurement locations) in which interpolation location occurs.
	# Assumptions - Locations to interpolate are within range of (in.x, in.y), inputs have no missing.

	# Arguments
	# in.x vector of input horizontal coordinates
	# in.y vector of input vertical   coordinates
	# in.direction vector of input direction in radians

	# Value
	# out.x vector of interpolation output horizontal coordinates
	# out.y vector of interpolation output vertical   coordinates
	# out.direction vector of interpolation output direction

	# Verify input
	minx.in <- min(in.x); maxx.in <- max(in.x); miny.in <- min(in.y); maxy.in <- max(in.y)
	minx.out <- min(out.x); maxx.out <- max(out.x); miny.out <- min(out.y); maxy.out <- max(out.y)
	if(minx.out < minx.in | maxx.out > maxx.in | miny.out < miny.in | maxy.out > maxy.in) stop("Interpolation range exceeds range of (in.x, in.y)")

	if( (length(in.x) != length(in.y)) | (length(in.x) != length(in.direction)) | (length(in.y) != length(in.direction)))
		stop("lengths of vector inputs unequal")

	if( length(out.x) != length(out.y) ) stop("lengths of vector outputs unequal")


	# Organize model data
	X <- sort(unique(in.x), decreasing = FALSE) # Increases left to right
	m <- length(X)
	Y <- sort(unique(in.y), decreasing = TRUE) # Decreases top to bottom
	n <- length(Y)
	# Col of matrix of directions reflects the horiz or west to east component location
	xmin <- min(X); deltax <- X[2] - X[1]; ymax <- max(Y); deltay <- Y[1] - Y[2]
	Col <- 1 + (in.x -xmin)/deltax
	Row <- 1 + (ymax - in.y)/deltay
	directions <- matrix(data = NA, nrow=n, ncol=m)
	directions[cbind(Row, Col)] <- in.direction # matrix of organized directions
	U <- cos(directions) # matrix of organized cosines of directions
	V <- sin(directions) # matrix of organized sines of directions

	n <- length(out.x)

	CosOut <- rep(NA, n) # for interpolated cosine
	SinOut <- CosOut # for interpolated sin

	p <- 1:length(X)
	q <- 1:length(Y)

	for(i in 1:n)
	{
		xx <- out.x[i]
		yy <- out.y[i]
		Vert=FALSE; Horiz=FALSE
		if(sum(X==xx)==1) Vert=TRUE
		if(sum(Y==yy)==1) Horiz=TRUE

		if(Vert==FALSE & Horiz==FALSE)
		{
			west <- max(p[X <= xx])
			east <- west + 1
			south <- min(q[Y <= yy])
			north <- south - 1
			x.west <- X[west]
			x.east <- X[east]
			y.south <- Y[south]
			y.north <- Y[north]
			cos.nw <- U[north,west]
			cos.ne <- U[north,east]
			cos.sw <- U[south,west]
			cos.se <- U[south,east]
			sin.nw <- V[north,west]
			sin.ne <- V[north,east]
			sin.sw <- V[south,west]
			sin.se <- V[south,east]

			m <- (y.north-y.south)/(x.east-x.west) # 1 if vert res=horiz res
			b <- y.north-m*x.east
			ydiag <- m*xx+ b
			if(yy <= ydiag) # On diagonal or in lower triangular
			{
				# Fit plane to lower triangular
				AB <- c(x.east-x.west, 0, cos.se-cos.sw)
				AC <- c(x.east-x.west, y.north-y.south, cos.ne-cos.sw)
				# Coefficients of cross product AB X AC
				a <- AB[2]*AC[3]-AB[3]*AC[2]
				b <- AB[3]*AC[1]-AB[1]*AC[3]
				c <- AB[1]*AC[2]-AB[2]*AC[1]
				CosOut[i] <- cos.sw + (a*(x.west-xx) + b*(y.south-yy))/c
				# Fit plane to lower triangular
				AB <- c(x.east-x.west, 0, sin.se-sin.sw)
				AC <- c(x.east-x.west, y.north-y.south, sin.ne-sin.sw)
				# Coefficients of cross product AB X AC
				a <- AB[2]*AC[3]-AB[3]*AC[2]
				b <- AB[3]*AC[1]-AB[1]*AC[3]
				c <- AB[1]*AC[2]-AB[2]*AC[1]
				SinOut[i] <- sin.sw + (a*(x.west-xx) + b*(y.south-yy))/c
			}
			else
			{
				# In upper triangular
				AC <- c(x.east-x.west, y.north-y.south, cos.ne-cos.sw)
				AD <- c(0,y.north-y.south, cos.nw-cos.sw)
				a <- AC[2]*AD[3]-AC[3]*AD[2]
				b <- AC[3]*AD[1]-AC[1]*AD[3]
				c <- AC[1]*AD[2]-AC[2]*AD[1]
				CosOut[i] <- cos.sw + (a*(x.west-xx) + b*(y.south-yy))/c
				AC <- c(x.east-x.west, y.north-y.south, sin.ne-sin.sw)
				AD <- c(0,y.north-y.south, sin.nw-sin.sw)
				a <- AC[2]*AD[3]-AC[3]*AD[2]
				b <- AC[3]*AD[1]-AC[1]*AD[3]
				c <- AC[1]*AD[2]-AC[2]*AD[1]
				SinOut[i] <- sin.sw + (a*(x.west-xx) + b*(y.south-yy))/c
			}
		}
		else if(Vert==TRUE & Horiz==FALSE)
		{
			p1 <- p[X==xx] # Column of vert grid line
			q1 <- min(q[Y < yy]) # Even spacing is not assumed
			q2 <- q1 - 1
			cos1 <- U[q1, p1]
			cos2 <- U[q2, p1]
			CosOut[i] <- cos1 + (cos2-cos1)*(yy-Y[q1])/(Y[q2]-Y[q1])
			sin1 <- V[q1, p1]
			sin2 <- V[q2, p1]
			SinOut[i] <- sin1 + (sin2- sin1)*(yy-Y[q1])/(Y[q2]-Y[q1])
		}
		else if(Vert==FALSE & Horiz==TRUE)
		{
			q1 <- q[Y==yy] # Row of horiz grid line
			p1 <- max(p[X < xx])
			p2 <- p1 + 1
			cos1 <- U[q1, p1]
			cos2 <- U[q1, p2]
			CosOut[i] <- cos1 + (cos2-cos1)*(xx-X[p1])/(X[p2]-X[p1])
			sin1 <- V[q1, p1]
			sin2 <- V[q1, p2]
			SinOut[i] <- sin1 + (sin2-sin1)*(xx-X[p1])/(X[p2]-X[p1])
		}
		else # Vert==TRUE & Horiz==TRUE
		{
			CosOut[i] <- U[q[Y==yy], p[X==xx]]
			SinOut[i] <- V[q[Y==yy], p[X==xx]]
		}
	}
	dir <- atan2(SinOut, CosOut)
	return(list(x=out.x, y=out.y, direction=dir))
} 
