`CircDataimage` <-
function()
{
	# 2008-11-12.1919

	# require(tcltk, quietly=TRUE, warn.conflicts=TRUE)
	# require(fields, quietly=TRUE, warn.conflicts=TRUE)

	# Sys.setenv("TCL_LIBRARY"="C:/Tcl/lib/tcl8.5")
	# Sys.setenv("MY_TCLTK"="Yes")
	# addTclPath(path = "C:/Tcl/lib/teapot/package/win32-ix86/lib")
	# tclRequire("img::jpeg")


	# Make color wheel data
	x <- seq(-1,1,length=201) # x must be consistent with next image statement
	y <- x
	x2 <- rep(x, 201)
	y2 <- rep(y, ea=201)
	dir <- atan2(y2,x2)
	dir[dir<0] <- dir[dir<0] + 2*pi # Directions in [0, 2*pi)
	Dist <- sqrt(x2^2 + y2^2) # distance from origin
	filter <- Dist > 1
	dir[filter] <- NA
	wheel <- matrix(data=dir, nrow=201, ncol=201, byrow=FALSE)

	# Make FirstColorVector
	Angles1 <- 0:89
	Angles2 <- 90:179
	Angles3 <- 180:269
	Angles4 <- 270:359
	Dist1 = 255*Angles1/90
	Dist2 = 255*(Angles2-90)/90
	Dist3 = 255*(Angles3-180)/90
	Dist4 = 255*(Angles4-270)/90
	Q1 <- rgb(0, 255-Dist1, Dist1, maxColorValue=255)
	Q2 <- rgb(Dist2, Dist2, 255-Dist2, maxColorValue=255)
	Q3 <- rgb(255, 255-Dist3, 0, maxColorValue=255)
	Q4 <- rgb(255-Dist4, Dist4, 0, maxColorValue=255)
	FirstColorVector <- c(Q1,Q2,Q3,Q4) # GBYR

	if(is.null(dev.list())) dev.image=2 else dev.image= max(dev.list()) + 1
	dev.wheel = dev.image + 1
 	windows() # device dev.image
	windows(width = 1.15, height = 1, pointsize = 7) # device 3, width so menu bar on one row
	# Current device is dev.wheel
	par(plt=c(0.03,0.97,0.03,0.97)) # Applies to current device, min margin between labels and window
	angles=seq(0, 315, by=45)
	plot(x=1.2*cos(angles*pi/180), y=1.2*sin(angles*pi/180), type="n", asp=1, xaxt="n", yaxt="n", xlab="", ylab="",
		bty="n")
	text(x=1.2*cos(angles*pi/180), y=1.2*sin(angles*pi/180), labels=as.character(angles))
	image(x, y, z= wheel, col= FirstColorVector, add=TRUE)
	CircDataimageGlobals <<- list()
###########################################################################################################
R1.Prime <- function()
{
	#2007-08-20.1347

	# The following global variables are not dependent on data

	R1.WriteBinColorVectors() # 360 elements for each color vector

	CircDataimageGlobals$ColorGap <<- 0
	R1.WriteContColorVectors() # Must come after CircDataimageGlobals$ColorGap
	
	CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$GBYR # Must come after R1.WriteContColorVectors
	CircDataimageGlobals$ColorVector   <<- CircDataimageGlobals$GBYR
	CircDataimageGlobals$ColorVectorID <<- 1
	CircDataimageGlobals$ColorRotation <<- 0

	CircDataimageGlobals$PlotArrows <<- FALSE
	CircDataimageGlobals$ArrowAdj <<- 1
	CircDataimageGlobals$cpa <<- 15

	CircDataimageGlobals$Mask <<- NULL
	CircDataimageGlobals$PlotMask <<- FALSE
}
###########################################################################################################
R1.WriteBinColorVectors <- function()
{
	# 2007-08-04.1103

	# Each vector has 360 elements for each of 360 degrees

	Q1 <-  rep(rgb(  0, 235,  35, maxColorValue=255), 18)
	Q2 <-  rep(rgb(  0, 245,   0, maxColorValue=255), 18)
	Q3 <-  rep(rgb(102, 250,   0, maxColorValue=255), 18)
	Q4 <-  rep(rgb(153, 255,   0, maxColorValue=255), 18)
	Q5 <-  rep(rgb(204, 255,   0, maxColorValue=255), 18)
	Q6 <-  rep(rgb(255, 255,   0, maxColorValue=255), 18)
	Q7 <-  rep(rgb(255, 204,   0, maxColorValue=255), 18)
	Q8 <-  rep(rgb(255, 153,   0, maxColorValue=255), 18)
	Q9 <-  rep(rgb(255, 102,   0, maxColorValue=255), 18)
	Q10 <- rep(rgb(255,   0,   0, maxColorValue=255), 18)
	Q11 <- rep(rgb(230,   0,  20, maxColorValue=255), 18)
	Q12 <- rep(rgb(195,   0,  51, maxColorValue=255), 18)
	Q13 <- rep(rgb(153,   0, 102, maxColorValue=255), 18)
	Q14 <- rep(rgb(102,   0, 153, maxColorValue=255), 18)
	Q15 <- rep(rgb(  0,   0, 150, maxColorValue=255), 18)
	Q16 <- rep(rgb( 51,   0, 175, maxColorValue=255), 18)
	Q17 <- rep(rgb(  0,  51, 204, maxColorValue=255), 18)
	Q18 <- rep(rgb(  0, 102, 153, maxColorValue=255), 18)
	Q19 <- rep(rgb(  0, 153, 102, maxColorValue=255), 18)
	Q20 <- rep(rgb(  0, 204,  51, maxColorValue=255), 18)
	CircDataimageGlobals$Rainbow.20Bin <<- c(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12, Q13, Q14, Q15, Q16, Q17, Q18, Q19, Q20)	

	Q1 <-  rep(rgb(  0,   0, 128, maxColorValue=255), 30)
	Q2 <-  rep(rgb(  0,   0, 192, maxColorValue=255), 30)
	Q3 <-  rep(rgb(  0,   0, 255, maxColorValue=255), 30)
	Q4 <-  rep(rgb(128, 128, 255, maxColorValue=255), 30)
	Q5 <-  rep(rgb(192, 192, 255, maxColorValue=255), 30)
	Q6 <-  rep(rgb(255, 255, 255, maxColorValue=255), 30)
	Q7 <-  rep(rgb(255, 219, 219, maxColorValue=255), 30)
	Q8 <-  rep(rgb(255, 128, 128, maxColorValue=255), 30)
	Q9 <-  rep(rgb(255,   0,   0, maxColorValue=255), 30)
	Q10 <- rep(rgb(192,   0,  32, maxColorValue=255), 30)
	Q11 <- rep(rgb(130,   0,   0, maxColorValue=255), 30)
	Q12 <- rep(rgb(  0,   0,   0, maxColorValue=255), 30)
	CircDataimageGlobals$KBWR.12Bin <<- c(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12)

	Q1 <-  rep(rgb(103,   0,  31, maxColorValue=255), 36)
	Q2 <-  rep(rgb(178,  24,  43, maxColorValue=255), 36)
	Q3 <-  rep(rgb(214,  96,  77, maxColorValue=255), 36)
	Q4 <-  rep(rgb(244, 165, 130, maxColorValue=255), 36)
	Q5 <-  rep(rgb(253, 219, 199, maxColorValue=255), 36)
	Q6 <-  rep(rgb(224, 224, 224, maxColorValue=255), 36)
	Q7 <-  rep(rgb(186, 186, 186, maxColorValue=255), 36)
	Q8 <-  rep(rgb(135, 135, 135, maxColorValue=255), 36)
	Q9 <-  rep(rgb( 77,  77,  77, maxColorValue=255), 36)
	Q10 <- rep(rgb( 64,  13,  28, maxColorValue=255), 36)	
	CircDataimageGlobals$Brewer10Div6.10Bin <<- c(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10)

	Q1 <- rep(rgb(255,     255,      0,       maxColorValue=255),23) # yellow
	Q2 <- rep(rgb(255,     255*0.65, 0,       maxColorValue=255),45) # orange
	Q3 <- rep(rgb(255,     0,        0,       maxColorValue=255),45) # red
	Q4 <- rep(rgb(255*0.75, 0,       0,       maxColorValue=255),45) # dark red
	Q5 <- rep(rgb(0,       255,      0,       maxColorValue=255),45) # green
	Q6 <- rep(rgb(0,       255*0.6,  0,       maxColorValue=255),45) # dark green
	Q7 <- rep(rgb(0,       255*.75,  255,     maxColorValue=255),45) # blue
	Q8 <- rep(rgb(0,       0,        255,     maxColorValue=255),45) # dark blue
	Q9 <- rep(rgb(255,     255,      0,       maxColorValue=255),22) # yellow
	CircDataimageGlobals$YRGB.8Bin <<- c(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9)

	Q1 <-  rep(rgb(140,  81,  10, maxColorValue=255), 45)
	Q2 <-  rep(rgb(191, 129,  45, maxColorValue=255), 45)
	Q3 <-  rep(rgb(223, 194, 125, maxColorValue=255), 45)
	Q4 <-  rep(rgb(246, 232, 195, maxColorValue=255), 45)
	Q5 <-  rep(rgb(199, 234, 229, maxColorValue=255), 45)
	Q6 <-  rep(rgb(128, 205, 193, maxColorValue=255), 45)
	Q7 <-  rep(rgb( 53, 151, 143, maxColorValue=255), 45)
	Q8 <-  rep(rgb(  1, 102,  94, maxColorValue=255), 45)
	CircDataimageGlobals$Brewer8Div2.8Bin <<- c(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8)

	Q1 <-  rep(rgb(255, 0,   0, maxColorValue=255), 60) # red
	Q2 <-  rep(rgb(255, 0,   255, maxColorValue=255), 60) # magenta
	Q3 <-  rep(rgb(0,   0,   255, maxColorValue=255), 60) # blue
	Q4 <-  rep(rgb(0,   255, 0, maxColorValue=255), 60) # green
	Q5 <-  rep(rgb(255, 255, 0, maxColorValue=255), 60) # yellow
	Q6 <-  rep(rgb(255, 165, 0, maxColorValue=255), 60) # orange
	CircDataimageGlobals$RMBGYO.6Bin <<- c(Q1, Q2, Q3, Q4, Q5, Q6)
}
########################################################################################################
R1.WriteContColorVectors <- function()
{
	# 2007-09-11.1943

	# Each vector has 360 elements for each of 360 degrees
	
	gap <- CircDataimageGlobals$ColorGap

	Angles1 <- 0:89
	Angles2 <- 90:179
	Angles3 <- 180:269
	Angles4 <- 270:359

	Dist1 = (1-gap)*255*Angles1/90
	Dist2 = (1-gap)*255*(Angles2-90)/90
	Dist3 = (1-gap)*255*(Angles3-180)/90
	Dist4 = (1-gap)*255*(Angles4-270)/90

	Q1 <- rgb(0, 255-Dist1, Dist1, maxColorValue=255)
	Q2 <- rgb(Dist2, Dist2, 255-Dist2, maxColorValue=255)
	Q3 <- rgb(255, 255-Dist3, 0, maxColorValue=255)
	Q4 <- rgb(255-Dist4, Dist4, 0, maxColorValue=255)
	CircDataimageGlobals$GBYR <<- c(Q1,Q2,Q3,Q4)

	Q1 <- rgb(Dist1, 255, 0, maxColorValue=255)
	Q2 <- rgb(255, 255-Dist2, 0, maxColorValue=255)		
	Q3 <- rgb(255-Dist3, 0, Dist3, maxColorValue=255)
	Q4 <- rgb(0, Dist4, 255-Dist4, maxColorValue=255)
	CircDataimageGlobals$GYRB <<- c(Q1,Q2,Q3,Q4)

	Q1 <- rgb(Dist1, 255, 0, maxColorValue=255)
	Q2 <- rgb(255-Dist2, 255-Dist2, Dist2, maxColorValue=255)
	Q3 <- rgb(Dist3, 0, 255-Dist3, maxColorValue=255)	
	Q4 <- rgb(255-Dist4, Dist4, 0, maxColorValue=255)
	CircDataimageGlobals$GYBR <<- c(Q1,Q2,Q3,Q4)

	Q1 <- rgb(0, 0, Dist1, maxColorValue=255)
	Q2 <- rgb(Dist2, Dist2, 255, maxColorValue=255)
	Q3 <- rgb(255, 255-Dist3, 255-Dist3, maxColorValue=255)
	Q4 <- rgb(255-Dist4, 0, 0, maxColorValue=255)
	CircDataimageGlobals$KBWR <<- c(Q1,Q2,Q3,Q4)

	Dist1 = (1-gap)*Angles1/360
	Dist2 = 0.25 + (1-gap)*0.25*(Angles2 -90)/90
	Dist3 = 0.50 + (1-gap)*0.25*(Angles3-180)/90
	Dist4 = 0.75 + (1-gap)*0.25*(Angles4-270)/90
	Q1 <- hsv(h=Dist1, s=0.5, v=1)
	Q2 <- hsv(h=Dist2, s=0.5, v=1)
	Q3 <- hsv(h=Dist3, s=0.5, v=1)
	Q4 <- hsv(h=Dist4, s=0.5, v=1)
	CircDataimageGlobals$HSV <<- c(Q1,Q2,Q3,Q4)

	Angles1 <- 0:59
	Angles2 <- 60:119
	Angles3 <- 120:179
	Angles4 <- 180:239
	Angles5 <- 240:299
	Angles6 <- 300:359
	Dist1 = (1-gap)*255*Angles1/60
	Dist2 = (1-gap)*255*(Angles2-60)/60
	Dist3 = (1-gap)*255*(Angles3-120)/60
	Dist4 = (1-gap)*255*(Angles4-180)/60
	Dist5 = (1-gap)*255*(Angles5-240)/60
	Dist6 = (1-gap)*255*(Angles6-300)/60
	Q1 <- rgb(0, 0, Dist1, maxColorValue=255)
	Q2 <- rgb(0, Dist2, 255, maxColorValue=255)
	Q3 <- rgb(Dist3, 255, 255, maxColorValue=255)
	Q4 <- rgb(255, 255, 255-Dist4, maxColorValue=255)
	Q5 <- rgb(255, 255-Dist5, 0, maxColorValue=255)
	Q6 <- rgb(255-Dist6, 0, 0, maxColorValue=255)
	CircDataimageGlobals$KBCWYR <<- c(Q1, Q2, Q3, Q4, Q5, Q6)

	Angles1 <- 0:59
	Angles2 <- 60:119
	Angles3 <- 120:179
	Angles4 <- 180:239
	Angles5 <- 240:299
	Angles6 <- 300:359
	Dist1 = (1-gap)*(Angles1-  0)/60
	Dist2 = (1-gap)*(Angles2- 60)/60
	Dist3 = (1-gap)*(Angles3-120)/60
	Dist4 = (1-gap)*(Angles4-180)/60
	Dist5 = (1-gap)*(Angles5-240)/60
	Dist6 = (1-gap)*(Angles6-300)/60
	Q1 <- rgb(          255,           165*Dist1,             0, maxColorValue=255)
	Q2 <- rgb(          255, 165+(255-165)*Dist2,             0, maxColorValue=255)
	Q3 <- rgb(255*(1-Dist3),                 255,     255*Dist3, maxColorValue=255)
	Q4 <- rgb(            0,       255*(1-Dist4),           255, maxColorValue=255)
	Q5 <- rgb(    255*Dist5,                   0,           255, maxColorValue=255)
	Q6 <- rgb(          255,                   0, 255*(1-Dist6), maxColorValue=255)
	CircDataimageGlobals$ROYBgBPb <<- c(Q1, Q2, Q3, Q4, Q5, Q6)

	Angles1 <- 0:35
	Angles2 <- 36:71
	Angles3 <- 72:107
	Angles4 <- 108:143
	Angles5 <- 144:179
	Angles6 <- 180:215
	Angles7 <- 216:251
	Angles8 <- 252:287
	Angles9 <- 288:323
	Angles10 <- 324:359
	Dist1 = (1-gap)*Angles1/36
	Dist2 = (1-gap)*(Angles2-36)/36
	Dist3 = (1-gap)*(Angles3-72)/36
	Dist4 = (1-gap)*(Angles4-108)/36
	Dist5 = (1-gap)*(Angles5-144)/36
	Dist6 = (1-gap)*(Angles6-180)/36
	Dist7 = (1-gap)*(Angles7-216)/36
	Dist8 = (1-gap)*(Angles8-252)/36
	Dist9 = (1-gap)*(Angles9-288)/36
	Dist10 = (1-gap)*(Angles10-324)/36
	Q1 <- rgb(103+(178-103)*Dist1,   0   +(24-0)*Dist1,    31+(43-31)*Dist1, maxColorValue=255)
	Q2 <- rgb(178+(214-178)*Dist2,  24  +(96-24)*Dist2,    43+(77-43)*Dist2, maxColorValue=255)
	Q3 <- rgb(214+(244-214)*Dist3,  96+(165- 96)*Dist3,   77+(130-77)*Dist3, maxColorValue=255)
	Q4 <- rgb(244+(253-244)*Dist4, 165+(219-165)*Dist4, 130+(199-130)*Dist4, maxColorValue=255)
	Q5 <- rgb(253+(224-253)*Dist5, 219+(224-219)*Dist5, 199+(224-199)*Dist5, maxColorValue=255)
	Q6 <- rgb(224+(186-224)*Dist6, 224+(186-224)*Dist6, 224+(186-224)*Dist6, maxColorValue=255)	
	Q7 <- rgb(186+(135-186)*Dist7, 186+(135-186)*Dist7, 186+(135-186)*Dist7, maxColorValue=255)
	Q8 <- rgb(135 +(77-135)*Dist8, 135 +(77-135)*Dist8,  135+(77-135)*Dist8, maxColorValue=255)
	Q9 <- rgb( 77  +(64-77)*Dist9,  77  +(13-77)*Dist9,   77 +(28-77)*Dist9, maxColorValue=255)
	Q10 <- rgb(64+(103-64)*Dist10,  13  +(0-13)*Dist10,   28+(31-28)*Dist10, maxColorValue=255)
	CircDataimageGlobals$Brewer10Div6 <<- c(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10)
}
###########################################################################################################
R1.Initialize <- function(data.name2, mask.name2, nObs.cb.value2)
{
	# 2007-09-12.1930

	# Variable name suffix ".g" indicates variable is global, i.e. at limits of data
	# Variable name suffix ".d" indicates variable has been subset for display
	# Coordinates and direction will apply at center of pixel

	InputData <- as.matrix(eval(parse(file="", text=as.character(tclvalue(data.name2))))) # Must be matrix for loop below

	mask <- as.character(tclvalue(mask.name2))
	if(mask == "unknown" | mask == "") CircDataimageGlobals$Mask <<- NULL else CircDataimageGlobals$Mask <<- as.matrix(eval(parse(file="", text=mask)))
	
	CircDataimageGlobals$Data <<- InputData
	x <- sort(unique(InputData[,1])) # Ascending unique horizontal coordinates of sampling locations.
	y <- sort(unique(InputData[,2])) # Ascending unique vertical coordinates of sampling locations.
	CircDataimageGlobals$MinX.g <<- min(x) # Global minimum X.
	CircDataimageGlobals$MaxX.g <<- max(x) # Global maximum X.
	CircDataimageGlobals$MinY.g <<- min(y) # Global minimum Y.
	CircDataimageGlobals$MaxY.g <<- max(y) # Global maximum Y.
	# Measurement location horizontal spacing assumed to be constant in X.
	# Grid vertical spacing assumed to be constant in Y.  Horiz and vert spacing do not have to be equal.
	CircDataimageGlobals$DX <<- x[2] - x[1] # Horizontal spacing of sampling grid.
	CircDataimageGlobals$DY <<- y[2] - y[1] # Vertical spacing of sampling grid.
	# Simple check
	DX2 <- x[3] - x[2]
	DY2 <- y[3] - y[2]
	if(DX2 != CircDataimageGlobals$DX | DY2 != CircDataimageGlobals$DY) stop("Measurement spacing not constant")

	CircDataimageGlobals$nx.g <<- round((CircDataimageGlobals$MaxX.g-CircDataimageGlobals$MinX.g)/CircDataimageGlobals$DX + 1, digits = 0)
	CircDataimageGlobals$ny.g <<- round((CircDataimageGlobals$MaxY.g-CircDataimageGlobals$MinY.g)/CircDataimageGlobals$DY + 1, digits = 0)
	CircDataimageGlobals$x.g <<- seq(from=CircDataimageGlobals$MinX.g, to=CircDataimageGlobals$MaxX.g,length=CircDataimageGlobals$nx.g)
	CircDataimageGlobals$y.g <<- seq(from=CircDataimageGlobals$MinY.g, to=CircDataimageGlobals$MaxY.g,length=CircDataimageGlobals$ny.g)
	
	# for display if Pan() not invoked
	CircDataimageGlobals$MinX.d <<- CircDataimageGlobals$MinX.g
	CircDataimageGlobals$MaxX.d <<- CircDataimageGlobals$MaxX.g
	CircDataimageGlobals$MinY.d <<- CircDataimageGlobals$MinY.g
	CircDataimageGlobals$MaxY.d <<- CircDataimageGlobals$MaxY.g
	# The number of rows of the matrix will be = CircDataimageGlobals$nx.g = length(CircDataimageGlobals$x.g)
	CircDataimageGlobals$StartRow <<- 1
	CircDataimageGlobals$EndRow <<- CircDataimageGlobals$nx.g
	CircDataimageGlobals$StartCol <<- 1
	CircDataimageGlobals$EndCol <<- CircDataimageGlobals$ny.g

	if(as.character(tclvalue(nObs.cb.value2)) == "0")
	{
		u.g <- matrix(data=NA,nrow=CircDataimageGlobals$nx.g,ncol=CircDataimageGlobals$ny.g) # u accumulator, because atan2(0,0)=0
		v.g <- u.g # v accumulator
		Rows    <- round((InputData[, 1]- CircDataimageGlobals$MinX.g)/CircDataimageGlobals$DX + 1, digits = 0) # Indexing vector
		Columns <- round((InputData[, 2]- CircDataimageGlobals$MinY.g)/CircDataimageGlobals$DY + 1, digits = 0) # Indexing vector
		u.g[cbind(Rows, Columns)] <- InputData[, 3]
		v.g[cbind(Rows, Columns)] <- InputData[, 4]
	} else
	{
		cat("The initial computations necessarily may take significant time\n")

		u.g <- matrix(data=0, nrow=CircDataimageGlobals$nx.g, ncol=CircDataimageGlobals$ny.g) # u accumulator
		v.g <- u.g # v accumulator
		N.g <- u.g # Number of observations per cell

		Row    <- round((InputData[, 1]- CircDataimageGlobals$MinX.g)/CircDataimageGlobals$DX + 1, digits = 0) # Indexing scalar
		Column <- round((InputData[, 2]- CircDataimageGlobals$MinY.g)/CircDataimageGlobals$DY + 1, digits = 0) # Indexing scalar
		for (i in 1:nrow(InputData))
		{	
			u.g[Row[i], Column[i]] <- u.g[Row[i], Column[i]] + InputData[i, 3]
			v.g[Row[i], Column[i]] <- v.g[Row[i], Column[i]] + InputData[i, 4]
			N.g[Row[i], Column[i]] <- N.g[Row[i], Column[i]] + 1
		}

		# Averages
		filter1 <- N.g > 0
		u.g[filter1] <- u.g[filter1]/N.g[filter1]
		v.g[filter1] <- v.g[filter1]/N.g[filter1]
		# Replace 0’s with NAs where there are no observations
		u.g[!filter1] <- NA
		v.g[!filter1] <- NA
	}

		
	CircDataimageGlobals$u.g <<- u.g # Cell contains average u or NA
	CircDataimageGlobals$v.g <<- v.g
	CircDataimageGlobals$Direction.g <<- R1.Standardize(atan2(v.g, u.g)) # atan2(NA,NA)=NA
	CircDataimageGlobals$Direction <<- CircDataimageGlobals$Direction.g

	R1.SubsetColorScale(CircDataimageGlobals$Direction[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow, CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol])
	CircDataimageGlobals$PlotMask <<- FALSE
	CircDataimageGlobals$PlotArrows <<- FALSE
	R1.PlotImage()
}
########################################################################################################
R1.Standardize <- function(Input)
{
	# 2007-08-05.1218

	# Input and Output in radians
	filter <- !is.na(Input)
	temp <- Input[filter]
	temp[temp < 0] <- temp[temp < 0] + 2*pi # negative angles occur
	temp[temp > 2*pi] <- temp[temp > 2*pi] - 2*pi # I don't believe angles > 2pi occur
	Input[filter] <- temp
	return(Input)
}
###########################################################################################################
R1.SubsetColorScale <- function(Input)
{
	# 2007-08-20.1331

	filter <- !is.na(Input)
	Range <- floor(range(Input[filter]*180/pi))
	a <- which((0:359) == Range[1])
	b <- which((0:359) == Range[2])
	CircDataimageGlobals$ColorFilter <<- a:b	
}
###########################################################################################################
R1.PlotImage <- function()
{
	# 2007-09-14.1720

	# Composite image = data overplotted with arrows overplotted with mask
	dev.set(which=dev.image)
	# The image color vector is subset based on range of data.
	image(x= CircDataimageGlobals$x.g[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow],
	      y= CircDataimageGlobals$y.g[CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol],
	      z= CircDataimageGlobals$Direction[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow, CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol],
	      col= CircDataimageGlobals$ColorVector[CircDataimageGlobals$ColorFilter], xlab="X", ylab="Y", asp=1)
	
	if(CircDataimageGlobals$PlotMask) R1.PlotMask()
	if(CircDataimageGlobals$PlotArrows) R1.PlotArrows() 
}
########################################################################################################
R1.PlotArrows <- function()
{
	# 2007-09-20.2244

	x <- CircDataimageGlobals$x.g[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow]
	y <- CircDataimageGlobals$y.g[CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol]
	Directions <- CircDataimageGlobals$Direction[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow, CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol]

	nx=length(x)
	ny=length(y)
	x <- rep(x, ny)
	y <- rep(y, each=nx)
	Directions <- as.vector(Directions)

	filter1 <- rep(rep(CircDataimageGlobals$cpa:1, length=nx), ny) == CircDataimageGlobals$cpa
	filter2 <- as.vector(t(matrix(data=rep(rep(CircDataimageGlobals$cpa:1, length=ny), nx), nrow=ny))) == CircDataimageGlobals$cpa
	if(!CircDataimageGlobals$PlotMask | is.null(CircDataimageGlobals$Mask)) filter <- !is.na(Directions) & filter1 & filter2
	if(CircDataimageGlobals$PlotMask & !is.null(CircDataimageGlobals$Mask))
	{
		mask.boolean <- matrix(data=TRUE, nrow=CircDataimageGlobals$nx.g, ncol=CircDataimageGlobals$ny.g)
		mask.boolean[!is.na(CircDataimageGlobals$Mask)] <- FALSE
		mask.boolean <- mask.boolean[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow, CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol]
		filter <- !is.na(Directions) & filter1 & filter2 & as.vector(mask.boolean)
	}
	if(sum(filter) > 0)
	{
		x <- x[filter]
		y <- y[filter]
		Directions <- Directions[filter]
		arrow.plot(x, y, u = cos(Directions), v = sin(Directions), arrow.ex = 0.05*CircDataimageGlobals$ArrowAdj,  xpd = FALSE,
			true.angle = TRUE, arrowfun=arrows, length=.05, angle=15, col=1)
	} else cat("No arrows can be displayed at current spacing\n")
}
###########################################################################################################
R1.PlotMask <- function()
{
	#2007-08-09.2013

	image(x=CircDataimageGlobals$x.g[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow],
	      y=CircDataimageGlobals$y.g[CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol],
	      z=CircDataimageGlobals$Mask[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow, CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol],
	      col= "tan", add=TRUE)
}
########################################################################################################
R1.PlotWheel <- function()
{
	dev.set(which=dev.wheel)
	# The image color vector is not subset based on range of data.
	image(x=seq(-1,1,length=201), y=seq(-1,1,length=201), z= wheel, col= CircDataimageGlobals$ColorVector, add=TRUE)
}
########################################################################################################
R1.Pan <- function()
{
	# 2007-09-11.2139

	CircDataimageGlobals$StartRow <<- round((CircDataimageGlobals$MinX.d - CircDataimageGlobals$MinX.g)/CircDataimageGlobals$DX + 1, digits=0)
	CircDataimageGlobals$EndRow   <<- round((CircDataimageGlobals$MaxX.d - CircDataimageGlobals$MinX.g)/CircDataimageGlobals$DX + 1, digits=0)
	CircDataimageGlobals$StartCol <<- round((CircDataimageGlobals$MinY.d - CircDataimageGlobals$MinY.g)/CircDataimageGlobals$DY + 1, digits=0)
	CircDataimageGlobals$EndCol   <<- round((CircDataimageGlobals$MaxY.d - CircDataimageGlobals$MinY.g)/CircDataimageGlobals$DY + 1, digits=0)

	R1.SubsetColorScale(CircDataimageGlobals$Direction[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow, CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol])
	R1.PlotImage()
}
###########################################################################################################
R1.ChangeColorWheel <- function()
{
	# 2007-08-20.1903

	ID <- CircDataimageGlobals$ColorVectorID
	if(ID == "1")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$GBYR
	if(ID == "2")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$GYRB
	if(ID == "3")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$ROYBgBPb
	if(ID == "4")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$HSV
	if(ID == "5")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$KBWR
	if(ID == "6")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$KBCWYR
	if(ID == "7")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$Brewer10Div6
	if(ID == "8")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$Rainbow.20Bin
	if(ID == "9")  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$KBWR.12Bin
	if(ID == "10") CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$Brewer10Div6.10Bin
	if(ID == "11") CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$YRGB.8Bin
	if(ID == "12") CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$Brewer8Div2.8Bin
	if(ID == "13") CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$RMBGYO.6Bin

	R1.AutoRotateColorWheel()
	R1.PlotImage()
	R1.PlotWheel()
}
###########################################################################################################
R1.AutoRotateColorWheel <- function()
{
	# 2007-08-06.1603

	Rotation <- CircDataimageGlobals$ColorRotation
	if(Rotation > 0)
	{
		a <- (360-Rotation+1):360
		CircDataimageGlobals$ColorVector <<- c(CircDataimageGlobals$ColorVector.g[a], CircDataimageGlobals$ColorVector.g[-a])
	} else
	if(Rotation == 0) {CircDataimageGlobals$ColorVector <<- CircDataimageGlobals$ColorVector.g} else
	{CircDataimageGlobals$ColorVector <<- c(CircDataimageGlobals$ColorVector.g[-1:Rotation], CircDataimageGlobals$ColorVector.g[1:-Rotation])}
}
###########################################################################################################
R1.RotateColorWheel <- function()
{
	# 2007-08-20.1933
	# To return to unrotated color wheel, enter zero for rotation.

	Rotation <- CircDataimageGlobals$ColorRotation
	if(Rotation > 0)
	{
		a <- (360-Rotation+1):360
		CircDataimageGlobals$ColorVector <<- c(CircDataimageGlobals$ColorVector.g[a], CircDataimageGlobals$ColorVector.g[-a])
	} else
	if(Rotation == 0) {CircDataimageGlobals$ColorVector <<- CircDataimageGlobals$ColorVector.g} else
	{CircDataimageGlobals$ColorVector <<- c(CircDataimageGlobals$ColorVector.g[-1:Rotation], CircDataimageGlobals$ColorVector.g[1:-Rotation])}

	R1.PlotImage()
	R1.PlotWheel()
}
###########################################################################################################
R1.ChangeColorGap <- function()
{
	# 2007-08-11.1223

	R1.WriteContColorVectors() # Recompute with gap
	if(CircDataimageGlobals$ColorVectorID == 1)  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$GBYR
	if(CircDataimageGlobals$ColorVectorID == 2)  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$GYRB
	if(CircDataimageGlobals$ColorVectorID == 3)  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$ROYBgBPb
	if(CircDataimageGlobals$ColorVectorID == 4)  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$HSV
	if(CircDataimageGlobals$ColorVectorID == 5)  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$KBWR
	if(CircDataimageGlobals$ColorVectorID == 6)  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$KBCWYR
	if(CircDataimageGlobals$ColorVectorID == 7)  CircDataimageGlobals$ColorVector.g <<- CircDataimageGlobals$Brewer10Div6

	R1.AutoRotateColorWheel()
	R1.PlotImage()
	R1.PlotWheel()
}
###########################################################################################################
R1.Prime()
   
Top <- tktoplevel()
tkwm.geometry(Top,"565x600")
tkwm.title(Top,"Circular Dataimage")
FontHeading <- tkfont.create(family="arial", size=8, weight="bold")

FrameTop <- tkframe(Top, relief="flat", borderwidth=2)
data.name <- tclVar("unknown")
data.name.entry <-tkentry(FrameTop, width="15", textvariable=data.name)
mask.name <- tclVar("unknown")
mask.name.entry <- tkentry(FrameTop, width="15",textvariable=mask.name)
nObs.cb.value <- tclVar("0")
nObs.cb <- tkcheckbutton(FrameTop); tkconfigure(nObs.cb,variable=nObs.cb.value)

Input.but <- tkbutton(FrameTop, text="OK", command=function(){
	R1.Initialize(data.name, mask.name, nObs.cb.value)
	tclvalue(MinX.g) <- as.character(CircDataimageGlobals$MinX.g)
	tclvalue(MaxX.g) <- as.character(CircDataimageGlobals$MaxX.g)
	tclvalue(MinY.g) <- as.character(CircDataimageGlobals$MinY.g)
	tclvalue(MaxY.g) <- as.character(CircDataimageGlobals$MaxY.g)
	tclvalue(MinX) <- as.character(CircDataimageGlobals$MinX.g)
	tclvalue(MaxX) <- as.character(CircDataimageGlobals$MaxX.g)
	tclvalue(MinY) <- as.character(CircDataimageGlobals$MinY.g)
	tclvalue(MaxY) <- as.character(CircDataimageGlobals$MaxY.g)
	tclvalue(Smooth) <- "0"
	tclvalue(arrow.cb.value) <- "0"
	tclvalue(mask.cb.value) <- "0"
	}
)

tkgrid(tklabel(FrameTop, text="Input Dataframe"), data.name.entry, tklabel(FrameTop,text="     "),
tklabel(FrameTop,text="Mask Matrix"), mask.name.entry,      tklabel(FrameTop,text="     "),
tklabel(FrameTop,text="Obs Per Cell > 1"), nObs.cb,         tklabel(FrameTop,text="     "),
Input.but, sticky="w")
  
FrameLeft <- tkframe(Top, relief="groove", borderwidth=2)
tkgrid(tklabel(FrameLeft,text="Continuous Color Scales", font=FontHeading), sticky="e")

image1 <- tclVar()
image2 <- tclVar()
image3 <- tclVar()
image4 <- tclVar()
image5 <- tclVar()
image6 <- tclVar()
image7 <- tclVar()
image8 <- tclVar()
image9 <- tclVar()
image10 <- tclVar()
image11 <- tclVar()
image12 <- tclVar()
image13 <- tclVar()

tcl("image", "create", "photo", image1,  file=system.file("graphics", "GBYR.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image2,  file=system.file("graphics", "GYRB.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image3,  file=system.file("graphics", "ROYBgBPb.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image4,  file=system.file("graphics", "HSV.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image5,  file=system.file("graphics", "KBWR.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image6,  file=system.file("graphics", "KBCWYR.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image7,  file=system.file("graphics", "BREWER10D6.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image8,  file=system.file("graphics", "Rainbow.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image9,  file=system.file("graphics", "KBWR.12.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image10, file=system.file("graphics", "Brewer10D6.10.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image11, file=system.file("graphics", "YRGB.8.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image12, file=system.file("graphics", "Brewer8D2.8.jpeg", package="CircSpatial"))
tcl("image", "create", "photo", image13, file=system.file("graphics", "RMBGYO.6.jpeg", package="CircSpatial"))

wheel1 <-  tklabel(FrameLeft, image=image1) # Image as label
wheel2 <-  tklabel(FrameLeft, image=image2)
wheel3 <-  tklabel(FrameLeft, image=image3)
wheel4 <-  tklabel(FrameLeft, image=image4)
wheel5 <-  tklabel(FrameLeft, image=image5)
wheel6 <-  tklabel(FrameLeft, image=image6)
wheel7 <-  tklabel(FrameLeft, image=image7)
wheel8 <-  tklabel(FrameLeft, image=image8)
wheel9 <-  tklabel(FrameLeft, image=image9)
wheel10 <- tklabel(FrameLeft, image=image10)
wheel11 <- tklabel(FrameLeft, image=image11)
wheel12 <- tklabel(FrameLeft, image=image12)
wheel13 <- tklabel(FrameLeft, image=image13)

rb1 <- tkradiobutton(FrameLeft)
rb2 <- tkradiobutton(FrameLeft)
rb3 <- tkradiobutton(FrameLeft)
rb4 <- tkradiobutton(FrameLeft)
rb5 <- tkradiobutton(FrameLeft)
rb6 <- tkradiobutton(FrameLeft)
rb7 <- tkradiobutton(FrameLeft)
rb8 <- tkradiobutton(FrameLeft)
rb9 <- tkradiobutton(FrameLeft)
rb10 <- tkradiobutton(FrameLeft)
rb11 <- tkradiobutton(FrameLeft)
rb12 <- tkradiobutton(FrameLeft)
rb13 <- tkradiobutton(FrameLeft)
rbValue <- tclVar("1")

ChangeColor <- function() {CircDataimageGlobals$ColorVectorID <<- as.character(tclvalue(rbValue)); R1.ChangeColorWheel()}
tkconfigure(rb1, variable=rbValue,value="1", command=ChangeColor)
tkconfigure(rb2, variable=rbValue,value="2", command=ChangeColor)
tkconfigure(rb3, variable=rbValue,value="3", command=ChangeColor)
tkconfigure(rb4, variable=rbValue,value="4", command=ChangeColor)
tkconfigure(rb5, variable=rbValue,value="5", command=ChangeColor)
tkconfigure(rb6, variable=rbValue,value="6", command=ChangeColor)
tkconfigure(rb7, variable=rbValue,value="7", command=ChangeColor)
tkconfigure(rb8, variable=rbValue,value="8", command=ChangeColor)
tkconfigure(rb9, variable=rbValue,value="9", command=ChangeColor)
tkconfigure(rb10,variable=rbValue,value="10", command=ChangeColor)
tkconfigure(rb11,variable=rbValue,value="11", command=ChangeColor)
tkconfigure(rb12,variable=rbValue,value="12", command=ChangeColor)
tkconfigure(rb13,variable=rbValue,value="13", command=ChangeColor)

tkgrid(tklabel(FrameLeft,text="GBYR "), wheel1, rb1, sticky="e")
tkgrid(tklabel(FrameLeft,text="GYRB "), wheel2, rb2, sticky="e")
tkgrid(tklabel(FrameLeft,text="ROYBgBPb "), wheel3, rb3, sticky="e")
tkgrid(tklabel(FrameLeft,text="HSV "), wheel4, rb4, sticky="e")
tkgrid(tklabel(FrameLeft,text="KBWR "), wheel5, rb5, sticky="e")
tkgrid(tklabel(FrameLeft,text="KBCWYR "), wheel6, rb6, sticky="e")
tkgrid(tklabel(FrameLeft,text="Brewer divergent #6 "), wheel7, rb7, sticky="e")
tkgrid(tklabel(FrameLeft,text="           "), column=1)
tkgrid(tklabel(FrameLeft,text="Binned Color Scales", font=FontHeading), sticky="e")
tkgrid(tklabel(FrameLeft,text="Rainbow 20 bins "), wheel8, rb8, sticky="e")
tkgrid(tklabel(FrameLeft,text="KBWR 12 bins "), wheel9, rb9, sticky="e")
tkgrid(tklabel(FrameLeft,text="Brewer divergent #6 10 bins "), wheel10, rb10, sticky="e")
tkgrid(tklabel(FrameLeft,text="YRGB 8 bins "), wheel11, rb11, sticky="e")
tkgrid(tklabel(FrameLeft,text="Brewer divergent #2 8 bins "), wheel12, rb12, sticky="e")
tkgrid(tklabel(FrameLeft,text="RMBGYO 6 bins "), wheel13, rb13, sticky="e")
tkgrid(tklabel(FrameLeft,text="           "))

SliderValue1 <- tclVar("0")
SliderValueLabel1 <- tklabel(FrameLeft,text=as.character(tclvalue(SliderValue1)))
tkconfigure(SliderValueLabel1,textvariable=SliderValue1)
slider1 <- tkscale(FrameLeft, from=-180, to=180, showvalue=TRUE, variable=SliderValue1, resolution=5, orient="horizontal", length="1.15i")
tkbind(slider1,"<ButtonRelease-1>", function() {CircDataimageGlobals$ColorRotation <<- as.numeric(tclvalue(SliderValue1)); R1.RotateColorWheel()})
tkgrid(tklabel(FrameLeft,text="Color Scale Rotation", font=FontHeading), column=0, sticky="e")
tkgrid(slider1, column=0, sticky="e")
tkgrid(tklabel(FrameLeft,text="-180                       +180"), sticky="e")

FrameRight <- tkframe(Top, relief="groove", borderwidth=2)
tkgrid(tklabel(FrameRight, text="Display Coordinates", font=FontHeading))

MinX <- tclVar("")
MinX.entry <-tkentry(FrameRight, width="12",textvariable= MinX)
MinX.g <- tclVar("unknown")
MinX.g.label <- tklabel(FrameRight,text=tclvalue(MinX.g))
tkconfigure(MinX.g.label, textvariable=MinX.g)
tkgrid(tklabel(FrameRight,text="Min X"), MinX.entry, tklabel(FrameRight, text="Global Min  X ="), MinX.g.label, sticky="e")

MaxX <- tclVar("")
MaxX.entry <-tkentry(FrameRight, width="12",textvariable= MaxX)
MaxX.g <- tclVar("unknown")
MaxX.g.label <- tklabel(FrameRight,text=tclvalue(MaxX.g))
tkconfigure(MaxX.g.label, textvariable=MaxX.g)
tkgrid(tklabel(FrameRight,text="Max X"), MaxX.entry, tklabel(FrameRight, text="Global Max X ="), MaxX.g.label, sticky="e")

MinY <- tclVar("")
MinY.entry <-tkentry(FrameRight, width="12",textvariable= MinY)
MinY.g <- tclVar("unknown")
MinY.g.label <- tklabel(FrameRight,text=tclvalue(MinY.g))
tkconfigure(MinY.g.label, textvariable=MinY.g)
tkgrid(tklabel(FrameRight,text="Min Y"), MinY.entry, tklabel(FrameRight, text="Global Min  Y ="), MinY.g.label, sticky="e")

MaxY <- tclVar("")
MaxY.entry <-tkentry(FrameRight, width="12",textvariable= MaxY)
MaxY.g <- tclVar("unknown")
MaxY.g.label <- tklabel(FrameRight,text=tclvalue(MaxY.g))
tkconfigure(MaxY.g.label, textvariable=MaxY.g)
tkgrid(tklabel(FrameRight,text="Max Y"), MaxY.entry, tklabel(FrameRight, text="Global Max Y ="), MaxY.g.label, sticky="e")

Coord.but <- tkbutton(FrameRight,text="OK", command=function(){
	CircDataimageGlobals$MinX.d <<- as.numeric(tclvalue(MinX))
	CircDataimageGlobals$MaxX.d <<- as.numeric(tclvalue(MaxX))
	CircDataimageGlobals$MinY.d <<- as.numeric(tclvalue(MinY))
	CircDataimageGlobals$MaxY.d <<- as.numeric(tclvalue(MaxY))

	indexClosest <- which.min(abs(CircDataimageGlobals$x.g - CircDataimageGlobals$MinX.d)); CircDataimageGlobals$MinX.d <<- CircDataimageGlobals$x.g[indexClosest]
	indexClosest <- which.min(abs(CircDataimageGlobals$x.g - CircDataimageGlobals$MaxX.d)); CircDataimageGlobals$MaxX.d <<- CircDataimageGlobals$x.g[indexClosest]
	indexClosest <- which.min(abs(CircDataimageGlobals$y.g - CircDataimageGlobals$MinY.d)); CircDataimageGlobals$MinY.d <<- CircDataimageGlobals$y.g[indexClosest]
	indexClosest <- which.min(abs(CircDataimageGlobals$y.g - CircDataimageGlobals$MaxY.d)); CircDataimageGlobals$MaxY.d <<- CircDataimageGlobals$y.g[indexClosest]
	
	tclvalue(MinX) <- as.character(CircDataimageGlobals$MinX.d)
	tclvalue(MaxX) <- as.character(CircDataimageGlobals$MaxX.d)
	tclvalue(MinY) <- as.character(CircDataimageGlobals$MinY.d)
	tclvalue(MaxY) <- as.character(CircDataimageGlobals$MaxY.d)
	R1.Pan()
    }
)
tkgrid(Coord.but, column=1, sticky="e")
tkgrid(tklabel(FrameRight,text="           "))
tkgrid(tklabel(FrameRight,text="           "))

Smooth <- tclVar("0")
Smooth.function <- function()
{
	Bandwidth <- as.numeric(tclvalue(Smooth))
	if(Bandwidth > 0)
	{
		XVEC <- rep(CircDataimageGlobals$x.g, CircDataimageGlobals$ny.g)
		YVEC <- rep(CircDataimageGlobals$y.g, ea=CircDataimageGlobals$nx.g)
		ImageList.x <- as.image(as.vector(CircDataimageGlobals$u.g), x=data.frame(lon=XVEC, lat=YVEC),
			nrow= CircDataimageGlobals$nx.g, ncol= CircDataimageGlobals$ny.g, boundary.grid=FALSE, na.rm=TRUE)
		u.g.Smooth <- image.smooth(ImageList.x, theta = Bandwidth)

		ImageList.y <- as.image(as.vector(CircDataimageGlobals$v.g), x=data.frame(lon=XVEC, lat=YVEC),
			nrow= CircDataimageGlobals$nx.g, ncol= CircDataimageGlobals$ny.g, boundary.grid=FALSE, na.rm=TRUE)
		v.g.Smooth <- image.smooth(ImageList.y, theta = Bandwidth)

		CircDataimageGlobals$Direction <<- R1.Standardize(atan2(v.g.Smooth$z, u.g.Smooth$z))
	} else CircDataimageGlobals$Direction <<- CircDataimageGlobals$Direction.g
	R1.SubsetColorScale(CircDataimageGlobals$Direction[CircDataimageGlobals$StartRow:CircDataimageGlobals$EndRow, CircDataimageGlobals$StartCol:CircDataimageGlobals$EndCol])
	R1.PlotImage()
}
Smooth.entry <-tkentry(FrameRight, width="12", textvariable=Smooth)
Smooth.but <- tkbutton(FrameRight,text="OK", command=Smooth.function)
tkgrid(tklabel(FrameRight,text="Smooth Bandwidth", font=FontHeading), Smooth.entry, sticky="e")
tkgrid(Smooth.but, column=1, sticky="e")
tkgrid(tklabel(FrameRight,text="           "))
tkgrid(tklabel(FrameRight,text="           "))

SliderValue2 <- tclVar("0")
SliderValueLabel2 <- tklabel(FrameRight,text=as.character(tclvalue(SliderValue2)))
tkconfigure(SliderValueLabel2,textvariable=SliderValue2)
slider2 <- tkscale(FrameRight, from=0, to=1, showvalue=TRUE, variable=SliderValue2, resolution=.05, orient="horizontal", length=".8i")
tkbind(slider2,"<ButtonRelease-1>", function() {CircDataimageGlobals$ColorGap <<- as.numeric(tclvalue(SliderValue2)); R1.ChangeColorGap()})
tkgrid(tklabel(FrameRight,text="Color Scale Gap", font=FontHeading), column=0, sticky="e")
tkgrid(slider2, column=1, sticky="e")
tkgrid(tklabel(FrameRight,text="0                       1"), column=1)
tkgrid(tklabel(FrameRight,text=" "), sticky="e")
tkgrid(tklabel(FrameRight,text=" "), sticky="e")

arrow.cb.value <- tclVar("0")
arrow.cb.function <- function()
{
	cbVal <- as.character(tclvalue(arrow.cb.value))
	if (cbVal=="1") {CircDataimageGlobals$PlotArrows <<- TRUE; R1.PlotImage()}
	if (cbVal=="0") {CircDataimageGlobals$PlotArrows <<- FALSE; R1.PlotImage()}
}
arrow.cb <- tkcheckbutton(FrameRight, command=arrow.cb.function)
tkconfigure(arrow.cb, variable=arrow.cb.value)
tkgrid(tklabel(FrameRight,text="Arrows", font=FontHeading), sticky="e")
tkgrid(arrow.cb, row=17, column=1, sticky="w")

arrow.length <- tclVar("1")
arrow.density <- tclVar("15")
arrow.function <- function()
{
	CircDataimageGlobals$ArrowAdj <<- as.numeric(tclvalue(arrow.length))
	CircDataimageGlobals$cpa <<- as.numeric(tclvalue(arrow.density))
	R1.PlotImage()
}
arrow.length.entry <-tkentry(FrameRight, width="6",textvariable=arrow.length)
tkgrid(tklabel(FrameRight,text="Arrow Length Multiplier"), arrow.length.entry,
	tklabel(FrameRight, text=">   0                "), sticky="e")
arrow.density.entry <-tkentry(FrameRight, width="6",textvariable=arrow.density)
tkgrid(tklabel(FrameRight,text="Arrow Spacing in Pixels"), arrow.density.entry,
	tklabel(FrameRight, text="1, 2, 3, ...        "), sticky="e")
Arrow.but <- tkbutton(FrameRight,text="OK", command=arrow.function)
tkgrid(Arrow.but, column=1, sticky="e")
tkgrid(tklabel(FrameRight,text="           "))
tkgrid(tklabel(FrameRight,text="           "))

mask.cb.value <- tclVar("0")
mask.cb.function <- function()
{
	cbVal <- as.character(tclvalue(mask.cb.value))
	if (cbVal=="1") {if(!is.null(CircDataimageGlobals$Mask)) {CircDataimageGlobals$PlotMask <<- TRUE; R1.PlotImage()}}
	if (cbVal=="0") {CircDataimageGlobals$PlotMask <<- FALSE; R1.PlotImage()}
}
mask.cb <- tkcheckbutton(FrameRight, command=mask.cb.function)
tkconfigure(mask.cb, variable=mask.cb.value)
	
tkgrid(tklabel(FrameRight,text="Mask", font=FontHeading), sticky="e")
tkgrid(mask.cb, row=23, column=1, sticky="w")
tkgrid(tklabel(FrameRight,text="           "))
tkgrid(tklabel(FrameRight,text="           "))

tkpack(FrameTop, side="top", fill="x")
tkpack(FrameLeft, side="left", fill="both", expand=TRUE)
tkpack(FrameRight, side="right", fill="both", expand=TRUE)
}
