library(TDA)

###################################################
# 0. Rips complex toy examples
###################################################
## EXAMPLE 1: rips diagram for circles (euclidean distance)
X = circleUnif(30)
plot(X)
maxscale = 5
maxdimension = 1
## note that the input X is a point cloud
DiagRips = ripsDiag(
	X = X, maxdimension = maxdimension, maxscale = maxscale,
	library = "Dionysus", location = TRUE, printProgress = TRUE)

# plot
layout(matrix(c(1, 3, 2, 2), 2, 2))
plot(X, cex = 0.5, pch = 19)
title(main = "Data")
plot(DiagRips[["diagram"]])
title(main = "rips Diagram")
one = which(
	DiagRips[["diagram"]][, 1] == 1 &
		DiagRips[["diagram"]][, 3] - DiagRips[["diagram"]][, 2] > 0.5)
plot(X, col = 2, main = "Representative loop of data points")
for (i in seq(along = one)) {
	for (j in seq_len(dim(DiagRips[["cycleLocation"]][[one[i]]])[1])) {
		lines(
			DiagRips[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1,
			col = i)
	}
}

## EXAMPLE 2: rips diagram with arbitrary distance
### Points do not have to be in Euclidean space
### As long as points are in a metric space, you can compute pairwise distances
### and plug the distance matrix into theripsDiag function

## distance matrix for triangle with edges of length: 1,2,4
par(mfrow=c(1,1))
distX = matrix(c(0, 1, 2, 1, 0, 4, 2, 4, 0), ncol = 3)
distX
maxscale = 5
maxdimension = 1
## note that the input distXX is a distance matrix
DiagTri = ripsDiag(distX, maxdimension, maxscale, dist = "arbitrary",
										printProgress = TRUE)
#points with lifetime = 0 are not shown. e.g. the loop of the triangle.
DiagTri[["diagram"]]
plot(DiagTri$diagram)

###################################################
# 1. Three circles data example
###################################################
# load data
load("datasets.RData")

# point cloud data
plot(threecircledat)

# Rips complex
rdiag=ripsDiag(threecircledat, maxdimension=1, maxscale=4)
rdiag$diagram # dimension, birth, and death
plot(rdiag$diagram) # plot persistence diagram
plot(rdiag$diagram, barcode = TRUE) # plot barcode

# Using density estimators
## input variables
Xlim = c(-4,12)
Ylim = c(-4,9)
lim = cbind(Xlim, Ylim)
h=0.1 # bandwidth for the function kde
by=0.05 # space between points of the grid

Xseq = seq(Xlim[1], Xlim[2], by = by)
Yseq = seq(Ylim[1], Ylim[2], by = by)
Grid = expand.grid(Xseq, Yseq)
KDE = kde(X = threecircledat, Grid = Grid, h = h) # using Gaussian kernel
## plot density function
persp(Xseq, Yseq,
			matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
			ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
			col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
			expand = 3, shade = 0.9)
## compute persistence diagram using the density function
gdiag=gridDiag(threecircledat, FUN=kde, h=h, lim=lim, by=by, sublevel = FALSE) 
gdiag$diagram # dimension, birth, and death
plot(gdiag$diagram) # plot persistence diagram
plot(gdiag$diagram, barcode = TRUE) # plot barcode

###################################################
# 2. Two circles of different size example
# For Rips complex, larger the size, longer the feature persist
###################################################
plot(twocirclesizedat)

# Rips complex
rdiag=ripsDiag(twocirclesizedat, maxdimension=1, maxscale=5)
rdiag$diagram # dimension, birth, and death
plot(rdiag$diagram) # plot persistence diagram
plot(rdiag$diagram, barcode = TRUE) # plot barcode

# Using density estimators
## input variables
Xlim = c(-4,13)
Ylim = c(-7,7)
lim = cbind(Xlim, Ylim)
by=0.1
h=0.2

Xseq = seq(Xlim[1], Xlim[2], by = by)
Yseq = seq(Ylim[1], Ylim[2], by = by)
Grid = expand.grid(Xseq, Yseq)
KDE = kde(X = twocirclesizedat, Grid = Grid, h = h) # using Gaussian kernel
## plot density function
persp(Xseq, Yseq,
			matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
			ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
			col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
			expand = 3, shade = 0.9)
## compute persistence diagram using the density function
gdiag=gridDiag(twocirclesizedat, FUN=kde, h=h, lim=lim, by=by, sublevel = FALSE) 
gdiag$diagram # dimension, birth, and death
plot(gdiag$diagram) # plot persistence diagram
plot(gdiag$diagram, barcode = TRUE) # plot barcode

###################################################
# 3. Two circles of different density example
# For the approach using the density, higher the point density, longer it persist
###################################################

plot(twocircledensitydat)

# Rips complex
rdiag=ripsDiag(twocircledensitydat, maxdimension=1, maxscale=2)
rdiag$diagram # dimension, birth, and death
plot(rdiag$diagram) # plot persistence diagram
plot(rdiag$diagram, barcode = TRUE) # plot barcode

# Using density estimators
## input variables
Xlim = c(-3,6)
Ylim = c(-3,3)
lim = cbind(Xlim, Ylim)
by=0.1
h=0.2

Xseq = seq(Xlim[1], Xlim[2], by = by)
Yseq = seq(Ylim[1], Ylim[2], by = by)
Grid = expand.grid(Xseq, Yseq)
KDE = kde(X = twocircledensitydat, Grid = Grid, h = h) # using Gaussian kernel
## plot density function
persp(Xseq, Yseq,
			matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
			ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
			col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
			expand = 3, shade = 0.9)
## compute persistence diagram using the density function
gdiag=gridDiag(twocircledensitydat, FUN=kde, h=h, lim=lim, by=by, sublevel = FALSE) 
gdiag$diagram # dimension, birth, and death
plot(gdiag$diagram) # plot persistence diagram
plot(gdiag$diagram, barcode = TRUE) # plot barcode

###################################################
# 4. Two circles with noise example
###################################################

# point cloud data
plot(twocirclenoise)

# Rips complex
rdiag=ripsDiag(twocirclenoise, maxdimension=1, maxscale=1)
rdiag$diagram # dimension, birth, and death
plot(rdiag$diagram) # plot persistence diagram
plot(rdiag$diagram, barcode = TRUE) # plot barcode

# Using density estimators
## input variables
Xlim = c(-2,3)
Ylim = c(-2,2)
lim = cbind(Xlim, Ylim)
h=0.1 # bandwidth for the function kde
by=0.05 # space between points of the grid
Xseq = seq(Xlim[1], Xlim[2], by = by)
Yseq = seq(Ylim[1], Ylim[2], by = by)
Grid = expand.grid(Xseq, Yseq)
KDE = kde(X = twocirclenoise, Grid = Grid, h = h) # using Gaussian kernel
## plot density function
persp(Xseq, Yseq,
			matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
			ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
			col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
			expand = 3, shade = 0.9)

# different bandwidth
h=0.2 # bandwidth for the function kde
by=0.05 # space between points of the grid
Xseq = seq(Xlim[1], Xlim[2], by = by)
Yseq = seq(Ylim[1], Ylim[2], by = by)
Grid = expand.grid(Xseq, Yseq)
KDE = kde(X = twocirclenoise, Grid = Grid, h = h) # using Gaussian kernel
## plot density function
persp(Xseq, Yseq,
			matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
			ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
			col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
			expand = 3, shade = 0.9)

## compute persistence diagram using the density function
gdiag=gridDiag(threecircledat, FUN=kde, h=h, lim=lim, by=by, sublevel = FALSE) 
gdiag$diagram # dimension, birth, and death
plot(gdiag$diagram) # plot persistence diagram
plot(gdiag$diagram, barcode = TRUE) # plot barcode

