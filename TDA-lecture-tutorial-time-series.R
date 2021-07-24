library(TDA)
library(tidyverse)

##########################################################
# Point cloud embedding using Tekken's delay embedding
##########################################################
## This part's R code is modified using the code used in 
## Ravishanker, Nalini, and Renjie Chen.
## "Topological data analysis (TDA) for time series." 
## arXiv preprint arXiv:1909.10604 (2019).

par(mfrow=c(1,3))

# period: 12
per1=12
T=480 # length of time series
d=2 # dimension of point: two-dimensional space
ts.ex = cos(1:T*2*pi/per1) # cosine signal
## delay parameter is selected as the smallest time lag 
## where the sample autocorrelation function (ACF) becomes insignificant
tau = which(abs(acf(ts.ex, plot = F)$acf) < 2/sqrt(T))[1]-1
tau
# time series data embedded as a point cloud
PC=t(purrr::map_dfc(1:(T-(d-1)*tau+1),~ts.ex[seq(from=.x, by=tau, length.out=d)]))
# compute persistent homology
diag=ripsDiag(PC, maxdimension=1, maxscale=max(dist(PC)))
# plot data and results
ts.plot(ts.ex)
plot(PC,xlab ="x1",ylab="x2")
plot(diag$diagram)

# period: 48
per2=48 
T=480
d=2
ts.ex = cos(1:T*2*pi/per2)
# delay parameter
tau = which(abs(acf(ts.ex, plot = F)$acf) < 2/sqrt(T))[1]-1
tau
PC=t(purrr::map_dfc(1:(T-(d-1)*tau+1),~ts.ex[seq(from=.x, by=tau, length.out=d)]))
# compute persistent homology
diag=ripsDiag(PC, maxdimension=1, maxscale=max(dist(PC)))
ts.plot(ts.ex)
plot(PC,xlab ="x1",ylab="x2")
plot(diag$diagram)

# period: 96
per3=96
T=480
d=2
ts.ex = cos(1:T*2*pi/per3)
tau = which(abs(acf(ts.ex, plot = F)$acf) < 2/sqrt(T))[1]-1
tau
PC=t(purrr::map_dfc(1:(T-(d-1)*tau+1),~ts.ex[seq(from=.x, by=tau, length.out=d)]))
diag=ripsDiag(PC, maxdimension=1, maxscale=max(dist(PC)))
ts.plot(ts.ex)
plot(PC,xlab ="x1",ylab="x2",main="PC")
plot(diag$diagram)

##########################################################
# Time series as discretized function
##########################################################
par(mfrow=c(1,2))
# the age of death of successive kings of England
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
# time series of first differences
kingsdiff1 = diff(kings, difference=1)
# plot two time series
plot.ts(kings)
plot.ts(kingsdiff1)

# compute persistent homology
pd.kings = gridDiag(FUNvalues = kings, sublevel=TRUE)
plot.ts(kings)
plot(pd.kings$diagram,diagLim=c(-40,90))

# compute persistent homology
pd.kings.diff1 = gridDiag(FUNvalues = kingsdiff1, sublevel=TRUE)
plot.ts(kingsdiff1)
plot(pd.kings.diff1$diagram,diagLim=c(-40,90))
