library(TDA)
library(tidyverse)
library(imager)

# 10 by 10 image
resx=10
resy=10
par(mfrow=c(1,1))
##################################################
# Toy example with binary image
##################################################
# generate example image
imgex = imdirac(dims=c(resx,resy),c(8,3,4,5),c(3,7,8,9))

# plot example image
gridx=1:resx
gridy=resy:1
dfex=data.frame(x=rep(gridx,each=10),y=rep(gridy,10),z=as.vector(imgex))
ggplot(dfex,aes(x,y,fill=z))+
	geom_raster()

# distance transform using Euclidean distance for reference value 0
imgex_dt0=distance_transform(imgex,value=0,metric=2) 
# plot distance transformed image
dtex0=data.frame(x=rep(gridx,each=10),y=rep(gridy,10),z=as.vector(imgex_dt0))
ggplot(dtex0,aes(x,y,fill=z))+
	geom_raster()

# distance transform using Euclidean distance for reference value 1
revimgex=as.cimg(1-as.array(imgex))
imgex_dt1=distance_transform(revimgex,value=0,metric=2)  
# plot distance transformed image
dtex1=data.frame(x=rep(gridx,each=10),y=rep(gridy,10),z=as.vector(imgex_dt1))
ggplot(dtex1,aes(x,y,fill=z))+
	geom_raster()

# signed distance transform
dtex = (as.array(revimgex)==1)*as.array(imgex_dt1) -(as.array(imgex)==1)*as.array(imgex_dt0)
df=data.frame(x=rep(gridx,each=10),y=rep(gridy,10),z=as.vector(dtex))
ggplot(df,aes(x,y,fill=z))+
	geom_raster()+
	scale_fill_gradient2(limits=c( -2,3 ) )

# compute persistent homology using filtration diagram
gftest = gridFiltration(FUNvalues=dtex,sublevel=TRUE)
diagtest = filtrationDiag(filtration = gftest,maxdimension = 1, 
													library="Dionysus",location=TRUE)
plot(diagtest[["diagram"]])
diagtest$diagram

# visualization according to filtration
ggplot(df,aes(x,y,fill=(z<=-2)*1))+
	geom_raster()+
	scale_fill_gradient2(limits=c( -2,3 ) )
ggplot(df,aes(x,y,fill=(z<=-1)*1))+
	geom_raster()+
	scale_fill_gradient2(limits=c( -2,3 ) )
ggplot(df,aes(x,y,fill=(z<=1)*1))+
	geom_raster()+
	scale_fill_gradient2(limits=c( -2,3 ) )
ggplot(df,aes(x,y,fill=(z<=2)*1))+
	geom_raster()+
	scale_fill_gradient2(limits=c( -2,3 ) )
ggplot(df,aes(x,y,fill=(z<=3)*1))+
	geom_raster()+
	scale_fill_gradient2(limits=c( -2,3 ) )

##################################################
# Toy example with grayscale image
##################################################
file = system.file('extdata/parrots.png',package='imager')
parrots = load.image(file)
grayscale(parrots) %>% plot # 0-1 scale: 0 is black 1 is white

par(mfrow=c(2,2))
# parrot 1
gparrot.sub1 = imsub(grayscale(parrots),x < 400, y > 100)
gparrot.sub1 %>% plot()

gfparrot1 = gridFiltration(FUNvalues=as.array(gparrot.sub1),sublevel=TRUE)
diagparrot1 = filtrationDiag(filtration = gfparrot1,maxdimension = 1)
plot(diagparrot1$diagram)

# parrot 2
gparrot.sub2 = imsub(grayscale(parrots),x > 400, y > 100)
gparrot.sub2 %>% plot()

gfparrot2 = gridFiltration(FUNvalues=as.array(gparrot.sub2),sublevel=TRUE)
diagparrot2 = filtrationDiag(filtration = gfparrot2,maxdimension = 1,location=T)
plot(diagparrot2$diagram)
