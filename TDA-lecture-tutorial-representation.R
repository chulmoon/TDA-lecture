library(tidyverse)
library(spatstat)
library(ggthemes)

##########################################################
# Representation: Persistence Image (PI)
##########################################################

# persistence diagram
birth=c(0.06,0.02,0.1,0.12,0.16,0.42,0.22,0.45,0.5,0.53,0.55,0.73,0.76,0.85,0.9)
death=c(0.1,0.17,0.15,0.15,0.75,0.73,0.45,0.57,0.53,0.75,0.62,0.85,0.81,0.92,0.95)

# plot persistence diagram
plot(birth,death,xlim=c(0,1),ylim=c(0,1))
abline(0,1)

# parameters for PI
res=40 # resolution of image: 40 by 40
range = rangex = rangey = c(0,1) # range of image

# ppp object of spatstat
pd.ppp = ppp(birth, death, rangex, rangey)

## weights
linweight = death - birth # proportional to the length
conweight = 1 # constant weight

## smoothing parameter for Gaussian smoothing function
sigma0 = 0.1

pi=density(pd.ppp,sigma=sigma0,dimyx=c(res,res),
						weights=data.frame(x=linweight,y=conweight))
## for plots
cunit=(range[2]-range[1])/(2*res)
gridx=seq(range[1]+cunit,range[2]-cunit,length.out = res)
gridy=seq(range[1]+cunit,range[2]-cunit,length.out = res)

## PI with linear weight
pi.linweight=pi$x$v
df1=data.frame(x=rep(gridx,each=res),y=rep(gridy,res),z=as.vector(pi.linweight))
dflw = df1 %>% 
	filter(y>=x) %>%
	mutate(value=case_when(
		z < 0 ~ 0,
		TRUE ~ z)
	)
ggplot(dflw,aes(x=x,y=y,fill=value))+
	geom_raster()+
	scale_fill_gradient2(limits=c( 0,12 ) ) +
	theme_tufte()

## PI with no weight
pi.noweight=pi$y$v
df2=data.frame(x=rep(gridx,each=res),y=rep(gridy,res),z=as.vector(pi.noweight))
dfnw = df2 %>% 
	filter(y>=x) %>%
	mutate(value=case_when(
		z < 0 ~ 0,
		TRUE ~ z)
	)
ggplot(dfnw,aes(x=x,y=y,fill=value))+
	geom_raster()+
	scale_fill_gradient2(limits=c( 0,100 ) ) +
	theme_tufte()
