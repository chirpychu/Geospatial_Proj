
library(sp)
library(maptools)
library(magrittr) # for pipe function
library(maps)
library(rgeos) # for gSimplify
library(raster)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

require(rgeos)
library(maptools)
library(RColorBrewer)
library(plyr)
require(tmap)
library(GISTools)

library(rgdal)
library(krige)
library(automap)

blueSGLongLat <- read.csv("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021/blueSGLongLat.csv")
View(blueSGLongLat)
y <- readOGR("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021/master-plan-2014-planning-area-boundary-no-sea", "MP14_PLNG_AREA_NO_SEA_PL")
y = st_as_sf(y)
sf::st_is_valid(y)

library(geojsonio)
subzone <- rgdal::readOGR("master-plan-2019-subzone-boundary-no-sea-geojson.geojson")
singaporepolygonmap <- tm_shape(subzone) + tm_borders(col = "black") +tm_fill()

library(broom)
spdf_y <- tidy(y)
spdf_subzone <- tidy(subzone)
  
  
library(tmap)
tmap_mode("view")
tmap_options(check.and.fix = TRUE)
y.fix <- st_make_valid(y)
roads <- readOGR("national-map-line-geojson.geojson")
tm_shape(y.fix) + tm_borders() + tm_shape(subzone) + tm_borders()+tm_fill("tomato")+ tm_shape(roads) + tm_lines(col = "black")


tm_shape(y.fix) + tm_borders() + tm_shape(roads) + tm_lines(col = "red")


population <- readOGR("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021/singapore-residents-by-subzone-and-type-of-dwelling-june-2016", "SUBZONE_DWELLING_TYPE_2016")
tm_shape(y.fix) + tm_borders() + tm_shape(subzone) + tm_borders()+tm_fill(population)

population_area <- readOGR("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021/singapore-residents-by-planning-area-and-type-of-dwelling-june-2016", "PLAN_BDY_DWELLING_TYPE_2016")


tmap_mode("view")
tmap_options(check.and.fix = TRUE)
# tm_shape(y.fix) + tm_borders() + tm_shape(subzone) + tm_borders()+tm_shape(population) +
#   tm_polygons(col = "TOTAL")

population_density <-tm_shape(y.fix) + tm_borders() + tm_shape(subzone) + tm_borders()+tm_shape(population_area) + tm_polygons(col = "TOTAL") + tm_text("PLN_AREA_N", just = "centre", xmod = 1, size = 0.5)+ tm_shape(roads) + tm_lines(col = "black", alpha = 0.1)

bluesg<- st_as_sf(blueSGLongLat, coords = c("lng", "lat"),crs = 4326)

#bluesg location on map
bluesglocation <-tm_shape(y.fix) + tm_borders() + tm_shape(subzone) + tm_borders()+tm_shape(population_area) + tm_polygons(col = "TOTAL") + tm_shape(roads) + tm_lines(col = "black", alpha = 0.1) + tm_shape(bluesg) + tm_dots()

tm_shape(y.fix) + tm_borders() + tm_shape(bluesg) + tm_dots(col = 'red')

malls <- read.csv("C:/Users/lxliow/Desktop/13102021/malls.csv")
malls2<- st_as_sf(malls, coords = c("lon", "lat"),crs = 4326)

tmap_mode('view')
#mall location
malllocation <- tm_shape(y.fix) + tm_borders() + tm_shape(subzone) + tm_borders() +tm_shape(population_area) + tm_polygons(col = "TOTAL") + tm_shape(roads) + tm_lines(col = "black", alpha = 0.1) + tm_shape(malls2) + tm_dots(col = 'lightblue')


#KDE
sg <- readOGR("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021", "SGP_adm0")
sg_sp <- as(sg, "SpatialPolygons")
require(spatstat)
x <- readShapeSpatial("bluesg.shp")
bsg2<- as(x,"ppp")
clarkevans.test(bsg2, correction = "none", clipregion = 'sg_sp', alternative = c('two.sided'), nsim = 99)

kf <- envelope(bluesg_ppp,Kest,correction = 'border')
plot(kf)

lf.env <- envelope(bluesg_ppp,Lest,correction = 'border')
plot(lf.env)

require(sp) # the trick is that this package must be loaded!
median_household_income[1,1] <- "ANG MO KIO"
class(median_household_income)

#pop density map
popincome <- read.csv("~/NUS/y3sem1/geospatial analytics/Project_Documents/10102021/popincome.csv")
popincome <- merge(population_area, popincome, by.x = "PLN_AREA_N", by.y = "population_area.PLN_AREA_N")
popincome$income
incomedensitymap<-tm_shape(population_area) + tm_borders() +tm_shape(popincome) + tm_polygons(col = "income", title= "Median Income per Region") + tm_shape(bluesg) + tm_dots(col = 'red',size =0.2)

population2 <- readOGR("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021", "MP14_REGION_WEB_PL")
population2 <- readShapeSpatial("MP14_REGION_WEB_PL.shp")
plot(population2)

#point pattern analysis
require(maptools)
require(spatstat)


planningArea_poly <- st_read("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021/master-plan-2014-planning-area-boundary-no-sea/MP14_PLNG_AREA_NO_SEA_PL.shp")
##create a window of Singapore
planning_area_window <- as.owin(planningArea_poly)
pg = planningArea_poly[planningArea_poly$PLN_AREA_N== "PUNGGOL",]
plot(pg)

bluesg_shp <- readOGR("C:/Users/Wolflx/OneDrive/Documents/NUS/y3sem1/geospatial analytics/Project_Documents/10102021/bluesg", "bluesg")

##convert bluesg shape file to spatial points
bluesg_shp_sp <- as(bluesg_shp, "SpatialPoints")

##the crs systems for planning area and bluesg are not the same
crs(planningArea_poly)
crs(bluesg_shp_sp)
##transform the CRS of bluesg to use the same one as planning area
bluesg_shp_sp <- spTransform(bluesg_shp_sp, CRS("+proj=tmerc +lat_0=1.36666666666667 +lon_0=103.833333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +datum=WGS84 +units=m +no_defs"))

##convert to ppp format using the coordinates of bluesg locations
bluesg_ppp <- ppp(bluesg_shp_sp@coords[,1], bluesg_shp_sp@coords[,2], window=planning_area_window)
plot(bluesg_ppp)


qt <- quadrat.test(bluesg_ppp, nx = 20, ny = 15)
qt

##Complete Spatial Randomness (CSR) for a given point pattern, based on quadrat counts by using quadrat.test() of spatstat
##H0 = The distribution of bluesg parking location are randomly distributed
##H1 = The distribution of bluesg parking location are not randomly distributed
##Confident interval = 95%
##(Default) Chi-squared test of CSR using quadrat counts
plot(bluesg_ppp)
plot(qt, add = TRUE, cex = 0.1)

##Conditional Monte Carlo test of CSR using quadrat counts
quadrat.test(bluesg_ppp, nx = 20, ny = 15, method = "M", nsim = 999)

##Clark-Evans test of aggregation for a spatial point pattern by using clarkevans.test() of spatstat
##H0 = The distribution of bluesg parking location are randomly distributed
##H1 = The distribution of bluesg parking location are not randomly distributed
##Confident interval = 95%
clarkevans.test(bluesg_ppp, correction = "none", clipregion = 'planning_area_window', alternative = c('two.sided'), nsim = 99)


###Computing KDE using automatic bandwidth selection method
kde_bluesg_bw <- density(bluesg_ppp, sigma= bw.diggle, edge=TRUE, kernel= 'gaussian')
plot(kde_bluesg_bw)
kde_bluesg_bw.km <- rescale(bluesg_ppp, 1000, "km")
kde_bluesg_bw.bw <- density(kde_bluesg_bw.km, sigma = bw.diggle, edge = TRUE, kernel = 'gaussian')
plot(kde_bluesg_bw.bw)

#hypothesis testing
# First, we'll run an ANN analysis for bluesg parking locations assuming a uniform point density across the state 
bluesg_ppp.km <- rescale(bluesg_ppp, 1000, "km")
ann.p <- mean(nndist(bluesg_ppp.km, k=1))
ann.p

# The observed average nearest neighbor distance is 0.431 km.
# 
# Next, we will generate the distribution of expected ANN values given a homogeneous (CSR/IRP) point process using Monte Carlo methods. This is our null model.


n     <- 599L
ann.r <- vector(length=n)
for (i in 1:n){
  rand.p   <- rpoint(n=bluesg_ppp.km$n, win=population_area) 
  ann.r[i] <- mean(nndist(rand.p, k=1))
}
# plot the last realization of the homogeneous point process to see what a completely random placement of bluesg parking locations could look like
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

# Next, let's plot the histogram of expected values under the null and add a blue vertical line showing where our observed ANN value lies relative to this distribution.

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")


# Recall that the ANN analysis explores the 2nd order process underlying a point pattern thus requiring that we control for the first order process (e.g. population density distribution).
# This is a non-homogeneous test. Here, we pass the parameter f=pop.km to the function rpoint telling it that the population density raster pop.km should be used to define where a point should be most likely placed (high population density) and least likely placed (low population density) under this new null model. Here, we'll use the non-transformed representation of the population density raster, pop.km.

population_area2 <- as.owin(population_area)
pop  <- as.im(population_area2)

pop.km    <- rescale(pop, 1000, "km")
n     <- 599L
ann.r <- vector(length=n)
for (i in 1:n){
  rand.p   <- rpoint(n=bluesg_ppp.km$n, f=pop.km) # f defines the probability density of the points
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

popincome2 <- as.owin(popincome)
popincome2  <- as.im(popincome2)
popincome2.km    <- rescale(popincome2, 1000, "km")

#Computing a pseudo p-value from the simulation

# First, we need to find the number of simulated ANN values greater than our observed ANN value.
N.greater <- sum(ann.r > ann.p)
#To compute the p-value, find the end of the distribution closest to the observed ANN value, then divide that count by the total count. Note that this is a so-called one-sided P-value.
pvalue <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
pvalue

#Test for a poisson point process model with a covariate effect
# The ANN analysis addresses the 2nd order effect of a point process. Here, we'll address the 1st order process using the poisson point process model.

pop.lg <- log(pop)
pop.lg.km <- rescale(pop.lg, 1000, "km")

#poisson point model
require(spatialEco)
r <- raster(ncol=180, nrow=180)
extent(r) <- extent(population_area)
popdensityraster <- rasterize(population_area, r, 'TOTAL')
popdensityraster <- sp.na.omit(popdensityraster,col.name = "TOTAL") 
plot(popdensityraster)
popdensityraster  <- as.im(popdensityraster)

r2 <- raster(ncol=180, nrow=180)
extent(r2) <- extent(popincome)
incomedensityraster <- rasterize(popincome, r2, 'income')
incomedensityraster  <- as.im(incomedensityraster)


plot(incomedensityraster)
PPM1 <- ppm(bluesg_ppp ~ popdensityraster+incomedensityraster)

?ppm
PPM0 <- ppm(bluesg_ppp ~ 1)
PPM0
PPM2 <- ppm(bluesg_ppp ~ popdensityraster)
PPM3 <- ppm(bluesg_ppp ~incomedensityraster)
anova(PPM0, PPM1,test="LRT")
anova(PPM0, PPM1,PPM2,PPM3, test="LRT")


#spatial autocorrelation
library(spdep)
#find queen neighbours for pop income density
neighbours <- poly2nb(popincome, queen = TRUE)
sub_popincomeQ <- subset(popincome, subset=card(neighbours) > 0)
neighbours <- poly2nb(sub_popincome, queen = TRUE)
neighbours

#Plot queen neighbours links
plot(sub_popincomeQ, border = 'lightgrey')
plot(neighbours, coordinates(sub_popincomeQ), add=TRUE, col='red')


#Find rook neighbours
neighbours2 <- poly2nb(popincome, queen = FALSE)
neighbours2
sub_popincome <- subset(popincome, subset=card(neighbours2) > 0)
neighbours2 <- poly2nb(sub_popincome, queen = FALSE)


#plot rook neighbours
plot(sub_popincome, border = 'lightgrey')
plot(neighbours2, coordinates(sub_popincome), add=TRUE, col='blue')

plot(sub_popincome, border = 'lightgrey')
plot(neighbours, coordinates(sub_popincomeQ), add=TRUE, col='red')
plot(neighbours2, coordinates(sub_popincome), add=TRUE, col='blue')

listw <- nb2listw(neighbours2)
globalMoran <- moran.test(sub_popincome$TOTAL, listw)
globalMoran

globalMoran[["estimate"]][["Moran I statistic"]]

globalMoran[["p.value"]]
# The Moran I statistic is 0.09523379, we can, therefore, determine that there our population income variable is positively auto correlated in Singapore. In other words, the data does spatially cluster. We can also consider the p-value as a measure of the statistical significance of the model.

#local moran I
#moran scatterplot
moran <- moran.plot(sub_popincome$TOTAL, listw = nb2listw(neighbours2, style = "W"))

#compute local moran
local <- localmoran(x = sub_popincome$TOTAL, listw = nb2listw(neighbours2, style = "W"))

moran.map <- cbind(sub_popincome, local)
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic") 

#plot local moran
# binds results to our polygon shapefile
moran.map <- cbind(sub_popincome, local)

#plot LISA clusters
quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.popincome <- sub_popincome$TOTAL - mean(sub_popincome$TOTAL)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.1 

# builds a data quadrant
quadrant[m.popincome >0 & m.local>0] <- 4  
quadrant[m.popincome <0 & m.local<0] <- 1      
quadrant[m.popincome <0 & m.local>0] <- 2
quadrant[m.popincome >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(sub_popincome,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
box()
legend("bottomright", legend = c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n")


#ANN analysis
mean(nndist(bluesg_ppp.km, k=1))

ANN <- apply(nndist(bluesg_ppp.km, k=1:100),2,FUN=mean)
plot(ANN ~ eval(1:100), type="b", main=NULL, las=1)


f.1 <- as.formula(popincome$TOTAL ~ 1)

rfVariogram <- autofitVariogram(f.1, popincome)

plot(rfVariogram)

rf_sph <- autofitVariogram(f.1, popincome,model = c("Sph"))
rf_exp <- autofitVariogram(f.1, popincome,model = c("Exp"))
rf_gau <- autofitVariogram(f.1, popincome,model = c("Gau"))
rf_ste <- autofitVariogram(f.1, popincome,model = c("Ste"))
plot(rf_sph)

rfVariogram$var_model

rf.krg <- krige(f.1, popincome,grd,fVariogram$var_model)




