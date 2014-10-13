#########################################################################
#Balanced sampling to get even distribution of samples through feature space
#########################################################################
install.packages("sampling")
library("sampling", lib.loc="~/R/win-library/3.0")
##output of environmental variable analysis e.g. water temperature salinity etc
attach(n1)

##weighted distribution with probability for inclusion based on StDev value
##see https://docs.google.com/a/oceandtm.com/file/d/0ByK_t_0IPCScWlVQQjRPU1hiTzQ/edit?usp=drivesdk
##page 47 (orig link=http://www.eustat.es/productosServicios/52.2_balanced_sampling.pdf)
X<-cbind(xsd,lon,lat,level,tmp1,tmp7,tmp14,one=rep(1,length(xsd)))
pik=inclusionprobabilities(X[,1],100)
s=samplecube(X,pik)
##export the selected samples and join them to the summary stats 
temp_sel<-n1[,c("lon","lat","level","xsd")]
temp_sel$prob<-s


sample_pts <- temp_sel[ which(temp_sel$prob > 0), ]

  



##Random weighted sampling based on raster values

install.packages('raster') 
library(raster) 
r <- raster(file.choose()) # note forward slashes in path, not backslashes 
n <- 100 
cells <- sample(x=seq_len(ncell(r)), size=n, prob=values(r)) 
d <- data.frame(popdens=values(r)[cells]) 
coords <- xyFromCell(r, cells, spatial=TRUE) # NB: CRS here is WGS84... you can get the appropriate proj4 string from www.spatialreference.org 
pts <- SpatialPointsDataFrame(coords, d, proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) 
writeOGR(pts, '.', 'randpts100v3', 'ESRI Shapefile')
plot(pts)

Now, with comments to explain what's going on: 
install.packages('raster') library(raster) ## Create dummy raster (see commented line below for reading in true data) 
# This creates a 10000 x 100 matrix of random uniform numbers between 0 and 1000. 
#r <- raster(matrix(runif(1000000, 0, 1000), ncol=100)) 
r <- raster('path/to/raster') # note forward slashes in path, not backslashes 
## Random sample of n raster cells, weighted by pop density (i.e. weighted by the values of the raster). 
# Here, we take a sample of size n from the numbers 1 through 1,000,000 (seq_len(ncell(r)) produces a vector of the integers from 1 to the number of cells of r), where the probability weights of each of those numbers is equal to the values of r, i.e. our dummy population density data. 
n <- 1000 
cells <- sample(x=seq_len(ncell(r)), size=n, prob=values(r)) 
# We then create a data.frame with a single column, popdens, which contains just the pop density for the sampled cells. values(r)[cells] subsets the vector of pop densities, values(r), to just those values that correspond to the sampled cells. 
d <- data.frame(popdens=values(r)[cells]) 
## Extract lat & lon of the chosen cells and return as a SpatialPoints object 
# The xyFromCell function identifies the cell-center coordinates for given cells (in this case, cells, the cell numbers of the sampled cells) of the raster object (in this case r, our raster of pop densities). Setting spatial=TRUE returns the object as a SpatialPoints object, which facilitates writing out to a shapefile. 
coords <- xyFromCell(r, cells, spatial=TRUE) ## combine coords with data to create SpatialPointsDataFrame object 
# NB: CRS here is WGS84... you can get appropriate proj4 string from www.spatialreference.org 
pts <- SpatialPointsDataFrame(coords, d, proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) 
## write out to ESRI shapefile # requires gdal installed on system 
writeOGR(pts, '.', 'testpopdens', 'ESRI Shapefile')