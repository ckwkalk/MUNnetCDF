## munging netCDF data to get environmental variables to produce habitat density maps
library("ncdf4")
nWph2011<-nc_open("F:\\Devotes_GIS\\MetO-NWS-REAN-PHYS-monthly_1411121212536.nc")

dname <- "votemper"
tmp.array <- ncvar_get(nWph2011, dname)
tmp.vec.long <- as.vector(tmp.array)

lon <- ncvar_get(nWph2011, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nWph2011, "lat")
nlat <- dim(lat)
head(lat)

t <- ncvar_get(nWph2011, "time")
tunits <- ncatt_get(nWph2011, "time", "units")
nt <- dim(t)

depth <- ncvar_get(nWph2011, "depth")
ndepth <- dim(depth)
head(depth)


dtmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat *ndepth, ncol = nt)

lonlatd <- expand.grid(lon, lat, depth)

dtmp.df02 <- data.frame(cbind(lonlatd, dtmp.mat))

names(dtmp.df02) <- c("lon", "lat", "level", "tmp1", "tmp2", "tmp3", "tmp4", "tmp5", 
                      "tmp6", "tmp7", "tmp8", "tmp9", "tmp10", "tmp11", "tmp12", "tmp13", "tmp14")

## create index fot lat lon cells
dtmp.df02$idx <- as.integer(interaction(dtmp.df02$lat, dtmp.df02$lon))


## two methods to select only the deepest values 1 using dplyr 1 using table

## dplyr - needed to convert the format of lon lat buy exporting to csv and re-importing
library("dplyr")

write.csv(dtmp1, file="dtmp1.csv")
dtmp1 <- read.csv("~/dtmp1.csv")

result <- dtmp1 %.% group_by(idx) %.%
  filter(level == max(level)) %.%
  arrange(lon,lat,idx,level,tmp1)
plot(result$lat~result$lon, col=result$level)


## data table method**preffered - quicker but less understandable?
library("data.table")
ttmp1<-as.data.table(dtmp.df02)
##get rid of all the NAs
cttmp1<-ttmp1[complete.cases(ttmp1),]
##select the maximum level per row
ntt1<-cttmp1[cttmp1[, .I[level == max(level)], by=idx]$V1]
##test it works 
plot(ntt1$lat~ntt1$lon, col=ntt1$level)
plot(ntt1$lat~ntt1$lon, col=ntt1$tmp14)


##calculate sd for the rows
##ned to convert to dataframe
n1<-as.data.frame(ntt1)
n1$sd<-apply(n1[4:17],1, sd, na.rm=TRUE)
n1$max<-apply(n1[4:17],1, max, na.rm=TRUE)
n1$min<-apply(n1[4:17],1, min, na.rm=TRUE)
n1$mean<-apply(n1[4:17],1, mean, na.rm=TRUE)

install.packages("moments")
library("moments")
n1$kurt<-apply(n1[4:17],1, kurtosis, na.rm=TRUE)
hist(n1$kurt)
n1$skew<-apply(n1[4:17],1, skewness, na.rm=TRUE)
hist(n1$skew)

##optional plots
plot(n1$lat~n1$lon, col=heat.colors(length(n1$kurt))[rank(n1$kurt)])
plot(n1$lat~n1$lon, col=heat.colors(length(n1$skew))[rank(n1$skew)])
plot(n1$lat~n1$lon, col=heat.colors(length(n1$min))[rank(n1$min)])
plot(n1$lat~n1$lon, col=heat.colors(length(n1$max))[rank(n1$max)])
plot(n1$lat~n1$lon, col=heat.colors(length(n1$sd))[rank(n1$sd)])



