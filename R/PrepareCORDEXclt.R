# This script is to prepeare an ensemble from CORDEX outputs for the East African cloudiness
# more detailed comments can be found in the similarly structured PrepareCORDEXpr.R script in this directory

library(ncdf4)
library(abind)
library(raster)

# set your directory where all your historical CORDEX outputs for clt are
setwd("/path/to/your/historical/CORDEX/clt/files")

file.names.hs <- list.files()
clt.nc.hs     <- lapply(file.names.hs, nc_open)

setwd("/path/to/your/future/CORDEX/clt/files")
file.names.rcp <- list.files()
clt.nc.rcp     <- lapply(file.names.rcp, nc_open)

latarr <- ncvar_get(nc=clt.nc.hs[[1]],varid="lat")
lonarr <- ncvar_get(nc=clt.nc.hs[[1]],varid="lon")

latmin <-  -12.25
latmax <-  12.25 
lonmin <-  28.75 
lonmax <-  51.75

startlat <-  which(latarr==latmin)
startlon <-  which(lonarr==lonmin)

nlat.p <-  which(latarr==latmax)-which(latarr==latmin)+1
nlon.p <-  which(lonarr==lonmax)-which(lonarr==lonmin)+1


clt.hs <- sapply(clt.nc.hs, ncvar_get, "clt", start=c(startlon,startlat,1), count=c(nlon.p,nlat.p,-1))
lapply(clt.nc.hs, nc_close)

clt.rcp <- sapply(clt.nc.rcp, ncvar_get, "clt", start=c(startlon,startlat,1), count=c(nlon.p,nlat.p,-1))
lapply(clt.nc.rcp, nc_close)

# 1:1951-1960, 2:1961-1970, 3:1971-1980, 4:1980-1990, 5:1991-2000, 6:2001-2005
c.hs1 <- apply(simplify2array(clt.hs[seq(1,length(file.names.hs),6)]), c(1,2,3), mean)
c.hs2 <- apply(simplify2array(clt.hs[seq(2,length(file.names.hs),6)]), c(1,2,3), mean)
c.hs3 <- apply(simplify2array(clt.hs[seq(3,length(file.names.hs),6)]), c(1,2,3), mean)
c.hs4 <- apply(simplify2array(clt.hs[seq(4,length(file.names.hs),6)]), c(1,2,3), mean)
c.hs5 <- apply(simplify2array(clt.hs[seq(5,length(file.names.hs),6)]), c(1,2,3), mean)
c.hs6 <- apply(simplify2array(clt.hs[seq(6,length(file.names.hs),6)]), c(1,2,3), mean)
clt.hs.ensemble <- abind(c.hs1,c.hs2,c.hs3,c.hs4,c.hs5,c.hs6,along=3)

# 1:2006-2010, 2:2011-2020, 3:2021-2030, 4:2031-2040, 5:2041-2050
# 6:2051-2060, 6:2061-2070, 6:2071-2080, 6:2081-2090, 6:2091-2100
c.rcp1 <- apply(simplify2array(clt.rcp[seq(1,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp2 <- apply(simplify2array(clt.rcp[seq(2,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp3 <- apply(simplify2array(clt.rcp[seq(3,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp4 <- apply(simplify2array(clt.rcp[seq(4,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp5 <- apply(simplify2array(clt.rcp[seq(5,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp6 <- apply(simplify2array(clt.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp7 <- apply(simplify2array(clt.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp8 <- apply(simplify2array(clt.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp9 <- apply(simplify2array(clt.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
c.rcp10 <- apply(simplify2array(clt.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
clt.rcp.ensemble <- abind(c.rcp1,c.rcp2,c.rcp3,c.rcp4,c.rcp5,c.rcp6,c.rcp7,c.rcp8,c.rcp9,c.rcp10,along=3)

clt.ensemble <- abind(clt.hs.ensemble,clt.rcp.ensemble,along=3)

cltEA.rt1 <- aperm(clt.ensemble, c(2,1,3))
cltEA.rt2 <- cltEA.rt1[nrow(cltEA.rt1):1,,]
rasclt.ensemble <- brick(cltEA.rt2, xmn=lonmin, xmx=lonmax, ymn=latmin, ymx=latmax, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

