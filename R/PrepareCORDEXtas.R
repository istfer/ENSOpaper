# This script is to prepeare an ensemble from CORDEX outputs for the East African temperature
# more detailed comments can be found in the similarly structured PrepareCORDEXpr.R script in this directory

library(ncdf4)
library(abind)
library(raster)

# set your directory where all your historical CORDEX outputs for temperature are
setwd("/path/to/your/historical/CORDEX/temperature/files")

# get the file names in the directory
file.names.hs <- list.files()

# open all the netcdf files
tas.nc.hs <- lapply(file.names.hs, nc_open)

# same for the RCP scenario
setwd("/path/to/your/future/CORDEX/temperature/files")
file.names.rcp <- list.files()
tas.nc.rcp     <- lapply(file.names.rcp, nc_open)


# get lat/lon from one (any) of them  
latarr <- ncvar_get(nc=tas.nc.hs[[1]], varid="lat")
lonarr <- ncvar_get(nc=tas.nc.hs[[1]], varid="lon")

# define your window
latmin= -12.25
latmax= 12.25 
lonmin= 28.75 
lonmax= 51.75

# getting index and count values of ncvar_get function
startlat <-  which(latarr==latmin)
startlon <-  which(lonarr==lonmin)
nlat.p   <-  which(latarr==latmax)-which(latarr==latmin)+1
nlon.p   <-  which(lonarr==lonmax)-which(lonarr==lonmin)+1

# read all the temperature values and close files
tas.hs <- sapply(tas.nc.hs,ncvar_get, "tas", start=c(startlon,startlat,1),count=c(nlon.p,nlat.p,-1))
lapply(tas.nc.hs, nc_close)

tas.rcp <- sapply(tas.nc.rcp,ncvar_get, "tas", start=c(startlon,startlat,1),count=c(nlon.p,nlat.p,-1))
lapply(tas.nc.rcp,nc_close)

# take the ensemble mean for each period (CORDEX outputs are provided in seperate ncdf files for these periods)
# 1:1951-1960, 2:1961-1970, 3:1971-1980, 4:1980-1990, 5:1991-2000, 6:2001-2005
t.hs1 <- apply(simplify2array(tas.hs[seq(1,length(file.names.hs),6)]), c(1,2,3), mean)
t.hs2 <- apply(simplify2array(tas.hs[seq(2,length(file.names.hs),6)]), c(1,2,3), mean)
t.hs3 <- apply(simplify2array(tas.hs[seq(3,length(file.names.hs),6)]), c(1,2,3), mean)
t.hs4 <- apply(simplify2array(tas.hs[seq(4,length(file.names.hs),6)]), c(1,2,3), mean)
t.hs5 <- apply(simplify2array(tas.hs[seq(5,length(file.names.hs),6)]), c(1,2,3), mean)
t.hs6 <- apply(simplify2array(tas.hs[seq(6,length(file.names.hs),6)]), c(1,2,3), mean)
tas.hs.ensemble <- abind(t.hs1,t.hs2,t.hs3,t.hs4,t.hs5,t.hs6,along=3)

# 1:2006-2010, 2:2011-2020, 3:2021-2030, 4:2031-2040, 5:2041-2050
# 6:2051-2060, 6:2061-2070, 6:2071-2080, 6:2081-2090, 6:2091-2100
t.rcp1 <- apply(simplify2array(tas.rcp[seq(1,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp2 <- apply(simplify2array(tas.rcp[seq(2,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp3 <- apply(simplify2array(tas.rcp[seq(3,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp4 <- apply(simplify2array(tas.rcp[seq(4,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp5 <- apply(simplify2array(tas.rcp[seq(5,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp6 <- apply(simplify2array(tas.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp7 <- apply(simplify2array(tas.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp8 <- apply(simplify2array(tas.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp9 <- apply(simplify2array(tas.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
t.rcp10 <- apply(simplify2array(tas.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
tas.rcp.ensemble <- abind(t.rcp1,t.rcp2,t.rcp3,t.rcp4,t.rcp5,t.rcp6,t.rcp7,t.rcp8,t.rcp9,t.rcp10,along=3)

tas.ensemble <- abind(tas.hs.ensemble,tas.rcp.ensemble,along=3)

tasEA.rt1 <- aperm(tas.ensemble, c(2,1,3))
tasEA.rt2 <- tasEA.rt1[nrow(tasEA.rt1):1,,]
rastas.ensemble <- brick(tasEA.rt2, xmn=lonmin, xmx=lonmax, ymn=latmin, ymx=latmax, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")



