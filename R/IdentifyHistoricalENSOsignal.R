library(ncdf4)
library(remote)

###### prepare pacific SST raster brick #####

# download the NOAA ERSST 1854-present 2x2 global dataset
# noaaERSSTurl <- "ftp://ftp.cdc.noaa.gov/Datasets/noaa.ersst/sst.mnmean.v4.nc"
# set the working directory where the NOAA ERSST data is
setwd("/path/to/ersst/data")
# open netCDF for reading
sst = nc_open("sst.mnmean.v4.nc")

# retrieve lat/lon
lat.p=ncvar_get(nc=sst,varid="lat")
lon.p=ncvar_get(nc=sst,varid="lon")

# equatorial pacific window
latmin= 16
latmax= -16 
lonmin=  150 
lonmax= 290 

# define temporal window
startyear = 1951
endyear = 2005

# indices and counts for get.var.ncdf
startlat = which(lat.p==latmin)
startlon = which(lon.p==lonmin)
startmonth = (startyear-1854)*12 +1

nlat.p = which(lat.p==latmax)-which(lat.p==latmin)+1
nlon.p = which(lon.p==lonmax)-which(lon.p==lonmin)+1
nyear.p = endyear-startyear+1
nmonths.p=nyear.p*12

# read the SST data
pac.SST=ncvar_get(sst,"sst",start=c(startlon,startlat,startmonth),count=c(nlon.p,nlat.p,nmonths.p))
nc_close(sst)

# rotating step
pac.SST.rt <- aperm(pac.SST, c(2,1,3))

# create raster
pacificSST <- brick(pac.SST.rt,xmn=150,xmx=290,ymn=-16,ymx=16, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# plot(pacificSST[[1]])


###### prepare CRU precipitation raster brick #####

# download the CRU TS3.2 1901-2011 0.5x0.5 global dataset http://badc.nerc.ac.uk/data/cru/
# set the working directory where the CRU precipitation data is
setwd("/path/to/cru/precipitation/data")


# open netCDF for reading
pre.nc=nc_open("cru_ts3.20.1901.2011.pre.dat.nc")

# retrieve lat/lon
lats=ncvar_get(nc=pre.nc,varid="lat")
lons=ncvar_get(nc=pre.nc,varid="lon")

# define spatial window
# e.g. for East Africa : 12N-12S, 29E-52E 
lat.min= -12.25 
lat.max= 12.25 
lon.min= 28.75 
lon.max= 51.75 


# define temporal window
# same as above
start.year = 1951
end.year = 2005


# indices and counts for get.var.ncdf

start.lat = which(lats==lat.min)
start.lon = which(lons==lon.min)
start.month = (start.year-1901)*12 +1

nlat = which(lats==lat.max)-which(lats==lat.min)+1
nlon = which(lons==lon.max)-which(lons==lon.min)+1

nyear = end.year-start.year+1
nmonths=nyear*12

# read precipitation data
preEA=ncvar_get(pre.nc,"pre",start=c(start.lon,start.lat,start.month),count=c(nlon,nlat,nmonths))
nc_close(pre.nc)

# rotate
preEA.rt1 <- aperm(preEA, c(2,1,3))
preEA.rt2 <- preEA.rt1[nrow(preEA.rt1):1,,]

# create raster brick
cruEastAfrica <- brick(preEA.rt2,xmn=lon.min,xmx=lon.max,ymn=lat.min,ymx=lat.max,crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
# plot(cruEastAfrica[[1]])


###### EOT calculations #####

# deseason response and predictor
cruEastAfrica.ds <- deseason(cruEastAfrica, cycle.window = 12)
pacificSST.ds <- deseason(pacificSST, cycle.window = 12)

# denoise response and predictor
cruEastAfrica.dns <- denoise(cruEastAfrica.ds, expl.var = 0.9)
pacificSST.dns <- denoise(pacificSST.ds, expl.var = 0.9)

# calculate EOT modes
EA_modes <- eot(x = pacificSST.dns, y = cruEastAfrica.dns, n = 3,
                reduce.both = TRUE, standardised = FALSE,
                verbose = TRUE)


# first mode is ENSO
plot(EA_modes, y = 1, 
     show.bp = TRUE, 
     arrange = "long")


###### making sure that the obtained 1st eot is ENSO ######

# Download Nino3.4 Index from
# https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino34.long.anom.data
# Can be found under "data" directory in this repository
nino <- read.table("nino34.long.anom.txt")
# extract 1951-2005
nino5105 <- nino[which(nino$V1==1951):which(nino$V1==2005),]
# head(nino5105)
nino.ind <- c(t(nino5105[2:13]))
# correlation
cor(nino.ind, EA_modes@modes$mode_01@eot)



########## save the prepared datasets ########## 
# raw
save(cruEastAfrica, file = "/path/to/your/raw/historical_CRU_precipitation_r.Rdata")
save(pacificSST,    file = "/path/to/your/raw/historical_Pacific_SST_r.Rdata")
# deseasoned and denoised
save(cruEastAfrica.dns, file = "/path/to/your/deseasoned/and/denoised/historical_CRU_precipitation_dns.Rdata")
save(pacificSST.dns,    file = "/path/to/your/deseasoned/and/denoised/historical_Pacific_SST_dns.Rdata")