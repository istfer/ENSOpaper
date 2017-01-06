# This script is to bring Pacific SST outputs from different GCMs to the same spatial resolution and exten,
# and prepare an ensemble them

library(ncdf4)
library(fields)
library(raster)
library(abind)

############## historical ###################

# set your directory where all the historical Pacific SST output files are located
setwd("path/to/your/folder/with/historical/pacificSST/ncfiles")

file.names.hs <- list.files()

# here file.names.hs should be like this
# [1] "ts_Amon_CanESM2_historical_r1i1p1_185001-200512.nc"      
# [2] "ts_Amon_CNRM-CM5_historical_r1i1p1_195001-200512.nc"     
# [3] "ts_Amon_CSIRO-Mk3-6-0_historical_r1i1p1_185001-200512.nc"
# [4] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_195101-195512.nc"   
# [5] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_195601-196012.nc"   
# [6] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_196101-196512.nc"   
# [7] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_196601-197012.nc"   
# [8] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_197101-197512.nc"   
# [9] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_197601-198012.nc"   
# [10] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_198101-198512.nc"   
# [11] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_198601-199012.nc"   
# [12] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_199101-199512.nc"   
# [13] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_199601-200012.nc"   
# [14] "ts_Amon_GFDL-ESM2M_historical_r1i1p1_200101-200512.nc"   
# [15] "ts_Amon_IPSL-CM5A-MR_historical_r1i1p1_185001-200512.nc" 
# [16] "ts_Amon_MIROC5_historical_r1i1p1_185001-201212.nc"       
# [17] "ts_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"   
# [18] "ts_Amon_NorESM1-M_historical_r1i1p1_185001-200512.nc"  

# open each file
pac.nc.hs <- lapply(file.names.hs, nc_open)

# retrieve lat/lon variables from each 
lats <- lapply(pac.nc.hs,ncvar_get,varid="lat")
lons <- lapply(pac.nc.hs,ncvar_get,varid="lon")

# equatorial pacific window
latmin <-  16
latmax <-  -16 
lonmin <-  150 
lonmax <-  290 

# determine the closest window for each GCM and the indices for reading the data

start.lats <- sapply(lats, function(x) which.min(abs(x-latmax)))
start.lons <- sapply(lons, function(x) which.min(abs(x-lonmin)))
end.lats   <- sapply(lats, function(x) which.min(abs(x-latmin)))
end.lons   <- sapply(lons, function(x) which.min(abs(x-lonmax)))

# calculate how many grids to read
nlat.p <-  end.lats-start.lats + 1
nlon.p <-  end.lons-start.lons + 1

# read from ncdf files
pac.SST.hs <- list()
for(i in 1:18){
  pac.SST.hs[[i]]=ncvar_get(pac.nc.hs[[i]], "ts", start=c(start.lons[i],start.lats[i],1), count=c(nlon.p[i],nlat.p[i],-1))
}

# close nc-files
lapply(pac.nc.hs, nc_close)


# prepare rasters 

# function to create raster brick objects
createRasterBrick <- function(pac){
  pac.SST.rt  <- aperm(pac, c(2,1,3))
  pac.SST.rt1 <- pac.SST.rt[nrow(pac.SST.rt):1,,]
  
  # create raster
  raspac <- brick(pac.SST.rt1, xmn=150, xmx=290, ymn=-16, ymx=16, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
  return(raspac)
}


# model-1 : CanESM2
pacCanESM2 <- pac.SST.hs[[1]]
pacCanESM2 <- pacCanESM2[,,1213:1872] # cut for 1951-2005 period
CanESM2    <- createRasterBrick(pacCanESM2)
# dim(CanESM2)
# plot(CanESM2[[1]])


# model-2 : CNRMCM5
pacCNRMCM5 <- pac.SST.hs[[2]]
pacCNRMCM5 <- pacCNRMCM5[,,13:672] # cut for 1951-2005 period
CNRMCM5    <- createRasterBrick(pacCNRMCM5)
# dim(CNRMCM5)
# plot(CNRMCM5[[1]])


# model-3 : CSIRO
pacCSIRO <- pac.SST.hs[[3]]
pacCSIRO <- pacCSIRO[,,1213:1872]
CSIRO    <- createRasterBrick(pacCSIRO)
#dim(CSIRO)
#plot(CSIRO[[1]])

# model-4 : GFDL
pacGFDL <- abind(pac.SST.hs[[4]],pac.SST.hs[[5]],pac.SST.hs[[6]], 
                 pac.SST.hs[[7]],pac.SST.hs[[8]],pac.SST.hs[[9]],
                 pac.SST.hs[[10]],pac.SST.hs[[11]],pac.SST.hs[[12]],
                 pac.SST.hs[[13]],pac.SST.hs[[14]],along=3)
GFDL <- createRasterBrick(pacGFDL)
#dim(GFDL)
#plot(GFDL[[1]])


# model-5 : IPSL
pacIPSL <- pac.SST.hs[[15]]
pacIPSL <- pacIPSL[,,1213:1872]
IPSL    <- createRasterBrick(pacIPSL)
#dim(IPSL)
#plot(IPSL[[1]])


# model-6 : MIROC5
pacMIROC5 <- pac.SST.hs[[16]]
pacMIROC5 <- pacMIROC5[,,1213:1872]
MIROC5    <- createRasterBrick(pacMIROC5)
#dim(MIROC5)
#plot(MIROC5[[1]])


# model-7 : MPI-ESM-LR
pacMPIESMLR <- pac.SST.hs[[17]]
pacMPIESMLR <- pacMPIESMLR[,,1213:1872]
MPIESMLR    <- createRasterBrick(pacMPIESMLR)
#dim(MPIESMLR)
#plot(MPIESMLR[[1]])


# model-8 : NorESM1-M
pacNorESM1 <- pac.SST.hs[[18]]
pacNorESM1 <- pacNorESM1[,,1213:1872]
NorESM1    <- createRasterBrick(pacNorESM1)
#dim(NorESM1)
#plot(NorESM1[[1]])

# function to resize GCM rasters same as NOAA ERSST
resizePacificRaster <- function(raspac){
  e    <- extent(150, 290, -16, 16)
  s    <- raster(e, nrows=17, ncols=71, crs=raspac@crs)
  rems <- resample(raspac, s, method="bilinear")
  return(rems)
}

# resize all, same as NOAA
CanESM2.r  <- resizePacificRaster(CanESM2)
CNRMCM5.r  <- resizePacificRaster(CNRMCM5)
CSIRO.r    <- resizePacificRaster(CSIRO)
GFDL.r     <- resizePacificRaster(GFDL)
IPSL.r     <- resizePacificRaster(IPSL)
MIROC5.r   <- resizePacificRaster(MIROC5)
MPIESMLR.r <- resizePacificRaster(MPIESMLR)
NorESM1.r  <- resizePacificRaster(NorESM1)


# take ensemble mean
historical_ensemble <- CanESM2.r # place holder for dimensions
for(i in 1:660){
  historical_ensemble[[i]]<- mean(CanESM2.r[[i]],
                                  CNRMCM5.r[[i]],
                                  CSIRO.r[[i]],
                                  GFDL.r[[i]],
                                  IPSL.r[[i]],
                                  MIROC5.r[[i]],
                                  MPIESMLR.r[[i]],
                                  NorESM1.r[[i]])
}

# dim(historical_ensemble)
# plot(historical_ensemble[[1]])

############## future ###################

# set your directory where all the future pacific SST files are located
setwd("path/to/your/folder/with/historical/pacificSST/ncfiles")
setwd("~/Desktop/paper2_future/submission/data_code/future_pacific/rcp85/8models")

file.names.rcp <- list.files()

# file.names.rcp should look like this: 
# [1] "ts_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc"      
# [2] "ts_Amon_CNRM-CM5_rcp85_r1i1p1_200601-205512.nc"     
# [3] "ts_Amon_CNRM-CM5_rcp85_r1i1p1_205601-210012.nc"     
# [4] "ts_Amon_CSIRO-Mk3-6-0_rcp85_r1i1p1_200601-210012.nc"
# [5] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_200601-201012.nc"   
# [6] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_201101-201512.nc"   
# [7] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_201601-202012.nc"   
# [8] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_202101-202512.nc"   
# [9] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_202601-203012.nc"   
# [10] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_203101-203512.nc"   
# [11] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_203601-204012.nc"   
# [12] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_204101-204512.nc"   
# [13] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_204601-205012.nc"   
# [14] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_205101-205512.nc"   
# [15] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_205601-206012.nc"   
# [16] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_206101-206512.nc"   
# [17] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_206601-207012.nc"   
# [18] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_207101-207512.nc"   
# [19] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_207601-208012.nc"   
# [20] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_208101-208512.nc"   
# [21] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_208601-209012.nc"   
# [22] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_209101-209512.nc"   
# [23] "ts_Amon_GFDL-ESM2M_rcp85_r1i1p1_209601-210012.nc"   
# [24] "ts_Amon_IPSL-CM5A-MR_rcp85_r1i1p1_200601-210012.nc" 
# [25] "ts_Amon_MIROC5_rcp85_r1i1p1_200601-210012.nc"       
# [26] "ts_Amon_MPI-ESM-LR_rcp85_r1i1p1_200601-210012.nc"   
# [27] "ts_Amon_NorESM1-M_rcp85_r1i1p1_200601-210012.nc"  

# open each file
pac.nc.rcp <- lapply(file.names.rcp,nc_open)

# get lat/lon
lats       <- lapply(pac.nc.rcp, ncvar_get, varid="lat")
lons       <- lapply(pac.nc.rcp, ncvar_get, varid="lon")

# equatorial pacific window
latmin <-  16
latmax <-  -16 
lonmin <-  150 
lonmax <-  290 

# find indices
start.lons <- sapply(lons, function(x) which.min(abs(x-lonmin)))
start.lats <- sapply(lats, function(x) which.min(abs(x-latmax)))
end.lons   <- sapply(lons, function(x) which.min(abs(x-lonmax)))
end.lats   <- sapply(lats, function(x) which.min(abs(x-latmin)))

# how many grids to count
nlat.p <-  end.lats-start.lats + 1
nlon.p <-  end.lons-start.lons + 1

# read climate data
pac.SST.rcp <- list()
for(i in 1:27){
  pac.SST.rcp[[i]]=ncvar_get(pac.nc.rcp[[i]],"ts",start=c(start.lons[i],start.lats[i],1),count=c(nlon.p[i],nlat.p[i],-1))
}


# read and prepare raster

# model-1 : CanESM2
pacCanESM2.ft <- pac.SST.rcp[[1]]
CanESM2.ft    <- createRasterBrick(pacCanESM2.ft)
# dim(CanESM2.ft)
# plot(CanESM2.ft[[1]])


# model-2 : CNRMCM5
pacCNRMCM5.ft <- abind(pac.SST.rcp[[2]],pac.SST.rcp[[3]],along=3)
CNRMCM5.ft    <- createRasterBrick(pacCNRMCM5.ft)
# dim(CNRMCM5.ft)
# plot(CNRMCM5.ft[[1]])


# model-3 : CSIRO
pacCSIRO.ft <- pac.SST.rcp[[4]]
CSIRO.ft    <- createRasterBrick(pacCSIRO.ft)
#dim(CSIRO.ft)
#plot(CSIRO.ft[[1]])

# model-4 : GFDL
pacGFDL.ft <- abind(pac.SST.rcp[[5]],pac.SST.rcp[[6]],pac.SST.rcp[[7]], 
                    pac.SST.rcp[[8]],pac.SST.rcp[[9]],pac.SST.rcp[[10]],
                    pac.SST.rcp[[11]],pac.SST.rcp[[12]],pac.SST.rcp[[13]],
                    pac.SST.rcp[[14]],pac.SST.rcp[[15]],pac.SST.rcp[[16]],
                    pac.SST.rcp[[17]],pac.SST.rcp[[18]],pac.SST.rcp[[19]],
                    pac.SST.rcp[[20]],pac.SST.rcp[[21]],pac.SST.rcp[[22]],
                    pac.SST.rcp[[23]],along=3)
GFDL.ft <- createRasterBrick(pacGFDL.ft)
# dim(GFDL.ft)
# plot(GFDL.ft[[1]])

# model-5 : IPSL
pacIPSL.ft <- pac.SST.rcp[[24]]
IPSL.ft    <- createRasterBrick(pacIPSL.ft)
# dim(IPSL.ft)
# plot(IPSL.ft[[1]])


# model-6 : MIROC5
pacMIROC5.ft <- pac.SST.rcp[[25]]
MIROC5.ft    <- createRasterBrick(pacMIROC5.ft)
# dim(MIROC5.ft)
# plot(MIROC5.ft[[1]])

 
# model-7 : MPI-ESM-LR
pacMPIESMLR.ft <- pac.SST.rcp[[26]]
MPIESMLR.ft    <- createRasterBrick(pacMPIESMLR.ft)
#dim(MPIESMLR.ft)
#plot(MPIESMLR.ft[[1]])



# model-8 : NorESM1-M
pacNorESM1.ft <- pac.SST.rcp[[27]]
NorESM1.ft    <- createRasterBrick(pacNorESM1.ft)
#dim(NorESM1.ft)
#plot(NorESM1.ft[[1]])

# resize all, same as NOAA
CanESM2.ftr  <- resizePacificRaster(CanESM2.ft)
CNRMCM5.ftr  <- resizePacificRaster(CNRMCM5.ft)
CSIRO.ftr    <- resizePacificRaster(CSIRO.ft)
GFDL.ftr     <- resizePacificRaster(GFDL.ft)
IPSL.ftr     <- resizePacificRaster(IPSL.ft)
MIROC5.ftr   <- resizePacificRaster(MIROC5.ft)
MPIESMLR.ftr <- resizePacificRaster(MPIESMLR.ft)
NorESM1.ftr  <- resizePacificRaster(NorESM1.ft)


# take ensemble mean
future_ensemble <- CanESM2.ftr # place holder for dimensions
for(i in 1:1140){
  future_ensemble[[i]]<- mean(CanESM2.ftr[[i]],
                              CNRMCM5.ftr[[i]],
                              CSIRO.ftr[[i]],
                              GFDL.ftr[[i]],
                              IPSL.ftr[[i]],
                              MIROC5.ftr[[i]],
                              MPIESMLR.ftr[[i]],
                              NorESM1.ftr[[i]])
}

# if you stack both historical and future ensemble together you can have a continious time series
raspacSST <- stack(historical_ensemble, future_ensemble)

# now you can use your Pacific SST ensembles in EOT analysis with CORDEX East Africa precipitation ensemble
# see PrepareCORDEXensemblePRE.R for preparing the CORDEX data in a similar way

