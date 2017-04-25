# This script is to prepeare an ensemble from CORDEX outputs for the East African rainfall

library(ncdf4)
library(abind)
library(raster)

# set your working directory to where the CORDEX outputs for historical simulations are
# these data can be downladed from ESGF data portal https://pcmdi.llnl.gov/search/esgf-llnl/
# 25/04/2017 UPDATE: ESGF server has been renamed to esgf-node.llnl.gov
# data can be downloaded from https://esgf-node.llnl.gov/search/esgf-llnl/

setwd("/path/to/your/historical/CORDEX/precipitation/files")

# get file names
file.names.hs <- list.files()
# file.names.hs should look like this
# [1] "pr_AFR-44i_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"            
# ... CanESM2    
# [6] "pr_AFR-44i_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"            
# [7] "pr_AFR-44i_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"    
# ...  CNRM-CM5  
# [12] "pr_AFR-44i_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"    
# [13] "pr_AFR-44i_CSIRO-QCCCE-CSIRO-Mk3-6-0_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"
# ...  Mk3-6-0
# [18] "pr_AFR-44i_CSIRO-QCCCE-CSIRO-Mk3-6-0_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"
# [19] "pr_AFR-44i_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22T_v1_mon_195101-196012.nc"       
# ... EC-EARTH 
# [24] "pr_AFR-44i_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22T_v1_mon_200101-200512.nc"       
# [25] "pr_AFR-44i_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"        
# ... CM5A-MR      
# [30] "pr_AFR-44i_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"        
# [31] "pr_AFR-44i_MIROC-MIROC5_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"             
# ... MIROC5          
# [36] "pr_AFR-44i_MIROC-MIROC5_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"             
# [37] "pr_AFR-44i_MPI-M-MPI-ESM-LR_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"         
# ... MPI-ESM-LR     
# [42] "pr_AFR-44i_MPI-M-MPI-ESM-LR_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"         
# [43] "pr_AFR-44i_NCC-NorESM1-M_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"            
# ... NorESM1      
# [48] "pr_AFR-44i_NCC-NorESM1-M_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"            
# [49] "pr_AFR-44i_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_mon_195101-196012.nc"     
# ... GFDL    
# [54] "pr_AFR-44i_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_mon_200101-200512.nc"  


# open netcdf files
pre.nc.hs <- lapply(file.names.hs, nc_open)

# set your working directory to where the CORDEX outputs for RCP8.5 simulations are
setwd("/path/to/your/future/CORDEX/precipitation/files")


# get file names
file.names.rcp <- list.files()
# file.names.rcp should look like this
# [1] "pr_AFR-44i_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"            
# ... CanESM2           
# [10] "pr_AFR-44i_CCCma-CanESM2_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"            
# [11] "pr_AFR-44i_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"    
# ... CNRM-CM5  
# [20] "pr_AFR-44i_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"    
# [21] "pr_AFR-44i_CSIRO-QCCCE-CSIRO-Mk3-6-0_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"
# ... Mk3-6-0
# [30] "pr_AFR-44i_CSIRO-QCCCE-CSIRO-Mk3-6-0_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"
# [31] "pr_AFR-44i_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22T_v1_mon_200601-201012.nc"       
# ... EC-EARTH      
# [40] "pr_AFR-44i_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22T_v1_mon_209101-210012.nc"       
# [41] "pr_AFR-44i_IPSL-IPSL-CM5A-MR_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"        
# ... CM5A-MR      
# [50] "pr_AFR-44i_IPSL-IPSL-CM5A-MR_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"        
# [51] "pr_AFR-44i_MIROC-MIROC5_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"             
# ... MIROC5            
# [60] "pr_AFR-44i_MIROC-MIROC5_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"             
# [61] "pr_AFR-44i_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"         
# ... MPI-ESM-LR       
# [70] "pr_AFR-44i_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"         
# [71] "pr_AFR-44i_NCC-NorESM1-M_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"            
# ... NorESM1           
# [80] "pr_AFR-44i_NCC-NorESM1-M_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"            
# [81] "pr_AFR-44i_NOAA-GFDL-GFDL-ESM2M_rcp85_r1i1p1_SMHI-RCA4_v1_mon_200601-201012.nc"     
# ... GFDL    
# [90] "pr_AFR-44i_NOAA-GFDL-GFDL-ESM2M_rcp85_r1i1p1_SMHI-RCA4_v1_mon_209101-210012.nc"     

# open netcdf files
pre.nc.rcp <- lapply(file.names.rcp, nc_open)

# retrieve lat/lon, CORDEX outputs are already standardized 
# therefore lat/lons in each are the same, reading from one of them is enough
latarr <- ncvar_get(nc = pre.nc.hs[[1]], varid="lat")
lonarr <- ncvar_get(nc = pre.nc.hs[[1]], varid="lon")

# East Africa, spatial window
latmin <-  -12.25
latmax <-  12.25 
lonmin <-  28.75 
lonmax <-  51.75

# calculate the indices and number of grids to read from files
startlat <-  which(latarr==latmin)
startlon <-  which(lonarr==lonmin)
nlat.p   <-  which(latarr==latmax)-which(latarr==latmin)+1
nlon.p   <-  which(lonarr==lonmax)-which(lonarr==lonmin)+1

# read historical precipitation
pre.hs <- sapply(pre.nc.hs,ncvar_get, "pr", start=c(startlon,startlat,1),count=c(nlon.p,nlat.p,-1))
# close netcdf files
lapply(pre.nc.hs, nc_close)

# read future precipitation and close files
pre.rcp <- sapply(pre.nc.rcp,ncvar_get,"pr",start=c(startlon,startlat,1),count=c(nlon.p,nlat.p,-1))
lapply(pre.nc.rcp,nc_close)

# take ensemble mean of all models for historical period
# 1:1951-1960, 2:1961-1970, 3:1971-1980, 4:1981-1990, 5:1991-2000,  6:2001-2005
p.hs1 <- apply(simplify2array(pre.hs[seq(1,length(file.names.hs),6)]), c(1,2,3), mean)
p.hs2 <- apply(simplify2array(pre.hs[seq(2,length(file.names.hs),6)]), c(1,2,3), mean)
p.hs3 <- apply(simplify2array(pre.hs[seq(3,length(file.names.hs),6)]), c(1,2,3), mean)
p.hs4 <- apply(simplify2array(pre.hs[seq(4,length(file.names.hs),6)]), c(1,2,3), mean)
p.hs5 <- apply(simplify2array(pre.hs[seq(5,length(file.names.hs),6)]), c(1,2,3), mean)
p.hs6 <- apply(simplify2array(pre.hs[seq(6,length(file.names.hs),6)]), c(1,2,3), mean)
# and bind them in order to obtain a continuous record from 1951 to 2005
pre.hs.ensemble <- abind(p.hs1,p.hs2,p.hs3,p.hs4,p.hs5,p.hs6,along=3)

# take ensemble mean of all models for future period
# 1:2006-2010, 2:2011-2020, 3:2021-2030, 4:2031-2040, 5:2041-2050
# 6:2051-2060, 6:2061-2070, 6:2071-2080, 6:2081-2090, 6:2091-2100
p.rcp1 <- apply(simplify2array(pre.rcp[seq(1,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp2 <- apply(simplify2array(pre.rcp[seq(2,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp3 <- apply(simplify2array(pre.rcp[seq(3,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp4 <- apply(simplify2array(pre.rcp[seq(4,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp5 <- apply(simplify2array(pre.rcp[seq(5,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp6 <- apply(simplify2array(pre.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp7 <- apply(simplify2array(pre.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp8 <- apply(simplify2array(pre.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp9 <- apply(simplify2array(pre.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
p.rcp10 <- apply(simplify2array(pre.rcp[seq(6,length(file.names.rcp),10)]), c(1,2,3), mean)
# and bind them in order to obtain a continuous record from 2006 to 2100
pre.rcp.ensemble <- abind(p.rcp1,p.rcp2,p.rcp3,p.rcp4,p.rcp5,p.rcp6,p.rcp7,p.rcp8,p.rcp9,p.rcp10,along=3)

# you can also combine pre.hs.ensemble and pre.rcp.ensemble to obtain a record from 1951 to 2100
# pre.ensemble <- abind(pre.hs.ensemble,pre.rcp.ensemble,along=3)

m1<-seq(1,1800,by=12)
m2<-seq(2,1800,by=12)
m3<-seq(3,1800,by=12)
m4<-seq(4,1800,by=12)
m5<-seq(5,1800,by=12)
m6<-seq(6,1800,by=12)
m7<-seq(7,1800,by=12)
m8<-seq(8,1800,by=12)
m9<-seq(9,1800,by=12)
m10<-seq(10,1800,by=12)
m11<-seq(11,1800,by=12)
m12<-seq(12,1800,by=12)

# changin precipitation flux to precipitation mm/month
for (i in 1:55){
  pre.hs.ensemble[,,m1[i]] <- pre.hs.ensemble[,,m1[i]]*60*60*24*31
  pre.hs.ensemble[,,m2[i]] <- pre.hs.ensemble[,,m2[i]]*60*60*24*28
  pre.hs.ensemble[,,m3[i]] <- pre.hs.ensemble[,,m3[i]]*60*60*24*31 
  pre.hs.ensemble[,,m4[i]] <- pre.hs.ensemble[,,m4[i]]*60*60*24*30 
  pre.hs.ensemble[,,m5[i]] <- pre.hs.ensemble[,,m5[i]]*60*60*24*31
  pre.hs.ensemble[,,m6[i]] <- pre.hs.ensemble[,,m6[i]]*60*60*24*30
  pre.hs.ensemble[,,m7[i]] <- pre.hs.ensemble[,,m7[i]]*60*60*24*31
  pre.hs.ensemble[,,m8[i]] <- pre.hs.ensemble[,,m8[i]]*60*60*24*31
  pre.hs.ensemble[,,m9[i]] <- pre.hs.ensemble[,,m9[i]]*60*60*24*30
  pre.hs.ensemble[,,m10[i]] <- pre.hs.ensemble[,,m10[i]]*60*60*24*31
  pre.hs.ensemble[,,m11[i]] <- pre.hs.ensemble[,,m11[i]]*60*60*24*30
  pre.hs.ensemble[,,m12[i]] <- pre.hs.ensemble[,,m12[i]]*60*60*24*31
}


for (i in 1:95){
  pre.rcp.ensemble[,,m1[i]] <- pre.rcp.ensemble[,,m1[i]]*60*60*24*31
  pre.rcp.ensemble[,,m2[i]] <- pre.rcp.ensemble[,,m2[i]]*60*60*24*28
  pre.rcp.ensemble[,,m3[i]] <- pre.rcp.ensemble[,,m3[i]]*60*60*24*31 
  pre.rcp.ensemble[,,m4[i]] <- pre.rcp.ensemble[,,m4[i]]*60*60*24*30 
  pre.rcp.ensemble[,,m5[i]] <- pre.rcp.ensemble[,,m5[i]]*60*60*24*31
  pre.rcp.ensemble[,,m6[i]] <- pre.rcp.ensemble[,,m6[i]]*60*60*24*30
  pre.rcp.ensemble[,,m7[i]] <- pre.rcp.ensemble[,,m7[i]]*60*60*24*31
  pre.rcp.ensemble[,,m8[i]] <- pre.rcp.ensemble[,,m8[i]]*60*60*24*31
  pre.rcp.ensemble[,,m9[i]] <- pre.rcp.ensemble[,,m9[i]]*60*60*24*30
  pre.rcp.ensemble[,,m10[i]] <- pre.rcp.ensemble[,,m10[i]]*60*60*24*31
  pre.rcp.ensemble[,,m11[i]] <- pre.rcp.ensemble[,,m11[i]]*60*60*24*30
  pre.rcp.ensemble[,,m12[i]] <- pre.rcp.ensemble[,,m12[i]]*60*60*24*31
}

# rotate and convert to raster brick
preEA.rt1 <- aperm(pre.hs.ensemble, c(2,1,3))
preEA.rt2 <- preEA.rt1[nrow(preEA.rt1):1,,]
raspre.hs <- brick(preEA.rt2,xmn=lonmin,xmx=lonmax,ymn=latmin,ymx=latmax, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
# dim(raspre.hs)
# plot(raspre.hs[[1]])

preEA.rt1  <- aperm(pre.rcp.ensemble, c(2,1,3))
preEA.rt2  <- preEA.rt1[nrow(preEA.rt1):1,,]
raspre.rcp <- brick(preEA.rt2,xmn=lonmin,xmx=lonmax,ymn=latmin,ymx=latmax, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
# dim(raspre.rcp)
# plot(raspre.rcp[[1]])

# save these outputs for later use 
save(raspre.hs,  file = "/path/to/your/historical/ensemble_mean/precipitation/CORDEX_ensemble_historical_precipitation.Rdata")
save(raspre.rcp, file = "/path/to/your/future/ensemble_mean/precipitation/CORDEX_ensemble_future_precipitation.Rdata")

# ensemble mean calculations for other variables, temperature (PrepareCORDEXtas.R) and cloudiness (PrepareCORDEXclt.R) also follow the same workflow.



