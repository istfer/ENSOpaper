# Bias correction function
# Takes GCM simulated dataset sim.dataset, and observational dataset cru.dataset 
# Calculates the 1951-2005 climatology for both
# Removes the 1951-2005 climatology from GCM simulations
# Adds back the 1951-2005 CRU climatology to the residuals
# Returns bias corrected dataset

# e.g.
# load the CORDEX ensembles you created in PrepareCORDEXpr.R
load("/path/to/your/historical/ensemble_mean/precipitation/CORDEX_ensemble_historical_precipitation.Rdata")
load("/path/to/your/future/ensemble_mean/precipitation/CORDEX_ensemble_future_precipitation.Rdata")
# stack them together
sim.dataset <- stack(raspre.hs, raspre.rcp)

# load the CRU dataset you created in IdentifyHistoricalENSOSignal.R
load("/path/to/your/raw/historical_CRU_precipitation_r.Rdata")
cru.dataset <- cruEastAfrica

# bias correction
XPRE_oncru <- add.on.cru(sim.dataset, cru.dataset)
save(XPRE_oncru, file = "/path/to/your/bias/corrected/rcp85/precipiation/dataset/CORDEXpreBC.Rdata")

# function
add.on.cru <- function(sim.dataset, cru.dataset){
  # ensemble climatology 1951-2005
  m.p1<-mean(sim.dataset[[seq(1,660,by=12)]])
  m.p2<-mean(sim.dataset[[seq(2,660,by=12)]])
  m.p3<-mean(sim.dataset[[seq(3,660,by=12)]])
  m.p4<-mean(sim.dataset[[seq(4,660,by=12)]])
  m.p5<-mean(sim.dataset[[seq(5,660,by=12)]])
  m.p6<-mean(sim.dataset[[seq(6,660,by=12)]])
  m.p7<-mean(sim.dataset[[seq(7,660,by=12)]])
  m.p8<-mean(sim.dataset[[seq(8,660,by=12)]])
  m.p9<-mean(sim.dataset[[seq(9,660,by=12)]])
  m.p10<-mean(sim.dataset[[seq(10,660,by=12)]])
  m.p11<-mean(sim.dataset[[seq(11,660,by=12)]])
  m.p12<-mean(sim.dataset[[seq(12,660,by=12)]])
  
  # CRU climatology
  m.n1<-mean(cru.dataset[[seq(1,660,by=12)]])
  m.n2<-mean(cru.dataset[[seq(2,660,by=12)]])
  m.n3<-mean(cru.dataset[[seq(3,660,by=12)]])
  m.n4<-mean(cru.dataset[[seq(4,660,by=12)]])
  m.n5<-mean(cru.dataset[[seq(5,660,by=12)]])
  m.n6<-mean(cru.dataset[[seq(6,660,by=12)]])
  m.n7<-mean(cru.dataset[[seq(7,660,by=12)]])
  m.n8<-mean(cru.dataset[[seq(8,660,by=12)]])
  m.n9<-mean(cru.dataset[[seq(9,660,by=12)]])
  m.n10<-mean(cru.dataset[[seq(10,660,by=12)]])
  m.n11<-mean(cru.dataset[[seq(11,660,by=12)]])
  m.n12<-mean(cru.dataset[[seq(12,660,by=12)]])
  
  # calculate ensemble residuals
  r1  <- sim.dataset[[seq(1,dim(sim.dataset)[3],by=12)]]-m.p1
  r2  <- sim.dataset[[seq(2,dim(sim.dataset)[3],by=12)]]-m.p2
  r3  <- sim.dataset[[seq(3,dim(sim.dataset)[3],by=12)]]-m.p3
  r4  <- sim.dataset[[seq(4,dim(sim.dataset)[3],by=12)]]-m.p4
  r5  <- sim.dataset[[seq(5,dim(sim.dataset)[3],by=12)]]-m.p5
  r6  <- sim.dataset[[seq(6,dim(sim.dataset)[3],by=12)]]-m.p6
  r7  <- sim.dataset[[seq(7,dim(sim.dataset)[3],by=12)]]-m.p7
  r8  <- sim.dataset[[seq(8,dim(sim.dataset)[3],by=12)]]-m.p8
  r9  <- sim.dataset[[seq(9,dim(sim.dataset)[3],by=12)]]-m.p9
  r10 <- sim.dataset[[seq(10,dim(sim.dataset)[3],by=12)]]-m.p10
  r11 <- sim.dataset[[seq(11,dim(sim.dataset)[3],by=12)]]-m.p11
  r12 <- sim.dataset[[seq(12,dim(sim.dataset)[3],by=12)]]-m.p12
  
  
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
  
  # add on CRU clim
  dec <- sim.dataset*2 # just a place-holder
  for(i in 1:150){
    dec[[(m1[i])]]  <- r1[[i]]+ m.n1
    dec[[(m2[i])]]  <- r2[[i]]+ m.n2
    dec[[(m3[i])]]  <- r3[[i]]+ m.n3
    dec[[(m4[i])]]  <- r4[[i]]+ m.n4
    dec[[(m5[i])]]  <- r5[[i]]+ m.n5
    dec[[(m6[i])]]  <- r6[[i]]+ m.n6
    dec[[(m7[i])]]  <- r7[[i]]+ m.n7
    dec[[(m8[i])]]  <- r8[[i]]+ m.n8
    dec[[(m9[i])]]  <- r9[[i]]+ m.n9
    dec[[(m10[i])]] <- r10[[i]]+ m.n10
    dec[[(m11[i])]] <- r11[[i]]+ m.n11
    dec[[(m12[i])]] <- r12[[i]]+ m.n12
  }
  
  return(dec)
  
}
