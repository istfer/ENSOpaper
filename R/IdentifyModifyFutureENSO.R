library(ncdf4)
library(remote)

# load the raspacSST you created in PreparePacificRCP85.R
load("/path/to/processed/PacificSST/GCM/GCM_Pacific_SST.Rdata")

# load raspre.hs and raspre.rcp you created in PrepareCORDEXpr.R
load("/path/to/your/historical/ensemble_mean/precipitation/CORDEX_ensemble_historical_precipitation.Rdata")
load("/path/to/your/future/ensemble_mean/precipitation/CORDEX_ensemble_future_precipitation.Rdata")
# stack them together
raspre <- stack(raspre.hs, raspre.rcp)

# deseason and denoise
raspre.ds <- deseason(raspre, cycle.window = 12)
raspacSST.ds <- deseason(raspacSST, cycle.window = 12)

raspre.dns <- denoise(raspre.ds, expl.var = 0.9)
raspacSST.dns <- denoise(raspacSST.ds, expl.var = 0.9)

# calculate EOT modes from simulation set
EA_modes_rcp <- eot(x = raspacSST.dns, y = raspre.dns, n = 3,
                    reduce.both = TRUE, standardised = FALSE,
                    verbose = TRUE)

# The indice for the base point of the first mode 
bp <- EA_modes_rcp@modes$mode_01@cell_bp

eot.rcp <- rep(NA,1800) # EOT should be a time series of length 1800, declaring
for(i in 1:1800) eot.rcp[i] <- raspacSST.dns[[i]][bp]

# create a time-series object
eot.rcp.ts <- ts(eot.rcp,frequency=12,start=c(1951,1))

# detrend
deot <- eot.rcp.ts-lowess(eot.rcp.ts)$y

# modify to obtain peaks such as the observed nino index 
deot_m <- deot * 3

# difference layers should be 50 x 47 x 1800, allocate
diff_new <- diff <- EA_modes_rcp@modes$mode_01@resid_response 
# use the slopes and intercepts obtained from EOT analysis on observational datasets
# EA_modes object is obtained as explained in IdentifyHistoricalENSOSignal.R script
for (i in 1:1800) diff[[i]]     <- deot[i]   * EA_modes@modes$mode_01@slp_response   + EA_modes@modes$mode_01@int_response
for (i in 1:1800) diff_new[[i]] <- deot_m[i] * EA_modes@modes$mode_01@slp_response + EA_modes@modes$mode_01@int_response

# load the bias corrected future precipitation data created as in biasCorrection.R
load("/path/to/your/bias/corrected/rcp85/precipiation/dataset/CORDEXpreBC.Rdata")
resid      <- XPRE_oncru - diff
XPRE_oncru <- resid + diff_new
# now XPRE_oncru raster is the future precipitation data with intensified ENSO signal

