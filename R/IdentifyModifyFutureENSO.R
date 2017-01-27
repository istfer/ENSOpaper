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
# to obtain peaks such as the observed nino index 
# we need to multiply by max(nino3.4) / max(deot)  ~= 2.25

back.eot <- 2.25 * eot.rcp.ts
deot_m <- back.eot - lowess(back.eot)$y

plot(deot_m, lwd=2, xlab= "Years", ylab = "Anomalies in degrees C", main = "Modified ENSO signal")
lines(deot, col = "gray70", lwd=2)
legend("bottomright", legend = c("Modified ENSO", "ENSO future"), 
       lwd=2, cex= 0.75, col=c("black", "gray70"), bty="n")
# put back
diff_new = EA_modes_rcp@modes$mode_01@resid_response # diff should be 50 x 47 x 1800, allocate
for (i in 1:1800) diff_new[[i]]=back.eot[i]*EA_modes_rcp@modes$mode_01@slp_response + EA_modes_rcp@modes$mode_01@int_response

diff <- raspre.dns - EA_modes_rcp@modes$mode_01@resid_response
resid <- raspre - diff
EA_pre_new <- resid + diff_new
# EA_pre_new to be used in the LPJ-GUESS simulations

