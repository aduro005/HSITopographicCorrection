# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: reference & melted reference for each spectrum
# Author: Alyssa M. Duro
# Last edited: 6/13/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------

library(dplyr) # df %>% filter()
library(data.table) # as.data.table() for melt

# ---------------------------------------------------------------------------
# Load files
# ---------------------------------------------------------------------------

load("../Output Files/HSICalLib_wavevec.RData")
load("../Output Files/HSICalLib_20230223_b1-b30_pga.RData")

# ---------------------------------------------------------------------------
# reference spectrum
# ---------------------------------------------------------------------------

# pga --> pga_refI same length, but spectra are the same for each configuration

# New df to hold all the refs for all samples in pf
refI <- data.frame()

hnum <- sort(unique(pga$HSInumber))

# create a progress bar where 100% means each batch has been processed
pb <- txtProgressBar(min=0, max=length(hnum), style=3) 

for(i in 1:length(hnum)) { # i goes with HSI number
  
  # 1 HSI number (same soil sample, all orientations) at a time
  pff <-  pga %>% filter(HSInumber==hnum[i])
  
  # New df to hold refI's for this well
  refI3 <- data.frame(matrix(nrow=nrow(pff), ncol=ncol(pff)) )
  refI3[,c(1:14,486)] <- pff[,c(1:14,486)]
  
  # refI spectrum = average intensity at every wavelength across all aspects at s0
  zz <- as.vector(colMeans(pff[which(pff$slope==0),15:485], na.rm=TRUE))
  
  for(j in 1:nrow(pff)){
    # 1 refI spectrum for each HSI number (all configurations, all rows in pff)
    refI3[j,15:485] <- zz
  } # close the j (pff row) loop
  
  # Add all the configurations for this well from refI3 to refI
  refI <- rbind(refI,refI3)
  
  # update progress bar
  setTxtProgressBar ( pb , i )
  
} # Close the i loop & move on to the next HSI number in pf

close(pb)

names(refI) <- names(pga)

names(refI)[15:485] <- paste("refI", wavevec, sep="")

pga_refI <- refI

# Output
save ( list = c ( 'pga_refI' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_pga_refI.RData", 
                     sep="") )

rm(list=c('refI','refI3','pff','i','j','zz'))

# Load the file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_pga_refI.RData")

# ---------------------------------------------------------------------------
# Melt refI 
# ---------------------------------------------------------------------------

datatable <- pga_refI[,c(3,13:485)]

names(datatable)[4:474] <- wavevec

melt2 <- as.data.table(datatable)
melt1 <- melt(melt2,
              id.vars=colnames(melt2)[c(1:3)],
              measure.vars=colnames(melt2)[4:474],
              variable.name="wavelength", 
              value.name="refI") 

# make wavelength numeric for deltaI prediction
melt1$wavelength <- as.numeric(as.character(melt1$wavelength))

pga_refI_melt <- melt1

save ( list = c ( 'pga_refI_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_pga_refI_melt.RData", 
                     sep="") )

rm(list=c('melt1','melt2','datatable'))

# Load the file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_pga_refI_melt.RData")

# ---------------------------------------------------------------------------
# Plots with pga_refI_melt & pga_melt (obsI)
# ---------------------------------------------------------------------------

# color by slope (7 includes 0)
mycol <- hcl.colors(7, palette = "Dynamic") 

for (i in 800:805) {
  
  of <- pga_melt[which(pga_melt$HSInumber==i),]
  rf <- pga_refI_melt[which(pga_refI_melt$HSInumber==i),]
  
  #pdf(file = paste("../Plots/HSICalLib_20221025_obsI_refI_spectra_HSInumber", i, ".pdf", 
  #               sep=""), width=8, height=8)
  plot(obsI~wavelength, data=of, pch=".", col=mycol[as.factor(of$slope)], 
       xlim=c(400,1000), 
       ylim=c(0,0.5),
       xlab=expression(lambda~"(nm)") , 
       ylab="Reflectance Intensity")
  points(refI~wavelength, data=rf, pch=".", col="black")
  legend("bottomright", title=expression(paste("Slope (",degree,")")),
         legend=c(sort(unique(of$slope))),
         pch=19, col=c(mycol, "darkgrey"), cex=1, bty ="n")
  #dev.off()
  
} # close the i (HSInumber) loop

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ----------
# output for Daniel 

firedf_ref <- pga_refI[,c(1:4,8:486)]
firedf_ref <- firedf_ref[which(pga_refI$archive=='GrayLab'),]
firedf_ref <- firedf_ref %>% filter (slope==0&aspect==0)
fireoutcsv <- paste ( "../Output Files/HSICalLib_FireSamples_spectral_SOC_reference.csv" , sep = "_" )
write.csv(firedf_ref, file = paste(fireoutcsv))
