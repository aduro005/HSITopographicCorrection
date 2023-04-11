# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: calculate dI
# Author: Alyssa M. Duro
# Last edited: 4/10/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------

library(dplyr) # df %>% filter()
library(data.table) # melt

# ---------------------------------------------------------------------------
# Load files
# ---------------------------------------------------------------------------

load("../Output Files/HSICalLib_wavevec.RData") 
load("../Output Files/HSICalLib_20230223_b1-b30_pga.RData")

# ---------------------------------------------------------------------------
# Calculate deltaI = observed - reference (avg across all aspects at s0)
# ---------------------------------------------------------------------------

# New df to hold all the delta I's for all samples in pf
deltaI <- data.frame()

hnum <- sort(unique(pga$HSInumber))

# create a progress bar where 100% means each batch has been processed
pb <- txtProgressBar (min=0, max=length(hnum), style=3) 

for(i in 1:length(hnum)) {
  
  # 1 HSI number (same soil sample, all orientations) at a time
  pff <-  pga %>% filter(HSInumber==hnum[i])
  
  # New df to hold deltaI's for this well
  deltaI3 <- data.frame(matrix(nrow=nrow(pff), ncol=ncol(pff))) 
  deltaI3[,c(1:14,486)] <- pff[,c(1:14,486)]
  
  # average intensity at every wavelength across all aspects at s0 (reference)
  zz <- as.vector(colMeans(pff[which(pff$slope==0),15:485], na.rm=TRUE))
  
  # each configuration of this batch, well
  for(j in 1:nrow(pff)){
    # calculate deltaI
    deltaI3[j,15:485] <- pff[j,15:485]-zz
  } # Move on to the next row (configuration) for this well
  
  # Add all the configurations for this well from deltaI3 to deltaI
  deltaI <- rbind(deltaI,deltaI3)
  
  # update progress bar
  setTxtProgressBar ( pb , i )
  
} # Close the i loop & move on to the next HSI number in pf

close(pb)

names(deltaI) <- names(pga)

# Add dI to the wavelength cols corresponding to delta I
names(deltaI)[15:485] <- paste("dI", wavevec, sep="")
 
# ----------
pga_dI <- deltaI

# Free up memory
rm(list=c('zz','deltaI3','pff','pb','i','j'))

# Output file
save ( list = c ( 'pga_dI' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_pga_dI.RData", 
                     sep="") )

# Load the file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_pga_dI.RData")

# ---------------------------------------------------------------------------
# melt deltaI 
# ---------------------------------------------------------------------------

datatable <- pga_dI[,c(3,13:485)]

names(datatable)[4:474] <- wavevec

melt2 <- as.data.table(datatable)
melt1 <- melt(melt2,
              id.vars=colnames(melt2)[c(1:3)],
              measure.vars=colnames(melt2)[4:474],
              variable.name="wavelength", 
              value.name="dI") 

# make wavelength numeric
melt1$wavelength <- as.numeric(as.character(melt1$wavelength))

# ----------
pga_dI_melt <- melt1

# Free up memory
rm(list=c('melt1','melt2'))

save ( list = c ( 'pga_dI_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_pga_dI_melt.RData", 
                     sep="") )

# Load file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_pga_dI_melt.RData")

# ---------------------------------------------------------------------------
# Plots with pga_dI_melt
# ---------------------------------------------------------------------------

# color by slope (7 includes 0)
mycol <- hcl.colors(7, palette = "Dynamic") 

for (i in 800:805) {
  
  df <- pga_dI_melt[which(pga_dI_melt$HSInumber==hnum[i]),]

  #pdf(file = paste("../Plots/HSICalLib_20221025_dI_spectra_HSInumber", i, ".pdf", 
  #               sep=""), width=8, height=8)
  plot(dI~wavelength, data=df, pch=".", col=mycol[as.factor(df$slope)], 
       xlim=c(400,1000), 
       #ylim=c(-0.25,0.025),
       xlab=expression(lambda~"(nm)") ,
       main=hnum[i],
       ylab=expression(Delta~"Reflectance Intensity") )
  legend("bottomright", title=expression(paste("Slope (",degree,")")),
         legend=c(sort(unique(df$slope))),
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
