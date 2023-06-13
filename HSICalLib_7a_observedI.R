# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Observed & melted obs for each spectrum
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

load("../Output Files/HSICalLib_20230223_b1-b30_p_gold_acor.RData")
load("../Output Files/HSICalLib_wavevec.RData")

# ---------------------------------------------------------------------------
# remove na's from p_gold_acor
# ---------------------------------------------------------------------------

# remove any rows with missing spectra --> 99,537 
pga <- p_gold_acor[which(!is.na(p_gold_acor[,15])),] # waveband 1
pga <- pga[which(!is.na(pga$HSInumber)),]

save ( list = c ( 'pga' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_pga.RData", 
                     sep="") )

load("../Output Files/HSICalLib_20230223_b1-b30_pga.RData")

# ---------------------------------------------------------------------------
# melt pga
# ---------------------------------------------------------------------------

datatable <- pga[,c(3,13:485)]

names(datatable)[4:474] <- wavevec

melt2 <- as.data.table(datatable)
melt1 <- melt(melt2,
              id.vars=colnames(melt2)[c(1:3)],
              measure.vars=colnames(melt2)[4:474],
              variable.name="wavelength", 
              value.name="obsI") 

# make wavelength numeric
melt1$wavelength <- as.numeric(as.character(melt1$wavelength))

pga_melt <- melt1

rm(list=c('melt1','melt2','datatable'))

save ( list = c ( 'pga_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_pga_melt.RData", 
                     sep="") )

# Load file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_pga_melt.RData")

# ---------------------------------------------------------------------------
# Plots with observed I
# ---------------------------------------------------------------------------

# color by slope (7 includes 0)
mycol <- hcl.colors(7, palette = "Dynamic") 

for (i in 800:805) { # choose which samples you want to plot
  
  of <- pga_melt[which(pga_melt$HSInumber==i),]
  
  #pdf(file = paste("../Plots/HSICalLib_20221025_obsIspectra_HSInumber", i, ".pdf", 
   #               sep=""), width=8, height=8)
  plot(obsI~wavelength, data=of, pch=".", col=mycol[as.factor(of$slope)], 
       xlim=c(400,1000), 
       ylim=c(0,0.5),
       xlab=expression(lambda~"(nm)") , 
       ylab="Reflectance Intensity")
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
