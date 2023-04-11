# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Observed & melted obs for each spectrum
# Author: Alyssa M. Duro
# Last edited: 4/10/2023
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
# melt p_gold_acor
# ---------------------------------------------------------------------------

datatable <- p_gold_acor[,c(3,13:485)]

names(datatable)[4:474] <- wavevec

melt2 <- as.data.table(datatable)
melt1 <- melt(melt2,
              id.vars=colnames(melt2)[c(1:3)],
              measure.vars=colnames(melt2)[4:474],
              variable.name="wavelength", 
              value.name="obsI") 

# make wavelength numeric
melt1$wavelength <- as.numeric(as.character(melt1$wavelength))

p_gold_acor_melt <- melt1

rm(list=c('melt1','melt2','datatable'))

save ( list = c ( 'p_gold_acor_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_p_gold_acor_melt.RData", 
                     sep="") )

# Load file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_p_gold_acor_melt.RData")

# ---------------------------------------------------------------------------
# remove na's from p_gold_acor
# ---------------------------------------------------------------------------

pga <- p_gold_acor[which(!is.na(p_gold_acor[,15])),]
pga <- pga[which(!is.na(pga$batch)),]

save ( list = c ( 'pga' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_pga.RData", 
                     sep="") )

load("../Output Files/HSICalLib_20230223_b1-b30_pga.RData",)

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

# ----------
# total number of samples = 1180 x 14 aspects x 7 slopes = 115,640 spectra
# -
# 1 sample missing (709) = 1179 samples = 115,542 spectra (p)
# -
# 3 configurations (imaging mistakes) removed = 115,422 spectra
# -
# entire 10 slope for batch 24 removed (imaging mistake) = 114,862 spectra (p_clean)
# -
# 159 spectra identified as I outliers removed = 114,703 spectra (p_clean_obsI)
# -
# 543 spectra identified as dI outliers removed = 114,160 spectra (gold)
#
# aspect corrected --> 106,088
#
# remove NA's --> 105,968

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------
