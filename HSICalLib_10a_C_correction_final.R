# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Calculate C corrected intensity (ccorI)
# Author: Alyssa M. Duro
# Last edited: 6/13/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------

library(dplyr) # filter %>%
library(data.table) # as.data.table() for melt
library(RColorBrewer)

# ---------------------------------------------------------------------------
# load files
# ---------------------------------------------------------------------------

load("../Output Files/HSICalLib_wavevec.RData") 
load("../Output Files/HSICalLib_20230223_s0-s60_cosIL_all.RData")

# output from cosine correction
load("../Output Files/HSICalLib_20230613_pga_slm_cosIL.RData")

# for subsetting pga by HSI number
load("../Output Files/HSICalLib_20230310_rutgerssamples_rand1.RData")

# for spectral stats
load("../Output Files/HSICalLib_20230223_b1-b30_pga_melt.RData")
load("../Output Files/HSICalLib_20230223_b1-b30_pga_refI_melt.RData")

# ---------------------------------------------------------------------------
# Calculate C coefficient for each wavelength from obsI & cosIL
# ---------------------------------------------------------------------------

# ----------
# 664 HSInumbers - 2 missing samples = 662 HSInumbers (soil samples)
# Otherwise length(hnums) = 683 - 2 missing samples = 681
hnums <- c(rand1,301:876,1027:1083)

pga_slm_cosIL_C <- pga_slm_cosIL %>% filter ( slope!=0 )
pga_slm_cosIL_C <- pga_slm_cosIL_C[,c(15:485,496)]

# new df to hold C for each wavelength
ccdf <- data.frame(matrix(nrow=471, ncol = 4))
names(ccdf) <- c("wavelength","intercept","slope","ccoef")

# i goes with wavelength in wavevec & row in ccdf & column in pga_slm_cosIL_C
for (i in 1:length(wavevec)) { 
  
  ccdf1 <- data.frame(obsI = pga_slm_cosIL_C[,i], cosIL = pga_slm_cosIL_C$meancosIL)
  cmodel <- lm(obsI~cosIL, data=ccdf1)
  ccdf[i,1] <- wavevec[i]
  ccdf[i,2] <- coef(cmodel)[1] # intercept
  ccdf[i,3] <- coef(cmodel)[2] # slope
  ccdf[i,4] <- coef(cmodel)[1] / coef(cmodel)[2] # C coef for this wavelength
  
} # close the i (wavelength) loop

rm(list=c("cmodel","ccdf1"))

# Output file
save ( list = c ( 'ccdf' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_C-coefficient_ccdf.RData", 
                     sep="") )

# ----------
# plot c coef spectrum

pdf(file = paste("../Plots/HSICalLib_20230613_Ccoefspectrum.pdf", 
                 sep=""), width=8, height=8)
plot(ccoef~wavelength, data=ccdf, type="l", main="C coefficient")
dev.off()

# ---------------------------------------------------------------------------
# Correct I using C correction method
# ---------------------------------------------------------------------------

# df to hold all the C corrected intensities
ccorI <- pga_slm_cosIL %>% filter ( slope==0 )

# spectra that need to be corrected before being added to ccorI
ccorI2 <- pga_slm_cosIL %>% filter ( slope!=0 )

# ----------
# for 1 slope,
#   for 1 aspect,
#     each row has a spectra (471 cols), cosIL, z, and identifiers

for ( i in 1:length(wavevec)) { # i goes with ccdf row (wavelength)
  j <- i+14 # j goes with column in ccorI2
  
  ccorI2[,j] <- ccorI2[,j]*((ccorI2$meancosz+ccdf$ccoef[i])/(ccorI2$meancosIL+ccdf$ccoef[i]))
  
} # close the i (wavelength) loop

ccorI <- rbind(ccorI, ccorI2)

# Add a prefix to the wavelength cols corresponding to the corrected I values
names(ccorI)[15:485] <- paste("ccorI", wavevec, sep="")

pga_slm_cosIL_ccorI <- ccorI

# Output file
save ( list = c ( 'pga_slm_cosIL_ccorI' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_slm_cosIL_ccorI.RData", 
                     sep="") )

# Free up memory
rm(list=c("ccorI2","ccdf","ccorI"))

# Load the file that was just output
load("../Output Files/HSICalLib_20230613_pga_slm_cosIL_ccorI.RData")

# ---------------------------------------------------------------------------
# melt pga_slm_ccorI
# ---------------------------------------------------------------------------

datatable <- pga_slm_cosIL_ccorI[,c(1,2,5,15:485)]

# remove the ccorI on these wavelengths so they can be numeric in the melted table
names(datatable)[4:474] <- wavevec

# make the wide table a "data table" for melt()
melt2 <- as.data.table(datatable)
melt <- melt(melt2,
             id.vars=colnames(melt2)[c(1:3)],
             measure.vars=colnames(melt2)[4:474],
             variable.name="wavelength", 
             value.name="ccorI") 

# make wavelength numeric 
melt$wavelength <- as.numeric(as.character(melt$wavelength))
pga_slm_ccorI_melt <- melt

# Output file
save ( list = c ( 'pga_slm_ccorI_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_slm_ccorI_melt.RData", 
                     sep="") )

# Load the file that was just output
load("../Output Files/HSICalLib_20230613_pga_slm_ccorI_melt.RData")

rm(list=c('melt','melt2','datatable'))

# ---------------------------------------------------------------------------
# merge pga_slm_cosIL_ccorI_melt with pga_refI_melt and pga_melt
# ---------------------------------------------------------------------------

pga_ccorI_refI_melt_slm <- merge( pga_slm_ccorI_melt, pga_melt,
                                  by=c("HSInumber","slope","aspect","wavelength") )

pga_ccorI_refI_melt_slm <- merge( pga_ccorI_refI_melt_slm, pga_refI_melt, 
                                  by=c("HSInumber","slope","aspect","wavelength") )

save ( list = c ( 'pga_ccorI_refI_melt_slm' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_ccorI_refI_melt_slm.RData", 
                     sep="") )

rm(list=c('pga_slm_ccorI_melt','pga_refI_melt','pga_melt'))

# ----------
load("../Output Files/HSICalLib_20230613_pga_ccorI_refI_melt_slm.RData")

# ---------------------------------------------------------------------------
# See how well the cosine corrected match the reference
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Define some variables and functions
# ---------------------------------------------------------------------------

#snum <- seq(10,60,by=10)
#snum <- seq(10,40,by=10)
mycol <- hcl.colors(6, palette = "Dynamic")
mycol2 <- hcl.colors(13, palette = "Roma")

# ----------
# summary stat functions

RMSEcalculation <- function(predicted, observed){ 
  sqdif <- (observed - predicted)^2
  RMSE <- sqrt(mean(sqdif))
  return(RMSE)
} # close RMSE function

NSEcalculation <- function(predicted, observed){
  sqdif <- (observed - predicted)^2
  varmean <- (observed - mean(observed))^2
  NSE <- 1 - ( sum(sqdif) / sum(varmean) )
  return(NSE)
} # close NSE function

#R2calculation <- function(predicted, observed){
#  lm_op <- lm(observed~predicted)
#  R2 <- summary(lm_op)$r.squared # R2
#  return(R2)
#} # close R2 function

# ---------------------------------------------------------------------------
# summary stats for spectra at each configuration across all wavelengths
# ---------------------------------------------------------------------------

# RMSE and NSE and KGE for obs, ccor, and rd:
# for every sample
# for every configuration

pga_ccorI_refI_melt_slm <- pga_ccorI_refI_melt_slm %>% filter (slope!=0)

spectralstats_ccor <- data.frame()

hnum <- sort(unique(pga_ccorI_refI_melt_slm$HSInumber))

pb <- txtProgressBar ( min = 0 , max = length(hnum) , style = 3 ) 

for(j in 1:length(hnum)) { # HSInumber
  
  spectralstats2 <- pga_ccorI_refI_melt_slm  %>% filter ( HSInumber==hnum[j] )
  
  snum <- sort(unique(spectralstats2$slope))
  
  for(s in 1:length(snum)) { # slope
    
    spectralstats3 <- spectralstats2 %>% filter ( slope==snum[s] )
    
    anum <- sort(unique(spectralstats3$aspect))
    
    for(a in 1:length(anum)) { # aspect
      
      spectralstats4 <- spectralstats3 %>% filter (aspect==anum[a])
      
      spectralstats0 <- data.frame(matrix(nrow=1, ncol=15))
      names(spectralstats0) <- c('slope','aspect','HSInumber',
                                 'RMSE_obs','RMSE_ccor',
                                 'NSE_obs','NSE_ccor', 
                                 'KGE_obs', 'KGE_obs_r', 'KGE_obs_beta', 'KGE_obs_alpha',
                                 'KGE_ccor', 'KGE_ccor_r', 'KGE_ccor_beta', 'KGE_ccor_alpha' )
      
      spectralstats0$slope <- snum[s]
      spectralstats0$aspect <- anum[a]
      spectralstats0$HSInumber <- hnum[j]
      
      obs <- spectralstats4$refI # reference
      pred_obs <- spectralstats4$obsI # uncorrected
      pred_ccor <- spectralstats4$ccorI # C corrected
      
      spectralstats0$RMSE_obs <- RMSEcalculation(pred_obs, obs)
      spectralstats0$RMSE_ccor <- RMSEcalculation(pred_ccor, obs)
      
      spectralstats0$NSE_obs <- NSEcalculation(pred_obs, obs)
      spectralstats0$NSE_ccor <- NSEcalculation(pred_ccor, obs)
      
      KGEoutput <- KGE(pred_obs, obs, s=c(1,1,1), na.rm=TRUE, 
                       method="2009", out.type="full")
      
      spectralstats0$KGE_obs <- KGEoutput$KGE.value
      spectralstats0$KGE_obs_r <- KGEoutput$KGE.elements[1]
      # r : the Pearson product-moment correlation coefficient. 
      # Ideal value is r=1 
      
      spectralstats0$KGE_obs_beta <- KGEoutput$KGE.elements[2]
      # Beta : the ratio between the mean of the simulated values and the mean 
      # of the observed ones. Ideal value is Beta=1
      
      spectralstats0$KGE_obs_alpha <- KGEoutput$KGE.elements[3]
      # variability measure: Alpha: the ratio between the standard deviation of 
      # the simulated values and the standard deviation of the observed ones. 
      # Ideal value is Alpha=1
      
      KGEoutput <- KGE(pred_ccor, obs, s=c(1,1,1), na.rm=TRUE, 
                       method="2009", out.type="full")
      
      spectralstats0$KGE_ccor <- KGEoutput$KGE.value
      spectralstats0$KGE_ccor_r <- KGEoutput$KGE.elements[1]
      spectralstats0$KGE_ccor_beta <- KGEoutput$KGE.elements[2]
      spectralstats0$KGE_ccor_alpha <- KGEoutput$KGE.elements[3]
      
      spectralstats_ccor <- rbind(spectralstats_ccor, spectralstats0)
      
    } # close the aspect loop
  } # close the slope loop
  
  setTxtProgressBar (pb,j)
  
} # close the HSInumber loop

save(list=c('spectralstats_ccor'), 
     file=paste("../Output Files/HSICalLib_20230613_spectralstats_ccor.RData", 
                sep="") )

rm(list=c('spectralstats0','spectralstats2','spectralstats3',
          'spectralstats4','pb'))

# ---------------------------------------------------------------------------
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
# ---------------------------------------------------------------------------
