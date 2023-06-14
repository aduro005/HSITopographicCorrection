# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Calculate cosine corrected intensity (coscorI) 
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
load("../Output Files/HSICalLib_20230223_s0-s60_cosIL_all.RData")

# for subsetting pga by HSI number
load("../Output Files/HSICalLib_20230310_rutgerssamples_rand1.RData")

# for spectral stats
load("../Output Files/HSICalLib_20230223_b1-b30_pga_melt.RData")
load("../Output Files/HSICalLib_20230223_b1-b30_pga_refI_melt.RData")

# ---------------------------------------------------------------------------
# subset by HSInumber before cosine correction
# ---------------------------------------------------------------------------

# ----------
# 664 HSInumbers - 2 missing samples = 662 HSInumbers (soil samples)
# Otherwise length(hnums) = 683 - 2 missing samples = 681
hnums <- c(rand1,301:876,1027:1083)

pga_slm <- pga %>% filter ( HSInumber %in% hnums )
pga_slm <- pga_slm %>% filter ( between(slope,10,60) )

rm(list=c('pga','hnums','rand1'))

# ---------------------------------------------------------------------------
# merge pf_acor & cosIL attach cosIL columns to each spectrum
# ---------------------------------------------------------------------------

pga_slm_cosIL <- merge(pga_slm, cosIL, by=c('slope', 'aspect'))

# Output file
save ( list = c ( 'pga_slm_cosIL' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_slm_cosIL.RData", 
                     sep="") )
# Free up memory
rm(list=c('pga_slm','cosIL'))

# Load the file that was just output
load("../Output Files/HSICalLib_20230613_pga_slm_cosIL.RData")

# ---------------------------------------------------------------------------
# Cosine correction method (calculate ccI)
# ---------------------------------------------------------------------------

# df to hold all the cosine corrected intensities (ccI)
ccI <- pga_slm_cosIL %>% filter (slope==0)

# spectra that need to be corrected
ccI2 <- pga_slm_cosIL %>% filter (slope!=0)

# ----------
# for 1 slope, 
# for 1 aspect, 
# each row has a spectra (471 cols), cosIL, z, and identifiers

for (a in 1:length(unique(ccI2$slope))) { # a goes with slope
  ccI3 <- ccI2 %>% filter (slope==unique(ccI2$slope)[a])
  
  for ( b in 1:length(unique(ccI2$aspect))) { # b goes with aspect
    ccI4 <- ccI3 %>% filter (aspect==unique(ccI2$aspect)[b])
    
    # cos(zenith)/cos(incident) for light bank 1
    #r1 <- cos(ccI4$z1[1]*pi/180) / ccI4$cosIL1[1]
    
    # cos(zenith)/cos(incident) for light bank 2
    #r2 <- cos(ccI4$z2[1]*pi/180) / ccI4$cosIL2[1]
    
    # cosine corrected I = rough I * mean(cosz/cosIL)
    # first r1, then r2, then mean(r1,r2)
    
    #ccI4[,6:476] <- ccI4[,6:476] * ( (r1+r2)/2 )
    
    # same r for every row since all rows in this df have the same orientation
    r <- ccI4$rcosmeans[1] 
    ccI4[,15:485] <- ccI4[,15:485] * r
    
    ccI <- rbind(ccI, ccI4)
    
  } # close the b (aspect) loop
  
} # close the a (slope) loop

# Add a cc to the wavelength cols corresponding to the corrected I values
names(ccI)[15:485] <- paste("coscorI", wavevec, sep="")

pga_slm_coscorI <- ccI

rm(list=c('ccI2','ccI3','ccI4','ccI','pga_slm_cosIL'))

# Output pga_slm_coscorI
save ( list = c ( 'pga_slm_coscorI' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_slm_coscorI.RData", 
                     sep="") )

# Load the file that was just output
load("../Output Files/HSICalLib_20230310_pga_slm_coscorI.RData")

# ---------------------------------------------------------------------------
# melt pga_slm_coscorI
# ---------------------------------------------------------------------------

datatable <- pga_slm_coscorI[,c(1,2,5,15:485)]

# remove the coscorI on these wavelengths so they can be numeric in the melted table
names(datatable)[4:474] <- wavevec

# make the wide table a "data table" for melt()
melt2 <- as.data.table(datatable)
melt <- melt(melt2,
             id.vars=colnames(melt2)[c(1:3)],
             measure.vars=colnames(melt2)[4:474],
             variable.name="wavelength", 
             value.name="coscorI") 

# make wavelength numeric 
melt$wavelength <- as.numeric(as.character(melt$wavelength))
pga_slm_coscorI_melt <- melt

# Output file
save ( list = c ( 'pga_slm_coscorI_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_slm_coscorI_melt.RData", 
                     sep="") )

# Load the file that was just output
load("../Output Files/HSICalLib_20230613_pga_slm_coscorI_melt.RData")

rm(list=c('datatable','melt','melt2','pga_slm_coscorI'))

# ---------------------------------------------------------------------------
# merge pga_slm_coscorI_melt with pga_refI_melt (refI) and pga_melt (obsI)
# ---------------------------------------------------------------------------

pga_coscorI_refI_melt_slm <- merge( pga_slm_coscorI_melt, pga_melt,
                                    by=c("HSInumber","slope","aspect","wavelength") )

pga_coscorI_refI_melt_slm <- merge( pga_coscorI_refI_melt_slm, pga_refI_melt, 
                                    by=c("HSInumber","slope","aspect","wavelength") )

save ( list = c ( 'pga_coscorI_refI_melt_slm' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_coscorI_refI_melt_slm.RData", 
                     sep="") )

rm(list=c('pga_slm_coscorI_melt','pga_refI_melt','pga_melt'))

# ----------
load("../Output Files/HSICalLib_20230613_pga_coscorI_refI_melt_slm.RData")

# ---------------------------------------------------------------------------
# Plots with pga_coscorI_refI_melt_slm
# ---------------------------------------------------------------------------

# -----------
# plot coscorI, obsI, refI spectra

# color by slope (7 if slope=0 included)
mycol <- hcl.colors(7, palette = "Dynamic")

for (i in 301:310) {
  
  c_f <- pga_coscorI_refI_melt_slm[which(pga_coscorI_refI_melt_slm$HSInumber==i),]
  
  #pdf(file = paste("../Plots/HSICalLib_20221010_ccIspectra_HSInumber", 
  #                i, ".pdf", sep=""), width=8, height=8)
  plot(obsI~wavelength, data=c_f, pch=".", col="lightgrey", 
       xlim=c(400,1000), 
       ylim=c(0,0.7),
       xlab=expression(lambda~"(nm)") ,
       ylab="Reflectance Intensity (I)" )
  points(coscorI~wavelength, data=c_f, pch=".", 
         col=mycol[as.factor(c_f$slope)])
  points(refI~wavelength, data=c_f, pch=".")
  legend("topleft", 
         legend=c(paste(unique(c_f$slope),"slope"), 
                  "0 slope reference", 
                  "observed" ),
         pch=19, col=c(mycol,"black","lightgray"), cex=1, bty ="n")
  #dev.off()
  
} # close the i (HSInumber) loop

# ----------
# color by aspect

mycol2 <- hcl.colors(13, palette = "Roma") # 13 for all aspects

for (i in 190:200) {
  
  r_f <- rImeltsample[which(rImeltsample$HSInumber==i),]
  c_f <- ccImeltsample[which(ccImeltsample$HSInumber==i),]
  I_f <- Imeltsample[which(Imeltsample$HSInumber==i),]
  
  pdf(file = paste("../Plots/HSICalLib_20220918_ccIspectrasample3_HSInumber", 
                   i, ".pdf", sep=""), width=8, height=8)
  plot(I~wavelength, data=I_f, pch=".", col="lightgrey", 
       xlim=c(400,1000), 
       ylim=c(0,0.7),
       xlab=expression(lambda~"(nm)") ,
       ylab="Reflectance Intensity (I)" )
  points(ccI~wavelength, data=c_f, pch=".", 
         col=mycol2[as.factor(c_f$aspect)])
  points(rI~wavelength, data=r_f, pch=".")
  legend("topleft", 
         legend=c(paste(sort(unique(c_f$aspect)),"aspect"), 
                  "0 slope reference", 
                  "> 0 slope observed" ),
         pch=19, col=c(mycol2,"black","lightgray"), cex=1, bty ="n")
  dev.off()
  
} # close the i (HSInumber) loop

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

# RMSE and NSE and KGE for obs, coscor, and rd:
# for every sample
# for every configuration

pga_coscorI_refI_melt_slm <- pga_coscorI_refI_melt_slm %>% filter (slope!=0)

spectralstats_coscor <- data.frame()

hnum <- sort(unique(pga_coscorI_refI_melt_slm$HSInumber))

pb <- txtProgressBar ( min = 0 , max = length(hnum) , style = 3 ) 

for(j in 1:length(hnum)) { # HSInumber
  
  spectralstats2 <- pga_coscorI_refI_melt_slm %>% filter ( HSInumber==hnum[j] )
  
  snum <- sort(unique(spectralstats2$slope))
  
  for(s in 1:length(snum)) { # slope
    
    spectralstats3 <- spectralstats2 %>% filter ( slope==snum[s] )
    
    anum <- sort(unique(spectralstats3$aspect))
    
    for(a in 1:length(anum)) { # aspect
      
      spectralstats4 <- spectralstats3 %>% filter (aspect==anum[a])
      
      spectralstats0 <- data.frame(matrix(nrow=1, ncol=15))
      names(spectralstats0) <- c('slope','aspect','HSInumber',
                                 'RMSE_obs','RMSE_coscor',
                                 'NSE_obs','NSE_coscor', 
                                 'KGE_obs', 'KGE_obs_r', 'KGE_obs_beta', 'KGE_obs_alpha',
                                 'KGE_coscor', 'KGE_coscor_r', 'KGE_coscor_beta', 'KGE_coscor_alpha' )
      
      spectralstats0$slope <- snum[s]
      spectralstats0$aspect <- anum[a]
      spectralstats0$HSInumber <- hnum[j]
      
      obs <- spectralstats4$refI # reference
      pred_obs <- spectralstats4$obsI # uncorrected
      pred_coscor <- spectralstats4$coscorI # cosine corrected
      
      spectralstats0$RMSE_obs <- RMSEcalculation(pred_obs, obs)
      spectralstats0$RMSE_coscor <- RMSEcalculation(pred_coscor, obs)
      
      spectralstats0$NSE_obs <- NSEcalculation(pred_obs, obs)
      spectralstats0$NSE_coscor <- NSEcalculation(pred_coscor, obs)
      
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
      
      KGEoutput <- KGE(pred_coscor, obs, s=c(1,1,1), na.rm=TRUE, 
                       method="2009", out.type="full")
      
      spectralstats0$KGE_coscor <- KGEoutput$KGE.value
      spectralstats0$KGE_coscor_r <- KGEoutput$KGE.elements[1]
      spectralstats0$KGE_coscor_beta <- KGEoutput$KGE.elements[2]
      spectralstats0$KGE_coscor_alpha <- KGEoutput$KGE.elements[3]
      
      spectralstats_coscor <- rbind(spectralstats_coscor, spectralstats0)
      
    } # close the aspect loop
  } # close the slope loop
  
  setTxtProgressBar (pb,j)
  
} # close the HSInumber loop

save(list=c('spectralstats_coscor'), 
     file=paste("../Output Files/HSICalLib_20230613_spectralstats_coscor.RData", 
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

# ---------------------------------------------------------------------------
#
# ---------------------------------------------------------------------------