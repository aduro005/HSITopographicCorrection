# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Remove samples from p based on obs outliers
# Author: Alyssa M. Duro
# Last edited: 4/10/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load packages:
# ---------------------------------------------------------------------------

library(dplyr) # filter %>%
library(data.table) # as.data.table() for melt

# ---------------------------------------------------------------------------
# load files
# ---------------------------------------------------------------------------

load("../Output Files/HSICalLib_wavevec.RData") 

# ----------
# total number of samples = 1180 x 14 aspects x 7 slopes = 115,640 spectra
# -
# 2 sample missing (709 & 650) = 1178 samples = 115,444 spectra (p)
load("../Output Files/HSICalLib_b1_b30_p.RData")

# ---------------------------------------------------------------------------
# remove samples from p that have mistakes in their observed spectra
# ---------------------------------------------------------------------------

# 3 configurations (imaging mistakes) removed 
# 115,542 samples --> 115,404 (removes 40 samples)
p <- p[-which(p$slope==30&p$aspect==75&p$batch==8),]

# --> 115,364 (removes 40 samples)
p <- p[-which(p$slope==20&p$aspect==15&p$batch==23),]

# --> 115,324 (removes 40 samples)
p <- p[-which(p$slope==60&p$aspect==90&p$batch==2),]

# entire 10 slope for batch 24 removed (imaging mistake) 
# --> 114,764 (removes 13 * 40 samples)
p_clean <- p[-which(p$slope==10&p$batch==24),]

save ( list = c ( 'p_clean' ) , 
       file = "../Output Files/HSICalLib_b1-b30_20230223_p_clean.RData" )

load("../Output Files/HSICalLib_b1-b30_20230223_p_clean.RData")

# ---------------------------------------------------------------------------
# melt p_clean
# ---------------------------------------------------------------------------

datatable <- p_clean[,c(1:3,13:486)]

names(datatable)[6:476] <- wavevec

melt2 <- as.data.table(datatable)
melt1 <- melt(melt2,
              id.vars=colnames(melt2)[c(1:5,477)],
              measure.vars=colnames(melt2)[6:476],
              variable.name="wavelength", 
              value.name="obsI") 

# make wavelength numeric
melt1$wavelength <- as.numeric(as.character(melt1$wavelength))

p_clean_melt <- melt1

rm(list=c('melt1','melt2'))

save ( list = c ( 'p_clean_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_p_clean_melt.RData", 
                     sep="") )

load("../Output Files/HSICalLib_20230223_b1-b30_p_clean_melt.RData")

# ---------------------------------------------------------------------------
# find and exclude wells (samples) that have obsI outliers
# ---------------------------------------------------------------------------

# Don't remove s=0 outliers (assuming all the s0 spectra are correctly aligned)
p_clean_melt <- p_clean_melt %>% filter (slope!=0)

# ----------
# define outliers based on 1.5*IQR
IQR <- as.numeric(quantile(p_clean_melt$obsI)[4] - quantile(p_clean_melt$obsI)[2])
maxobsI <- as.numeric(quantile(p_clean_melt$obsI)[4] + (1.5*IQR))

# minobsI turns out to be negative, so I just used 0
#minobsI <- (p_clean_melt$obsI)[2] - (1.5*IQR) 
minobsI <- 0

obsIoutliers <- vector()

pb <- txtProgressBar ( min = 0 , max = 30 , style = 3 ) 

for (e in 1:30) {
  obsIclean3 <- p_clean_melt %>% filter(batch==e)
  snum <- sort(unique(obsIclean3$slope))
  
  for (b in 1:length(snum)) {
    obsIclean4 <- obsIclean3 %>% filter(slope==snum[b])
    anum <- sort(unique(obsIclean4$aspect))
    
    for (a in 1:length(anum)) {
      obsIclean5 <- obsIclean4 %>% filter(aspect==anum[a])
      wnum <- sort(unique(obsIclean5$well))
      
      for (w in 1:length(wnum)) {
        obsIclean6 <- obsIclean5 %>% filter(well==wnum[w])

        # identify wells with at least 1 outlying obsI value
        if (max(obsIclean6$obsI)>maxobsI) {
          # put the batchwellID in a vector if there is >/= 1 outlying obsI value
          obsIoutliers <- c(obsIoutliers, obsIclean6$batchwellID[1])
        } # close the max if loop
        
        if (min(obsIclean6$obsI)<minobsI) {
          obsIoutliers <- c(obsIoutliers, obsIclean6$batchwellID[1])
        } # close the max if loop
        
      } # close the w loop
    } # close the a loop
  } # close the b loop
  
  setTxtProgressBar ( pb , e )
} # close the e loop

close(pb)

rm(list=c('obsIclean3','obsIclean4','obsIclean5','obsIclean6','maxobsI','minobsI','IQR'))

save ( list = c ( 'obsIoutliers' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_obsIoutliers.RData", 
                     sep="") )

# Load the file you just output
load("../Output Files/HSICalLib_20230223_b1-b30_obsIoutliers.RData")

# ----------
# exclude samples if they contain obsI outliers

# Keep rows in p_clean where p_clean$batchwellID is NOT in the outlier list
p_clean_obsI <- p_clean[which(!is.element(p_clean$batchwellID, obsIoutliers)),]

# ----------
# Output p with mistake spectra and spectra with obsI outliers removed

save ( list = c ( 'p_clean_obsI' ) , 
       file = "../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI.RData" )

# Load the file you just output
load("../Output Files/HSICalLib_b1-b30_20230223_p_clean_obsI.RData")

# ---------------------------------------------------------------------------
# melt p_clean_obsI 
# ---------------------------------------------------------------------------

# grab only the columns you need
datatable <- p_clean_obsI[,c(1:3,13:486)]

names(datatable)[6:476] <- wavevec

melt2 <- as.data.table(datatable)
melt1 <- melt(melt2,
              id.vars=colnames(melt2)[c(1:5,477)],
              measure.vars=colnames(melt2)[6:476],
              variable.name="wavelength", 
              value.name="obsI") 

# make wavelength numeric
melt1$wavelength <- as.numeric(as.character(melt1$wavelength))

p_clean_obsI_melt <- melt1

rm(list=c('melt1','melt2','datatable'))

save ( list = c ( 'p_clean_obsI_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI_melt.RData", 
                     sep="") )

# Load file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI_melt.RData")

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------
