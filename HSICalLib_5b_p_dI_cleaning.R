# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Remove samples from p based on dI outliers
# Author: Alyssa M. Duro
# Last edited: 6/13/2023
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
load("../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI.RData")

# ---------------------------------------------------------------------------
# Calculate dI from p_clean (similar to dI_calculation script)
# ---------------------------------------------------------------------------

# ----------
# Calculate deltaI = observed - reference (avg across all aspects at s0)
# ----------

# New df to hold all the delta I's for all samples in pf
p_clean_obsI_dI <- data.frame()

hnum <- sort(unique(p_clean_obsI$HSInumber))

# create a progress bar where 100% means each batch has been processed
pb <- txtProgressBar ( min = 0 , max = length(hnum) , style = 3 ) 

for (i in 1:length(hnum)) {
  
  # 1 HSI number (same soil sample, all orientations) at a time
  pff <-  p_clean_obsI %>% filter(HSInumber==hnum[i])
  
  # New df to hold deltaI's for this well
  deltaI3 <- data.frame(matrix(nrow=nrow(pff), ncol=ncol(pff)))
  deltaI3[,c(1:14,486)] <- pff[,c(1:14,486)]
  
  # average intensity at every wavelength across all aspects at s0
  z <- pff[which(pff$slope==0),15:485]
  zz <- as.vector(colMeans(z, na.rm=TRUE))

  # calculate deltaI
  for(j in 1:nrow(pff)){
  deltaI3[j,15:485] <- pff[j,15:485]-zz
  } # close the j (row) loop
  
  # Add all the configurations for this well from deltaI3 to deltaI
  p_clean_obsI_dI <- rbind(p_clean_obsI_dI,deltaI3)
  
  # update progress bar
  setTxtProgressBar ( pb , i )
  
} # Close the i loop & move on to the next HSI number in pf

close(pb)

# Free up memory
rm(list=c('zz','deltaI3','i','j','pff'))

names(p_clean_obsI_dI) <- names(p_clean_obsI)

# Add a d to the wavelength cols corresponding to delta I
names(p_clean_obsI_dI)[15:485] <- paste("dI", wavevec, sep="")

# Output file
save ( list = c ( 'p_clean_obsI_dI' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI_dI.RData", 
                     sep="") )

# Load the file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI_dI.RData")

# ---------------------------------------------------------------------------
# melt p_clean_obsI_dI to identify dI outliers
# ---------------------------------------------------------------------------

datatable <- p_clean_obsI_dI[,c(1:3,13:486)]

names(datatable)[6:476] <- wavevec

melt2 <- as.data.table(datatable)
melt1 <- melt(melt2,
              id.vars=colnames(melt2)[c(1:5,477)],
              measure.vars=colnames(melt2)[6:476],
              variable.name="wavelength", 
              value.name="dI") 

# make wavelength numeric
melt1$wavelength <- as.numeric(as.character(melt1$wavelength))

p_clean_obsI_dI_melt <- melt1

rm(list=c('melt1','melt2','datatable'))

save ( list = c ( 'p_clean_obsI_dI_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI_dI_melt.RData", 
                     sep="") )

# Load file that was just output
load("../Output Files/HSICalLib_20230223_b1-b30_p_clean_obsI_dI_melt.RData")

# ---------------------------------------------------------------------------
# plots with p_clean_obsI_dI_melt
# ---------------------------------------------------------------------------

# Sample by HSI number (734,760 = 13 aspects * 6 slopes * 471 * 20 samples)
dIsample <- p_clean_obsI_dI_melt %>% filter ( slope!=0 )
dIsample <- dIsample %>% filter ( between ( HSInumber,1084,1103 ))

hnum <- sort(unique(dIsample$HSInumber))

# color by slope
mycol <- hcl.colors(6, palette = "Dynamic")

for (i in 1100:1178) {
  d_f <- dIsample[which(dIsample$HSInumber==hnum[i]),]
  
  #pdf(file = paste("../Plots/HSICalLib_20220917_dIspectra_HSInumber", i, ".pdf", 
  #                sep=""), width=8, height=8)
  plot(dI~wavelength, data=d_f, pch=".", col=mycol[as.factor(d_f$slope)], 
       xlim=c(400,1000), 
       ylim=c(-0.135,0), # update manually
       xlab=expression(lambda~"(nm)") , main=paste("HSInumber",i),
       ylab="dI" )
  legend("bottomleft", legend=c(paste(sort(unique(d_f$slope)),"slope")), 
         pch=19, col=mycol, cex=1, bty ="n")
  # dev.off()
  
} # close the i (HSInumber) loop

rm(list=c('dIsample','d_f'))

# ---------------------------------------------------------------------------
# find and exclude wells (samples) that have dI outliers
# ---------------------------------------------------------------------------

# don't identify s0 dI outliers (assuming all the s0 spectra are correctly aligned)
p_clean_obsI_dI_melt <- p_clean_obsI_dI_melt %>% filter ( slope!=0 )

# ----------
# remove based on dI outliers using 1.5*IQR

# q1 <- round(quantile(pga_dI_melt$dI,0.25,names=FALSE), digits=6)
# q3 <- round(quantile(pga_dI_melt$dI,0.75,names=FALSE), digits=6)
# IQR <- q3 - q1
# lower <- q1 - (1.5*IQR)
# upper <- q3 + (1.5*IQR)

# pga_dI_melt_slm <- pga_dI_melt %>% filter ( dI < upper & dI > lower )
# rm(list=c('q1', 'q3', 'IQR', 'upper', 'lower'))

# ----------
# identify wells with at least 1 outlying dI value based on all the data

#IQR <- as.numeric(quantile(p_clean_obsI_dI_melt$dI)[4] - quantile(p_clean_obsI_dI_melt$dI)[2])
#maxdI <- as.numeric(quantile(p_clean_obsI_dI_melt$dI)[4] + (1.5*IQR))
#mindI <- as.numeric(quantile(p_clean_obsI_dI_melt$dI)[2] - (1.5*IQR))

# ----------
# determine outliers based on all the spectra collected at each slope position 

snum <- sort(unique(p_clean_obsI_dI_melt$slope))

outterms <- data.frame(matrix(ncol=4,nrow=6))
names(outterms) <- c("slope","IQR","maxdI","mindI")

for(a in 1:length(snum)) {
  dIclean <- p_clean_obsI_dI_melt %>% filter (slope==snum[a])
  
  outterms$slope[a] <- snum[a]
  IQR <- as.numeric(quantile(dIclean$dI)[4] - quantile(dIclean$dI)[2])
  outterms$IQR[a] <- IQR
  outterms$maxdI[a] <- as.numeric(quantile(dIclean$dI)[4] + (1.5*IQR))
  outterms$mindI[a] <- as.numeric(quantile(dIclean$dI)[2] - (1.5*IQR))
  
} # close the snum loop

#hist(p_clean_obsI_dI_melt$dI)
#summary(p_clean_obsI_dI_melt$dI)
#testout <- p_clean_obsI_dI_melt %>% filter (dI < mindI | dI > maxdI )

#pdI <- p_clean_obsI_dI_melt %>% filter (dI > 0)
#hist(pdI$dI)

bnum <- sort(unique(p_clean_obsI_dI_melt$batch))

dIoutliers <- vector()

pb <- txtProgressBar ( min = 0 , max = length(bnum) , style = 3 ) 

for (e in 1:length(bnum)) { 
  dIclean3 <- p_clean_obsI_dI_melt %>% filter(batch==bnum[e])
  snum <- sort(unique(dIclean3$slope))
  
  for (b in 1:length(snum)) {
    dIclean4 <- dIclean3 %>% filter(slope==snum[b])
    anum <- sort(unique(dIclean4$aspect))
    
    for (a in 1:length(anum)) {
      dIclean5 <- dIclean4 %>% filter(aspect==anum[a])
      wnum <- sort(unique(dIclean5$well))
      
      test <- as.numeric(vector())
      
      for (w in 1:length(wnum)) {
        dIclean6 <- dIclean5 %>% filter(well==wnum[w])
        
        # identify wells with at least 1 outlying dI value
        if (max(dIclean6$dI)>outterms$maxdI[b]) {
          # put the batchwellID in a vector if there is >/= 1 outlying dI value
          dIoutliers <- c(dIoutliers, dIclean6$batchwellID[1])
        } # close the max if loop
        
        if (min(dIclean6$dI)<outterms$mindI[b]) {
          dIoutliers <- c(dIoutliers, dIclean6$batchwellID[1])
        } # close the min if loop
        
      } # close the well loop
    } # close the aspect loop
  } # close the slope loop
  
  setTxtProgressBar ( pb , e )
} # close the batch loop

close(pb)

rm(list=c('dIclean3','dIclean4','dIclean5','dIclean6','IQR','outterms'))

save ( list = c ( 'dIoutliers' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_dIoutliers.RData", 
                     sep="") )

# Load the file you just output
load("../Output Files/HSICalLib_20230223_b1-b30_dIoutliers.RData")

# ----------
# exclude 7,163 samples (spectra) that contain dI outliers --> 107,486

# rows in p_clean where p_clean$batchwellID is NOT in the outlier list
p_gold <- p_clean_obsI[which(!is.element(p_clean_obsI$batchwellID, dIoutliers)),]

# ----------
# Output p_gold (input to aspect correction script)
# ----------

save ( list = c ( 'p_gold' ) , 
       file = "../Output Files/HSICalLib_b1-b30_20230223_p_gold.RData" )

# Load the file you just output
load("../Output Files/HSICalLib_b1-b30_20230223_p_gold.RData")

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ----------
# Examine which samples have positive dI's

hist(dImelt2$slope[which(dImelt2$dI>0.03)], xlab="slope", main="dI > 0.03")
length(unique(dImelt2[which(dImelt2$dI>0),6]))
sort(unique(dImelt[which(dImelt$dI<0.03&dImelt$dI>0.025),6]))
sort(unique(dImelt[which(dImelt$dI>0&dImelt$dI>0.025),6]))

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
