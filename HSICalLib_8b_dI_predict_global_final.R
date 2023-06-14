# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Predict dI from slope, aspect, wavelength, and their interactions
# Author: Alyssa M. Duro
# Last edited: 6/13/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------

library(dplyr) # df %>% filter()
library(data.table) # melt
#library(corrplot) 
library(hydroGOF) # KGE
# (https://www.rdocumentation.org/packages/hydroGOF/versions/0.4-0/topics/KGE)

# ---------------------------------------------------------------------------
# Load files
# ---------------------------------------------------------------------------

load("../Output Files/HSICalLib_wavevec.RData")
load("../Output Files/HSICalLib_20230223_b1-b30_pga_dI_melt.RData")
load("../Output Files/HSICalLib_20230223_b1-b30_pga_melt.RData")
load("../Output Files/HSICalLib_20230223_b1-b30_pga_refI_melt.RData")

# ---------------------------------------------------------------------------
# Merge obsI_melt, refI_melt, and dI_melt
# ---------------------------------------------------------------------------

pga_dI_refI_melt <- merge( pga_dI_melt, pga_melt,
                           by=c("HSInumber","slope","aspect","wavelength") )

pga_dI_refI_melt <- merge( pga_dI_refI_melt, pga_refI_melt, 
                           by=c("HSInumber","slope","aspect","wavelength") )

save ( list = c ( 'pga_dI_refI_melt' ) , 
       file = paste ("../Output Files/HSICalLib_20230224_pga_dI_refI_melt.RData", 
                     sep="") )

rm(list=c('pga_melt','pga_refI_melt','pga_dI_melt'))

# ----------
load("../Output Files/HSICalLib_20230224_pga_dI_refI_melt.RData")

# ---------------------------------------------------------------------------
# Plots with pga_dI_refI_melt
# ---------------------------------------------------------------------------

# color by slope (7 includes 0)
mycol <- hcl.colors(7, palette = "Dynamic") 
hnum <- sort(unique(pga_dI_refI_melt$HSInumber))

for (i in 1:5) {
  
  of <- pga_dI_refI_melt[which(pga_dI_refI_melt$HSInumber==hnum[i]),]
  
  #pdf(file = paste("../Plots/HSICalLib_20221025_obsI_refI_spectra_HSInumber", i, ".pdf", 
  #               sep=""), width=8, height=8)
  plot(obsI~wavelength, data=of, pch=".", col=mycol[as.factor(of$slope)], 
       xlim=c(400,1000), 
       ylim=c(0,0.5),
       xlab=expression(lambda~"(nm)") , main=paste("HSInumber", hnum[i],sep=""),
       ylab="Reflectance Intensity")
  points(refI~wavelength, data=of, pch=".", col="black")
  legend("bottomright", title=expression(paste("Slope (",degree,")")),
         legend=c(sort(unique(of$slope)),"Reference"),
         pch=19, col=c(mycol, "black"), cex=1, bty ="n")
  #dev.off()
  
} # close the i (HSInumber) loop

rm(list=c('of','i','mycol','hnum'))

# ---------------------------------------------------------------------------
# Remove slope=0 & subset by HSInumber before fitting lm 
# ---------------------------------------------------------------------------

# 25 randomly chosen between 1:300, 25 randomly chosen between 877:1026
rand1 <- sample(sort(unique(pga_dI_refI_melt$HSInumber))[c(1:300,877:1026)], 
                50, replace = FALSE, prob = NULL)

save ( list = c ( 'rand1' ) , 
       file = paste ("../Output Files/HSICalLib_20230310_rutgerssamples_rand1.RData", 
                     sep="") )

load("../Output Files/HSICalLib_20230310_rutgerssamples_rand1.RData")

# ----------
# 664 HSInumbers - 2 missing samples = 662 HSInumbers (soil samples) (1027:1064)
# Otherwise length(hnums) = 683 - 2 missing samples = 681
hnums <- c(rand1,301:876,1027:1083)

pga_dI_refI_melt <- pga_dI_refI_melt %>% filter ( HSInumber %in% hnums )
pga_dI_refI_melt_slm <- pga_dI_refI_melt %>% filter ( between(slope,10,60) )

rm(list=c('pga_dI_refI_melt','hnums','rand1'))

save ( list = c ( 'pga_dI_refI_melt_slm' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_pga_dI_refI_melt_slm.RData", 
                     sep="") )

load("../Output Files/HSICalLib_20230613_pga_dI_refI_melt_slm.RData")

# ---------------------------------------------------------------------------
# z score transform dI and predictors (just to get beta weights)
# ---------------------------------------------------------------------------

pga_dI_refI_melt_slm <- as.data.frame(pga_dI_refI_melt_slm)

pdrmsz <- data.frame ( matrix (ncol=ncol(pga_dI_refI_melt_slm), 
                               nrow=nrow(pga_dI_refI_melt_slm)) )

colnames(pdrmsz) <- names(pga_dI_refI_melt_slm)

# sample identifier (HSInumber)
pdrmsz[,1] <- pga_dI_refI_melt_slm[,1]

# also z-transform slope, aspect, and wavelength since they are the predictors
for (i in 2:ncol(pga_dI_refI_melt_slm)) {
  pdrmsz[,i] <- (pga_dI_refI_melt_slm[,i] - mean(pga_dI_refI_melt_slm[,i])) / 
    sd(pga_dI_refI_melt_slm[,i]) 
} # close the i loop

# ----------
# use z-score transformed data to get beta weights
lmodel <- lm(dI ~ slope + aspect + wavelength + 
               slope*aspect + slope*wavelength +
               aspect*wavelength + slope*aspect*wavelength, data=pdrmsz)

summary(lmodel)

# ---------------------------------------------------------------------------
# Lm to predict melted dI spectra from slope, aspect, and wavelength 
# ---------------------------------------------------------------------------

lmodel_all <- lm(dI ~ slope + aspect + wavelength,
                 #+ slope*aspect + slope*wavelength
                 #+ aspect*wavelength + slope*aspect*wavelength,
                 data=pga_dI_refI_melt_slm)

summary(lmodel_all)

# get residuals from lmodel_all & add this column to pga_dI_refI_melt_slm
#pga_dI_refI_melt_slm$res <- resid(lmodel)

# ----------
# plots to inspect residuals for normality
# 
# #pdf(file = paste("../Plots/HSICalLib_20230209_dIHSInumber_globaldI_",
#  #                "residualsvsfitted.pdf", sep=""), width=8, height=8)
# plot(fitted(lmodel_all), res, pch=".") # residual vs. fitted plot
# abline(0,0) # add a horizontal line at 0
# #dev.off()
# 
# #pdf(file = paste("../Plots/HSICalLib_20230209_dIHSInumber_globaldI_",
#  #                "residualsqqplot.pdf", sep=""), width=8, height=8)
# qqnorm(res, pch=".") # create Q-Q plot for residuals
# qqline(res) # add a straight diagonal line to the plot
# #dev.off()

# ----------
# try step() to see if all predictors are necessary
# 
# steplmodel_all <- step(lmodel_all)
# 
# summary(steplmodel_all)

# ---------------------------------------------------------------------------
# Predict dI using lmodel_all & correct obsI using predicted dI  
# ---------------------------------------------------------------------------

pga_dI_refI_melt_slm$dIp <- predict(lmodel_all, pga_dI_refI_melt_slm)
pga_dI_refI_melt_slm$dIc <- pga_dI_refI_melt_slm$obsI - pga_dI_refI_melt_slm$dIp

pga_dI_refI_melt_slm_dIp_dIc <- pga_dI_refI_melt_slm

# change number (just for outputting file) based on dI or dI+
pga_dI_refI_melt_slm_dIp_dIc_1 <- pga_dI_refI_melt_slm_dIp_dIc

rm(list=c('lmodel_all','pga_dI_refI_melt_slm'))

save ( list = c ( 'pga_dI_refI_melt_slm_dIp_dIc_1' ) , 
       file = paste ("../Output Files/HSICalLib_20230613_globaldI_predict_",
                     "lm_pga_dI_refI_melt_slm_dIp_dIc_1.RData", 
                     sep="") )

# ---------------------------------------------------------------------------
# Plots with dI predicted
# ---------------------------------------------------------------------------

# color by slope
mycol <- hcl.colors(6, palette = "Dynamic")

# ----------
# dI observed vs dI predicted

randnums <- sample(unique(pga_dI_refI_melt_slm_dIp_dIc$HSInumber), 10, 
                   replace = FALSE, prob = NULL)

for (i in 1:length(randnums)) {
  
  plotdf <- pga_dI_refI_melt_slm_dIp_dIc %>% filter (HSInumber==randnums[i])
  
  #pdf(file = paste("../Plots/HSICalLib_20230117_dIpspectra_HSInumber", i, ".pdf", 
  #                sep=""), width=8, height=8)
  
  plot(dI~wavelength, data=plotdf, pch=".", col="darkgrey",
       xlim=c(400,1000) ,
       ylim=c(-0.15,0.02) ,
       xlab=expression(lambda~"(nm)") ,
       ylab=expression(Delta~"I") ,
       main=paste('HSInumber',plotdf$HSInumber[i] ) )
  points(dIp~wavelength, data=plotdf, pch=".", col=mycol[as.factor(plotdf$slope)])
  legend("bottomleft", 
         legend=c(expression(paste("10",degree," slope")),
                  expression(paste("20",degree," slope")),
                  expression(paste("30",degree," slope")),
                  expression(paste("40",degree," slope")),
                  expression(paste("50",degree," slope")),
                  expression(paste("60",degree," slope")), 
                  expression(paste(Delta,"I observed"))), 
         pch=19, col=c(mycol,"darkgrey"), cex=1, bty ="n")
  #dev.off()
  
} # close the i (HSInumber) loop

# ---------------------------------------------------------------------------
# Plots with dI corrected
# ---------------------------------------------------------------------------

# -----------
# plot dI, I, rI spectra

# color by slope
mycol <- hcl.colors(6, palette = "Dynamic")

for (i in 1:length(randnums)) {
  
  plotdf <- pga_dI_refI_melt_slm_dIp_dIc %>% filter (HSInumber==randnums[i])
  
  #pdf(file = paste("../Plots/HSICalLib_20230117_dIcspectra_HSInumber", i, ".pdf", 
  #               sep=""), width=8, height=8)
  plot(obsI~wavelength, data=plotdf, pch=".", col="darkgrey", 
       xlim=c(400,1000), 
       ylim=c(0,0.7),
       xlab=expression(lambda~"(nm)") ,
       ylab="Reflectance Intensity (I)" )
  points(dIc~wavelength, data=plotdf, pch=".", col=mycol[as.factor(plotdf$slope)])
  points(refI~wavelength, data=plotdf, pch=".")
  legend("topleft", 
         legend=c(expression(paste("10",degree," slope")),
                  expression(paste("20",degree," slope")),
                  expression(paste("30",degree," slope")),
                  expression(paste("40",degree," slope")),
                  expression(paste("50",degree," slope")),
                  expression(paste("60",degree," slope")), 
                  expression(paste("0",degree," slope (reference)")),
                  expression(paste("> 0",degree," slope (uncorrected)"))),
         pch=19, col=c(mycol,"black","darkgrey"), cex=1, bty ="n")
  #dev.off()
  
} # close the i (HSInumber) loop

# ---------------------------------------------------------------------------
# See how well the dI corrected match the reference
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Define some variables and functions
# ---------------------------------------------------------------------------

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

# RMSE and NSE 
# for every sample
# for every configuration

hnum <- sort(unique(pga_dI_refI_melt_slm_dIp_dIc$HSInumber))

spectralstats_dIc <- data.frame()

pb <- txtProgressBar ( min = 0 , max = length(hnum) , style = 3 ) 

for(j in 1:length(hnum)) { # j goes with HSInumber
  
  spectralstats2 <- pga_dI_refI_melt_slm_dIp_dIc %>% filter ( HSInumber == hnum[j] )
  
  snum <- sort(unique(spectralstats2$slope))
  
  for(s in 1:length(snum)) { # slope
    
    spectralstats3 <- spectralstats2 %>% filter ( slope==snum[s] )
    
    anum <- sort(unique(spectralstats3$aspect))
    
    for(a in 1:length(anum)) { # aspect
      
      spectralstats4 <- spectralstats3 %>% filter (aspect==anum[a])
      
      spectralstats0 <- data.frame(matrix(nrow=1, ncol=15))
      names(spectralstats0) <- c('slope','aspect','HSInumber',
                                 'RMSE_obs','RMSE_dIc',
                                 'NSE_obs','NSE_dIc', 
                                 'KGE_obs', 'KGE_obs_r', 'KGE_obs_beta', 'KGE_obs_alpha',
                                 'KGE_dIc', 'KGE_dIc_r', 'KGE_dIc_beta', 'KGE_dIc_alpha' )
      
      spectralstats0$slope <- snum[s]
      spectralstats0$aspect <- anum[a]
      spectralstats0$HSInumber <- hnum[j]
      
      obs <- spectralstats4$refI # reference
      pred_obs <- spectralstats4$obsI # uncorrected
      pred_dIc <- spectralstats4$dIc # deltaI corrected
      
      spectralstats0$RMSE_obs <- RMSEcalculation(pred_obs, obs)
      spectralstats0$RMSE_dIc <- RMSEcalculation(pred_dIc, obs)
      
      spectralstats0$NSE_obs <- NSEcalculation(pred_obs, obs)
      spectralstats0$NSE_dIc <- NSEcalculation(pred_dIc, obs)
      
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
      
      KGEoutput <- KGE(pred_dIc, obs, s=c(1,1,1), na.rm=TRUE, 
                       method="2009", out.type="full")
      
      spectralstats0$KGE_dIc <- KGEoutput$KGE.value
      spectralstats0$KGE_dIc_r <- KGEoutput$KGE.elements[1]
      spectralstats0$KGE_dIc_beta <- KGEoutput$KGE.elements[2]
      spectralstats0$KGE_dIc_alpha <- KGEoutput$KGE.elements[3]
      
      spectralstats_dIc <- rbind(spectralstats_dIc, spectralstats0)
      
    } # close the aspect loop
  } # close the slope loop
  
  setTxtProgressBar (pb,j)
  
} # close the HSInumber loop

close(pb)

# change number (just for outputting file) based on dI or dI+
spectralstats_dIc_1 <- spectralstats_dIc

save(list=c('spectralstats_dIc_1'), 
     file=paste("../Output Files/HSICalLib_20230613_globaldI_",
                "spectralstats_dIc_1.RData", sep="") )

rm(list=c('spectralstats0','spectralstats2','spectralstats3',
          'spectralstats4','pb','KGEoutput','obs','pred_obs','pred_dIc'))

# ---------------------------------------------------------------------------
# summary stats for spectra at each wavelength & slope across all aspects
# ---------------------------------------------------------------------------

# RMSE, NSE & KGE
# for every slope
# for every wavelength

snum <- sort(unique(pga_dI_refI_melt_slm_dIp_dIc_2$slope))

spectralstats_dIc_w_s <- data.frame()

pb <- txtProgressBar ( min = 0 , max = length(snum) , style = 3 ) 

for(s in 1:length(snum)) { # s goes with slope
  
  spectralstats2 <- pga_dI_refI_melt_slm_dIp_dIc_2 %>% filter ( slope == snum[s] )
  
  for(w in 1:length(unique(wavevec))) { # wavelength
    
    spectralstats4 <- spectralstats2 %>% filter ( wavelength == wavevec[w] )
    
    spectralstats0 <- data.frame(matrix(nrow=1, ncol=14))
    names(spectralstats0) <- c('slope','wavelength',
                               'RMSE_obs','RMSE_dIc',
                               'NSE_obs','NSE_dIc', 
                               'KGE_obs', 'KGE_obs_r', 'KGE_obs_beta', 'KGE_obs_alpha',
                               'KGE_dIc', 'KGE_dIc_r', 'KGE_dIc_beta', 'KGE_dIc_alpha' )
    
    spectralstats0$slope <- snum[s]
    spectralstats0$wavelength <- wavevec[w]
    
    obs <- spectralstats4$refI # reference
    pred_obs <- spectralstats4$obsI # uncorrected
    pred_dIc <- spectralstats4$dIc # deltaI corrected
    
    spectralstats0$RMSE_obs <- RMSEcalculation(pred_obs, obs)
    spectralstats0$RMSE_dIc <- RMSEcalculation(pred_dIc, obs)
    
    spectralstats0$NSE_obs <- NSEcalculation(pred_obs, obs)
    spectralstats0$NSE_dIc <- NSEcalculation(pred_dIc, obs)
    
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
    
    KGEoutput <- KGE(pred_dIc, obs, s=c(1,1,1), na.rm=TRUE, 
                     method="2009", out.type="full")
    
    spectralstats0$KGE_dIc <- KGEoutput$KGE.value
    spectralstats0$KGE_dIc_r <- KGEoutput$KGE.elements[1]
    spectralstats0$KGE_dIc_beta <- KGEoutput$KGE.elements[2]
    spectralstats0$KGE_dIc_alpha <- KGEoutput$KGE.elements[3]
    
    spectralstats_dIc_w_s <- rbind(spectralstats_dIc_w_s, spectralstats0)
    
  } # close the wavelength loop
  
  setTxtProgressBar (pb,s)
  
} # close the slope loop

close(pb)

spectralstats_dIc_w_s_2 <- spectralstats_dIc_w_s # only need this for dI+

save(list=c('spectralstats_dIc_w_s_2'), 
     file=paste("../Output Files/HSICalLib_20230613_globaldI_",
                "spectralstats_dIc_w_s_2.RData", sep="") )

rm(list=c('spectralstats0','spectralstats2','spectralstats4','pb'))

# ---------------------------------------------------------------------------
# summary stats for spectra at each wavelength & aspect across all slopes 
# (same as before but aspect (rather than slope) & wavelength)
# ---------------------------------------------------------------------------

# RMSE and NSE 
# for every aspect
# for every wavelength

anum <- sort(unique(pga_dI_refI_melt_slm_dIp_dIc_2$aspect))

spectralstats_dIc_w_a <- data.frame()

pb <- txtProgressBar ( min = 0 , max = length(anum) , style = 3 ) 

for(a in 1:length(anum)) { # a goes with aspect
  
  spectralstats2 <- pga_dI_refI_melt_slm_dIp_dIc_2 %>% filter ( aspect == anum[a] )
  
  for(w in 1:length(unique(wavevec))) { # wavelength
    
    spectralstats4 <- spectralstats2 %>% filter ( wavelength == wavevec[w] )
    
    spectralstats0 <- data.frame(matrix(nrow=1, ncol=14))
    names(spectralstats0) <- c('aspect','wavelength',
                               'RMSE_obs','RMSE_dIc',
                               'NSE_obs','NSE_dIc', 
                               'KGE_obs', 'KGE_obs_r', 'KGE_obs_beta', 'KGE_obs_alpha',
                               'KGE_dIc', 'KGE_dIc_r', 'KGE_dIc_beta', 'KGE_dIc_alpha' )
    
    spectralstats0$aspect <- anum[a]
    spectralstats0$wavelength <- wavevec[w]
    
    obs <- spectralstats4$refI # reference
    pred_obs <- spectralstats4$obsI # uncorrected
    pred_dIc <- spectralstats4$dIc # dI corrected
    
    spectralstats0$RMSE_obs <- RMSEcalculation(pred_obs, obs)
    spectralstats0$RMSE_dIc <- RMSEcalculation(pred_dIc, obs)
    
    spectralstats0$NSE_obs <- NSEcalculation(pred_obs, obs)
    spectralstats0$NSE_dIc <- NSEcalculation(pred_dIc, obs)
    
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
    
    KGEoutput <- KGE(pred_dIc, obs, s=c(1,1,1), na.rm=TRUE, 
                     method="2009", out.type="full")
    
    spectralstats0$KGE_dIc <- KGEoutput$KGE.value
    spectralstats0$KGE_dIc_r <- KGEoutput$KGE.elements[1]
    spectralstats0$KGE_dIc_beta <- KGEoutput$KGE.elements[2]
    spectralstats0$KGE_dIc_alpha <- KGEoutput$KGE.elements[3]
    
    spectralstats_dIc_w_a <- rbind(spectralstats_dIc_w_a, spectralstats0)
    
  } # close the wavelength loop
  
  setTxtProgressBar (pb,a)
  
} # close the aspect loop

close(pb)

spectralstats_dIc_w_a_2 <- spectralstats_dIc_w_a # only need this for dI+

save(list=c('spectralstats_dIc_w_a_2'), 
     file=paste("../Output Files/HSICalLib_20230613_",
                "spectralstats_dIc_w_a_2.RData", sep="") )

rm(list=c('spectralstats0','spectralstats2','spectralstats4','pb'))

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

# ---------------------------------------------------------------------------
#
# ---------------------------------------------------------------------------