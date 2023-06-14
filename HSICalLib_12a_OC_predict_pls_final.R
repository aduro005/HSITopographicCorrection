# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: predict OC from corrected spectra
# Author: Alyssa M. Duro
# Last edited: 3/21/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------

library(dplyr) # df %>% filter()
library(data.table) # as.data.table()
library(reshape2) # dcast()
library(pls) # plsr (and pca, pcr) to predict OC 
library(hydroGOF) # KGE
library(soiltexture)

# ---------------------------------------------------------------------------
# Load files
# ---------------------------------------------------------------------------

load("../Output Files/HSICalLib_wavevec.RData")

# for soil properties
load("../Output Files/HSICalLib_b1-b30_20230223_p_gold.RData")

# dI corrected, reference, observed
load("../Output Files/HSICalLib_20230613_globaldI_predict_lm_pga_dI_refI_melt_slm_dIp_dIc_2.RData")

# 50 random HSI numbers from Duke Farms samples
load("../Output Files/HSICalLib_20230310_rutgerssamples_rand1.RData")

# ---------------------------------------------------------------------------
# filter p to match dIc, refI, obsI
# ---------------------------------------------------------------------------

# ----------
# 664 HSInumbers - 2 missing samples = 662 HSInumbers (soil samples)
# Otherwise length(hnums) = 683 - 2 missing samples = 681
hnums <- c(rand1,301:876,1027:1083)

# any 1 configuration since we just need HSInumber and volC from p

p <- p_gold %>% filter (slope==0 & aspect==0)
p <- p %>% filter ( HSInumber %in% hnums )
p <- p %>% filter ( volC > 0 )

# ----------
# df for plotting soil properties histograms

p_plots <- p[,c(1:14,486)]

save ( list = c ( 'p_plots' ) , 
       file=paste("../Output Files/HSICalLib_20230613_p_plots.RData", 
                  sep="") )

load("../Output Files/HSICalLib_20230613_p_plots.RData")

# ----------
# soil texture plots
# https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf

p_texture <- p_plots[complete.cases(p_plots),5:7]
names(p_texture) <- c("SAND","SILT","CLAY")

pdf(file = paste("../Plots/HSICalLib_20230613_soiltexturetriangle.pdf", sep="") ,
    width=8, height=8)
TT.plot( class.sys = "USDA-NCSS.TT", tri.data=p_texture, pch=".", col="red")
dev.off()

# ----------
# df for merge with dIc, obs, and ref

p <- p[,c(3,12)] # only need log10volC and HSInumber for merge

rm(list=c('p_gold'))

# ---------------------------------------------------------------------------
# dcast refIp_dIp (long --> wide)
# ---------------------------------------------------------------------------

# dcast(z ~ x, value.var = "y")
# melt(id.vars = "z", variable.name = "x", value.name = "y", na.rm = TRUE)

# ----------
# dIc (globaldI corrected)

dctable <- pga_dI_refI_melt_slm_dIp_dIc_2[,c(1:4,9)]
dc2 <- as.data.table(dctable)
dc <- dcast(dc2, 
            slope+aspect+HSInumber ~ wavelength,
            value.var="dIc") 
names(dc)[4:474] <- paste("dIc", wavevec) 

dIc <- dc

rm(list=c('dc','dc2','dctable'))

save ( list = c ( 'dIc' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_OCpredict_dIc.RData", 
                  sep="") )

#load("../Output Files/HSICalLib_20230613_globaldI_OCpredict_dIc.RData")

# ----------
# refI (reference used for training pls)

dctable <- pga_dI_refI_melt_slm_dIp_dIc_2[,c(1:4,7)]
dc2 <- as.data.table(dctable)
dc <- dcast(dc2, 
            slope+aspect+HSInumber ~ wavelength,
            value.var="refI") 
names(dc)[4:474] <- paste("refI", wavevec) 

refI <- dc

rm(list=c('dc','dc2','dctable'))

save ( list = c ( 'refI' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_OCpredict_refI.RData", 
                  sep="") )

#load("../Output Files/HSICalLib_20230613_globaldI_OCpredict_refI.RData")

# ----------
# obsI

dctable <- pga_dI_refI_melt_slm_dIp_dIc_2[,c(1:4,6)]
dc2 <- as.data.table(dctable)
dc <- dcast(dc2, 
            slope+aspect+HSInumber ~ wavelength,
            value.var="obsI")
names(dc)[4:474] <- paste("obsI", wavevec) 

obsI <- dc

rm(list=c('dc','dc2','dctable'))

save ( list = c ( 'obsI' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_OCpredict_obsI.RData", 
                  sep="") )

#load("../Output Files/HSICalLib_20230613_globaldI_OCpredict_obsI.RData")

# ---------------------------------------------------------------------------
# merge with p by HSInumber and add log10volC as an outcome variable
# ---------------------------------------------------------------------------

refI_p <- data.frame()
obsI_p <- data.frame()
dIc_p <- data.frame()

# --- refI_p
refI_p <- merge(refI, p, by="HSInumber")

save ( list = c ( 'refI_p' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_OCpredict_refI_p.RData",
                  sep="") )

#load("../Output Files/HSICalLib_20230613_globaldI_OCpredict_refI_p.RData")

# --- obsI_p
obsI_p <- merge(obsI, p, by="HSInumber")

save ( list = c ( 'obsI_p' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_OCpredict_obsI_p.RData",
                  sep="") )

#load("../Output Files/HSICalLib_20230613_globaldI_OCpredict_obsI_p.RData")

# --- dIc
dIc_p <- merge(dIc, p, by="HSInumber")

save ( list = c ( 'dIc_p' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_OCpredict_dIc_p.RData",
                  sep="") )

#load("../Output Files/HSICalLib_20230613_globaldI_OCpredict_dIc_p.RData")

rm(list=c('refI_pls','obsI_pls','ccorI_pls','dIc_pls','p'))
rm(list=c('pga_refI','pga_obsI','pga_dIc'))


# ---------------------------------------------------------------------------
# Define some variables and functions
# ---------------------------------------------------------------------------

snum <- sort(unique(refI_p$slope))
mycol <- hcl.colors(6, palette = "Dynamic")

anum <- sort(unique(refI_p$aspect)) # same for the other _p df's
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

R2calculation <- function(predicted, observed){
  lm_op <- lm(observed~predicted)
  R2 <- summary(lm_op)$r.squared # R2
  return(R2)
} # close R2 function

# ---------------------------------------------------------------------------
# train pls with refI_p to predict OC from 471 wavelengths 
# ---------------------------------------------------------------------------

# Find which orientation has ALL reference spectra for every sample
# Alternatively could get reference spectra from 0 slope, 0 aspect
# but it was easier to start with 1 df above (pga_dI_refI_melt_slm_dIp_dIc_2)
numsamples <- data.frame(matrix(ncol=3, nrow=13))
names(numsamples) <- c("slope","aspect","numspectra")

i<-1 # update manually
sub1 <- refI_p %>% filter (slope == sort(unique(refI_p$slope))[i])

for(j in 1:length(unique(refI_p$aspect))) {
  sub2 <- sub1 %>% filter (aspect == sort(unique(refI_p$aspect))[j])
  
  numsamples$numspectra[j] <- nrow(sub2)
  numsamples$slope[j] <- unique(sub2$slope)
  numsamples$aspect[j] <- unique(sub2$aspect)
  
} # close j (aspect) loop

# ---
# train pls using reference
# get refI from 1 configuration (same refI regardless of slope & aspect)

train_ref <- refI_p %>% filter (aspect==0 & slope==60)
names(train_ref)[4:474] <- wavevec

# # ----------
# # train with 1 randomly chosen obsI_p configuration per sample rather than ref
# 
# hnum <- sort(unique(obsI_p$HSInumber))
# 
# train <- data.frame()
# 
# for (i in 1:length(hnum)) {
#   
#   # train using uncorrected obsI
#   train1 <- obsI_p %>% filter (HSInumber==hnum[i])
#   
#   # find a random orientation from this HSInumber
#   s <- sample(sort(unique(train1$slope)), 1)
#   train2 <- train1 %>% filter (slope==s)
#   
#   a <- sample(sort(unique(train2$aspect)), 1)
#   train3 <- train2 %>% filter (aspect==a)
#   
#   train <- rbind(train, train3)
#   
# } # close the HSInumber loop
# 
# save ( list = c ( 'train' ) , 
#        file=paste("../Output Files/HSICalLib_20230320_globaldI_obsI_train.RData", sep="") )

# --- build the model
plsmodel_ref <- plsr(log10volC~., data=train_ref[,c(4:475)], ncomp=50, validation="CV")

save ( list = c ( 'plsmodel_ref' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_",
                  "plsmodel_log10volC_refI_train.RData", sep="") )

nc_ref <- selectNcomp(plsmodel_ref, method="onesigma", plot=FALSE)

# ----------
# MSE & R2 plots (output from pls)

# same info as validation plot, just MSE rather than RMSE
pdf(file = "../Plots/HSICalLib_20230613_PLSR_log10volC_refI_p_RMSE.pdf", 
    width=8, height=8)
validationplot(plsmodel_ref, val.type="RMSE", main = "PLSR log10volC refI")
abline(v=nc_ref, lty=2)
dev.off()

pdf(file = "../Plots/HSICalLib_20230613_PLSR_logvolC_refI_p_R2.pdf", 
    width=8, height=8)
validationplot(plsmodel_ref, val.type="R2", main = "PLSR log10volC refI")
abline(v=nc_ref, lty=2)
dev.off()

#nc <- 13 # if nc selected visually based on R2 and MSE plots

# ---------------------------------------------------------------------------
# Evaluate pls on training data
# ---------------------------------------------------------------------------

# --- summary stats for training 
predicted_tr <- as.vector(predict(plsmodel_ref, train_ref, ncomp=nc_ref))
observed_tr <- train_ref$log10volC

sstatdf_tr <- data.frame(predicted = predicted_tr, observed = observed_tr, 
                         slope = train_ref$slope, aspect = train_ref$aspect, 
                         HSInumber = train_ref$HSInumber)

sstatdf_tr$RMSE <- RMSEcalculation(sstatdf_tr$predicted, sstatdf_tr$observed)
sstatdf_tr$NSE <- NSEcalculation(sstatdf_tr$predicted, sstatdf_tr$observed)
sstatdf_tr$R2 <- R2calculation(sstatdf_tr$predicted, sstatdf_tr$observed)

KGEoutput <- KGE(sstatdf_tr$predicted, sstatdf_tr$observed, 
                 s=c(1,1,1), na.rm=TRUE, 
                 method="2009", out.type="full")

sstatdf_tr$KGE <- KGEoutput$KGE.value
sstatdf_tr$KGE_r <- KGEoutput$KGE.elements[1]
# r : the Pearson product-moment correlation coefficient. 
# Ideal value is r=1 

sstatdf_tr$KGE_beta <- KGEoutput$KGE.elements[2]
# Beta : the ratio between the mean of the simulated values and the mean 
# of the observed ones. Ideal value is Beta=1

sstatdf_tr$KGE_alpha <- KGEoutput$KGE.elements[3]
# variability measure: Alpha: the ratio between the standard deviation of 
# the simulated values and the standard deviation of the observed ones. 
# Ideal value is Alpha=1

# ----------
# PLS trained with ref spectra

sstatdf_tr_ref <- sstatdf_tr

save(list=c('sstatdf_tr_ref'),
     file=paste("../Output Files/HSICalLib_20230613_",
                "globaldI_PLSR_log10volC_sstatdf_tr_ref.RData", 
                sep="") )

rm(list=c('sstatdf_tr','predicted_tr','observed_tr'))

# ----------
# fit a line through all the training data

#fit0_ref <- lm(observed ~ predicted, data=sstatdf_tr_ref)
# 
# # --- reference (train)
# # training data=reference, so 1 OC & 1 prediction per soil sample
# 
# pdf(file = paste("../Plots/HSICalLib_20230303_PLSR_train_obsI_colaspect_logvolC.pdf", 
#                  #pdf(file = paste("../Plots/HSICalLib_20230303_PLSR_train_dIc_logvolC.pdf", 
#                  sep=""), width=8, height=8)
# plot(observed ~ predicted, data=sstatdf_tr_ref,
#      #main= paste("PLSR logvolC train n=",length(train[,1]), sep=""), 
#      ylim=c(-3,3), xlim=c(-3,3),
#      col=apalette[factor(sstatdf_tr_ref$aspect)],
#      xlab="Predicted log(OC) (vol %)", ylab = "Observed log(OC) (vol %)" )
# abline(a=0,b=1)
# #abline(fit0_ref, lty=2, col="grey")
# legend("topleft", legend=c(paste("RMSE = ",round(sstatdf_tr_ref$RMSE[1], digits=3)),
#                            paste("R2 = ",round(sstatdf_tr_ref$R2[1], digits=3)),
#                            paste("n = ",nrow(train)),
#                            paste("ncomp = ",nc) ), cex=1, bty ="n")
# # legend("bottomleft", 
# #        legend=expression(paste("0", degree," Slope (Reference)", sep="")), 
# #        cex=1, bty="n", pch=15, col = "black")
# 
# color.legend(2,-1,2.25,-2.5,c(expression(paste(" 0",degree))
#                               ," Aspect",
#                               expression(paste(" 180",degree))),
#              rect.col=apalette,
#              gradient="y",align="rb",cex=numsize)
# 
# legend("topright", inset=c(0.02,0.1), legend="1:1 line", cex=1, bty="n")
# dev.off()

# ---------------------------------------------------------------------------
# Evaluate pls on testing data (obsI = uncorrected) data
# ---------------------------------------------------------------------------

# ----------
test_obs <- obsI_p
#test_obs <- test_obs[,c(1:474,481)]
names(test_obs)[4:474] <- wavevec

predicted_obs <- as.vector(predict(plsmodel_ref, test_obs[,c(4:474)], ncomp=nc_ref))
observed_obs <- test_obs$log10volC

sstatdf_obs <- data.frame(predicted = predicted_obs, observed = observed_obs, 
                          slope = test_obs$slope, aspect = test_obs$aspect, 
                          HSInumber = test_obs$HSInumber)

sstatdf_obs$RMSE <- RMSEcalculation(predicted_obs, observed_obs)
sstatdf_obs$NSE <- NSEcalculation(predicted_obs, observed_obs)
sstatdf_obs$R2 <- R2calculation(predicted_obs, observed_obs)

KGEoutput <- KGE(predicted_obs, observed_obs, 
                 s=c(1,1,1), na.rm=TRUE, 
                 method="2009", out.type="full")

sstatdf_obs$KGE <- KGEoutput$KGE.value
sstatdf_obs$KGE_r <- KGEoutput$KGE.elements[1]
# r : the Pearson product-moment correlation coefficient. 
# Ideal value is r=1 

sstatdf_obs$KGE_beta <- KGEoutput$KGE.elements[2]
# Beta : the ratio between the mean of the simulated values and the mean 
# of the observed ones. Ideal value is Beta=1

sstatdf_obs$KGE_alpha <- KGEoutput$KGE.elements[3]
# variability measure: Alpha: the ratio between the standard deviation of 
# the simulated values and the standard deviation of the observed ones. 
# Ideal value is Alpha=1

save(list=c('sstatdf_obs'),
     file=paste("../Output Files/HSICalLib_20230613_",
                "globaldI_PLSR_log10volC_sstatdf_obs.RData", 
                sep="") )

# ----------
# fit a line at each slope

fit1_obs <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$slope==10),])
fit2_obs <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$slope==20),])
fit3_obs <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$slope==30),])
fit4_obs <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$slope==40),])
fit5_obs <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$slope==50),])
fit6_obs <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$slope==60),])
# 
# # ----------
# # color by slope
# 
# pdf(file = paste("../Plots/HSICalLib_20230316_PLSR_test_logvolC_obsI_obsItrain.pdf", 
#                  sep=""), width=8, height=8)
# plot(observed ~ predicted, data=sstatdf_obs, cex.axis=1.25,
#      #main= paste("PLSR logvolC obsI (uncorrected) n=",length(test_obs[,1]), sep=""), 
#      xlab=list(cex=1.25, "Predicted log(OC) (vol %)"), 
#      ylab = list(cex=1.25, "Observed log(OC) (vol %)"),
#      ylim=c(-3,3), xlim=c(-3,3), # update manually
#      col=spalette[factor(sstatdf_obs$slope)] ) 
# abline(a=0,b=1) # 1:1 line
# abline(fit1, col=spalette[1])
# abline(fit2, col=spalette[2])
# abline(fit3, col=spalette[3])
# abline(fit4, col=spalette[4])
# abline(fit5, col=spalette[5])
# abline(fit6, col=spalette[6])
# legend("topleft", inset=c(0.02,0.02) , 
#        legend=c(paste("RMSE = " , round(sstatdf_obs$RMSE[1], digits=3)),
#                 paste("R2 =", round(sstatdf_obs$NSE[1], digits=3)),
#                 paste("n = ", nrow(test_obs)),
#                 paste("ncomp = ", nc) ), cex=1, bty ="n")
# # legend("bottomright", title=expression("Slope ("~degree~")"),
# #        legend=c(snum), col=spalette, pch=20, cex=1, bty="n")
# 
# color.legend(2,-1,2.25,-2.5,c(expression(paste(" 10",degree))
#                               ," Slope",
#                               expression(paste(" 60",degree))),
#              rect.col=spalette,
#              gradient="y",align="rb",cex=1.25)
# 
# #legend("topright", inset=c(0.02,0.1), legend="1:1 line", cex=1, bty="n")
# dev.off()
# 
# # ----------
# # color by aspect
# 
# fit1 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==0),])
# fit2 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==15),])
# fit3 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==30),])
# fit4 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==45),])
# fit5 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==60),])
# fit6 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==75),])
# fit7 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==90),])
# fit8 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==105),])
# fit9 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==120),])
# fit10 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==135),])
# fit11 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==150),])
# fit12 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==165),])
# fit13 <- lm(observed ~ predicted, data=sstatdf_obs[which(sstatdf_obs$aspect==180),])
# 
# pdf(file = paste("../Plots/HSICalLib_20230306_PLSR_test_logvolC_obsI_colaspect_obsItrain.pdf", 
#                  sep=""), width=8, height=8)
# plot(observed ~ predicted, data=sstatdf_obs, cex.axis=1.25,
#      #main= paste("PLSR logvolC obsI (uncorrected) n=",length(test_obs[,1]), sep=""), 
#      xlab=list(cex=1.25, "Predicted log(OC) (vol %)"), 
#      ylab = list(cex=1.25, "Observed log(OC) (vol %)"),
#      ylim=c(-3,3), xlim=c(-3,3), # update manually
#      col=apalette[factor(sstatdf_obs$aspect)] ) 
# #abline(a=0,b=1) # 1:1 line
# abline(fit1, col=apalette[1])
# abline(fit2, col=apalette[2])
# abline(fit3, col=apalette[3])
# abline(fit4, col=apalette[4])
# abline(fit5, col=apalette[5])
# abline(fit6, col=apalette[6])
# abline(fit7, col=apalette[7])
# abline(fit8, col=apalette[8])
# abline(fit9, col=apalette[9])
# abline(fit10, col=apalette[10])
# abline(fit11, col=apalette[11])
# abline(fit12, col=apalette[12])
# abline(fit13, col=apalette[13])
# 
# legend("topleft", inset=c(0.02,0.02) , 
#        legend=c(paste("RMSE = " , round(sstatdf_obs$RMSE[1], digits=3)),
#                 paste("R2 =", round(sstatdf_obs$NSE[1], digits=3)),
#                 paste("n = ", nrow(test_obs)),
#                 paste("ncomp = ", nc) ), cex=1, bty ="n")
# # legend("bottomright", title=expression("aspect ("~degree~")"),
# #        legend=c(snum), col=spalette, pch=20, cex=1, bty="n")
# 
# color.legend(2,-1,2.25,-2.5,c(expression(paste(" 0",degree))
#                               ," aspect",
#                               expression(paste(" 180",degree))),
#              rect.col=apalette,
#              gradient="y",align="rb",cex=1.25)
# 
# #legend("topright", inset=c(0.02,0.1), legend="1:1 line", cex=1, bty="n")
# dev.off()

# ---------------------------------------------------------------------------
# train pls with dIc_p (1 orientation) to predict OC from 471 wavelengths
# ---------------------------------------------------------------------------

# ----------
# train with 1 randomly chosen configuration per sample

hnum <- sort(unique(dIc_p$HSInumber))

train_dIc <- data.frame()

for (i in 1:length(hnum)) {
  
  # train using global dI corrected
  train1 <- dIc_p %>% filter (HSInumber==hnum[i])
  
  # find a random orientation from this HSInumber
  s <- sample(sort(unique(train1$slope)), 1)
  train2 <- train1 %>% filter (slope==s)
  
  a <- sample(sort(unique(train2$aspect)), 1)
  train3 <- train2 %>% filter (aspect==a)
  
  train_dIc <- rbind(train_dIc, train3)
  
} # close the HSInumber loop

save ( list = c ( 'train_dIc' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_dIc_train.RData", sep="") )

#load("../Output Files/HSICalLib_20230613_globaldI_dIc_train.RData")

names(train_dIc)[4:474] <- wavevec

# --- build the model
plsmodel_dIc <- plsr(log10volC~., data=train_dIc[,c(4:475)], ncomp=50, validation="CV")

save ( list = c ( 'plsmodel_dIc' ) , 
       file=paste("../Output Files/HSICalLib_20230613_globaldI_",
                  "plsmodel_log10volC_dIc_train.RData", sep="") )

nc_dIc <- selectNcomp(plsmodel_dIc, method="onesigma", plot=FALSE)

# ----------
# MSE & R2 plots (output from pls)

# same info as validation plot, just MSE rather than RMSE
pdf(file = "../Plots/HSICalLib_20230614_PLSR_log10volC_refI_p_nc_RMSE.pdf", 
    width=8, height=8)
validationplot(plsmodel_dIc, val.type="RMSE", main = "PLSR log10volC refI")
abline(v=nc_dIc, lty=2)
dev.off()

pdf(file = "../Plots/HSICalLib_20230614_PLSR_log10volC_refI_p_nc_R2.pdf", 
    width=8, height=8)
validationplot(plsmodel_dIc, val.type="R2", main = "PLSR log10volC refI")
abline(v=nc_dIc, lty=2)
dev.off()

# ---------------------------------------------------------------------------
# Evaluate pls on dIc training data
# ---------------------------------------------------------------------------

# --- summary stats for training 
predicted_tr <- as.vector(predict(plsmodel_dIc, train_dIc, ncomp=nc_dIc))
observed_tr <- train_dIc$log10volC

sstatdf_tr <- data.frame(predicted = predicted_tr, observed = observed_tr, 
                         slope = train_dIc$slope, aspect = train_dIc$aspect, 
                         HSInumber = train_dIc$HSInumber)

sstatdf_tr$RMSE <- RMSEcalculation(sstatdf_tr$predicted, sstatdf_tr$observed)
sstatdf_tr$NSE <- NSEcalculation(sstatdf_tr$predicted, sstatdf_tr$observed)
sstatdf_tr$R2 <- R2calculation(sstatdf_tr$predicted, sstatdf_tr$observed)

KGEoutput <- KGE(sstatdf_tr$predicted, sstatdf_tr$observed, 
                 s=c(1,1,1), na.rm=TRUE, 
                 method="2009", out.type="full")

sstatdf_tr$KGE <- KGEoutput$KGE.value
sstatdf_tr$KGE_r <- KGEoutput$KGE.elements[1]
# r : the Pearson product-moment correlation coefficient. 
# Ideal value is r=1 

sstatdf_tr$KGE_beta <- KGEoutput$KGE.elements[2]
# Beta : the ratio between the mean of the simulated values and the mean 
# of the observed ones. Ideal value is Beta=1

sstatdf_tr$KGE_alpha <- KGEoutput$KGE.elements[3]
# variability measure: Alpha: the ratio between the standard deviation of 
# the simulated values and the standard deviation of the observed ones. 
# Ideal value is Alpha=1

# ----------
# PLS trained with 1 observation per sample & dIc spectra

sstatdf_tr_dIc <- sstatdf_tr

save(list=c('sstatdf_tr_dIc'),
     file=paste("../Output Files/HSICalLib_20230613_",
                "globaldI_PLSR_log10volC_sstatdf_tr_dIc.RData", 
                sep="") )

rm(list=c('sstatdf_tr','predicted_tr','observed_tr'))
# 
# # ----------
# # fit a line through all the dIc_train data
# 
# fit0 <- lm(observed ~ predicted, data=sstatdf_tr_dIc)
# 
# # --- reference (train)
# # training data=reference, so 1 OC & 1 prediction per soil sample
# 
# pdf(file = paste("../Plots/HSICalLib_20230303_PLSR_train_obsI_logvolC.pdf", 
#                  #pdf(file = paste("../Plots/HSICalLib_20230303_PLSR_train_dIc_logvolC.pdf", 
#                  sep=""), width=8, height=8)
# plot(observed ~ predicted, data=sstatdf_tr_dIc,
#      #main= paste("PLSR logvolC train n=",length(train[,1]), sep=""), 
#      ylim=c(-3,3), xlim=c(-3,3),
#      col=spalette[factor(sstatdf_tr_dIc$slope)],
#      #col="black",
#      xlab="Predicted log(OC (vol %))", ylab = "Observed log(OC (vol %))" )
# abline(a=0,b=1)
# #abline(fit0, lty=2, col="grey")
# legend("topleft", legend=c(paste("RMSE = ",round(sstatdf_tr_dIc$RMSE[1], digits=3)),
#                            paste("R2 = ",round(sstatdf_tr_dIc$R2[1], digits=3)),
#                            #paste("KGE = ",round(sstatdf_tr$KGE[1], digits=3)),
#                            paste("n = ",nrow(train)),
#                            paste("ncomp = ",nc) ), cex=1, bty ="n")
# # legend("bottomright", 
# #        legend=expression(paste("0", degree," Slope (Reference)", sep="")), 
# #        cex=1, bty="n", pch=1, col = "black")
# 
# color.legend(2,-1,2.25,-2.5,c(expression(paste(" 10",degree))
#                               ," Slope",
#                               expression(paste(" 60",degree))),
#              rect.col=spalette,
#              gradient="y",align="rb",cex=1.25)
# 
# legend("topright", inset=c(0.01,0.06), legend="1:1", cex=1, bty="n")
# dev.off()

# ---------------------------------------------------------------------------
# Evaluate pls on testing (dI corrected) data
# ---------------------------------------------------------------------------

# ---
test_dIc <- dIc_p 
names(test_dIc)[4:474] <- wavevec

predicted_dIc <- as.vector(predict(plsmodel_dIc, test_dIc[,4:474], ncomp=nc_dIc))
observed_dIc <- test_dIc$log10volC

sstatdf_dIc <- data.frame(predicted = predicted_dIc, observed = observed_dIc, 
                          slope = test_dIc$slope, aspect = test_dIc$aspect, 
                          HSInumber = test_dIc$HSInumber)

sstatdf_dIc$RMSE <- RMSEcalculation(predicted_dIc, observed_dIc)
sstatdf_dIc$NSE <- NSEcalculation(predicted_dIc, observed_dIc)
sstatdf_dIc$R2 <- R2calculation(predicted_dIc, observed_dIc)

KGEoutput <- KGE(predicted_dIc, observed_dIc, 
                 s=c(1,1,1), na.rm=TRUE, 
                 method="2009", out.type="full")

sstatdf_dIc$KGE <- KGEoutput$KGE.value
sstatdf_dIc$KGE_r <- KGEoutput$KGE.elements[1]
# r : the Pearson product-moment correlation coefficient. 
# Ideal value is r=1 

sstatdf_dIc$KGE_beta <- KGEoutput$KGE.elements[2]
# Beta : the ratio between the mean of the simulated values and the mean 
# of the observed ones. Ideal value is Beta=1

sstatdf_dIc$KGE_alpha <- KGEoutput$KGE.elements[3]
# variability measure: Alpha: the ratio between the standard deviation of 
# the simulated values and the standard deviation of the observed ones. 
# Ideal value is Alpha=1

save(list=c('sstatdf_dIc'),
     file=paste("../Output Files/HSICalLib_20230613_",
                "globaldI_PLSR_log10volC_sstatdf_dIc.RData", 
                sep="") )

# ----------
# fit a line at each slope

fit1_dIc <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$slope==10),])
fit2_dIc <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$slope==20),])
fit3_dIc <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$slope==30),])
fit4_dIc <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$slope==40),])
fit5_dIc <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$slope==50),])
fit6_dIc <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$slope==60),])
 
# # ----------
# # color by slope
# 
# pdf(file = paste("../Plots/HSICalLib_20230303_PLSR_test_logvolC_globaldIc_dIctrain.pdf", 
#                  sep=""), width=8, height=8)
# plot(observed ~ predicted, data=sstatdf_dIc, cex.axis=1.25,
#      #main= paste("PLSR logvolC global dI corrected n=",length(test_dIc[,1]), sep=""), 
#      xlab=list(cex=1.25, "Predicted log(OC) (vol %)"), 
#      ylab = list(cex=1.25, "Observed log(OC) (vol %)"),
#      ylim=c(-3,3), xlim=c(-3,3), # update manually
#      col=spalette[factor(sstatdf_dIc$slope)] ) 
# abline(fit1, col=spalette[1])
# abline(fit2, col=spalette[2])
# abline(fit3, col=spalette[3])
# abline(fit4, col=spalette[4])
# abline(fit5, col=spalette[5])
# abline(fit6, col=spalette[6])
# #abline(a=0,b=1, cex=2)
# #legend("bottomright", title=expression("Slope ("~degree~")"),
# #       legend=c(snum), col=spalette, pch=20, cex=1, bty="n")
# 
# color.legend(2,-1,2.25,-2.5,c(expression(paste(" 10",degree))
#                               ," Slope",
#                               expression(paste(" 60",degree))),
#              rect.col=spalette,
#              gradient="y",align="rb",cex=1.25)
# 
# # legend("bottomright", inset=c(0.01,0.35), legend="Observed (uncorrected)", 
# #        cex=1.25, bty ="n")
# 
# legend("topleft", inset=c(0.02,0.02) , 
#        legend=c(paste("RMSE = " , round(sstatdf_dIc$RMSE[1], digits=3)),
#                 paste("R2 =", round(sstatdf_dIc$R2[1], digits=3)),
#                 paste("n = ", nrow(test_dIc)),
#                 paste("ncomp = ", nc) ), cex=1, bty ="n")
# #legend("topright", inset=c(0.02,0.1), legend="1:1 line", cex=1, bty="n")
# 
# dev.off()
# 
# 
# # ----------
# # color by aspect
# 
# fit1 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==0),])
# fit2 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==15),])
# fit3 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==30),])
# fit4 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==45),])
# fit5 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==60),])
# fit6 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==75),])
# fit7 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==90),])
# fit8 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==105),])
# fit9 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==120),])
# fit10 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==135),])
# fit11 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==150),])
# fit12 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==165),])
# fit13 <- lm(observed ~ predicted, data=sstatdf_dIc[which(sstatdf_dIc$aspect==180),])
# 
# pdf(file = paste("../Plots/HSICalLib_20230306_PLSR_test_logvolC_globaldIc_colaspect_dIctrain.pdf", 
#                  sep=""), width=8, height=8)
# plot(observed ~ predicted, data=sstatdf_dIc, cex.axis=1.25,
#      #main= paste("PLSR logvolC global dI corrected n=",length(test_dIc[,1]), sep=""), 
#      xlab=list(cex=1.25, "Predicted log(OC) (vol %)"), 
#      ylab = list(cex=1.25, "Observed log(OC) (vol %)"),
#      ylim=c(-3,3), xlim=c(-3,3), # update manually
#      col=apalette[factor(sstatdf_dIc$aspect)] ) 
# abline(fit1, col=apalette[1])
# abline(fit2, col=apalette[2])
# abline(fit3, col=apalette[3])
# abline(fit4, col=apalette[4])
# abline(fit5, col=apalette[5])
# abline(fit6, col=apalette[6])
# abline(fit7, col=apalette[7])
# abline(fit8, col=apalette[8])
# abline(fit9, col=apalette[9])
# abline(fit10, col=apalette[10])
# abline(fit11, col=apalette[11])
# abline(fit12, col=apalette[12])
# abline(fit13, col=apalette[13])
# 
# #abline(a=0,b=1, cex=2)
# #legend("bottomright", title=expression("aspect ("~degree~")"),
# #       legend=c(snum), col=spalette, pch=20, cex=1, bty="n")
# 
# color.legend(2,-1,2.25,-2.5,c(expression(paste(" 0",degree))
#                               ," aspect",
#                               expression(paste(" 180",degree))),
#              rect.col=apalette,
#              gradient="y",align="rb",cex=1.25)
# 
# # legend("bottomright", inset=c(0.01,0.35), legend="Observed (uncorrected)", 
# #        cex=1.25, bty ="n")
# 
# legend("topleft", inset=c(0.02,0.02) , 
#        legend=c(paste("RMSE = " , round(sstatdf_dIc$RMSE[1], digits=3)),
#                 paste("R2 =", round(sstatdf_dIc$R2[1], digits=3)),
#                 paste("n = ", nrow(test_dIc)),
#                 paste("ncomp = ", nc) ), cex=1, bty ="n")
# #legend("topright", inset=c(0.02,0.1), legend="1:1 line", cex=1, bty="n")
# 
# dev.off()


# ---------------------------------------------------------------------------
# summary stats for spectra at each configuration across all wavelengths
# ---------------------------------------------------------------------------

# RMSE and NSE and KGE
# for every sample
# for every configuration

snum <- sort(unique(sstatdf_obs$slope))

summarystats_OC <- data.frame()

#pb <- txtProgressBar ( min = 0 , max = length(snum) , style = 3 ) 

for(s in 1:length(snum)) { # slope
  
  summarystats4 <- sstatdf_obs %>% filter  (slope==snum[s])
  summarystats5 <- sstatdf_dIc %>% filter (slope==snum[s])
  
  anum <- sort(unique(summarystats4$aspect))
  
  for(a in 1:length(anum)) { # aspect
    
    summarystats6 <- summarystats4 %>% filter (aspect==anum[a])
    summarystats7 <- summarystats5 %>% filter (aspect==anum[a])
    
    summarystats0 <- data.frame(matrix(nrow=1, ncol=14))
    names(summarystats0) <- c('slope','aspect',
                              'RMSE_obs','RMSE_dIc',
                              'NSE_obs','NSE_dIc', 
                              'KGE_obs', 'KGE_obs_r', 'KGE_obs_beta', 'KGE_obs_alpha',
                              'KGE_dIc', 'KGE_dIc_r', 'KGE_dIc_beta', 'KGE_dIc_alpha' )
    
    summarystats0$slope <- snum[s]
    summarystats0$aspect <- anum[a]
    
    # ----------
    # obsI 
    pred <- summarystats6$predicted # uncorrected
    obs <- summarystats6$observed # uncorrected
    
    summarystats0$RMSE_obs <- RMSEcalculation(pred, obs)
    summarystats0$R2_obs <- R2calculation(pred, obs)
    summarystats0$NSE_obs <- NSEcalculation(pred, obs)
    
    KGEoutput <- KGE(pred, obs, s=c(1,1,1), na.rm=TRUE, 
                     method="2009", out.type="full")
    
    summarystats0$KGE_obs <- KGEoutput$KGE.value
    summarystats0$KGE_obs_r <- KGEoutput$KGE.elements[1]
    # r : the Pearson product-moment correlation coefficient. 
    # Ideal value is r=1 
    
    summarystats0$KGE_obs_beta <- KGEoutput$KGE.elements[2]
    # Beta : the ratio between the mean of the simulated values and the mean 
    # of the observed ones. Ideal value is Beta=1
    
    summarystats0$KGE_obs_alpha <- KGEoutput$KGE.elements[3]
    # variability measure: Alpha: the ratio between the standard deviation of 
    # the simulated values and the standard deviation of the observed ones. 
    # Ideal value is Alpha=1
    
    # ----------
    # dIc
    pred <- summarystats7$predicted # dI corrected
    obs <- summarystats7$observed # dI corrected
    
    summarystats0$RMSE_dIc <- RMSEcalculation(pred, obs)
    summarystats0$R2_dIc <- R2calculation(pred, obs)
    summarystats0$NSE_dIc <- NSEcalculation(pred, obs)
    
    KGEoutput <- KGE(pred, obs, s=c(1,1,1), na.rm=TRUE, 
                     method="2009", out.type="full")
    
    summarystats0$KGE_dIc <- KGEoutput$KGE.value
    summarystats0$KGE_dIc_r <- KGEoutput$KGE.elements[1]
    summarystats0$KGE_dIc_beta <- KGEoutput$KGE.elements[2]
    summarystats0$KGE_dIc_alpha <- KGEoutput$KGE.elements[3]
    
    summarystats_OC <- rbind(summarystats_OC, summarystats0)
    
  } # close the aspect loop
  
  #setTxtProgressBar (pb,s)
  
} # close the slope loop

#close(pb)

save(list=c('summarystats_OC'), # _OC by configuration
     file=paste("../Output Files/HSICalLib_202300613_",
                "globaldI_PLSR_log10volC_summarystats_OC.RData", 
                sep="") )

rm(list=c('summarystats0','summarystats4','summarystats5',
          'summarystats6','summarystats7','pb'))

# ---------------------------------------------------------------------------
# plots with summary stats for RMSE & NSE calculated for each configuration
# ---------------------------------------------------------------------------

# --- RMSE vs slope boxplot
pdf(file = paste("../Plots/HSICalLib_20230306_globaldI_",
                 "summarystats_OC_RMSEvsslope_boxplot.pdf", sep=""),
KGwidth=8, height=8)
boxplot(RMSE_obs~slope, data=summarystats_OC, col="lightgrey", boxwex=0.3, 
        #ylim=c(0.7,0.95)
        pch=".", 
        #notch=TRUE,
        xlab = expression("Slope ("~degree~")"), 
        #main =paste("n per point = 100 soil samples, 13 aspects per slope", sep=""),
        ylab = "RMSE Predicted log(OC) (g/cm3) - Observed log(OC) (g/cm3)")
boxplot(RMSE_dIc~slope, data=summarystats_OC, col=palette[2], add=T, boxwex=0.3, 
        at = 1:length(unique(summarystats_OC$slope))+0.35, 
        pch=".", #notch=TRUE, 
        xaxt="n")
legend("topleft", 
       legend=c("Observed (uncorrected)", 
                expression(paste(Delta,"I Corrected = s+a+w+interactions"))),
       col=c('lightgrey',palette[2]), pch=15, cex=1, bty ="n")
dev.off()

# --- R2 vs slope boxplot
pdf(file = paste("../Plots/HSICalLib_20230306_globaldI_",
                 "summarystats_OC_R2vsslope_boxplot.pdf", sep="") ,
    width=8, height=8)
boxplot(R2_obs~slope, data=summarystats_OC, col="lightgrey", boxwex=0.3, 
        #ylim=c(0.7,0.95), 
        pch=".", 
        notch=TRUE,
        xlab = expression("Slope ("~degree~")"), 
        #main =paste("n per point = 100 soil samples, 13 aspects per slope", sep=""),
        ylab = "R2 (Predicted vs. Observed log(OC) (g/cm3))")
boxplot(R2_dIc~slope, data=summarystats_OC, col=palette[2], add=T, boxwex=0.3, 
        at = 1:length(unique(summarystats_OC$slope))+0.35, 
        pch=".", #notch=TRUE, 
        xaxt="n")
legend("bottomleft", 
       legend=c("Observed (uncorrected)", 
                expression(paste(Delta,"I Corrected = s+a+w+interactions"))),
       col=c('lightgrey',palette[2]), pch=15, cex=1, bty ="n")
dev.off()

# ----------
# plot vs aspect 

# --- RMSE vs aspect boxplot
pdf(file = paste("../Plots/HSICalLib_20230306_globaldI_",
                 "summarystats_OC_RMSEvsaspect_boxplot.pdf", sep="") ,
    width=8, height=8)
boxplot(RMSE_obs~aspect, data=summarystats_OC, col="lightgrey", boxwex=0.3, 
        #ylim=c(0.7,0.95), 
        pch=".", 
        notch=TRUE,
        xlab = expression("aspect ("~degree~")"), 
        #main =paste("n per point = 100 soil samples, 13 aspects per aspect", sep=""),
        ylab = "RMSE Predicted log(OC) (g/cm3) - Observed log(OC) (g/cm3)")
boxplot(RMSE_dIc~aspect, data=summarystats_OC, col=palette[2], add=T, boxwex=0.3, 
        at = 1:length(unique(summarystats_OC$aspect))+0.35, 
        pch=".", notch=TRUE, xaxt="n")
legend("topleft", 
       legend=c("Observed (uncorrected)", 
                expression(paste(Delta,"I Corrected = s+a+w+interactions"))),
       col=c('lightgrey',palette[2]), pch=15, cex=1, bty ="n")
dev.off()

# --- R2 vs aspect boxplot
pdf(file = paste("../Plots/HSICalLib_20230306_globaldI_",
                 "summarystats_OC_R2vsaspect_boxplot.pdf", sep="") ,
    width=8, height=8)
boxplot(R2_obs~aspect, data=summarystats_OC, col="lightgrey", boxwex=0.3, 
        #ylim=c(0.7,0.95), 
        pch=".", 
        notch=TRUE,
        xlab = expression("aspect ("~degree~")"), 
        #main =paste("n per point = 100 soil samples, 13 aspects per aspect", sep=""),
        ylab = "R2 (Predicted vs. Observed log(OC) (g/cm3))")
boxplot(R2_dIc~aspect, data=summarystats_OC, col=palette[2], add=T, boxwex=0.3, 
        at = 1:length(unique(summarystats_OC$aspect))+0.35, 
        pch=".", notch=TRUE, xaxt="n")
legend("bottomleft", 
       legend=c("Observed (uncorrected)", 
                expression(paste(Delta,"I Corrected = s+a+w+interactions"))),
       col=c('lightgrey',palette[2]), pch=15, cex=1, bty ="n")
dev.off()

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