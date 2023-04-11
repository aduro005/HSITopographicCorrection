# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Correct aspect labels from p_gold
# Author: Alyssa M. Duro
# Last edited: 4/10/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------

library(dplyr) # df %>% filter()

# ---------------------------------------------------------------------------
# Load files
# ---------------------------------------------------------------------------

load("../Output Files/HSICalLib_wavevec.RData") 
load("../Output Files/HSICalLib_b1-b30_202302233_p_gold.RData")

# ---------------------------------------------------------------------------
# p aspect correction (98 --> 91 configurations per sample)
# ---------------------------------------------------------------------------

# Note: after this initial correction there are 2 spectra per sample per slope,
# aspect since 270 --> 90 while all other were translated to a unique aspect

# ----------
# Initial aspect correction (195-270 --> 165-90)

a1 <- c(195,210,225,240,255,270) # turn these aspects
a2 <- c(165,150,135,120,105,90) # into these aspects

for (i in 1:length(a1)) { 
  p_gold$aspect[which(p_gold$aspect==a1[i])] <- a2[i] 
} # close the initial aspect correction loop 

rm(list=c('a1','a2','i'))

# ----------
# Average the two spectra collected at 90 (90 & 270 both assigned aspect 90)

# start the new pfc_acor df with all the aspects that don't need correcting
p_gold_acor <- p_gold %>% filter (aspect!=90)
names(p_gold_acor) <-names(p_gold)

# grab the aspects that do need correcting (aspect=90)
p_gold_acor2 <- p_gold %>% filter (aspect==90)
names(p_gold_acor2) <-names(p_gold)

hnum <- sort(unique(p_gold_acor2$HSInumber))

pb <- txtProgressBar ( min=0, max=length(hnum) , style=3 ) 

for (i in 1:length(hnum)) {
  p_gold_acor3 <- p_gold_acor2 %>% filter( HSInumber==hnum[i] )
  snum <- sort(unique(p_gold_acor3$slope))
  
  # new df to hold the average I spectrum for each slope for this sample
  p_gold_acor6 <- data.frame()
  
  for (a in 1:length(snum)) {
    p_gold_acor4 <- p_gold_acor3 %>% filter( slope == snum[a] )
    #anum <- sort(unique(p_gold_acor3$aspect))
    
    m <- p_gold_acor4[2,] # so batchwellID shows a270
    m[,15:485] <- colMeans(p_gold_acor4[,15:485])
    
    p_gold_acor6 <- rbind(p_gold_acor6,m)
    
  } # close a (slope) loop
  
  # rbind the corrected aspects for this HSI number to the pfc_acor table
  # (which already contains all the non-90/270 spectra)
  p_gold_acor <- rbind(p_gold_acor, p_gold_acor6)
  
  setTxtProgressBar(pb,i)
  
} # close i (HSInumber) loop

close(pb)

rm(list=c("p_gold_acor2","p_gold_acor3","p_gold_acor4","p_gold_acor6","m"))

save ( list = c ( 'p_gold_acor' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_b1-b30_p_gold_acor.RData", 
                     sep="") )

# ----------
# load file that was just output

load("../Output Files/HSICalLib_20230223_b1-b30_p_gold_acor.RData")

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# STOP HERE IF ALL YOU WANT IS THE ASPECT CORRECTED P_GOLD_ACOR DATA FRAME
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# compare I for each aspect pair at each slope (acor pvalue spectra is output)
# ---------------------------------------------------------------------------

#### start with 20221001_b1-b30_p_clean_obsI_melt

# grab the data for the aspect pair you want to compare
pImelt_oa1 <- pImelt_o %>% filter(aspect==75) # change manually
pImelt_oa2 <- pImelt_o %>% filter(aspect==255) # change manually

slope <- c(0,10,20,30,40,50,60)
acor <- data.frame ()

for (s in 1:length(slope)) {
  
  acor1 <- data.frame( matrix (ncol=4, nrow=471) )
  
  # grab the data for one slope
  pImelt_oa1s <- pImelt_oa1 %>% filter(slope==slope[s])
  pImelt_oa2s <- pImelt_oa2 %>% filter(slope==slope[s])
  
  for (w in 1:length(wavevec)) {
    
    # grab the spectra for 1 wavelength at a time
    pImelt_oa1sw <- pImelt_oa1s %>% filter(wavelength==wavevec[w])
    pImelt_oa2sw <- pImelt_oa2s %>% filter(wavelength==wavevec[w])
    
    t <- t.test(pImelt_oa1sw$I, pImelt_oa2sw$I, na.rm=TRUE)
    
    acor1[w,1] <- "75255" # change manually
    acor1[w,2] <- slope[s]
    acor1[w,3] <- wavevec[w]
    acor1[w,4] <- t$p.value 
    
  } # close the wavelength (w) loop
  
  acor <- rbind(acor, acor1)
  
} # close the slope (s) loop

names(acor) <- c("pair","slope","w","pvalue")

length(which(acor$pvalue<0.01))

# plot log p spectra
mycol <- hcl.colors(7, palette = "Dynamic")

pdf(file = paste("../Plots/HSICalLib_I_pvaluespectra_75255.pdf", sep=""),
    width=8, height=8)
plot( log(pvalue)~w, data=acor, pch=20, col=mycol[as.factor(acor$slope)], 
      main="Ho: observed I a75 = observed I a255" )
legend("bottomleft", legend=slope, pch=20, col=mycol, cex=1, bty ="n")
dev.off()

acor2 <- rbind(acor2, acor)

save ( list = c ( 'acorI' ) , 
       file = paste ("../Output Files/HSICalLib_I_pvaluespectra_acor.RData", 
                     sep="") )

lm <- lm(slope~pvalue, data=acor)
summary(lm)

# plot I pvalue spectra

for (i in 1:length(a3)){
  plotdf <- acorI[which(acorI$pair==paste(a3[i],a4[i], sep="")),]
  pdf(file = paste("../Plots/HSICalLib_I_pvaluespectra_",a3[i],a4[i],"_2.pdf", sep=""),
      width=8, height=8)
  plot( log(pvalue)~w, 
        data=plotdf, pch=20, col=mycol[as.factor(plotdf$slope)], 
        ylim=c(-350,0),
        main=paste( "Ho: observed I a",a3[i], " = observed I a",a4[i],sep="" ) )
  legend("bottomleft", legend=slope, pch=20, col=mycol, cex=1, bty ="n", ncol=7)
  abline(h=log(0.01), lty=3)
  dev.off()
} # close the i (aspect pair) loop

# ---------------------------------------------------------------------------
# compare aspect pairs for dI
# ---------------------------------------------------------------------------

slope <- c(10,20,30,40,50,60)
a3 <- c(0,15,30,45,60,75,90) # compare these aspects
a4 <- c(180,195,210,225,240,255,270) # to these aspects

# create pvalue df (acor)
acor <- data.frame()

pb <- txtProgressBar ( min = 0 , max = length (a3) , style = 3 ) 

for (i in 1:length(a3)) {
  
  acor2 <- data.frame()
  
  # grab the data for the aspect pair you want to compare
  pdImelt_oa1 <- pdImelt_o %>% filter(aspect==a3[i]) 
  pdImelt_oa2 <- pdImelt_o %>% filter(aspect==a4[i]) 
  
  for (s in 1:length(slope)) {
    
    acor1 <- data.frame( matrix (ncol=4, nrow=471) )
    
    # grab the data for one slope
    pdImelt_oa1s <- pdImelt_oa1 %>% filter(slope==slope[s])
    pdImelt_oa2s <- pdImelt_oa2 %>% filter(slope==slope[s])
    
    for (w in 1:length(wavevec)) {
      
      # grab the spectra for 1 wavelength at a time
      pdImelt_oa1sw <- pdImelt_oa1s %>% filter(wavelength==wavevec[w])
      pdImelt_oa2sw <- pdImelt_oa2s %>% filter(wavelength==wavevec[w])
      
      t <- t.test(pdImelt_oa1sw$dI, pdImelt_oa2sw$dI, na.rm=TRUE)
      
      acor1[w,1] <- paste(a3[i],a4[i], sep="")
      acor1[w,2] <- slope[s]
      acor1[w,3] <- wavevec[w]
      acor1[w,4] <- t$p.value 
      
    } # close the wavelength (w) loop
    
    acor2 <- rbind(acor2, acor1)
    
  } # close the slope (s) loop
  
  acor <- rbind(acor, acor2)
  
  # update progress bar
  setTxtProgressBar (pb,i)
  
} # close the aspect (i) loop

# close progress bar
close (pb)

names(acor) <- c("pair","slope","w","pvalue")

# to distinguish acor dI from acor I
acordI <- acor

# output
save ( list = c ( 'acordI' ) , 
       file = paste ("../Output Files/HSICalLib_dI_pvaluespectra_acor.RData", 
                     sep="") )

# see what proportion of pvalues are significant
length(which(acordI$pvalue<0.01))

# plot log p spectra
mycol <- hcl.colors(7, palette = "Dynamic")

# ----------
# output dI pvalue spectra plots

acordI <- acordI %>% filter(slope!=0)

for (i in 1:length(a3)){
  plotdf <- acordI[which(acordI$pair==paste(a3[i],a4[i], sep="")),]
  pdf(file = paste("../Plots/HSICalLib_dI_pvaluespectra_",a3[i],a4[i],"_3.pdf", sep=""),
      width=8, height=8)
  plot( log(pvalue)~w, 
        data=plotdf, pch=20, col=mycol[as.factor(plotdf$slope)], 
        ylim=c(-350,0),
        main=paste( "Ho: observed dI a",a3[i], " = observed dI a",a4[i],sep="" ) )
  legend("bottomleft", legend=slope, pch=20, col=mycol, cex=1, bty ="n", ncol=7)
  abline(h=log(0.01), lty=3)
  dev.off()
} # close the i (aspect pair) loop


# ---------------------------------------------------------------------------
# 
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# adjust aspect labels in melted table -- takes wayyy longer
# ---------------------------------------------------------------------------

# ----------
# Initial aspect correction (195-270 --> 165-90)

a1 <- c(195,210,225,240,255,270) # turn these aspects
a2 <- c(165,150,135,120,105,90) # into these aspects

for (i in 1:length(a1)) { 
  pImelt_o$aspect[which(pImelt_o$aspect==a1[i])] <- a2[i] 
  } # close the initial aspect correction loop 

rm(list=c('a1','a2','i'))

# ----------
# average the two intensities collected at 90 & 270

slope <- seq(from=0, to=60, by=10)
HSInum <- unique(pImelt_o$HSInumber)

pImelt_acor <- pImelt_o %>% filter (aspect!=90)
names(pImelt_acor) <-names(pImelt_o)

pImelt_acor2 <- pImelt_o %>% filter (aspect==90)
names(pImelt_acor2) <-names(pImelt_o)

pb <- txtProgressBar ( min=0, max=length(HSInum) , style=3 ) 

for (i in 1:length(HSInum)) {
  pImelt_acor3 <- pImelt_acor2 %>% filter( HSInumber == HSInum[i] )
  pImelt_acor6 <- data.frame()
  
  for (a in 1:length(slope)) {
    pImelt_acor4 <- pImelt_acor3 %>% filter( slope == slope[a] )
  
  for (w in 1:length(wavevec)) {
    pImelt_acor5 <- pImelt_acor4 %>% filter( wavelength == wavevec[w] )
    
    temp <- data.frame()
    m <- mean(pImelt_acor5[,8])
    I <- pImelt_acor5[2,]
    I$I <- m
    pImelt_acor6 <- rbind(pImelt_acor6,I)

    } # close w (wavelength) loop
    } # close a (slope) loop

  pImelt_acor <- rbind(pImelt_acor, pImelt_acor6)
  setTxtProgressBar(pb,i)
  
  } # close i (HSInumber) loop

close (pb)

colnames(pImelt_acor)[9] <- 'Ia'

save ( list = c ( 'pImelt_acor' ) , 
       file = paste ("../Output Files/HSICalLib_b1-b28_pImelt_acor_Ioutrm.RData", 
                     sep="") )
# ---------------------------------------------------------------------------
# 
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 
# ---------------------------------------------------------------------------

