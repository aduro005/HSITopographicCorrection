# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Calculate and output cosIL matrix
# Author: Alyssa M. Duro
# Last edited: 6/13/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Calculate cosIL matrix 
# ---------------------------------------------------------------------------

# *pi/180 converts slope and aspect to radians for input to cos() and sin()
snum <- seq(from=0,to=60, by=10)
anum <- seq(from=0,to=180, by=15) # N is direction of scan stage movement
#anum2 <- rev(seq(from=0,to=180, by=15))

# horizontal distance between the light bank & camera lens
# measured for lb1 = 16.0 cm & lb2 = 20.8 cm
A <- c(16,20.8)

# height of the light source above the scan stage
# measured, lb1 = lb2 = 30.2 cm
E <- 30.2

# measured height of each triangular prism (h) (slope = 0 to 60 by 10)
h <- c(0,0.53,1.09,1.73,2.52,3.57,5.25)

# H = height of the surface = height of the triangular prism (h) /2 + 1.8 + 1.5
# 1.8 cm = diagonal height of the sample well midpoint (in the small tray)
# 1.5 cm = height of the large tray (2.0 cm max thickness - 0.5 cm inset)
# reference height = scan stage = 0 cm

# surface height (cm) for each slope (0 to 60)
H <- (h/2) + 3.3

# light bank 1 = N = 0 azimuth
# light bank 2 = S = 180 azimuth
azimuth <- c(0,180)

# = 180 - (90+slope) # trying azimuth as a function of slope
# = 90 + zenith1 # another way of trying changing azimuth with slope

# empty df for cosIL and cosz for all orientations
cosIL <- data.frame()

for ( a in 1 : length(snum) ) {
  # zenith angle (degrees) between the light bank and camera, varies with slope
  # * ( 180.0 / pi ) # converts to degrees
  zenith1 <- atan(A[1] / (E-H[a])) # in radians for input into cos() function
  zenith2 <- atan(A[2] / (E-H[a])) # in radians for input into cos() function
  meanz <- (zenith1+zenith2) / 2 # in radians for input into cos() function
  
  #azimuth <- c(0, (120+snum[a]))
  #azimuth <- c(0, (90 + zenith1))
  
  for ( b in 1 : length(anum) ) {
    cosIL3 <- data.frame(matrix(nrow=1, ncol=16))
    names(cosIL3) <- c('slope','aspect',
                       'z1','z2','meanz','cosz1','cosz2','cosmeanz','meancosz',
                       'cosIL1','cosIL2','meancosIL',
                       'r1','r2','rmeans','rcosmeans')
    
    cosIL3$slope <- snum[a] # slope
    cosIL3$aspect <- anum[b] # aspect1 (original aspect definition)
    #cosIL3[,3] <- anum2[b] # aspect2
    
    cosIL3$z1 <- zenith1*180/pi # units are degrees in the cosIL table 
    cosIL3$z2 <- zenith2*180/pi # units are degrees in the cosIL table 
    cosIL3$meanz <- meanz*180/pi # units are degrees in the cosIL table 
    
    cosIL3$cosz1 <- cos(zenith1) # cos(angle)=distance=length units
    cosIL3$cosz2 <- cos(zenith2) # cos(angle)=distance=length units
    cosIL3$cosmeanz <- cos(meanz) # cos(angle)=distance=length units
    meancosz <- ( cos(zenith1) + cos(zenith2) ) / 2 
    cosIL3$meancosz <- meancosz # mean(cos(z))
    
    # cos( illumination angle (IL) ) <- cos(zenith)*cos(slope) + 
    # sin(zenith)*sin(slope)*cos(azimuth-aspect)
    cosIL1 <- cos(zenith1)*cos(snum[a]*pi/180) + 
      sin(zenith1)*sin(snum[a]*pi/180)*cos((azimuth[1]-anum[b])*pi/180) 
    
    # azimuth=180 for light bank 2
    cosIL2 <- cos(zenith2)*cos(snum[a]*pi/180) + 
      sin(zenith2)*sin(snum[a]*pi/180)*cos((azimuth[2]-anum[b])*pi/180)
    
    cosIL3$cosIL1 <- cosIL1
    cosIL3$cosIL2 <- cosIL2
    
    meancosIL <- (cosIL1+cosIL2) / 2 # mean cosIL
    cosIL3$meancosIL <- meancosIL
    
    #cosIL3[,11] <- ( (cos(zenith1) / c1) + (cos(zenith2) / c2) ) / 2
    cosIL3$r1 <- cos(zenith1) / cosIL1
    cosIL3$r2 <- cos(zenith2) / cosIL2
    
    cosIL3$rmeans <- cos(meanz) / meancosIL
    cosIL3$rcosmeans <- meancosz / meancosIL # ** use this one
    
    cosIL <- rbind (cosIL, cosIL3) 
    
  } # close b (aspect)
  
} # close a (slope)

# Output file
save ( list = c ( 'cosIL' ) , 
       file = paste ("../Output Files/HSICalLib_20230223_s0-s60_cosIL_all.RData", 
                     sep="") )

load("../Output Files/HSICalLib_20230223_s0-s60_cosIL_all.RData")

# Free up memory
rm(list=c('cosIL3','cosIL1','cosIL2','meancosIL','meanz',
          'A','a','b','E','H','h',
          'zenith1','zenith2','azimuth','meancosz'))

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# plots with cosIL
# ---------------------------------------------------------------------------

mycol2 <- hcl.colors(13, palette = "Roma")
mycol <- hcl.colors(7, palette = "Dynamic") 

# --- light bank 1
pdf(file = "../Plots/HSICalLib_20221128_cosIL1_slope.pdf", 
    width=8, height=8)
plot(cosIL1~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)],
     ylim=c(-0.5,1),
     ylab="cos(illumination angle)", main="LightBank1 (N)")
legend("bottomleft", legend=c(unique(cosIL$aspect)), 
       col=mycol2, pch=20, cex=1, bty ="n")
dev.off()

# --- cosIL light bank 2
pdf(file = "../Plots/HSICalLib_20221128_cosIL2_slope.pdf", 
    width=8, height=8)
plot(cosIL2~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)],
     #ylim=c(-0.5,1),
     ylab="cos(illumination angle)", main="LightBank2 (S)")
legend("bottomleft", legend=c(unique(cosIL$aspect)), 
       col=mycol2, pch=20, cex=1, bty ="n")
dev.off()


# --- light bank 1
pdf(file = "../Plots/HSICalLib_20221128_cosIL1_aspect.pdf", 
    width=8, height=8)
plot(cosIL1~aspect, data=cosIL, col=mycol[factor(cosIL$slope)],
     ylim=c(-0.5,1),
     ylab="cos(illumination angle)", main="LightBank1 (N)")
legend("bottomleft", legend=c(unique(cosIL$slope)), 
       col=mycol, pch=20, cex=1, bty ="n")
dev.off()

# --- cosIL light bank 2
pdf(file = "../Plots/HSICalLib_20221128_cosIL2_aspect.pdf", 
    width=8, height=8)
plot(cosIL2~aspect, data=cosIL, col=mycol[factor(cosIL$slope)],
     #ylim=c(-0.5,1),
     ylab="cos(illumination angle)", main="LightBank2 (S)")
legend("bottomleft", legend=c(unique(cosIL$slope)), 
       col=mycol, pch=20, cex=1, bty ="n")
dev.off()


# --- zenith 
pdf(file = "../Plots/HSICalLib_20221128_zenith_slope.pdf", 
    width=8, height=8)
plot(z1~slope, data=cosIL, col="dark grey",
     ylim=c(25,45),
     ylab="zenith angle")
points(z2~slope, data=cosIL)
points(zmean~slope, data=cosIL, col="brown")
legend("bottomleft", legend=c("light bank 1","light bank 2","mean"), 
       col=c("dark grey","black","brown"), pch=20, cex=1, bty ="n")
dev.off()

# --- mean cosz
pdf(file = "../Plots/HSICalLib_20221128_coszmean_slope.pdf", 
    width=8, height=8)
plot(coszmean~slope, data=cosIL, col="brown",
     ylim=c(0.6,1),
     ylab="cos(zenith angle)")
dev.off()

# --- mean cosIL
pdf(file = "../Plots/HSICalLib_20221128_cosILmean_slope.pdf", 
    width=8, height=8)
plot(cmean~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)],
     ylim=c(-0.5,1),
     ylab="cos(illumination angle)", main="mean cosIL")
legend("bottomleft", legend=c(unique(cosIL$aspect)), 
       col=mycol2, pch=20, cex=1, bty ="n")
dev.off()

# --- mean zenith
pdf(file = "../Plots/HSICalLib_20221128_coszmean_slope.pdf", 
    width=8, height=8)
plot(coszmean~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)],
     ylim=c(-0.5,1),
     ylab="cos(zenith angle)", main="mean zenith")
legend("bottomleft", legend=c(unique(cosIL$aspect)), 
       col=mycol2, pch=20, cex=1, bty ="n")
dev.off()

# --- ratio

# light bank1
pdf(file = "../Plots/HSICalLib_20221128_r1_slope.pdf", 
    width=8, height=8)
plot(r1~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)], 
     ylab = "cos(z)/cos(I)", ylim=c(-50,200),
     main="lightbank1")
legend("topleft", legend=c(unique(cosIL$aspect)), 
       col=mycol2, pch=20, cex=1, bty ="n")
dev.off()

# light bank2
pdf(file = "../Plots/HSICalLib_20221128_r2_slope.pdf", 
    width=8, height=8)
plot(r2~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)], 
     ylab = "cos(z)/cos(I)", ylim=c(-50,200), 
     main="lightbank2")
legend("topleft", legend=c(unique(cosIL$aspect)), 
       col=mycol2, pch=20, cex=1, bty ="n")
dev.off()

# mean
# pdf(file = "../Plots/HSICalLib_20221128_rmean_slope.pdf", 
#     width=8, height=8)
# plot(rmean~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)], 
#      ylab = "cos(z)/cos(I)", ylim=c(-50,200), 
#      main="mean")
# legend("topleft", legend=c(unique(cosIL$aspect)), 
#        col=mycol2, pch=20, cex=1, bty ="n")
# dev.off()

# rcosmeans
plot(rcosmeans~slope, data=cosIL, col=mycol2[factor(cosIL$aspect)], 
     ylab = "cos(z)/cos(gamma)")
legend("topleft", legend=c(unique(cosIL$aspect)), 
       col=mycol2, pch=20, cex=1, bty ="n")

plot(rcosmeans~aspect, data=cosIL, col=mycol[factor(cosIL$slope)], 
     ylab = "cos(z)/cos(gamma)")
legend("topright", legend=c(unique(cosIL$slope)), 
       col=mycol, pch=20, cex=1, bty ="n")

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 
#
# ---------------------------------------------------------------------------
