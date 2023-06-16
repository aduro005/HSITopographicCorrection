# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Merge the rutgers, NEON, Fire, UCR, and intmean data
# Author: Alyssa M. Duro
# Last edited: 6/16/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Goals:
# ---------------------------------------------------------------------------

# 1. Merge the Rutgers, NEON, Fire, and UCR sample prep .csv files using HSI
# sample number as the primary key

# 2. Combine all C data into 1 column, calculate volC (g/cm3) and log10volC

# 3. Prepare master intmean (get all the batches in 1 data frame)

# ---------------------------------------------------------------------------
# Load packages:
# ---------------------------------------------------------------------------

library(dplyr) # df %>% filter()

# ---------------------------------------------------------------------------
# Set the working directory and subfolder directories:
# ---------------------------------------------------------------------------

# Run this each time you re-open the script or clear memory. 
setwd ( ".." ) 
wname <- getwd () 
sname <- paste ( wname , "R Scripts" , sep = "/" )
funname <- paste ( wname , "Functions" , sep = "/" )
dname <- paste ( wname , "Data" , sep = "/" )
fname <- paste ( wname , "Plots" , sep = "/" )
oname <- paste ( wname , "Output Files" , sep = "/" )

# ---------------------------------------------------------------------------
# Set some global variables: 
# ---------------------------------------------------------------------------

# This makes it so the batch and date reference number are the same
dateref <- c ( '20210916' , '20210916' , '20210917' , '20210918' ,
               '20210919' , '20210926' , '20210926' , '20210927' ,
               '20211016' , '20211016' , '20211018' , '20211018' ,
               '20211202' , '20211230' , '20220104' , '20220107' ,
               '20220111' , '20220111' , '20220115' , '20220117' ,
               '20220118' , '20220119' , '20220124' , '20220127' ,
               '20220201' , '20220203' , '20220209' , '20220222' , 
               '20220803' , '20220822' )

# s0  s10   s20   s30   s40   s50   s60
slope <- paste ( 's' , seq ( from = 0 , to = 60, by = 10 ) , sep = "" )

# a0    a15   a30   a45   a60   a75   a90
# a180    a195   a210   a225   a240   a255   a270
aspect <- paste ( 'a' , c ( seq ( from = 0 , to = 90 , by = 15 ) , 
                            seq ( from = 180 , to = 270 , by = 15 ) ) , 
                  sep = "" )

# b1  b2  b3...
batch <- paste ( 'b' , rep ( 1 : 30 ) , sep = "" )

prefix <- 'HSICalLib'

# Set to TRUE to output all files
outputlogical <- TRUE 

# ---------------------------------------------------------------------------
# Bring in rutgers, neon, and hsi sample prep .csv files
# ---------------------------------------------------------------------------

dat <- '20230418'

# name the files for loading
prepfile <- paste ( prefix , dat , 'SamplePrepData_R.csv', sep = "_" )
firefile <-  paste ( prefix ,  dat , 'Fire_R.csv', sep = "_" )
neonfile <- paste ( prefix ,  dat, 'NEON_R.csv', sep = "_" )
rutgersfile <- paste ( prefix ,  dat, 'Rutgers_R.csv', sep = "_" )

# read.csv the data files
prep <- read.csv ( paste ( dname, prepfile, sep = "/" ) )
rutgers <- read.csv ( paste ( dname, rutgersfile, sep = "/" ) )
neon <- read.csv ( paste ( dname, neonfile, sep = "/" ) )
fire <- read.csv ( paste ( dname, firefile, sep = "/" ) )

# change the class in these columns from character to numeric
prep$HSInumber <- as.integer ( as.numeric ( prep$HSInumber ) )
rutgers$HSInumber <- as.integer ( as.numeric ( rutgers$HSInumber ) )
neon$HSInumber <- as.integer ( as.numeric ( neon$HSInumber ) )
fire$HSInumber <- as.integer ( as.numeric ( fire$HSInumber ) )

# convert carbonTot in NEON to SOC
neon$SOC <- rep ( NA , length ( neon [ , 1 ] ) )

for (i in 1:nrow(neon)) {
  # if caco3Conc is populated, subtract caco3Conc/10 from carbonTot 
  if ( is.na(neon[i,"caco3Conc"] ) == FALSE ) { 
    neon$SOC[i] <- (neon$carbonTot[i] - (neon$caco3Conc[i]/10)) / 10
  } # close if loop
  
  # if caco3Conc is NA
  if ( is.na(neon[i,"caco3Conc"] ) == TRUE ) {
    neon$SOC[i] <- neon$carbonTot[i] / 10  # converts units from g/kg to percent
  } # close if loop
  
} # close for loop

# merge
pr <- merge ( prep , rutgers , by = "HSInumber" , all.x = TRUE )
prn <- merge ( pr , neon , by = "HSInumber" , all.x = TRUE )
prnf <- merge ( prn , fire , by = "HSInumber" , all.x = TRUE )

rm(list=c("pr","prn","rutgers","neon","fire","prep"))

# ----------
# combine matching columns into OC, archive, adod
# ----------

prnf$OC <- rep ( NA , length ( prnf [ , 1 ] ) )
prnf$archive <- rep ( NA , length ( prnf [ , 1 ] ) )
prnf$adod <- rep ( NA , length ( prnf [ , 1 ] ) )

#i <- 1

# Start at the first row and go through all the rows in p
for ( i in 1 : length ( prnf[,1] ) ) { 
  
  # if NEON_archive is populated
  if ( is.na(prnf[i,"NEON_archive"]) == FALSE ) { 
    
    prnf$archive[i] <- prnf$NEON_archive[i]
    prnf$OC[i] <- prnf$SOC[i]
    prnf$adod[i] <- prnf$airDryOvenDryRatio[i]
  
  } # close the neon if loop
  
  # if Rutgers_archive is populated
  if ( is.na(prnf[i,"Rutgers_archive"]) == FALSE ) { 
    
    prnf$archive[i] <- prnf$Rutgers_archive[i]
    prnf$OC[i] <- prnf$Rutgers_TotalC[i]
    prnf$adod[i] <- prnf$HSI_ADOD[i]
    
  } # close the rutgers if loop
  
  # if Fire_archive is populated
  if ( is.na(prnf[i,"Fire_archive"]) == FALSE ) { 
    
    prnf$archive[i] <- prnf$Fire_archive[i]
    prnf$OC[i] <- prnf$Fire_TotalC[i]
    prnf$adod[i] <- prnf$HSI_ADOD[i]
    
  } # close the fire if loop
      
} # Close the i (row) loop

# ----------
# calculate volC
# ----------

# volC = (g C/100g OD soil) / 100 * (g AD soil / cm3) * 1 / (g AD soil / g OD soil)

prnf$volC <- rep ( NA , length ( prnf[,1] ) )

# Start at the first row and go through all the rows in p
for ( i in 1 : length ( prnf[,1] ) ) { 
  
  # If the prnf$OC column for the ith row is not NA
  if ( is.na ( prnf[i,"OC"] ) == FALSE ) { 
    
    # Calculate volC and put it in the zth element of volC
    # C = g C / 100 g soil 
    # packed density = g soil / cm3 
    # adod = AD mass / OD mass (unitless)
    prnf[i,"volC"] <- (prnf[i,"OC"]/100)*prnf[i,"HSIPackedDensity"]*(1/prnf[i,"adod"])
    
  } # Close the if loop
  
} # Close the z (row) loop

# ----------
# calculate log10volC
# ----------

prnf$log10volC <- log10(prnf$volC)

# ----------
# Output prnf
# ----------

if ( outputlogical == TRUE ) {
  
  # name of the master file (all configurations for one batch)
  prnfout <- paste ( prefix , batch[1] , batch[30] , 
                     'prep' , 'rutgers' , 'neon', 'fire.RData' 
                     , sep = "_" )
  setwd ( oname )
  save ( list = c ( 'prnf' ) , file = paste ( prnfout ) )
  
} # Close the loop outputting prnf

# ---------------------------------------------------------------------------
# Prepare master intmean (get all the batches in 1 data frame)
# ---------------------------------------------------------------------------

# ----------
# Create intmean for each batch
# ----------

a <- 1 # slope
b <- 1 # aspect
e <- 1 # batch & dateref

# create a progress bar
pb <- txtProgressBar ( min = 0 , max = length(batch) , style = 3 )

for(e in 1 : 30) {
  
for ( a in 1 : length ( slope ) ) {
    for ( b in 1 : length ( aspect) ) {
      
      intmeanfile <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                         slope [ a ] , aspect [ b ] ,
                         'intmean.RData', sep = "_" )
      load ( paste ( oname, intmeanfile, sep = "/" ) )
      
      if ( a == 1 && b == 1 ) {
        x <- intmean 
      } # close the if loop
      
      else {
        x <- rbind ( x , intmean )
      } # close the else loop
      
      } # Close the b (aspect) loop
  
  # Remove the intensities, soilindices, and hsi lists
  rm ( list = c ( 'intmean' ) )
  
  } # Close the a (slope) loop

# Output intmean and intsd for this configuration
if ( outputlogical == TRUE ) {
  
  # name of the master file (all configurations for one batch)
  masterout <- paste ( prefix , batch [ e ] , dateref [ e ] , 'intmean.RData' 
                  , sep = "_" )
  
  setwd ( oname )
  save ( list = c ( 'x' ) , file = paste ( masterout ) )
  } # Close the loop outputting intmean for this configuration

  # update progress bar and move on to the next batch
  setTxtProgressBar ( pb , e )

} # Close the e (batch) loop

# close progress bar
close ( pb )

# ----------
# Create masterintmean for all batches
# ----------

e <- 1 # date & batch

# create a progress bar
pb <- txtProgressBar ( min = 0 , max = length ( dateref ) , style = 3 )

for ( e in 1 : length ( dateref ) ) {

    xfile <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                           'intmean.RData', sep = "_" )
    
    load ( paste ( oname, xfile, sep = "/" ) )
    
    if ( e == 1 ) {
      masterintmean <- x
    }
    
    else {
      masterintmean <- rbind ( masterintmean , x )
    }
  
  # Remove the intensities, soilindices, and hsi lists
  rm ( list = c ( 'x' ) )
  
  # update progress bar and move on to the batch & date
  setTxtProgressBar ( pb , e )
  
} # Close the e (date & batch) loop

# Output intmean and intsd for all configurations
if ( outputlogical == TRUE ) {
  
  # name of the master file (all configurations for one batch)
  masterout <- paste ( prefix , batch[1] , batch[30] , 
                       'masterintmean.RData' , 
                       sep = "_" )
  # masterintmean = 40 wells * 30 batches * 98 configurations
  
  setwd ( oname )
  save ( list = c ( 'masterintmean' ) , file = paste ( masterout ) )
  
} # Close the loop outputting masterintmean for these batches

# close progress bar
close ( pb )

# ---------------------------------------------------------------------------
# Merge the master file with the prnf file 
# ---------------------------------------------------------------------------

# ----------
# Load in the masterintmean file and prnf
# ----------

# Bring in existing masterintmean file
mifile <- paste ( prefix , batch[1] , batch[30] , 
                  'masterintmean.RData', sep = "_" )

load ( paste ( oname, mifile, sep = "/" ) )

# Bring in the merged prep, rutgers, and neon file
prnffile <- paste ( prefix , batch[1] , batch[30] , 
                   'prep' , 'rutgers' , 'neon', 'fire.RData' 
                   , sep = "_" )

load ( paste ( oname, prnffile, sep = "/" ) )

# merge the masterintmean file with the prn file by BOTH well and batch columns
prnfmasterintmean <- merge ( prnf , masterintmean , by = c ( "batch" , "well" ) 
                            , all.x = TRUE )

# remove sample 709 & 610 (missing soil samples) and grab relevant columns
p <- prnfmasterintmean[,c(1:4,29:31,54:531)]
p <- p[-which(prnfmasterintmean$HSInumber==709),]
p <- p[-which(prnfmasterintmean$HSInumber==650),]

# p = 1178 samples * 98 configurations = 115,444

p$batchwellID <- paste("b",p$batch,"_w",p$well,"_s",p$slope,"_a",p$aspect, sep="")
names(p)[15:485] <- paste("obsI" , names(p)[15:485] , sep = "")

rm(list=c("masterintmean","prnfmasterintmean", "prnf"))

# Output p for all configurations 
if ( outputlogical == TRUE ) {
  
  # name of the master file (all configurations for one batch)
  masterout <- paste ( prefix , batch[1] , batch[30] , 
                       'p.RData' , 
                       sep = "_" )

  setwd ( oname )
  save ( list = c ( 'p' ) , file = paste ( masterout ) )
  
} # Close the loop outputting masterintmean for these batches

# ----------
# if you want to output 'p' as csv
# ----------

poutcsv <- paste ( prefix , batch[1] , batch[30] , 'p.csv' , sep = "_" )
write.csv(p, file = paste(poutcsv))

# ----------
# output data for the fire samples for Daniel
# ----------

firedf <- prnfmasterintmean[,c(1:6,50:531)]
firedf <- firedf[which(prnfmasterintmean$archive=='GrayLab'),]
fireoutcsv <- paste ( prefix, "FireSamples_spectral_SOC_allorientations.csv" , sep = "_" )
write.csv(firedf, file = paste(fireoutcsv))

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
