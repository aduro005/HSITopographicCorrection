# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Get the mean intensity and SD for each wavelength for the whole 
#             sample well (rather than for each pixel)
# Author: Alyssa M. Duro
# Last edited: 4/10/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Goals:
# ---------------------------------------------------------------------------

# 1. Output hsi and rgbmat.RData and rgbmat.tiff from .hdr and .raw inputs.

#   1.a. Output template.RData with the row and column coordinates for every
#        pixel within all 40 sample wells for all 98 configurations from
#        rgbmat.RData inputs. 

# 2. Output intensities for every wavelength for every pixel that occurs
#    within a sample well from hsi and template.RData inputs.

# 3. Output the average and SD of intensities across all the pixels within 
#    each sample well for every wavelength.

# ---------------------------------------------------------------------------
# Load packages:
# ---------------------------------------------------------------------------
  
# Run this each time you re-open the script. 
require ( raster )
require ( tiff )

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
slopedeg <- seq ( from = 0 , to = 60, by = 10 ) 

# a0    a15   a30   a45   a60   a75   a90
# a180    a195   a210   a225   a240   a255   a270
aspect <- paste ( 'a' , c ( seq ( from = 0 , to = 90 , by = 15 ) , 
                          seq ( from = 180 , to = 270 , by = 15 ) ) , sep = "" )
aspectdeg <- c ( seq ( from = 0 , to = 90 , by = 15 ) , 
                    seq ( from = 180 , to = 270 , by = 15 ) ) 

# b1  b2  b3...
batch <- paste ( 'b' , rep ( 1 : 30 ) , sep = "" )
batchnum <- rep ( 1 : 30 ) 

# Change prefix as needed for output files
prefix <- 'HSICalLib'

# Set to TRUE to output all files
outputlogical <- TRUE 

# Load the .RData file that contains the vector with the values of all 471
# wavelengths (from the .hdr file)
wavefile <- paste ( prefix , 'wavevec.RData', sep = "_" )
load ( paste ( oname, wavefile, sep = "/" ) )

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Goal 3:
# Get the mean intensity and SD for each wavelength for the whole 
# sample well (rather than for each pixel)
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

a <- 1 # slope
b <- 1 # aspect
#d <- 1 # date
e <- 1 # batch & dateref

for ( e in 1:30 ) {

# create a progress bar
pb <- txtProgressBar ( min = 0 , max = length ( slope ) , style = 3 )

for ( a in 1 : length ( slope ) ) {
  for ( b in 1 : length ( aspect ) ) {
    
    # ----------
    # Bring in intensities.RData
    # ----------
    
    # name of the intensities file
    intfile <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                               slope [ a ] , aspect [ b ] , 'hsi' ,
                              'intensities.RData' , sep = "_" )
    
    # load the intensities file
    load ( paste ( oname, intfile, sep = "/" ) )
    
    # ----------
    # intensities --> intmean
    # ----------

    # Make a data frame called intmean full of NA's with 40 rows and 
    # 475 columns (1 row for each sample well, and a column for the  
    # spatially averaged intensities for 471 wavelengths, 1 well ID, 
    # 1 batch ID, 1 slope, 1 aspect.
    intmean <- data.frame( matrix ( as.numeric ( NA ), nrow = 40 , ncol = 475 ))
    
    # Rename the columns in the intmean data frame
    names(intmean) <- c('well', 'batch', 'slope', 'aspect', 
                        paste(wavevec[1:471]))
    
    # Number the rows 1 to 40 in the first (well ID) column of intmean.
    intmean[,1] <- 1:40 
    
    # Batch (2nd column), slope (3rd column), and aspect (4th column) 
    intmean[,2] <- batchnum [ e ]
    intmean[,3] <- slopedeg [ a ]
    intmean[,4] <- aspectdeg [ b ]

    # Go through each element of intensities (each sample well) and store the 
    # spatially averaged intensity for all 471 wavelengths in columns 5 to 475
    # of the intmean data frame.

      for ( c in 1 : length ( intensities ) ) {
    
        intmean[c,5:475] <- round ( colMeans ( intensities[[c]] ) , 
                                    digits = 5 )

      } # Close the c intensities element (sample well) --> intmean row loop

    # ----------
    # intensities --> intsd
    # ----------
    
    # Make a matrix to store the SD for each wavelength across all pixels in 
    # each sample well at this orientation.
    
    intsd <- data.frame(matrix ( as.numeric ( NA ), nrow = 40 , ncol = 475 ))
    # intsd <- data.frame ( matrix ( nrow = 40 , ncol = 475 ) ) 
    
    # Rename the columns in the intsd data frame
    names(intsd) <- c('well', 'batch', 'slope', 'aspect', 
                        paste(wavevec[1:471]))
    
    # Number the rows in the first (well ID) column of intsd 1 to 40.
    intsd[,1] <- 1:40 
    
    # batch (2nd column), slope (3rd column), and aspect (4th column) 
    intsd[,2] <- batchnum [ e ]
    intsd[,3] <- slopedeg [ a ]
    intsd[,4] <- aspectdeg [ b ]
    
    # Go through each element of intensities (each sample well) and store the 
    # sd of intensities for all 471 wavelengths in columns 5 to 475
    # of the intsd data frame.
    for ( c in 1 : length ( intensities ) ) {
      
      intsd[c,5:475] <- round( apply ( intensities[[c]] , 2 , sd ) , 
                               digits = 5 )
      
    } # Close the a (intensities element --> intsd row) loop
    
    # Output intmean and intsd for this configuration
    if ( outputlogical == TRUE ) {
      
      # name of the intmean file
      intmeanout <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                            slope [ a ] , aspect [ b ] ,
                            'intmean.RData' , sep = "_" )
      
      # name of the intsd file
      intsdout <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                          slope [ a ] , aspect [ b ] ,
                          'intsd.RData' , sep = "_" )
      
      setwd ( oname )
      save ( list = c ( 'intmean' ) , file = paste ( intmeanout ) )
      save ( list = c ( 'intsd' ) , file = paste ( intsdout ) )
      
    } # Close the loop outputting intmean and intsd for this configuration
    
    # Remove the intensities, soilindices, and hsi lists
    rm ( list = c ( 'intmean' , 'intsd' , 'intensities' ) )
    
  } # Close the b (aspect) loop
    
  # update progress bar and move on to the next slope
  setTxtProgressBar ( pb , a )
    
} # Close the a (slope) loop
  
# close progress bar
close ( pb )

} # Close the e (batch & dateref) loop

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
waveout <- paste ( prefix , 'wavevec.RData' , sep = "_" )

setwd ( oname )
save ( list = c ( 'wavevec' ) , file = paste ( waveout ) )
# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
