# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Load in .raw and .hdr files (output from HSI) and output
#             HSI array, RGB array, and RGB tiff
# Author: Alyssa M. Duro and Daniel Hirmas
# Last edited: 6/13/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Goals:
# ---------------------------------------------------------------------------

# 1. Output hsi.RData, rgbmat.RData, and rgbmat.tiff from .hdr and .raw inputs.

#   1.a. Output template.RData with the row and column coordinates for every
#        pixel within all 40 sample wells for all 98 configurations from
#        rgbmat.RData inputs.

# 2. Output intensities for every wavelength for every pixel that occurs
#    within a sample well from hsi.RData and template.RData inputs.

# 3. Output the average and SD of intensities across all the pixels within 
#    each sample well for every wavelength.

# ---------------------------------------------------------------------------
# Load packages:
# ---------------------------------------------------------------------------

# Run this each time you re-open the script:
require ( raster ) # to plot rasterImage
require ( caTools ) # read.ENVI
require ( tiff ) # to output images as .tiff

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

# Add each date you scan to the vector "date"
date <- c ( '20210810' , '20210916' , '20210917' , '20210918' , '20210919' , 
            '20210926' , '20210927' , '20211016' , '20211018' , '20211202' , 
            '20211230' , '20220104' , '20220107' , '20220111' , '20220115' ,
            '20220117' , '20220118' , '20220119' , '20220124' , '20220127' ,
            '20220201' , '20220203' , '20220209' , '20220222' , '20220803' , 
            '20220822' )

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

# Change prefix as needed for output files
prefix <- 'HSICalLib'

# Set to TRUE to output all files
outputlogical <- TRUE 

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Goal 1:
# Get hsi, rgbmat, and rgbmat.tiff output from .hdr and .raw inputs.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# ----------
# Use a for loop to bring in scans one at a time by iterating through each 
# element of the vectors above to get the file names for each scan.
# Slope first, then aspect so we get an aspect (0 to 90 and 180 to 270 by 15's)
# for each slope (0 to 60 by 10's).

# Read in the .raw and .hdr files for the HSI and white and dark calibrations
# using the read.ENVI function (in the caTools package).

# DesiredArrayName <- read.ENVI('FileName.raw',headerfile='FileName.hdr')
# ----------

# ----------
# batch and date can be updated manually (left out of the loop) while 
# working with them one at a time.
# ----------

# Set these manually depending on which configuration you want.
a <- 1 # slope (in the loop)
b <- 1 # aspect (in the loop)
d <- 30 # date & batch (not in the loop)
#e <- 1 # batch (not in the loop)

# create a progress bar where 100% means each slope has been processed
pb <- txtProgressBar ( min = 0 , max = length ( slope ) , style = 3 ) 

for ( a in 1 : length ( slope ) ) {
  for ( b in 1 : length ( aspect ) ) {
    
    # ----------
    # Assign the file names hsifile, whitefile, and darkfile for read.ENVI 
    # ----------
    
    # name of the white calibration file
    whitefile <- paste ( 'hsi' , prefix , batch[d] , dateref[d] , 'white' , 
                         sep = "_" )
    
    # name of the dark calibration file
    darkfile <- paste ( 'hsi' , prefix , batch[d] , dateref[d] , 'dark' , 
                        sep = "_" )
    
    # name of the hsi file
    hsifile <- paste ( 'hsi', prefix , batch[d] , dateref[d] , slope[a] , 
                       aspect[b] , sep = "_" )
    
    # ----------
    # Bring in the .hdr and .raw data files for these 3 arrays 
    # (hsifile, whitefile, and darkfile) using read.ENVI and put in a list
    # ----------
    
    # create a list of NULL's:
    hsi <- list ( NULL )
    
    # Look in the Data folder
    setwd ( dname )
  
    # Bring in hsifile (big file) from the data folder and assign it to the  
    # first element of the hsi list 
    
    # hsi[[1]] <- hsifile
    hsi[[1]] <- read.ENVI ( paste ( hsifile , ".raw" , sep = "" ) ,
                            headerfile = paste ( hsifile , ".hdr" , 
                                               sep = "" ) )
    
    # Bring in whitecal and darkcal (smaller files) from the data folder
    # and assign them to the 2nd and 3rd elements of the hsi list
    
    # hsi[[2]] <- whitefile
    hsi[[2]] <- read.ENVI ( paste ( whitefile , ".raw" , sep = "" ) ,
                            headerfile = paste ( whitefile , ".hdr" , 
                                               sep = "" ) )
    
    # hsi[[3]] <- darkfile
    hsi[[3]] <- read.ENVI ( paste ( darkfile , ".raw" , sep = "" ) ,
                            headerfile = paste ( darkfile , ".hdr" , 
                                               sep = "" ) )
    
    # ----------
    # White and dark calibration
    # ----------
    
    # 1. The vector darkcal contains the average intensities for 
    # each wavelength averaged across both spatial dimensions (rows and cols).
    # The 2D matrix whitecal contains the average intensity for each column
    # at each wavelength.
    
    # 2. A single dark intensity value (mdarkcal) is obtained by calculating 
    # the average intensity across all wavelengths in the darkcal vector
    
    # 3. The dark corrected whitecalcorr matrix is obtained by subtracting 
    # the mdarkcal value from the row averaged intensities for each column at 
    # each wavelength from the white calibration matrix (whitecal)
    
    # 4. Reassign the hsi list to contain only the array from the study scan
    # and not the white and dark calibration scans to save space
    
    # After all this you're left with hsi[[1]] containing the desired hsi
    # array and a matrix (whitecalcorr) with the spatially averaged intensities
    # across the rows for each column at each wavelength (862 columns, 471 rows)
    
    # ----------
    # Calculate the whitecal matrix and darkcal vector:
    # ----------
    
    # matrix containing the average white intensity for each column at 
    # each wavelength (862 columns x 471 rows)
    whitecal <- colMeans ( hsi[[2]] ) 
    
    # vector containing the average dark intensity for each wavelength
    darkcal <- colMeans ( colMeans ( hsi[[3]] ) )

    # ----------
    # Calculate a single value for dark calibration intensity:
    # ----------
    
    # mean dark calibration intensity across all rows, columns, and wavelengths 
    mdarkcal <- mean ( darkcal ) 
    
    # ----------
    # Dark correct the average white intensities for each wavelength:
    # ----------
    
    # white calibration matrix corrected using mdarkcal
    whitecalcorr <- whitecal - mdarkcal
    
    # Reassign hsi to remove the white and dark files to save space in memory
    hsi <- hsi[[1]]
    
    # Correct the hyperspectral data using the white and dark calibration data:
    hsicorr <- hsi - array(mdarkcal,dim=dim(hsi)) # dark correction
    hsicorr <- hsicorr / aperm(array(whitecalcorr, # white correction
                                     dim=c(dim(whitecalcorr),
                                           dim(hsicorr)[1])), c(3,1,2))
    
    # resaves the corrected data back as hsi
    hsi <- hsicorr 
      
    # Remove hsicorr from memory to save space
    rm ( hsicorr )
      
    # create an empty array to hold the RGB data
    rgbmat <- array ( as.numeric ( NA ) , c ( dim ( hsi ) [1:2] , 3 ) ) 
      
    # Calculate the index that holds the RGB data in hsi
    # position that corresponds to the red wavelength 
    # which is position 280 in hsi * dimensions of hsi[3] / 600
    # then round to 0 digits (nearest whole number)
    Rhold <- round ( ( 680 - 400 ) * dim ( hsi ) [3] / 600 , 0 ) # ~674 nm
      
    # position that corresponds to the green wavelength 
    Ghold <- round ( ( 545 - 400 ) * dim ( hsi ) [3] / 600 , 0 ) # ~540 nm
      
    # position that corresponds to the blue wavelength
    Bhold <- round ( ( 440 - 400 ) * dim ( hsi ) [3] / 600 , 0 ) # ~431 nm
      
    # Store the RGB data and correct the high and low values in rgbmat
    rgbmat[,,1] <- hsi[,,Rhold]
    rgbmat[,,2] <- hsi[,,Ghold]
    rgbmat[,,3] <- hsi[,,Bhold] 
    
    # capping normalized intensities between 0 and 1
    # if normalized intensity > 1, then assign it 1
    # if normalized intensity < 0, then assign it 0
    rgbmat[,,1][which(rgbmat[,,1] > 1)] <- 1
    rgbmat[,,1][which(rgbmat[,,1] < 0)] <- 0
    rgbmat[,,2][which(rgbmat[,,2] > 1)] <- 1
    rgbmat[,,2][which(rgbmat[,,2] < 0)] <- 0
    rgbmat[,,3][which(rgbmat[,,3] > 1)] <- 1
    rgbmat[,,3][which(rgbmat[,,3] < 0)] <- 0
    
    # set to TRUE to output hsi, rgbmat, and rgbmat.tiff
    if ( outputlogical == TRUE) {
      
      # Name the output files 
      hsiout <- paste ( prefix , batch[d] , dateref[d] , slope[a] , 
                         aspect[b] , sep = "_" )

      # Look in the Output Files folder
      setwd ( oname )
      
      # Output these .RData and .tiff files to the Output Files folder
      save ( list = c ( 'hsi' ) , 
             file = paste ( hsiout , 'hsi.RData' , sep = "_" ) )
      
      save ( list = c ( 'rgbmat' ) , 
             file = paste ( hsiout , 'rgbmat.RData' , sep = "_" ) )
      
      writeTIFF ( rgbmat , 
              paste ( hsiout , 'mmResolution_RGB.tiff' , sep = '_' ) )
      
    } # Close the if loop for outputting hsi, rgbmat, and rgmat.tiff
    
    # Remove rgbmat and hsi from memory
    rm ( hsi )
    rm ( rgbmat )
    
  } # close the aspect (b) loop
  
  # update progress bar
  setTxtProgressBar ( pb , a )
  
} # close the slope (a) loop

# close progress bar
close ( pb ) 

# ---------------------------------------------------------------------------
# Getting oriented with the RGB plot:
# ---------------------------------------------------------------------------

# Create an empty plot:
par ( mai = c ( 0 , 0 , 0 , 0 ) )
plot ( x = c ( 0 , 1 ) , y = c ( 0, 1 ) , 
type = "n" , xlab = "" , ylab = "")

# Add the rgbmat (or some other desired) array to the plot:
rasterImage ( rgbmat , 0 , 0 , 1 , 1 )

# Bottom left on the rgb plot (x = 0,y = 0) 
#   rows = lines = 1472 
#     number of pixels, parallel to the scan (camera movement)
#     depends on the scan settings (start and stop position)
#   samples = columns = 0 
#     width, parallel to the light banks
#     depends on the scan settings (position of the camera)

# Bottom right on the rgb plot (x = 1,y = 0) 
#     rows = lines = 1472, samples = columns = 862

# Top left on the rgb plot (x = 0,y = 1) 
#     rows = lines = 0, columns = samples = 0

# Top right on the rgb plot (x = 1,y = 1) 
#     rows = lines = 0, columns = samples = 862

# What you see on the plot is a mirror image of what the user sees in person 
# because the intensities are collected from the camera's perspective.

# The tray is being rotated clockwise under the camera from the user's
# perspective, but appears to rotate counterclockwise on the rgbplot.

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
