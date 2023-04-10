# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Output intensities for every wavelength for every pixel that occurs
#             within a sample well from hsi and template.RData inputs.
# Author: Alyssa M. Duro
# Last edited: 4/10/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Goals:
# ---------------------------------------------------------------------------

# 1. Output sghsi and rgbmat.RData and rgbmat.tiff from .hdr and .raw inputs.

#   1.a. Output template.RData with the row and column coordinates for every
#        pixel within all 40 sample wells for all 98 configurations from
#        rgbmat.RData inputs. 

# 2. Output intensities for every wavelength for every pixel that occurs
#    within a sample well from hsi and template.RData inputs.

# 3. Output the average and SD of intensities across all the pixels within 
#    each sample well for every wavelength.

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

# Change prefix as needed for output files
prefix <- 'HSICalLib'

# Set to TRUE to output all files
outputlogical <- TRUE 

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Goal 2:
# Get the average intensity for each wavelength for each pixel in the well
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# This loop goes through each element of soilindices, so
# run this loop using the soilindices template.

# ----------
# To get the intensities for each wavelength for each pixel within each
# sample well:
# ----------

# 1. Bring in soilindices list with 40 elements each containing the
# coordinates (soilrows, soilcols) of every pixel within each sample well.

# 2. Bring in the hsi array with the intensities (862ish rows by 
# 862 columns by 471 wavelengths)

# 3. Run this loop for every row of every element of soilindices to get the
# intensities for every pixel within each sample well.

# 4. Output a list called intensities where each element corresponds to a
# sample well (1 to 40), each column corresponds to a wavelength (471), and
# each row corresponds to a pixel (equals the number of rows in the same
# element of soilindices).

# # for now leaving out SG ----------
# The first and last 10 wavelengths (columns) in intensities are NA because
# the window for sg was 21 so sg began at 11 and ended at 461 (i.e., was not
# performed on wavelengths 1-10 or 462-471 nm). This is so that the number of
# wavelengths being input to sg calculations is never less than the window.

# Assign a list full of NULL's to store the intensities for each wavelength
# for each pixel from soilindices (each element of the list is a sample well, 
# each ROW of each element is a pixel and each COLUMN is a wavelength).

# ----------
# Use a for loop to bring in multiple scans at once by iterating through each
# element of the slope and aspect vectors to get the file names for each scan.
# Slope first, then aspect so we get an aspect (0 to 90 by 15's)
# for each slope (0 to 60 by 10's)
# ----------

a <- 1 # slope
b <- 1 # aspect
# d <- 20 # date
e <- 1 # batch & date

for ( e in 1:30 ) { # e goes with batch & date
  # create a progress bar
  pb <- txtProgressBar(min = 0, max = length(slope), style = 3)
  
  for ( a in 1:length(slope)) {
    for ( b in 1:length(aspect)) {
      
      # Reset intensities (measured array for each well) for this configuration
      intensities <- list ( NULL ) # each element is another well
      
      # name of the hsi file
      hsifile <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                           slope [ a ] , aspect [ b ] ,
                           'hsi.RData', sep = "_" )
      
      # name of the Tsoilindices file (each scan has it's own)
      Tsoilindicesfile <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                                 slope [ a ] , aspect [ b ] ,
                                 'Tsoilindices.RData', sep = "_" )
      
      # load the hsi.RData file
      load ( paste ( oname, hsifile, sep = "/" ) )
      
      # load("E:/HSI Calibration Library/Data Analysis/Output Files/
      #       hsi_HSICalLib_b1_20210726_s60_a0_hsi.RData")
                                    
      #load the soilindices.RData file
      load ( paste ( oname, Tsoilindicesfile, sep = "/" ) )
      
      # look at each element of the soil  indices list (each sample well)
      for ( c in 1 : length ( soilindices ) ) {
        
        # make a vector to store the intensity from hsi for each wavelength for
        # one pixel using one soilcols, soilrows pair or simply one row 
        # of soilindices[[c]] for the coordinates.
        
        # clear the matrix for each sample well (each cth element of soilindices)
        intmat <- matrix ( as.numeric ( NA ) , 
                    nrow = length ( soilindices[[c]]$soilrows ) , 
                    ncol = 471 ) 
        
        # look at each row of soilindices[[c]] for coordinates of each pixel
        for ( g in 1 : length ( soilindices[[c]]$soilrows ) ) {
        
        # Put the intensity for each wavelength for this pixel 
        # (i.e., this row of soilindices[[c]]) 
        # into a column of the matrix called intmat from
        # sghsi[soilrows, soilcols, blank so you get all the wavelengths]
        intmat[g,] <- hsi[ soilindices[[c]]$soilrows[g] ,
                         soilindices[[c]]$soilcols[g] , ]
        
        } # Close the g loop and move on to the next row of soilindices[[c]]
        
        # Before moving on to the next sample well
        # Store the intensity for each pixel and each wavelength in the same
        # element of intensities as soilindices so intensities[[c]]
        # corresponds to the same sample well as soilindices[[c]]
        
        intensities[[c]] <- intmat
        
        } # Close the c for loop and move on to the next element of soilindices
        
        # Output intensities list before moving on to the next configuration
      
        if ( outputlogical == TRUE ) {
        
          # Name the intensities output
          intensitiesout <- paste ( prefix , batch [ e ] , dateref [ e ] , 
                                    slope [ a ] , aspect [ b ] , 'hsi' , 
                                    'intensities.RData' , sep = "_" )
          setwd ( oname )
          save ( list = c ( 'intensities' ) , file = paste ( intensitiesout ) )
        
        } # Close the loop outputting the intensities list for this configuration
    
     } # Close the b (aspect) loop 
  
    # Remove the intensities, soilindices, and hsi lists
    rm ( list = c ( 'intensities' , 'soilindices' , 'hsi' ) )
    
    # update progress bar
    setTxtProgressBar ( pb , a )
  
  } # Close the a (slope) loop
  
  # close progress bar
  close ( pb )
  
  # clear memory 
  gc()

} # Close the e (batch & date) loop

# At last you have a list with the intensities for all wavelengths for every
# pixel within every sample well for this batch in this configuration.

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
