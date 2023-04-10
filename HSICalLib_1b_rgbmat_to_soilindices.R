# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Create "templates" for each configuration which indicate the 
#             row, column indices for each sample well at each configuration
# Author: Alyssa M. Duro
# Last edited: 4/10/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Goals
# ---------------------------------------------------------------------------

# 1. Output sghsi and rgbmat.RData and rgbmat.tiff from .hdr and .raw inputs.

#   1.a. Output template.RData with the row and column coordinates for every
#        pixel within all 40 sample wells for all 98 configurations from
#        rgbmat.RData inputs. 

# 2. Output intensities for every wavelength for every pixel that occurs
#    within a sample well from sghsi and template.RData inputs.

# 3. Output the average and SD of intensities across all the pixels within 
#    each sample well for every wavelength.

# ---------------------------------------------------------------------------
# Load packages
# ---------------------------------------------------------------------------

require(raster)
require(tiff)

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

# Add each date you scan to the vector "date".
date <- paste ( '2021' , c ( '0810' ) , sep = "" )

# Test scan dates: 
# '0624' '0625' '0708' '0719' '0726' '0728'

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
# Goal 1a:
# Make a template with the row and column coordinates for every pixel
# occurring within each sample well at each slope, aspect configuration
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# ----------
# Everything here is user-guided.
# ----------

# ----------
# Bring in rgbmat for one orientation at a time:
# ----------

a <- 1 # slope 
b <- 1 # aspect 

# name of the rgbmat file for use with load()
rgbfile <- paste ( prefix , batch [ 1 ] , date [ 1 ] , slope [ a ] ,
                   aspect [ b ] , 'rgbmat.RData' , sep = "_" )

# load the rgbmat.RData file
load ( paste ( oname, rgbfile, sep = "/" ) )

# load("E:/HSI Calibration Library/Data Analysis/Output Files/
#       HSICalLib_b1_20210726_s60_a0_rgbmat.RData")

# Plot rgbmat for use with locator()
par ( mai = c ( 0 , 0 , 0 , 0 ) )
plot ( x = c ( 0 , 1 ) , y = c ( 0, 1 ) , type = "n" , xlab = "" , ylab = "")
rasterImage ( rgbmat , 0 , 0 , 1 , 1 )

# ----------
# Identify the 4 row, column vertices (1 to 4) of each sample well and 
# create 4 linear models (a to d) that enclose a polygon that outlines
# each sample well.

# Fit a line between points 1-2, 2-3, 3-4, and 4-1
  # y = m*x + b 

# rows = slope*columns + intercept

# slope = (rows at point 2 - rows at point 1) / 
        # (columns at point 2 - columns at point 1) 

# intercept = rows - (slope*columns)
# ----------

# ----------
# Isolate the row and column coordinates for each pixel from each well
# using the models you just created:

# 1. isolate one wavelength at a time [,,i]

# 2. isolate one column index at a time at that wavelength [,j,]

# 3. isolate one row index at a time within that column [k,,]

# 4. compare the row index to the row index predicted by each model

# 5. if the condition is met, the intensity for that row, column, wavelength
# [k,j,i] are added to a vector that holds all the intensity values

# 6. put the average intensity for each wavelength into a vector (avgwaveint)
# so we have an average intensity value at each wavelength
# ----------

# Start this list way up here since it needs to be added to each time you run
# locator() on a different sample well. 
soilindices <- list ( NULL )
wellindices <- list ( NULL )

# m is the element of soilindices (the sample well)
m <- 1

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# START HERE FOR EACH SAMPLE WELL WHEN MAKING THE TEMPLATES.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Use locator() to select 4 corners of one sample well from the rgbmat plot
well <- locator ( 4 ) 

# Store those 4 points in two columns in a data.frame as x and y so 
# we can eventually add two more columns (soilrows and soil cols)
# and add each pair of row and column indices (which we calculate below) 
# as an additional row. 
welldf <- data.frame ( well ) 

# set the numcolumns variable to the number of rows in one column at one 
# wavelength which is length(sghsi[1,,1])
numcolumns <- length ( rgbmat[ 1,,1 ] )

# set the numrows variable to the number of columns in one row at one 
# wavelength which is length(sghsi[,1,1])
numrows <- length ( rgbmat[ ,1,1 ] )

# Calculate the column and row indices corresponding to x and y from locator.
# columnindex = x * 862
cindex <- trunc ( welldf$x * numcolumns )
welldf <- cbind ( welldf , cindex )

# rowindex = 1-y * (number of pixels in the direction of the scan)
rindex <- trunc ( ( 1 - welldf$y ) * numrows )
welldf <- cbind ( welldf, rindex )

# Store welldf in a list so you have the row and column indices 
# corresponding to the corners of each well
wellindices[[m]] <- welldf

# Linear equation for the lines between the points:
# 1 and 2     2 and 3    3 and 4    4 and 1 
aslope <- ( welldf$rindex[2] - welldf$rindex[1] ) / 
  ( welldf$cindex[2] - welldf$cindex[1] )
aintercept <- welldf$rindex[1] - ( aslope * welldf$cindex[1] )

bslope <- ( welldf$rindex[3] - welldf$rindex[2] ) / 
  ( welldf$cindex[3] - welldf$cindex[2] )
bintercept <- welldf$rindex[2] - ( bslope*welldf$cindex[2] )

cslope <- ( welldf$rindex[4] - welldf$rindex[3] ) / 
  ( welldf$cindex[4] - welldf$cindex[3] )
cintercept <- welldf$rindex[3] - ( cslope*welldf$cindex[3] )

dslope <- ( welldf$rindex[1] - welldf$rindex[4] ) / 
  ( welldf$cindex[1] - welldf$cindex[4] )
dintercept <- welldf$rindex[4] - ( dslope*welldf$cindex[4] )

# store the row and column indices occurring within these boundaries
soilrows <- numeric()
soilcols <- numeric()

# look at the jth column from the cindex at point 1 to the cindex at point 3
for ( j in welldf$cindex[1] : welldf$cindex[3] ) { 
  
  # These are the row indices predicted by the linear models 
  # fit between the points selected using locator()
  # using the column index being looked at right now as input
  amodel <- ( aslope * j ) + aintercept
  bmodel <- ( bslope * j ) + bintercept
  cmodel <- ( cslope * j ) + cintercept
  dmodel <- ( dslope * j ) + dintercept
  
  # kth row index in the jth vector (column index) meets this criteria
  for ( k in welldf$rindex[2] : welldf$rindex[4] ) { 
    
    if ( k >= amodel && k >= bmodel && k <= cmodel && k <= dmodel ) { 
    # a0 and 90 use k >= amodel && k <= cmodel 
    # a15 to a75 use k >= amodel && k >= bmodel && k <= cmodel && k <= dmodel
      
      # when k is in a certain range do these things:
      soilrows <- c ( soilrows , k )
      soilcols <- c ( soilcols , j )
      
    } # Close the k if loop
    
  } # Close the k for loop
  
} # Close the j for loop

# Store all the row, column pairs in a dataframe called soildf then put 
# soildf in a list where each element represents a sample well (1 to 80)
soildf <- data.frame ( soilcols , soilrows )

# Store soildf as the first element of the list soilindices for the first
# sample well then increase this number by one each time you repeat locator()
# So there will be 80 elements of soilindices for each scan.
soilindices[[m]] <- soildf

# Increase m by one each time to save the next sample well in the correct
# element of soilindices, intensity, and wellindices lists
m <- m + 1

# display the current m so you remember which sample well you're working on
m

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# FINISH HERE FOR EACH SAMPLE WELL WHEN MAKING THE TEMPLATES.
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# ----------
# Turn the red and blue band intensities to 0 and green to 1 and plot to see
# the boundaries of the sample wells defined in the template.
# ----------

rgbsoils <- rgbmat

for ( o in 1 : length ( soilindices ) ) {
  for ( p in 1 : nrow ( soilindices[[o]] ) ) {
    rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 1] <- 0
    rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 2] <- 1
    rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 3] <- 0
  }
}

par ( mai = c ( 0 , 0 , 0 , 0 ) )
plot ( x = c ( 0 , 1 ) , y = c ( 0 , 1 ) , type = "n" , xlab = "" , ylab = "" )
rasterImage ( rgbsoils ,0 , 0 , 1 , 1 ) 

# display the current m so you remember which sample well you're working on
m

# ----------
# Output this plot as a .tiff
# The resulting image shows the rbgmat of the scan with the pixels indicated
# by the template turned green. 

# Output soilindices list to the Output Files folder once all 40 elements
# (sample wells) are populated for this configuration.
# ----------

if ( outputlogical == TRUE ) {

  # Name the output file
  rgbsoilsout <- paste ( prefix , batch[ 1 ] , date[ 1 ] , slope[ a ] , 
                     aspect[ b ] , 'mmResolution' , 'RGB' ,  
                     'soilindices.tiff' , sep = "_" )
  
  soilindicesout <- paste ( prefix , batch[ 1 ] , date[ 1 ] , slope[ a ] , 
                            aspect[ b ] , 'soilindices.RData' , 
                            sep = "_" )
  
  setwd ( oname )
  
  save ( list = c ( 'soilindices' ) , 
         file = paste ( soilindicesout ) )
  
  writeTIFF ( rgbsoils , paste ( rgbsoilsout ) )
  
} # Close the loop used for outputting the .tiff and soilindices.RData file

# ----------
# Remove the rgbmat, rgbsoils, and soilindices lists from memory

rm ( rgbmat )
rm ( rgbsoils )
rm ( soilindices )

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
