# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Apply "templates" to scans obtained at each configuration to 
#             isolate spectra from row, column indices from each sample well
# Author: Alyssa M. Duro
# Last edited: 6/13/2023
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Goals:
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

# Add each date you scan to the vector "date".
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
# Goal 1b:
# Make an rgb plot of one scan with the row and column coordinates for
# every pixel occurring within each sample well turned blue using 
# the coordinates from the soil indices template.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# ----------
# Almost everything here is user-guided.
# ----------

# ----------
# Bring in rgbmat for one configuration and plot:
# ----------
# Set these manually depending on which configuration you want.
a <- 1 # slope (in the loop)
b <- 1 # aspect (in the loop)
d <- 30 # date & batch (not in the loop)
#e <- 1 # batch (not in the loop)

for ( a in 1 : length (slope) ) {
for ( b in 1 : length (aspect) ) {

# name of the rgbmat file for use with load()
rgbfile <- paste ( prefix , batch[ d ] , dateref[ d ] , slope[ a ] ,
                   aspect[ b ] , 'rgbmat.RData' , sep = "_" )

# load the rgbmat.RData file
load ( paste ( oname, rgbfile, sep = "/" ) )

# ----------
# Bring in the template:

# soilindices is a .RData file that is a list of 40 (one for each sample well). 
# Each of the 40 elements consist of a data frame with 2 columns 
# (soilrows and soilcols). These row, column coordinates indicate which pixels
# lie within each sample well. 
# ----------

# name of the soilindices file for use with load()
sifile <- paste ( prefix , batch [ 4 ] , date [ 4 ] , slope [ a ] ,
                   aspect [ b ] , 'Tsoilindices.RData' , sep = "_" )

# load the soilindices.RData file
load ( paste ( oname, sifile, sep = "/" ) )

# ----------
# Plot rgbmat with the pixels from soilindices turned blue:
# ----------

rgbsoils <- rgbmat

for ( o in 1 : length ( soilindices ) ) {
  for ( p in 1 : nrow ( soilindices[[o]] ) ) {
    rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 1] <- 0 # red
    rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 2] <- 0 # green
    rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 3] <- 1 # blue
  }
}

# view the plots in R (if you want):
#par ( mai = c ( 0 , 0 , 0 , 0 ) )
#plot ( x = c ( 0 , 1 ) , y = c ( 0 , 1 ) , type = "n" , xlab = "" , ylab = "" )
#rasterImage ( rgbsoils ,0 , 0 , 1 , 1 ) 

# ----------
# Adjust soilindices (if necessary):
# Set m to the well number (or use the template script) to adjust ONE at a time.  
# Use the loop to move ALL the wells at once. 
# ----------

# Plot rgbmat
#par ( mai = c ( 0 , 0 , 0 , 0 ) )
#plot ( x = c ( 0 , 1 ) , y = c ( 0, 1 ) , type = "n" , xlab = "" , ylab = "" )
#rasterImage ( rgbmat , 0 , 0 , 1 , 1 )

#cols <- 10 # number of columns you want to shift soilindices (pos to the right)
#rows <- 10 # number of rows you want to shift soilindices (pos down)

#for ( m in 1 : length ( soilindices ) ) {
  
#  soilindices[[m]] $ soilcols <- soilindices[[m]] $ soilcols + cols
#  soilindices[[m]] $ soilrows <- soilindices[[m]] $ soilrows + rows
  
#} # close the m loop

# ----------
# Output this plot as a .tiff
# The resulting image shows the rbgmat of the scan with the pixels indicated
# by the template turned blue. 

# Output soilindices list to the Output Files folder once all 40 elements
# (sample wells) are populated with the row, column coordinates within each 
# at this configuration.
# ----------

if ( outputlogical == TRUE ) {

  # Name the output file
  rgbsoilsout <- paste ( prefix , batch[d] , dateref[ d ] , slope[ a ] , 
                     aspect[b] , 'mmResolution' , 'RGB' ,  
                     'Tsoilindices.tiff' , sep = "_" )
  
  Tsoilindicesout <- paste ( prefix , batch[d] , dateref[ d ] , slope[ a ] , 
                            aspect[b] , 'Tsoilindices.RData' , 
                            sep = "_" )
  
  setwd ( oname )
  
  save ( list = c ( 'soilindices' ) , 
         file = paste ( Tsoilindicesout ) )
  
  writeTIFF ( rgbsoils , paste ( rgbsoilsout ) )
  
} # Close the loop used for outputting the .tiff and soilindices.RData file

rm ( list = c ( 'rgbmat' , 'rgbsoils' , 'soilindices' ) )

} # Close the b (aspect) loop

} # Close the a (slope) loop

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
