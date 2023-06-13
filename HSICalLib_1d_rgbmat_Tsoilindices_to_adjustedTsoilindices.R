# ---------------------------------------------------------------------------
# R Script for the HSI Calibration Library 
# ---------------------------------------------------------------------------
# Objective: Adjust "templates" after they've been applied to scans obtained at
#             each configuration so pixel indices better match each sample well
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

library(tiff)

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
# Goal 1c:
# Make an rgb plot of one scan with the row and column coordinates for
# every pixel occurring within each sample well turned grey using 
# the coordinates from the soil indices template.THEN, use the script below
# to adjust the row, column coordinates so that the pixels recorded
# (i.e., colored squares) fall within the sample well boundaries.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

# ----------
# Everything here is user-guided.
# ----------

# ----------
# Bring in rgbmat for one configuration and plot:
# ----------

# Set these manually depending on which configuration you want.
a <- 5 # slope 
b <- 14 # aspect 
d <- 11 # dateref & batch
#e <- 13 # batch 

# create a progress bar where 100% means each slope has been processed
#pb <- txtProgressBar ( min = 0 , max = length ( slope ) , style = 3 ) 

#for ( a in 2 : 3 ) { # a goes with slope
  #for ( b in 9 : length ( aspect ) ) { # b goes with aspect
    
    # ----------
    # Start here (unless using the slope(a), aspect(b) loop)

    # name of the rgbmat file for use with load()
    rgbfile <- paste ( prefix , batch [ d ] , dateref [ d ] , slope [ a ] ,
                       aspect [ b ] , 'rgbmat.RData' , sep = "_" )
    
    # name of the soilindices file for use with load()
    Tsifile <- paste ( prefix , batch [ d ] , dateref [ d ] , slope [ a ] ,
                       aspect [ b ] , 'Tsoilindices.RData' , sep = "_" )
    
    # load the rgbmat.RData file
    load ( paste ( oname, rgbfile, sep = "/" ) )
    
    # ----------
    # Bring in the template:
    
    # This is a .RData file that is a list of 40 (one for each sample well). 
    # Each of the 40 elements consist of a data frame with 2 columns 
    # (soilrows and soilcols). These row, column coordinates indicate which 
    # pixels (pixel = row, column pari) lie within each sample well. 
    # ----------
    
    # load the soilindices.RData file
    load ( paste ( oname, Tsifile, sep = "/" ) )
    
    # ----------
    # Plot rgbmat with the pixels from soilindices turned blue:
    # ----------
    
    # Make a copy of rgbmat for plotting and call it rgbsoils
    rgbsoils <- rgbmat
    
    # Turn the pixels in rgbsoils indicated by soilindices some color
    for ( o in 1 : length ( soilindices ) ) {
      for ( p in 1 : nrow ( soilindices[[o]] ) ) {
        # 1 = red 
        rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 1] <- 0.5
        
        # 1 = green
        rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 2] <- 0.5
        
        # 1 = blue
        rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 3] <- 0.5
      }
    }
    
    par ( mai = c ( 0 , 0 , 0 , 0 ) )
    plot ( x = c ( 0 , 1 ) , y = c ( 0 , 1 ) , 
           type = "n" , xlab = "" , ylab = "" )
    rasterImage ( rgbsoils ,0 , 0 , 1 , 1 ) 
    
    # ----------
    # Adjust soilindices where necessary:
    
    # Use the loop if you want to move ALL the wells at once.
    # ----------
    m <- 20 # set m the well number you want to adjust ONE at a time
    
    C <- 3 # number of columns you want to shift soilindices left or right
    R <- 3 # number of rows you want to shift soilindices up or down
      
    # Adjust m based on which squares (well) you want to move
     # for ( m in 1 : 40 ) # advance to the next well automatically
       
     # m <- m+1 # m goes with well number # advance to the next well manually
    
      # adjust soilindices left or right                # pos to the right
      soilindices[[m]]$soilcols <- soilindices[[m]]$soilcols - C # left
      soilindices[[m]]$soilrows <- soilindices[[m]]$soilrows - R # up
      # adjust soilindices up or down                   # pos down
      
      # adjust soilindices left or right                # pos to the right
      soilindices[[m]]$soilcols <- soilindices[[m]]$soilcols + C # right
      soilindices[[m]]$soilrows <- soilindices[[m]]$soilrows + R # down
      # adjust soilindices up or down                   # pos down
      
    #} # close the m loop
    
    # Reset rgbsoils to rgbmat then use the adjusted soilindices template
    # to turn the adjusted soilindices [some color]
    rgbsoils <- rgbmat
    
    for ( o in 1 : length ( soilindices ) ) {
      for ( p in 1 : nrow ( soilindices[[o]] ) ) {
        rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 1] <- 0.5
        rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 2] <- 0.5
        rgbsoils[soilindices[[o]][p,2] , soilindices[[o]][p,1] , 3] <- 0.5
      }
    }
    
    par ( mai = c ( 0 , 0 , 0 , 0 ) )
    plot ( x = c ( 0 , 1 ) , y = c ( 0 , 1 ) , type = "n" , 
                                               xlab = "" , ylab = "" )
    rasterImage ( rgbsoils ,0 , 0 , 1 , 1 )
      
    # ----------
    # Output this plot as a .tiff
    # The resulting image shows the rbgmat of the scan with the pixels 
    # indicated by the template turned [some color]
    
    # Output soilindices list to the Output Files folder once all 40 elements
    # (sample wells) are populated with the row, column coordinates for all 
    # pixels within each well (1 to 40) at this configuration.
    # ----------
    
    #if ( outputlogical == TRUE ) {
      
      # Name the output file
      rgbsoilsout <- paste ( prefix , batch[ d ] , dateref[ d ] , slope[ a ] , 
                             aspect[ b ] , 'mmResolution' , 'RGB' ,  
                             'Tsoilindices.tiff' , sep = "_" )
      
      Tsoilindicesout <- paste ( prefix , batch[ d ] , dateref[ d ] , slope[ a ] , 
                                 aspect[ b ] , 'Tsoilindices.RData' , 
                                 sep = "_" )
      
      setwd ( oname )
      
      save ( list = c ( 'soilindices' ) , 
             file = paste ( Tsoilindicesout ) )
      
      writeTIFF ( rgbsoils , paste ( rgbsoilsout ) )
      
    #} # Close the loop used for outputting the .tiff and soilindices.RData file
    
    rm ( list = c ( 'rgbmat' , 'rgbsoils' , 'soilindices' ) )
    
    # advance to the next configuration that needs correction
    #a <- a+1 # slope 
    #b <- b+1 # aspect 
    #d <- d+1 # dateref & batch
    
    # ----------
    # stop here unless using the slope(a), aspect(b) loop
    
  #} # close the aspect (b) loop
    #b <- b+1
  
  # update progress bar
  #setTxtProgressBar ( pb , a )
  
#} # close the slope (a) loop

# close progress bar
#close ( pb ) 

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Move some files to the external hard drive for Ray
# ---------------------------------------------------------------------------

# Set these manually depending on which configuration you want.
a <- 1 # slope 
b <- 1 # aspect 
d <- 1 # date
e <- 1 # batch

# create a progress bar where 100% means each slope has been processed
pb <- txtProgressBar ( min = 0 , max = length ( batch ) , style = 3 ) 

for ( e in 1 : length ( batch ) ) {
  for ( a in 1 : length ( slope ) ) {
    for ( b in 1 : 9 ) {
      
      # name of the rgbmat file for use with load()
      rgbfile <- paste ( prefix , batch [ e ] , dateref [ e ] , slope [ a ] ,
                         aspect [ b ] , 'rgbmat.RData' , sep = "_" )
      
      # load the rgbmat.RData file
      load ( paste ( oname, rgbfile, sep = "/" ) )
      
      # name of the soilindices file for use with load()
      Tsifile <- paste ( prefix , batch [ e ] , dateref [ e ] , slope [ a ] ,
                         aspect [ b ] , 'Tsoilindices.RData' , sep = "_" )
      
      # load the soilindices.RData file
      load ( paste ( oname, Tsifile, sep = "/" ) )
      
      if ( outputlogical == TRUE ) {
        
        setwd ( paste( "E:/HSI Calibration Library/Data Analysis/Output Files" ) )
        
        save ( list = c ( 'soilindices' ) , 
               file = paste ( Tsifile ) )
        
        save ( list = c ( 'rgbmat' ) , 
               file = paste ( rgbfile ) )
        
      } # Close the output loop
      
      rm ( list = c ( 'rgbmat' , 'soilindices' ) )
      
    } # close the aspect (b) loop
    
    # update progress bar
    
    
  } # close the slope (a) loop
  
  #setTxtProgressBar ( pb , e )
  
} # close the batch (e) loop

close ( pb ) # close progress bar

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
#
#
# ---------------------------------------------------------------------------
