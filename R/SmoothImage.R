library(pixmap)

#central function 
getnonbound <- function(img) {
  ## extract the pixel array, numbers in [0,1], darkest to lightest
  a <- img@grey
  
  nr <- nrow(a) #get number of rows
  nc <- ncol(a) #get number of columns
  totalpixels = (nr-2)*(nc-2) #don't get the border
  
  centerpixels <- as.numeric(t(a[2:(nr-1), 2:(nc-1)])) #centers
  north <- t(a[1:(nr-2), 2:(nc-1)]) #north
  south <- t(a[3:(nr), 2:(nc-1)]) #south
  east <- t(a[2:(nr-1), 3:(nc)]) #east
  west <- t(a[2:(nr-1), 1:(nc-2)]) #west
  
  #reshape to 1 column
  dim(north) <- c(totalpixels, 1)
  dim(south) <- c(totalpixels, 1)
  dim(east) <- c(totalpixels, 1)
  dim(west) <- c(totalpixels, 1)
  
  #set x and y to something
  neighborpixels <- matrix(data = 0, nrow = totalpixels, ncol = 4) #four columns for four directions
  neighborpixels[,4] <- north
  neighborpixels[,3] <- south
  neighborpixels[,1] <- east
  neighborpixels[,2] <- west
  
  # #returns our list containing x and y
  xylist <- list(neighborpixels, centerpixels)
  #xylist is a list with (xylist.x, xylist.y)
  return(xylist)
}

makenoise <- function(img) {
  a <- img@grey ### extract the pixel array, numbers in [0,1], darkest to lightest
  #makes 15% of pixels messed up 
  arbpixels <- ncol(a)*nrow(a)*0.15 
  #makes pixels 0,1
  pixvals <- runif(arbpixels)
  #make pixvals an array of 15% of n by n and makes them corrupt
  #sample pixels from 1 to lenghth of a and get a, return an array and assign them to values in pixvals
  a[sample(1:length(a), length(pixvals))] <- pixvals 
  img@grey <- a 
  #LLL.pmg now has specks 
  return(img)
}

denoise <- function(img, list, alpha=0) {
  ### extract the pixel array, numbers in [0,1], darkest to lightest 
  a <- img@grey
  nr <- nrow(a)
  nc <- ncol(a)
  
  #use the predict function from lm
  #use fitted.values
  pred <- predict(lm(list[2][[1]] ~ list[1][[1]][,1]+list[1][[1]][,2]+list[1][[1]][,3]+list[1][[1]][,4]))
  #""list[2][[1]]"" first goes to [2] for y, looks at the first thing in y 
  #"list[1][[1]][,1]" first goes to [1] for x, goes to first matrix, and look at first column - repeat 4 times
  pred[pred > 1] <- 1
  #condition for a white pixel (1)

  #set the center pixel to the predicted value predicted from N,S,E,W pixels
  finalist <- (alpha*list[2][[1]]) + ((1-alpha)*pred) #replace pixel by alpha*pixel + (1-alpha)*predicted value
  finalist <- as.matrix(finalist)
  dim(finalist) <- c((nc-2),(nr-2)) #dimensions flipped because we need to transpose
  a[2:(nr-1), 2:(nc-1)] <- t(finalist)
  
  #Set the denoised image to the image and return the final image
  denoised <- img
  denoised@grey <- a
  return(denoised)
}


main <- function(imgname) {
  img <- read.pnm(imgname)
  list <- getnonbound(img)
  #regress y on x - aka use the 4 neighbors
  yonx <- lm(list[2][[1]] ~ list[1][[1]][,1]+list[1][[1]][,2]+list[1][[1]][,3]+list[1][[1]][,4]) #linear model
  #plot(yonx) #plot
   
  #Add blemishes to the image
  print("Adding blemishes")
  noisedimg <- makenoise(img)
  noisedlist <- getnonbound(noisedimg)
  plot(noisedimg) #display
  
  #Used predicted value for blemishes
  print("Denoise with 0.5")
  denoisedimg <- denoise(noisedimg, noisedlist, 0.5)
  plot(denoisedimg) #display
}

main("LLL.pgm")
