library(pixmap)

#central function 
getnonbound <- function(img) {
  ## extract the pixel array, numbers in [0,1], darkest to lightest
  #a <- img@grey
  
  # temp1 = matrix(1:36,6,6)
  # temp2 = matrix(37:72,6,6)
  # colors <- list(temp1, temp2)
  
  colors <- list(img@red, img@blue, img@green)
  
  decompose <- lapply(colors, getnonboundMatrix)
  
  return(decompose)
}

  
getnonboundMatrix <- function(a) {
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
  colors <- list(img@red, img@blue, img@green)

  noisymatrices <- lapply(colors, makenoiseMatrix)

  img@red <- noisymatrices[1][[1]]
  img@blue <- noisymatrices[2][[1]]
  img@green <- noisymatrices[3][[1]]
  
  # img@red <- makenoiseMatrix(img@red)
  # img@blue <- makenoiseMatrix(img@blue)
  
  #img now has specks
  return(img)
}

makenoiseMatrix <- function(a) {
  #makes 15% of pixels messed up 
  arbpixels <- ncol(a)*nrow(a)*0.15 
  #makes pixels 0,1
  pixvals <- runif(arbpixels)
  #make pixvals an array of 15% of n by n and makes them corrupt
  #sample pixels from 1 to lenghth of a and get a, return an array and assign them to values in pixvals
  a[sample(1:length(a), length(pixvals))] <- pixvals 
  return(a)
}

denoise <- function(img, xylists, alpha=0) {
  img@red <- denoiseMatrix(img@red, xylists[1][[1]], alpha)
  img@blue <- denoiseMatrix(img@blue, xylists[2][[1]], alpha)
  img@green <- denoiseMatrix(img@green, xylists[3][[1]], alpha)
  
  #img is now denoised
  return(img)
}

denoiseMatrix <- function(a, list, alpha=0) {
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
  
  return(a)
}


main <- function(imgname) {
  img <- read.pnm(imgname)
  
  list <- getnonbound(img)
  #regress y on x - aka use the 4 neighbors
  # yonx <- lm(list[2][[1]] ~ list[1][[1]][,1]+list[1][[1]][,2]+list[1][[1]][,3]+list[1][[1]][,4]) #linear model
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

# run main("LLLColor.ppm") with colored ppm image
