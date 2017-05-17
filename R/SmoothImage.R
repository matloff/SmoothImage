#whoo
library(pixmap)

#central function 
getnonbound <- function(img, k=1) {
  colors <- list(img@red, img@blue, img@green)
  
  decompose <- lapply(colors, getnonboundMatrix, k=k)
  
  return(decompose)
}

getnonboundMatrix <- function(a,k=1) {
  nr <- nrow(a) #get number of rows
  nc <- ncol(a) #get number of columns
  totalpixels = (nr-2*k)*(nc-2*k)
  
  centerpixels <- as.numeric(t(a[(k+1):(nr-k), (k+1):(nc-k)])) #centers
  neighborpixels <- matrix(data = 0, nrow = totalpixels, ncol = 4*k) #four columns for four directions times k
  
  currentcol = 1
  for(direction in list("north", "south", "east", "west")) {
    for(offset in 1:k) {
      shiftedmatrix <- getshiftedmatrix(a, k, direction, offset)
      shiftedmatrix <- t(shiftedmatrix)
      dim(shiftedmatrix) <- c(totalpixels, 1) #reshape to 1 column
      neighborpixels[,currentcol] <- shiftedmatrix
      currentcol = currentcol + 1
    }
  }
  
  # #returns our list containing x and y
  xylist <- list(neighborpixels, centerpixels)
  #xylist is a list with (xylist.x, xylist.y)
  return(xylist)
}

getshiftedmatrix <- function(nullm, k, direction, distance) {
  offsetrow = 0
  offsetcol = 0
  
  if(direction == "north") {
    offsetrow = -distance
  } else if(direction == "south") {
    offsetrow = distance
  } else if(direction == "west") {
    offsetcol = -distance
  } else if(direction == "east") {
    offsetcol = distance
  }
  
  return(nullm[(k+1+offsetrow):(nrow(nullm)-k+offsetrow), (k+1+offsetcol):(ncol(nullm)-k+offsetcol)])
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

denoise <- function(img, xylists, k=1, alpha=0) {
  img@red <- denoiseMatrix(img@red, xylists[1][[1]], k, alpha)
  img@blue <- denoiseMatrix(img@blue, xylists[2][[1]], k, alpha)
  img@green <- denoiseMatrix(img@green, xylists[3][[1]], k, alpha)
  
  #img is now denoised
  return(img)
}

denoiseMatrix <- function(a, list, k=1, alpha=0) {
  nr <- nrow(a)
  nc <- ncol(a)
  
  #use the predict function from lm
  #use fitted.values
  pred <- predict(lm(list[2][[1]] ~ rowSums(list[1][[1]]))) # sum rows instead of list[1][[1]][,1]+list[1][[1]][,2]...
  #""list[2][[1]]"" first goes to [2] for y, looks at the first thing in y 
  #"list[1][[1]][,1]" first goes to [1] for x, goes to first matrix, and look at first column - repeat 4 times
  pred[pred > 1] <- 1
  pred[pred < 0] <- 0
  #condition for a white pixel (1)
  #set the center pixel to the predicted value predicted from N,S,E,W pixels
  finalist <- (alpha*list[2][[1]]) + ((1-alpha)*pred) #replace pixel by alpha*pixel + (1-alpha)*predicted value
  finalist <- as.matrix(finalist)
  dim(finalist) <- c((nc-2*k),(nr-2*k)) #dimensions flipped because we need to transpose
  a[(k+1):(nr-k), (k+1):(nc-k)] <- t(finalist)
  # a <- t(finalist)
  # a <- a[(k+1):(nrow(a)-k),(k+1):(ncol(a)-k)]
  
  return(a)
}


main <- function(imgname, k=1, alpha=0.5) {
  img <- read.pnm(imgname)
  
  list <- getnonbound(img, k)
  #regress y on x - aka use the 4 neighbors
  # yonx <- lm(list[2][[1]] ~ list[1][[1]][,1]+list[1][[1]][,2]+list[1][[1]][,3]+list[1][[1]][,4]) #linear model
  #plot(yonx) #plot

  #Add blemishes to the image
  print("Adding blemishes")
  noisedimg <- makenoise(img)
  print("K is")
  print(k)
  noisedlist <- getnonbound(noisedimg, k)
  plot(noisedimg) #display

  #Used predicted value for blemishes
  print("Alpha is")
  print(alpha)
  denoisedimg <- denoise(noisedimg, noisedlist, k, alpha)
  plot(denoisedimg) #display
}

# main("LLL.pgm")
