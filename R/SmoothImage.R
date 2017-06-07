#whoo
library(pixmap)

#divides matrices into equally sized blocks^2 pieces
divideintosubmatrices <- function(matrices, blocks = 1) {
  listsofsubmatrices <- lapply(matrices, function(matrixtodivide) {
    submatrices <- list()
    nr <- nrow(matrixtodivide) #get number of rows
    nc <- ncol(matrixtodivide) #get number of columns
    for(x in 1:blocks) {
      for(y in 1:blocks) {
        #ceiling to handle odd-numbered nr/nc
        xmin <- ceiling(nr/blocks*(x-1)+1)
        xmax <- ceiling(nr/blocks*x)
        ymin <- ceiling(nc/blocks*(y-1)+1)
        ymax <- ceiling(nc/blocks*y)
        submatrices[[length(submatrices)+1]] <- matrixtodivide[xmin:xmax, ymin:ymax]
      }
    }
    return(submatrices)
  })
  return(unlist(listsofsubmatrices, recursive = FALSE))
}

#combines matrices that have been divided into blocks^2 pieces
combinesubmatrices <- function(submatrices, blocks = 1) {
  matrices <- list()
  index <- 0
  while(index < length(submatrices)) {
    combinedmatrix <- NULL
    for(x in 1:blocks) {
      combinedmatrixy <- NULL
      for(y in 1:blocks) {
        combinedmatrixy <- cbind(combinedmatrixy, submatrices[[index + ((x-1)*blocks)+y]])
      }
      combinedmatrix <- rbind(combinedmatrix, combinedmatrixy)
    }
    matrices[[length(matrices) + 1]] <- combinedmatrix
    index <- length(matrices) * blocks^2
  }
  return(matrices)
}

#central function 
getnonbound <- function(img, k = 1, color = TRUE, blocks = 1) {
  if (color == TRUE) {
    colors <- list(img@red, img@blue, img@green)
  } else {
    colors <- list(img@grey)
  }
  submatrices <- divideintosubmatrices(colors, blocks)
  decompose <- lapply(submatrices, getnonboundMatrix, k=k)
  
  return(decompose)
}

getnonboundMatrix <- function(a, k = 1) {
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

makenoise <- function(img, color = TRUE) {
  if (color == TRUE) {
    colors <- list(img@red, img@blue, img@green)
  
    noisymatrices <- lapply(colors, makenoiseMatrix)
  
    img@red <- noisymatrices[1][[1]]
    img@blue <- noisymatrices[2][[1]]
    img@green <- noisymatrices[3][[1]]
  
    # img@red <- makenoiseMatrix(img@red)
    # img@blue <- makenoiseMatrix(img@blue)
  } else {
    colors <- list(img@grey)
    
    noisymatrices <- lapply(colors, makenoiseMatrix)
    
    img@grey <- noisymatrices[1][[1]]
  }
  
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

denoise <- function(img, xylists, k=1, alpha=0, color = TRUE, blocks = 1) {
  colors <- NULL
  if (color == TRUE) {
    colors <- list(img@red, img@blue, img@green)
  } else {
    colors <- list(img@grey)
  }
  #divide colors (or bw) into submatrices
  submatrices <- divideintosubmatrices(colors, blocks)
  
  #denoise
  #SIMPLIFY = FALSE is needed to prevent it from combining all the results into one misshapen matrix
  denoisedmatrices <- mapply(function(imgmatrix, xylist, k, alpha) {
    return(denoiseMatrix(imgmatrix, xylist, k, alpha)) # denoiseMatrix(submatrices[[1]], xylists[1][[1]], k, alpha)
  }, submatrices, xylists, k=k, alpha=alpha, SIMPLIFY = FALSE)
  
  #recombine
  matrices <- combinesubmatrices(denoisedmatrices, blocks)
  
  #write denoised matrices to image
  if (color == TRUE) {
    img@red <- matrices[1][[1]]
    img@blue <- matrices[2][[1]]
    img@green <- matrices[3][[1]]
  } else {
    img@grey <- matrices[1][[1]]
  }
  
  #img is now denoised
  return(img)
}

denoiseMatrix <- function(a, list, k=1, alpha=0) {
  nr <- nrow(a)
  nc <- ncol(a)
  
  # x <- c(5,2,3,2,9)
  # y <- c(1,2,1,0,5)
  # df <- data.frame(x,y)
  # names(df) <- c('x','y')
  # lmout <- lm(y ~ .,data = df)
  # df4 <- data.frame(x=4)
  # predict(lmout, df4)
  # lm(dfa[,1] ~ ., data = dfa[,-1])
  
  #use the predict function from lm
  #use fitted.values
  # instead of list[1][[1]][,1]+list[1][[1]][,2]..., do:
  # https://stackoverflow.com/questions/11991692/using-rs-lm-on-a-dataframe-with-a-list-of-predictors
  pred <- predict(lm(list[2][[1]] ~ ., data=as.data.frame(list[1][[1]])))
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

# alpha = 1 means use original image; alpha = 0 means use prediction only
main <- function(imgname = "LLLColor.ppm", k = 2, alpha = 0.0, color = TRUE, blocks = 4) {
  img <- read.pnm(imgname)
  
  list <- getnonbound(img, k, color, blocks)
  #regress y on x - aka use the 4 neighbors
  # yonx <- lm(list[2][[1]] ~ list[1][[1]][,1]+list[1][[1]][,2]+list[1][[1]][,3]+list[1][[1]][,4]) #linear model
  #plot(yonx) #plot

  #Add blemishes to the image
  print("Adding blemishes")
  noisedimg <- makenoise(img, color)
  plot(noisedimg) #display
  
  cat("K is", k)
  cat("Alpha is", alpha)
  
  noisedlist <- getnonbound(noisedimg, k, color, blocks)
  
  #Used predicted value for blemishes
  denoisedimg <- denoise(noisedimg, noisedlist, k, alpha, color, blocks)
  plot(denoisedimg) #display
}

# main("LLL.pgm")
