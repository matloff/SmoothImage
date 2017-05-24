\name{SmoothImage}
\alias{SmoothImage}
\title{Image Smoothing Via Regression Modeling}
\description{
    Use of regression models to ameliorate imperfections in color images.
    Option to choose percentage of original image to retain.
    Usage of k-NN with default at 1 and option to increase k. 
}

\usage{
    #"main" right now, change to smoothimage later?
    main(img, k, alpha)
}

\arguments{
    
    \item{img}{
        Black and white or colored image, convert to ppm format,
        read with pixmap package - "read.pnm(img)"
    }
    \item{k}{
        Number of nearest neighbors
    }
    \item{alpha}{
        Percentage of original image to retain (0.0 - 1.0)
    }
}

\details{
    #talk about stuff
    Talk about the stuff in general
}

\value{
    #what it will be outputted as
    The return value of \code{main} is a smoothed out ppm image. 
}

\author{
Norm Matloff <matloff@cs.ucdavis.edu>, Jessica Tsoi <jttsoi@ucdavis.edu>
}

\examples{
    # to be continued
}
