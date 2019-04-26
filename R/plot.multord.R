#' Plot function for MultOrd
#' 
#' Plot function for a \code{MultOrd} object. Plots show coefficients of the explanatory variables, both with repect to location and response styles.
#' The coefficient pairs are displayed as stars, where the rays represent (1-alpha) confidence intervals.
#' 
#' @usage \method{plot}{MultOrd}(x, alpha = 0.05, CIfactor = 0.9, \dots)
#' @param x \code{MultOrd} object
#' @param alpha Specifies the confidence level 1-alpha of the confidence interval. 
#' @param KI.factor Argument that helps to control the appearance (the width) of the stars that represent the confidence intervals of both 
#' paraemters (location and response style) corresponding to one covariate.
#' @param ... Further plot arguments.
#' @author Gunther Schauberger\cr \email{gunther.schauberger@@tum.de}\cr
#' \url{https://www.researchgate.net/profile/Gunther_Schauberger2}
#' @seealso \code{\link{multord}}, \code{\link{ctrl.multord}}
#' @examples
#' \dontrun{
#' data(confidence)
#' 
#' ## create a small subset of the data to speed up calculations
#' set.seed(1860)
#' confidence <- confidence[sample(1:nrow(confidence), 300),]
#' 
#' ## scale all metric variables to get comparable parameter estimates
#' confidence$age <- scale(confidence$age)
#' confidence$income <- scale(confidence$income)
#' 
#' ## two formulas, one without and one with explanatory variables (gender and age)
#' f.conf0 <- as.formula(paste("cbind(",paste(names(confidence)[1:4],collapse=","),") ~ 1"))
#' f.conf1 <- as.formula(paste("cbind(",paste(names(confidence)[1:4],collapse=","),") ~ gender + age"))
#' 
#' 
#' 
#' ####
#' ## Adjacent Categories Models
#' ####
#' 
#' ## Multivariate adjacent categories model, without response style, without explanatory variables
#' m.conf0 <- multord(f.conf0, data = confidence, control = ctrl.multord(RS = FALSE))
#' m.conf0
#' 
#' ## Multivariate adjacent categories model, with response style as a random effect, without explanatory variables
#' m.conf1 <- multord(f.conf0, data = confidence)
#' m.conf1
#' 
#' ## Multivariate adjacent categories model, with response style as a random effect, 
#' ## without explanatory variables for response style BUT for location
#' m.conf2 <- multord(f.conf1, data = confidence, control = ctrl.multord(XRS = FALSE))
#' m.conf2
#' 
#' ## Multivariate adjacent categories model, with response style as a random effect, with explanatory variables for location AND response style
#' m.conf3 <- multord(f.conf1, data = confidence)
#' m.conf3
#' 
#' plot(m.conf3)
#' 
#' 
#' 
#' ####
#' ## Cumulative Models
#' ####
#' 
#' ## Multivariate cumulative model, without response style, without explanatory variables
#' m.conf0.cumul <- multord(f.conf0, data = confidence, control = ctrl.multord(RS = FALSE), model = "cumulative")
#' m.conf0.cumul
#' 
#' ## Multivariate cumulative model, with response style as a random effect, without explanatory variables
#' m.conf1.cumul <- multord(f.conf0, data = confidence, model = "cumulative")
#' m.conf1.cumul
#' 
#' ## Multivariate cumulative model, with response style as a random effect, 
#' ## without explanatory variables for response style BUT for location
#' m.conf2.cumul <- multord(f.conf1, data = confidence, control = ctrl.multord(XRS = FALSE), model = "cumulative")
#' m.conf2.cumul
#' 
#' ## Multivariate cumulative model, with response style as a random effect, with explanatory variables for location AND response style
#' m.conf3.cumul <- multord(f.conf1, data = confidence, model = "cumulative")
#' m.conf3.cumul
#' 
#' plot(m.conf3.cumul)
#' }
plot.MultOrd <- function(x, alpha = 0.05, CIfactor = 0.9, ...){
  
  quant <- qnorm(1-alpha/2)
  
  if(is.na(x$beta.X[1])|is.na(x$beta.XRS[1])){
    stop("Plotting is only possible if covariates are used both for 
         the location and response style effect!")
  }
  
  betaX <- x$beta.X
  betaX.KI <- exp(cbind(betaX-quant*x$se.X,betaX+quant*x$se.X))
  betaX <- exp(betaX)
  
  betaXRS <- x$beta.XRS
  betaXRS.KI <- exp(cbind(betaXRS-quant*x$se.XRS,betaXRS+quant*x$se.XRS))
  betaXRS <- exp(betaXRS)
  
  
  
  plot(betaX,betaXRS,pch=16,xlim=range(c(1,betaX.KI)),ylim=range(c(1,betaXRS.KI)),
       xlab=expression(exp(gamma)),ylab=expression(exp(alpha)), ...)
  
  p.X <- length(betaX)
  
  label.x <- label.y <- c()

  for(i in 1:p.X){
    
    x <- c(betaX.KI[i,1],betaX.KI[i,1]+(betaX[i]-betaX.KI[i,1])*(CIfactor),betaX[i],betaX[i]+(betaX[i]-betaX.KI[i,1])*(1-CIfactor),
           betaX.KI[i,2],betaX[i]+(betaX[i]-betaX.KI[i,1])*(1-CIfactor),betaX[i],betaX.KI[i,1]+(betaX[i]-betaX.KI[i,1])*(CIfactor),
           betaX.KI[i,1])
    
    y <- c(betaXRS[i],betaXRS.KI[i,1]+(betaXRS[i]-betaXRS.KI[i,1])*(CIfactor),betaXRS.KI[i,1],betaXRS.KI[i,1]+(betaXRS[i]-betaXRS.KI[i,1])*(CIfactor),betaXRS[i],
           betaXRS[i]+(betaXRS[i]-betaXRS.KI[i,1])*(1-CIfactor),betaXRS.KI[i,2],betaXRS[i]+(betaXRS[i]-betaXRS.KI[i,1])*(1-CIfactor),betaXRS[i])
    
    polygon(x,y,col=grey(0.9))
    label.x <- c(label.x,x[6])
    label.y <- c(label.y,y[6])
  }
  points(betaX,betaXRS,pch=16)
  abline(h=1,lty=2,lwd=2,col="gray")
  abline(v=1,lty=2,lwd=2,col="gray")
  
  text(label.x,label.y,labels=names(betaX),adj=c(-0.1,-0.1))
}
