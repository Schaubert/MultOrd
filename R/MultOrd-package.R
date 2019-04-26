
#' Model Multivariate Ordinal Responses Including Response Styles
#' 
#' A model for multivariate ordinal responses. The response is modelled 
#' using a mixed model approach that is also capable of the inclusion 
#' of response style effects of the respondents.
#' 
#' 
#' @name MultOrd-package
#' @docType package
#' @author Gunther Schauberger\cr \email{gunther.schauberger@@tum.de}\cr
#' \url{https://www.researchgate.net/profile/Gunther_Schauberger2}
#' @seealso \code{\link{multord}}  \code{\link{ctrl.multord}}  \code{\link{plot.MultOrd}}
#' @keywords multivariate ordinal response style adjacent categories cumulative 
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
#' m.conf2 <- multord(f.conf1, data = confidence, control = ctrl.multord(XforRS = FALSE))
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
#' m.conf2.cumul <- multord(f.conf1, data = confidence, control = ctrl.multord(XforRS = FALSE), model = "cumulative")
#' m.conf2.cumul
#' 
#' ## Multivariate cumulative model, with response style as a random effect, with explanatory variables for location AND response style
#' m.conf3.cumul <- multord(f.conf1, data = confidence, model = "cumulative")
#' m.conf3.cumul
#' 
#' plot(m.conf3.cumul)
#' }
NULL


#' Tenseness data from the Freiburg Complaint Checklist (tenseness)
#' 
#' Data from the Freiburg Complaint Checklist. 
#' The data contain all 8 items corresponding to the scale \emph{Tenseness} for 2042 participants of the 
#' standardization sample of the Freiburg Complaint Checklist. 
#' 
#' @name tenseness
#' @docType data
#' @format A data frame containing data from the Freiburg Complaint Checklist with 2042 observations. 
#' All items refer to the scale \emph{Tenseness} and are measured on a 5-point Likert scale where low numbers 
#' correspond to low frequencies or low intensitites of the respective complaint and vice versa. 
#' \describe{ 
#' \item{Clammy hands}{Do you have clammy hands?}
#' \item{Sweat attacks}{Do you have sudden attacks of sweating?}
#' \item{Clumsiness}{Do you notice that you behave clumsy?}
#' \item{Wavering hands}{Are your hands wavering frequently, e.g. when lightning a cigarette or when holding a cup?}
#' \item{Restless hands}{Do you notice that your hands are restless?}
#' \item{Restless feet}{Do you notice that your feet are restless?}
#' \item{Twitching eyes}{Do you notice unvoluntary twitching of your eyes?}
#' \item{Twitching mouth}{Do you notice unvoluntary twitching of your mouth?}
#'  }
#' @references Tutz, Gerhard, Schauberger, Gunther and Berger, Moritz (2016): 
#' Response Styles in the Partial Credit Model, \emph{Department of Statistics, LMU
#' Munich}, Technical Report 196
#' @source 
#' ZPID (2013). PsychData of the Leibniz Institute for Psychology Information ZPID. Trier: Center for Research Data in Psychology.
#' 
#' Fahrenberg, J. (2010). Freiburg Complaint Checklist [Freiburger Beschwerdenliste (FBL)]. Goettingen, Hogrefe.
#' @keywords datasets
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
#' m.conf2 <- multord(f.conf1, data = confidence, control = ctrl.multord(XforRS = FALSE))
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
#' m.conf2.cumul <- multord(f.conf1, data = confidence, control = ctrl.multord(XforRS = FALSE), model = "cumulative")
#' m.conf2.cumul
#' 
#' ## Multivariate cumulative model, with response style as a random effect, with explanatory variables for location AND response style
#' m.conf3.cumul <- multord(f.conf1, data = confidence, model = "cumulative")
#' m.conf3.cumul
#' 
#' plot(m.conf3.cumul)
#' }
NULL
