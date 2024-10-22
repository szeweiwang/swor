#' @title Odds Ratio Confidence Interval Function
#' @description A function that returns the confidence interval of the estimated coefficients from logistic regression
#' @param coef numeric vector
#' @param se numeric vector
#' @param siglevel significance level
#' @param roundto significant digits
#' @return CI
#' @examples
#' data(toydata)
#' log.reg <- glm( y~x1+x2, data = toydata, family = "binomial")
#' coefs<- summary(log.reg)$coefficients[2,1]
#' se <- summary(log.reg)$coefficients[2,2]
#' OR_95CI(coef=coefs, se=se, siglevel=0.05, roundto = 2)
#' @author Szewei Wang


OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
