#' One-way ANCOVA Judgement and Plot
#'
#' Judge whether the input data set met the key assumptions for underlie the use of one-way analysis of covariance (ANCOVA). The assumptions include (i) linearity of regression between the dependent variable 'y' and covariate 'x'; (ii) homogeneity of regression slopes among groups (Miller and Chapman, 2001). After judgement, then we continue ANCOVA analysis and plot.

#' @param x,y,groups For covariate 'x', dependent 'y' and factor 'groups'.
#' @param data The dataset contains three columns (x,y,groups) data. \cr See an example via data("isotope",package = "basicANCOVA").
#' @param col,pch,lty The color, pch and linetype for plot.
#' @param Fig.slope If 'Fig.slope = 1', draw graph and output the result of 'ANCOVA with same slope'. \cr  Whereas 'Fig.slope = 0', draw graph and output result of 'linear regression line' for each group.
#' @param xlab,ylab The labels of x-axis and y-axis.
#' @param legendPos The position of legend, such as one of c("none","bottomright","bottom","bottomleft","left","topleft","top","topright","right","center").
#' @param ... additional parameters to \code{\link[graphics]{plot}},such as main, sub, xlab, ylab.
#'
#' @import graphics
#' @import stats
#' @import car
#' @export
#'
#' @examples
#'
#' library(basicANCOVA)
#' data("isotope",package = "basicANCOVA")
#'
#' # (same slope) output ANCOVA results and graphic
#' ANCOVAplot(x = isotope$d13C, y = isotope$d15N, groups = isotope$area,
#'            data = isotope, Fig.slope = 1) # (same slope) output ANCOVA results and graphic
#'
#' # Not run
#' ANCOVAplot(x = isotope$d13C, y = isotope$d15N, groups = isotope$area,
#'            data = isotope, Fig.slope = 0) # (different slopes) output "linear regression" results of each group and graphic
#' # End (Not run)
#'
#' @author  Weiping Mei
#' @seealso \code{\link[basicTrendline]{trendline}},  \code{\link[HH]{ancovaplot}}
#' @references The judging criteria is mainly contributed by \cr Gregory A. Miller and Jean P. Chapman. (2001). Misunderstanding analysis of covariance. Journal of Abnormal Psychology, 110(1), 40-48. Doi 10.1037//0021-843x.110.1.40
#' @references The interpretations of output results are mainly contributed by \cr Salvatore S. Mangiafico. (2015). An R Companion for the Handbook of BiologicalStatistics, s, version 1.3.2. rcompanion.org/documents/RCompanionBioStatistics.pdf . (Web version: rcompanion.org/rcompanion/ ).


ANCOVAplot<-
  function(x, y, groups,data,
           col=1:length(levels(factor(groups))),
           pch=1:length(levels(factor(groups))),
           lty=1:length(levels(factor(groups))),
           Fig.slope = 1,
		   xlab = NULL, ylab = NULL,
           legendPos="topleft",
           ...)
{
    group<- factor(groups)

## Judgement, mod.IA (Model with InterAction)
    mod.IA <- aov(y~x*group, data = data, contrasts = list(group = contr.sum))

    if (requireNamespace("car", quietly = TRUE)){
      car::Anova(mod.IA,type=2)->aov.mod.IA
      }

# print0
    cat("\n <<<< Result 0 Judgement!\n To determine if the dataset is suitable for running One-way ANCOVA.>>>>\n \n")
    Result0 <- aov.mod.IA
    print(Result0)

# Warning message
    if (aov.mod.IA[1,4]>0.05){
       message("^Warning: because covariate x has NOT significantly effect on y (P-value > 0.05), ANCOVA cannot proceed!")
      }
    if (aov.mod.IA[3,4]<0.05){
      message("^Warning: because interation of x*groups IS significant (P-value < 0.05), ANCOVA cannot proceed!")
    }
    if (aov.mod.IA[1,4]<0.05 & aov.mod.IA[3,4]>0.05){
      message("^ Note: the interation of x*groups is not significant (P-value > 0.05), i.e. the slope across groups is not significantly different. \n ANCOVA can be continued! (N.B., specific values are listed in 'Result 4' in the end!)\n")
     }

## mod.NIA (Model without interaction, group as contr.sum for the common slope and intercept values)

    mod.NIA <- aov(y ~ x + group, data = data, contrasts = list(group = contr.sum))
    if (requireNamespace("car", quietly = TRUE)){
      car::Anova(mod.NIA, type = 2) ->aov.mod.NIA
    }

# print1
    cat("\n \n <<<< Result 1 of One-way ANCOVA.\n For judging whether the intercepts among groups are significant different or not.>>>>\n")
    Result1 <- aov.mod.NIA
    print(Result1)

# Warning message
    if (aov.mod.NIA[2,4] <= 0.05){
      message("^Note: the category variable (group) is significant (P-value < 0.05), i.e. the intercepts among groups are significantly different.")
    }else{
      message("^Note: the category variable (group) is NOT significant (P-value > 0.05), i.e. the intercepts among groups are not significantly different.")
    }

    summary.lm(mod.NIA) -> sum.mod.NIA # not summary(aov.mod.NIA)

    commonSlope <- sum.mod.NIA$coefficients[2,1]
    commonIntercept <- sum.mod.NIA$coefficients[1,1]


## mod.NIA.tc (Model without interaction, group as contr.treatment for specific intercepts of each group)

    mod.NIA.tc<-aov(y~x+group,data=data,contrasts = list(group=contr.treatment))

    summary.lm(mod.NIA.tc)->sum.mod.NIA.tc

    Estimate <- sum.mod.NIA.tc$coefficients[,1] # Estimate conclude both all intercepts values and one common slope value.
    slope <- unname(Estimate[2])

    Estimate.intercept<-Estimate[-2]    # Estimate conclude all intercepts values only!
    Estimate.intercept<-unname(Estimate.intercept)
    for (i in 2:length(Estimate.intercept))
      {
      Estimate.intercept[i]<-Estimate.intercept[1]+Estimate.intercept[i]
    }

# print2,3
    cat("\n \n \n <<<< Result 2 of One-way ANCOVA.\n For common slope and different intercepts. While the output of 'Coefficients$Intercept' value in the first row indicates the 'common intercept' of all groups.>>>>\n")
    Result2 <- sum.mod.NIA
    print(Result2)

    cat("\n \n \n <<<< Result 3 of One-way ANCOVA.\n For common slope and different intercepts. while the output of 'Coefficients$Intercept' value in the first row indicates the 'specific intercept' of group1.>>>>\n")
    Result3 <- sum.mod.NIA.tc
    print(Result3)

## Result 4

    j = 1:length(levels(group))
    grp = paste("group", j, sep = "")

    Input.name = deparse(substitute(groups))
    data2 <- cbind(grp, levels(group))
    colnames(data2) <- c("group", Input.name)
    data2

    Result4 <- list(common.Slope = commonSlope, common.Intercept = commonIntercept, specific.Intercepts = Estimate.intercept, group.names = data2)

# print4
    cat("\n \n \n <<<< Result 4 of One-way ANCOVA which output specific values.>>>>\n \n")
    print(Result4)


## Graphics

    if (is.null(xlab)){xlab = deparse(substitute(x))}
    if (is.null(ylab)){ylab = deparse(substitute(y))}
  plot(y~x, col = col[group], pch = pch[group], xlab = xlab, ylab = ylab,...)

# case 1, for ANCOVA graph of same slope with different intercept.
  if (Fig.slope == 1){
    for (j in 1:length(levels(group))){
       abline(a=Estimate.intercept[j],b=slope,col=col[j],lty=lty[j])
    }
  }
# case 1 finished.

###############
# case 2, for linear regression graphs with different slopes and intercepts.
  if (Fig.slope == 0){
     dataset0<- data.frame(x, y, groups)
     colnames(dataset0)<-c("x", "y", "groups")
     cc<-factor(dataset0$groups)
     #yxc.coef <- sapply(levels(cc), function(cci) coef(aov(dataset0$y ~ dataset0$x, subset=(cc==cci))))
     yxc.coef <- sapply(levels(cc), function(cci) coef(lm(dataset0$y ~ dataset0$x, subset=(cc==cci))))
     #summary result for output
     yxc.r2 <- sapply(levels(cc), function(cci) summary(lm(dataset0$y ~ dataset0$x, subset=(cc==cci)))$r.squared)
     Fval.1 <- sapply(levels(cc), function(cci) summary(lm(dataset0$y ~ dataset0$x, subset=(cc==cci)))$fstatistic[1])
     Fval.2 <- sapply(levels(cc), function(cci) summary(lm(dataset0$y ~ dataset0$x, subset=(cc==cci)))$fstatistic[2])
     Fval.3 <- sapply(levels(cc), function(cci) summary(lm(dataset0$y ~ dataset0$x, subset=(cc==cci)))$fstatistic[3])

     intercept.yxc <- yxc.coef[1,]
     slope.yxc <- yxc.coef[2,]
     intercept.yxc <- rep(intercept.yxc, length=length(levels(cc)))
     slope.yxc <-  rep(slope.yxc, length=length(levels(cc)))

     yxc.r2 <-  rep(yxc.r2, length=length(levels(cc)))
     Fval.1 <-  rep(Fval.1, length=length(levels(cc)))
     Fval.2 <-  rep(Fval.2, length=length(levels(cc)))
     Fval.3 <-  rep(Fval.3, length=length(levels(cc)))
     yxc.pvalue <- pf(Fval.1, Fval.2, Fval.3, lower.tail = F)

     for (cci in 1:length(intercept.yxc)) {
       abline(a=intercept.yxc[cci],
                    b=slope.yxc[cci],
                    col=col[cci],
                    lty=lty[cci])
          }

# print4
     cat("\n <<<< Result of linear regression line for each group.>>>>\n")
     cat(" \n $slope \n")
     print(slope.yxc)
     cat("\n $intercept \n")
     print(intercept.yxc)
     cat("\n $R.squared \n")
     print(yxc.r2)
     cat("\n $P.value \n")
     print(yxc.pvalue)
     }
# case 2 finished.

# lengend codes of both case 1 and 2.
  if (legendPos==c("none")){}else{
      legend(legendPos,legend = (levels(group)),col=col,lty=lty,pch=pch)
  }

# Return
  if (Fig.slope == 1){
    invisible(list(Result0 = Result0, Result1 = Result1, Result2 = Result2, Result3 = Result3, Result4 = Result4))
  }else{
    invisible(list(intercept = intercept.yxc, slope = slope.yxc, R.squared = yxc.r2, P.value = yxc.pvalue))
  }

}
