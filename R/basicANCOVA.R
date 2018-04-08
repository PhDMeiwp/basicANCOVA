#' One-way ANCOVA Judgement and Plot

#' @param x,y,groups For covariate 'x', dependent 'y' and factor 'groups'.
#' @param data The dataset contains three columns (x,y,groups) data. See an example via data("isotope",package = "basicANCOVA").
#' @param col,pch,lty The color, pch and linetype for plot.
#' @param legendPos The position of legend, such as one of c("none","bottomright","bottom","bottomleft","left","topleft","top","topright","right","center").
#' @param ... additional parameters to \code{\link[graphics]{plot}},such as main, sub, xlab, ylab.
#'
#' @import graphics
#' @import stats
#' @import car
#' @export
#'
#' @examples
#' library(basicANCOVA)
#' data("isotope",package = "basicANCOVA")
#' View(isotope)
#' ANCOVAplot(x=isotope$d13C,y=isotope$d15N,groups=isotope$area,data=isotope)
#'
#'  @author Weiping Mei
#'  @seealso  \code{\link[basicTrendline]{trendline}}

ANCOVAplot<-
  function(x, y, groups,data,
           col=1:length(levels(factor(groups))),
           pch=1:length(levels(factor(groups))),
           lty=1:length(levels(factor(groups))),
           legendPos="topleft",
           ...)
{
    group<- factor(groups)

############## Judgement, mod.IA
    mod.IA<-aov(y~x*group,data=data,  contrasts=list(group=contr.sum))

    require(car)
    Anova(mod.IA,type=3)->aov.mod.IA

# print
    cat("\n <<<< Result 0 Judgement!\n To determine if the dataset is suitable for running One-way ANCOVA.>>>>\n \n")
    Result0<-aov.mod.IA
    print(Result0)

    if (aov.mod.IA[2,4]>0.05){
    stop("Because covariate x is NOT significantly correlated with y (P-value > 0.05), ANCOVA cannot proceed!")
      }

    if (aov.mod.IA[4,4]<0.05){
      stop("Because interation of x*groups IS significant (P-value < 0.05), ANCOVA cannot proceed!")
    }

    if (aov.mod.IA[2,4]<0.05 & aov.mod.IA[4,4]>0.05){
      cat("\n \n \n >>>> ANCOVA can be continued! (N.B., specific values are listed in 'Result 3' in the end!)\n")
     }

############## mod.NIA

    mod.NIA<-aov(y~x+group,data=data,contrasts = list(group=contr.sum))
    Anova(mod.NIA, type=3)

    summary.lm(mod.NIA)->aov.mod.NIA

    commonSlope<-aov.mod.NIA$coefficients[2,1]
    commonIntercept<-aov.mod.NIA$coefficients[1,1]

############## mod.NIA.tc

    mod.NIA.tc<-aov(y~x+group,data=data,contrasts = list(group=contr.treatment))

    summary.lm(mod.NIA.tc)->aov.mod.NIA.tc

    Estimate <- aov.mod.NIA.tc$coefficients[,1] # Estimate conclude both all intercepts values and one common slope value.
    slope <- unname(Estimate[2])

    Estimate.intercept<-Estimate[-2]    # Estimate conclude all intercepts values only!
    Estimate.intercept<-unname(Estimate.intercept)
    for (i in 2:length(Estimate.intercept))
      {
      Estimate.intercept[i]<-Estimate.intercept[1]+Estimate.intercept[i]
    }

# print
    cat("\n \n \n <<<< Result 1 of One-way ANCOVA.\n For common slope and different intercepts. While the output of 'Coefficients$Intercept' value in the first row indicates the 'common intercept' of all groups.>>>>\n")
    Result1<-aov.mod.NIA
    print(Result1)

    cat("\n \n \n <<<< Result 2 of One-way ANCOVA.\n For common slope and different intercepts. while the output of 'Coefficients$Intercept' value in the first row indicates the 'specific intercept' of group1.>>>>\n")
    Result2<-aov.mod.NIA.tc
    print(Result2)

################ Result 3

    Input.name=deparse(substitute(groups))
    data2<-cbind(1:length(levels(group)),levels(group))
    colnames(data2)<-c("group",Input.name)
    data2

    Result3<-list(common.Slope=commonSlope,common.Intercept=commonIntercept,specific.Intercepts=Estimate.intercept,group.names=data2)

    # print
    cat("\n \n \n <<<< Result 3 of One-way ANCOVA which output specific values.>>>>\n \n")
    print(Result3)

#################### Graphics, ANCOVA figure for same slope with different intercepts.

    plot(y~x, col = col[group], pch = pch[group], ...)

    for (j in 1:length(levels(group)))
      {
       abline(a=Estimate.intercept[j],b=slope,col=col[j],lty=lty[j])
        }

    if (legendPos==c("none")){}else{
      legend(legendPos,legend = (levels(group)),col=col,lty=lty,pch=pch)
    }

#################### Return values
  invisible(list(Result0 = Result0, Result1 = Result1, Result2 = Result2, Result3 = Result3))

}
