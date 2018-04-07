#' One-way ANCOVA Judgement and Plot

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

ANCOVAplot<-
  function(x, y, groups,data, 
           col=1:length(levels(factor(groups))),
           pch=1:length(levels(factor(groups))),
           lty=1:length(levels(factor(groups))),
           Fig.slope=1,
           legendPos="topleft",
           ...)
{
    grp<- factor(groups)
  
############## Judgement, mod.IA
    mod.IA<-aov(y~x*grp,data=data,  contrasts=list(grp=contr.sum))
    
    require(car)
    Anova(mod.IA,type=3)->aov.mod.IA
    
# print
    cat("\n >>>> Result 0 Judgement! To determine if the dataset is suitable for running One-way ANCOVA.\n")
    Result0<-aov.mod.IA
    print(Result0) 
    
    if (aov.mod.IA[2,4]>0.05){
    stop("Because covariate x is NOT significantly correlated with y (P-value > 0.05), ANCOVA cannot proceed!")
      }
	  
    if (aov.mod.IA[4,4]<0.05){
      stop("Because interation of x*groups IS significant (P-value < 0.05), ANCOVA cannot proceed!")
    }
    
    if (aov.mod.IA[2,4]<0.05 & aov.mod.IA[4,4]>0.05){
      cat("\n \n \n >>>> ANCOVA can be continued!\n")
     }
    
############## mod.NIA

    mod.NIA<-aov(y~x+grp,data=data,contrasts = list(grp=contr.sum))
    Anova(mod.NIA, type=3)
    
    summary.lm(mod.NIA)->aov.mod.NIA
    
    commonSlope<-aov.mod.NIA$coefficients[2,1]
    commonIntercept<-aov.mod.NIA$coefficients[1,1]
    
############## mod.NIA.tc

    mod.NIA.tc<-aov(y~x+grp,data=data,contrasts = list(grp=contr.treatment))
    
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
    cat("\n \n \n >>>> Result 1 of One-way ANCOVA with common slope and different intercepts. While the output of 'Coefficients$Intercept' value in the first row indicates the 'common intercept' of all groups.\n")
    Result1<-aov.mod.NIA
    print(Result1)
    
    cat("\n \n \n >>>> Result 2 of One-way ANCOVA with common slope and different intercepts. while the output of 'Coefficients$Intercept' value in the first row indicates the 'specific intercept' of grp1.\n")
    Result2<-aov.mod.NIA.tc
    print(Result2)
        
####################
# Step4: Graphics
    
  plot(y~x, col = col[grp], pch = pch[grp], ...)
     
  if (Fig.slope==1)  #ANCOVA figure for same slope value with different intercepts.
    {
      for (k in 1:length(levels(factor(groups))))
      {
       abline(a=Estimate.intercept[k],b=slope,col=col[k],lty=lty[k])
        }
    } 
 
   

  legend(legendPos,legend = (levels(grp)),col=col,lty=lty,pch=pch)    

  invisible(list(Result0 = Result0, Result1 = Result1, Result2 = Result2))
  
} 