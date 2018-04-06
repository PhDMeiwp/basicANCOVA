#' One-way ANCOVA Judgement and Plot

#' @import graphics
#' @import stats
#' @import car
#' @export
#' 
#' @examples
#' library(basicTrendline)
#' data("hotdog",package = "HH")
#' View(hotdog)
#' ancovaplot(x=hotdog$Sodium,y=hotdog$Calories,groups=hotdog$Type,data=hotdog)

ancovaplot<-
  function(x, y, groups,data, 
           col=1:length(levels(groups)),
           pch=1:length(levels(groups)),
           Fig.slope=1,
           lty=1:length(levels(groups)),
           legendPos="topleft",
           ...)
{
    grp<- factor(groups)
  
    ############## Judgement, mod.IA
    # Step1: ANCOVA with InterAction
    mod.IA<-aov(y~x*grp,data=data,  contrasts=list(grp=contr.sum))
    
    require(car)
    Anova(mod.IA,type=3)->aov.mod.IA
    if (aov.mod.IA[2,4]>0.05){
      print(aov.mod.IA) &
      stop("covariate x is NOT significantly related with y (P > 0.05), ANCOVA cannot access!")
      }
    if (aov.mod.IA[4,4]<0.05){
      print(aov.mod.IA) &
      stop("interation of x*groups IS significant (P < 0.05), ANCOVA cannot access!")
      }
    
    ############## mod.NIA
    # Step2: then ANCOVA¡¡NonInterAction, for equal slope, type III
    mod.NIA<-aov(y~x+grp,data=data,contrasts = list(grp=contr.sum))
    Anova(mod.NIA, type=3)
    
    summary.lm(mod.NIA)->aov.mod.NIA
    
    commonSlope<-aov.mod.NIA$coefficients[2,1]
    commonIntercept<-aov.mod.NIA$coefficients[1,1]
    
    ############## mod.NIA.tc
    # Step3: then ANCOVA NonInterAction with treatment contrasts, or type I.
    mod.NIA.tc<-aov(y~x+grp,data=data,contrasts = list(grp=contr.treatment))
    
    summary.lm(mod.NIA.tc)->aov.mod.NIA.tc

    Estimate <- aov.mod.NIA.tc$coefficients[,1] # Estimate conclude both all intercepts values and one common slope value.
    slope <- unname(Estimate[2])
    slope <-  rep(slope, length=length(levels(grp)))

    Estimate.intercept<-Estimate[-2]    # Estimate conclude all intercepts values only!
    Estimate.intercept<-unname(Estimate.intercept)
    for (i in 2:length(Estimate.intercept))
      {
      Estimate.intercept[i]<-Estimate.intercept[1]+Estimate.intercept[i]
    }
    
    j=1:length(Estimate.intercept)
    Estimate.intercept[j]<-Estimate.intercept[j]
    intercept <-  rep(Estimate.intercept[j], length=length(levels(grp)))  

    
    
    print(aov.mod.NIA)
    print(aov.mod.NIA.tc)
        
    ####################
    # Step4: Graphics

    plot(y~x, col = col, pch = pch, ...)

  if (Fig.slope==1)  #Figure for same slope value with different intercepts values
    {
      for (k in 1:length(intercept))
      {abline(a=intercept[k],b=slope,col=col[k],lty=lty[k])
        }
    } 
    
    legend(legendPos,legend = (levels(groups)),col=col,lty=lty,pch=pch)    

    #£¨Î´Íê´ýÐø...Fig.slope=2 # different slopes and intercepts£©
} 