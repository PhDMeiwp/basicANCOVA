# basicANCOVA: an R package for One-way Analysis of Covariance Judgement and Plot

[![HitCount](http://hits.dwyl.io/PhDMeiwp/basicANCOVA.svg)](http://hits.dwyl.io/PhDMeiwp/basicANCOVA)

## Installation

	install.packages("devtools")
	library(devtools)   
	install_github("PhDMeiwp/basicANCOVA@master", force = TRUE)
	library(basicANCOVA)


## Usage

    ANCOVAplot(x, y, groups, data, 
       			col = 1:length(levels(groups)),
       			pch = 1:length(levels(groups)),
       			lty = 1:length(levels(groups)),
       			legendPos = "topleft",
				xlab = NULL, ylab = NULL, 
       			...)


# Examples
	

    library(basicANCOVA)
    data("isotope",package = "basicANCOVA")
    View(isotope)
    #rename
    groups<-isotope$area
    x<-isotope$d13C
    y<-isotope$d15N
    data<-isotope
    
    ANCOVAplot(x, y, groups, data)   # same slope (ANCOVA results).

 <img src="docs/images/Fig.slope1.png" width="490" align= center/>

 # Contributors
 
 - Statistical Analysis and Data Display: Heiberger and Holland. See [library(HH)](https://github.com/cran/HH/blob/master/R/ancovaplot.R#subset=(cc==cci)).
 - Gregory A. Miller and Jean P. Chapman. (2001). Misunderstanding analysis of covariance. Journal of Abnormal Psychology, 110(1), 40-48. Doi 10.1037//0021-843x.110.1.40
 - Salvatore S. Mangiafico. (2015). An R Companion for the Handbook of BiologicalStatistics, s, version 1.3.2. https://rcompanion.org/documents/RCompanionBioStatistics.pdf . (Web version: https://rcompanion.org/rcompanion/ ).
