# basicANCOVA: R Package of One-Way ANCOVA Judgement and Plot

[![HitCount](http://hits.dwyl.io/PhDMeiwp/basicANCOVA.svg)](http://hits.dwyl.io/PhDMeiwp/basicANCOVA)

## Authors

<img src="https://github.com/PhDMeiwp/PhDMeiwp.github.io/blob/hexo/Common_images/Mei_Logo.JPG" width="70"/>

Weiping MEI https://PhDMeiwp.github.io


Graduate School of Fisheries and Environmental Sciences, Nagasaki University


## Installation

Get the development version from github:
	
	install.packages("devtools")
	devtools::install_github("PhDMeiwp/basicANCOVA@master", force = TRUE)


## Usage

    ANCOVAplot(x, y, groups, data, 
       			col = 1:length(levels(groups)),
       			pch = 1:length(levels(groups)),
       			lty = 1:length(levels(groups)),
       			legendPos = "topleft",
				xlab = NULL, ylab = NULL, 
				Fig.slope = 1,
       			...)


# Examples
	

    library(basicANCOVA)
    data("isotope",package = "basicANCOVA")

    #(Not run) Same slope (ANCOVA results, lm(y ~ x + groups)).    
    ANCOVAplot(x = isotope$d13C, y = isotope$d15N, groups = isotope$area, 
			   data = isotope, Fig.slope = 1)

 <img src="docs/images/Fig.slope1.png" width="490" align= center/>
   
    #(Not run) Different slopes (lm results, lm( y ~ x * groups)).     
    ANCOVAplot(x = isotope$d13C, y = isotope$d15N, groups = isotope$area, 
			   data = isotope, Fig.slope = 0)

 <img src="docs/images/Fig.slope0.png" width="490" align= center/>
 
 # Contributors
 
 - Statistical Analysis and Data Display: Heiberger and Holland. See [library(HH)](https://github.com/cran/HH/blob/master/R/ancovaplot.R#subset=(cc==cci)).
 - Gregory A. Miller and Jean P. Chapman. (2001). Misunderstanding analysis of covariance. Journal of Abnormal Psychology, 110(1), 40-48. Doi 10.1037//0021-843x.110.1.40
 - Salvatore S. Mangiafico. (2015). An R Companion for the Handbook of BiologicalStatistics, s, version 1.3.2. https://rcompanion.org/documents/RCompanionBioStatistics.pdf . (Web version: https://rcompanion.org/rcompanion/ ).
