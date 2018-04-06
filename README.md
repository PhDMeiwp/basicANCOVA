# basicANCOVA: an R package for One-way ANCOVA Judgement and Plot



## Installation (1.0.1)

**version 1.0.1** from Github

    ### version 1.0.1
	
	install.packages("devtools")
	library(devtools)   
	install_github("PhDMeiwp/basicANCOVA@master", force = TRUE)
	library(basicANCOVA)



## Usage

    ancovaplot(x, y, groups,data, 
       			col=1:length(levels(groups)),
       			pch=1:length(levels(groups)),
       			Fig.slope=1,
       			lty=1:length(levels(groups)),
       			legendPos="topleft",
       			...)


# Examples
	

    library(basicANCOVA)
    data("hotdog",package = "basicANCOVA")
    View(hotdog)
    #rename
    groups<-hotdog$Type
    x<-hotdog$Sodium
    y<-hotdog$Calories
    data<-hotdog
    
    ancovaplot(x,y,groups,data,xlab="xexample",ylab="yvalues")

 <img src="docs/images/example.png" width="490"/>
