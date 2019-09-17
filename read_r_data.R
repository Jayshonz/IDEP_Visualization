##################################################################################################################################
# set path to data

library(survey)
options("survey.replicates.mse=TRUE")
options(scipen=999)
setwd(".")

##################################################################################################################################
### function to take data frame and convert codes to factors based on metadata file
##################################################################################################################################
applyFormats<-function(hh)
{
library(sqldf)
options(gsubfn.engine = "R")
# metadata folder path
metadata=read.csv("../metadata/metadata.csv")
# code to convert raw codes to factors from metadata

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#recoding raw codes to metadata labels in r
levels(metadata$VariableName)<-tolower(trim(levels(metadata$VariableName)))

vars<-sqldf('select distinct VariableName,VarLabel from metadata')

#for each var in meta data conver var in to factor and use labels
for (i in levels(metadata$VariableName)[levels(metadata$VariableName) %in% names(hh)] )
{
                print(i)
        if (as.character(subset(metadata,VariableName==i)$Value)!="")
          {
                     hh[i]<-factor(unlist(hh[i]),levels=subset(metadata,VariableName==i)$Code,labels=as.character(subset(metadata,VariableName==i)$Value))
             table(hh[i])
          }
}

hh$h<-hh$hhsupwgt/10000000

return(hh)

}


# example
##################################################################################################################################
##################################################################################################################################
# read in appropriate file for data examples using multiyear, 15,13,11,09 ...
##################################################################################################################################
##################################################################################################################################
#hh<-read.csv("hh2013_analys.csv",na.strings=".")
#hh<-read.csv("hh2011_analys.csv",na.strings=".")
#hh<-read.csv("hh2009_analys.csv",na.strings=".")

# example change to file you want to read in

hh<-read.csv("hh2017_analys.csv",na.strings=".")
hh=applyFormats(hh)


#subset to respondents
hh=subset(hh,hsupresp=='Respondent')

##################################################################################################################################
# survey design object
hh_svy <- svydesign(id=~1, weights=~h,data=hh,repweights =hh[,grepl("repwgt.*",names(hh)) & !grepl("repwgt0.*",names(hh))  ],type="JKn",scale = 0.025, rscales=rep(1, 160),combined.weights=TRUE)

##################################################################################################################################
# example stats
svyby(~hunbnk,~hryear4,hh_svy,svymean )
svyby(~hunbnk,~hryear4,hh_svy,svytotal )
