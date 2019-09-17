##############################################################################################################
# R session info
#R version 3.2.2 (2015-08-14)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 7 x64 (build 7601) Service Pack 1

#locale:
#[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 #LC_NUMERIC=C                           LC_TIME=English_United States.1252    

#attached base packages:
#[1] tcltk     compiler  stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
# [1] rapportools_1.0 reshape_0.8.5   rapport_1.0     R2HTML_2.3.2    foreign_0.8-66  plyr_1.8.4      sqldf_0.4-10    RSQLite_1.0.0 #  DBI_0.5         gsubfn_0.6-6    proto_0.3-10   

#loaded via a namespace (and not attached):
#[1] Rcpp_0.12.6   digest_0.6.10 chron_2.3-47  magrittr_1.5  stringi_1.1.1 tools_3.2.2   stringr_1.1.0 pander_0.6.0  yaml_2.1.13  

##############################################################################################################


### place this file under a directory containing a data/raw folder containing the raw cps and replicate weight .dat files ####################

# setwd('C:/LOCAL DATA/unbanked/downloads_2015/hhmultiyear/src')
# set dir to src directory
# code is in src folder which should be sibling to data folder with raw subdirectory with input raw data sets
setwd('.')
  

basefiles=list()
repfiles=list()
layoutfiles=list()

years<-c('2009','2011','2013','2015','2017')


basefiles['2009']="jan09pub_rwgt"
basefiles['2011']="jun11pub"    
basefiles['2013']="jun13pub"  
basefiles['2015']="jun15pub" 
basefiles['2017']="jun17pub" 

repfiles['2009']="jan09rep"
repfiles['2011']="jun11rep"    
repfiles['2013']="jun13rep" 
repfiles['2015']="jun15rep" 
repfiles['2017']="jun17rep" 


layoutfiles['2009']="2009_layout.sas"
layoutfiles['2011']="cpsjun2011_fixed.sas"    
layoutfiles['2013']="2013_modified_layout.sas" 
layoutfiles['2015']="2015_layout_file.sas" 
layoutfiles['2017']="2017_layout_file.sas" 



metadata<-read.csv("../metadata/metadata.csv");


library(sqldf)
library(LaF)
library(plyr)
library(R2HTML)
library(rapport)
library(SAScii)
library(foreign)
options(gsubfn.engine = "R")


#### helper function #######################################################################################################################
# code to create hh replicate weight csvs if not already there 
# make generic based on existence of files

createRepWgtData<-function(basefile,repfile,year=2017)
{

#rep<-read.fwf(paste(file,".dat",sep=""),


if (year<2015)
{
widths=c (5,2,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9, 9,9,9,9,9,9,9,9,9,9,9,9,9,9,
9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9)

}

else
{
widths=c (5,2,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 10,10,10,10,10,10,10,10,10,10,10,10,10,10,
10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10)

}
#,fill=TRUE)



library(LaF)

 rep <- laf_open_fwf(filename = paste("./raw/",repfile,".dat",sep=""),
  column_types = c(rep("integer",length(widths) )), column_widths = widths)
  rep <- rep[, ]




names(rep)<-c('qstnum','occurnum','repwgt0','repwgt1','repwgt2','repwgt3','repwgt4','repwgt5','repwgt6','repwgt7','repwgt8','repwgt9','repwgt10','repwgt11','repwgt12','repwgt13',
'repwgt14','repwgt15','repwgt16','repwgt17','repwgt18','repwgt19','repwgt20','repwgt21','repwgt22','repwgt23','repwgt24','repwgt25','repwgt26','repwgt27','repwgt28','repwgt29',
'repwgt30','repwgt31','repwgt32','repwgt33','repwgt34','repwgt35','repwgt36','repwgt37','repwgt38','repwgt39','repwgt40','repwgt41','repwgt42','repwgt43','repwgt44','repwgt45',
'repwgt46','repwgt47','repwgt48','repwgt49','repwgt50','repwgt51','repwgt52','repwgt53','repwgt54','repwgt55','repwgt56','repwgt57','repwgt58','repwgt59','repwgt60','repwgt61',
'repwgt62','repwgt63','repwgt64','repwgt65','repwgt66','repwgt67','repwgt68','repwgt69','repwgt70','repwgt71','repwgt72','repwgt73','repwgt74','repwgt75','repwgt76','repwgt77',
'repwgt78','repwgt79','repwgt80','repwgt81','repwgt82','repwgt83','repwgt84','repwgt85','repwgt86','repwgt87','repwgt88','repwgt89','repwgt90','repwgt91','repwgt92','repwgt93',
'repwgt94','repwgt95','repwgt96','repwgt97','repwgt98','repwgt99','repwgt100','repwgt101','repwgt102','repwgt103','repwgt104','repwgt105','repwgt106','repwgt107','repwgt108','repwgt109',
'repwgt110','repwgt111','repwgt112','repwgt113','repwgt114','repwgt115','repwgt116','repwgt117','repwgt118','repwgt119','repwgt120','repwgt121','repwgt122','repwgt123','repwgt124','repwgt125' 
,'repwgt126','repwgt127','repwgt128','repwgt129','repwgt130','repwgt131','repwgt132','repwgt133','repwgt134','repwgt135','repwgt136','repwgt137','repwgt138','repwgt139','repwgt140','repwgt141',
'repwgt142','repwgt143','repwgt144','repwgt145','repwgt146','repwgt147','repwgt148','repwgt149','repwgt150','repwgt151','repwgt152','repwgt153','repwgt154','repwgt155','repwgt156','repwgt157',
'repwgt158','repwgt159','repwgt160')


rep


pp<-read.csv(paste("./raw/",basefile,".csv",sep=""))


# join rep w to survey;  for each qstnum key row where ps wgt =rep wgt 0 as that signifies hh weights which may be diff that perrp id;
#  means hh weights



library(plyr)
names(pp)<-tolower(names(pp))
hh<-join(pp,rep)

hh$hh<-ifelse(hh$pwsupwgt == hh$hhsupwgt,1,0)
table(hh$hh);
hh<-subset(hh,hh==1 & !is.na(repwgt0))

library(sqldf)
sqldf('select count(*), count(distinct qstnum) from hh')
hhrep<-hh[,grepl("repwgt.*|qstnum",names(hh))]
hhrep<-sqldf('select distinct * from hhrep ')

hhrep
}


#######################################################################################################################
############################   create raw frequencies of variables and print names of variables  ####################

createSummaryStats<-function(hh,year,metadata)
{
  library(R2HTML)
  library(rapport)
  library(sqldf)

library(R2HTML)
library(rapport)
library(rapportools)

  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  options("R2HTML.format.digits" = 4)
  options(scipen=999)
  HTMLStart(outdir=paste(getwd(),sep=""), file=paste("HH",year,"_CodeBookVariableFrequency",sep=""),    extension="html", echo=FALSE, HTMLframe=FALSE)
  HTML.title("HH Survey Code Frequencies",HR=1)
  HTML.title(year)
  
  vars<-sqldf('select distinct VariableName VariableName, VarLabel from metadata where code is not null')
  
  
  hh<- hh [,colSums(is.na(hh ))<nrow(hh )]
  q<-lapply(tolower(trim(vars[,1])[trim(tolower(vars[,1])) %in% names(hh) & grepl("wgt|qstnum|occurnum",trim(tolower(vars[,1])))==FALSE]), 
            function (x)  {
              HTML.title(paste(x,":",vars[tolower(trim(vars[,1]))==x,2]),HR=2)
              HTML(as.data.frame(rp.freq(x,hh[tolower(trim(vars[,1])[trim(tolower(vars[,1])) %in% names(hh)])])))
              #HTMLhr()
            } 
  )
  
  HTMLStop()
}


###################################################################################################




#############  if intermediate files don't exist create them ########################
# create intermediate csv from flat file
#######################################################################################################################

	
#############################   convert fixed width file to csv ########################################################
library(SAScii)
library(LaF)


for (y in years)
{
# loop through years and create csv from flat files
if (file.exists(paste("./raw/",basefiles[y],".dat",sep="")))
{
  if (!file.exists(paste("./raw/",basefiles[y],".csv",sep="")))
  {
    
    lfile=paste("../src/layout/",layoutfiles[y],sep="")
    linenum<-grep('INPUT',	readLines(lfile))
    x <- parse.SAScii(  lfile)
    y1 <- x[ !is.na( x[ , 'varname' ] ) , ]
                  
    dat<-laf_open_fwf(paste("./raw/",basefiles[y],".dat",sep=""), column_types=c(rep("string",length(x$width))),column_widths=abs(x$width))
    d<-dat[,]
    d<-d[,x$width>0]
    names(d)<-tolower(y1$varname)
                  
    
    write.csv(d, file=paste("./raw/",basefiles[y],".csv",sep=""), row.names=FALSE)
                  
  }
  
}

}




##### create replicate weight csvs ###############################################################################


for (i in years)    
  if ( #file.exists(paste("./raw/",repfiles[i],".dat",sep="")) &
        file.exists(paste("./raw/",basefiles[i],".csv",sep="")) 
        & !file.exists(paste("./raw/",repfiles[i],".csv",sep=""))
            )
                                                                                                                         
      { 
        print(i)
        hhrep<-createRepWgtData(basefiles[i],repfiles[i],i)
        write.csv(hhrep,file=paste("./raw/",repfiles[i],".csv",sep=""))
      }
      



################################################################################################################################

#######################################################################



###########  create all 3 years worth of data
processData<-function(basefile,repfile,year,metadata)
{

  source("../src/ProcessRawDataVariableCreationFunction.R")

hh<-createAnalyticHHData(paste("./raw/",basefile,".csv",sep=""))
names(hh)<-tolower(names(hh))


# read in rep weight and merge in 
rep<-read.csv(paste("./raw/",repfile,".csv",sep=""))


library(plyr)

hh<-join(hh,rep)


hh<-as.data.frame(lapply(hh,as.numeric))


# divide weights by 10000000
hh[,grepl("repwgt.*",names(hh))]<-hh[,grepl("repwgt.*",names(hh))]/10000000

# set to missing if huspresp==0

hh[hh$hsupresp==0,grepl("repwgt.*",names(hh))]<-NA


#################   write file out ################################
write.table(hh,file=paste("hh",year,"_analys.csv",sep=""),row.names=FALSE,sep=",",na=".")

#library(foreign)
# write.dta(hh,file=paste("hh",year,".dta",sep=""))
          
createSummaryStats(hh,year,metadata)          
          
}



# if base and resp exist call processData to create final analytic data set
############   create final analysis data ######################################################################################


### add more steps into function
### move code book up as well
### format creation
### stata creation
### frequency distribution of variables codebook file in html
# if src csvs exists rep and cps go ahead and create analytic data set
    
for (i in years)    
  if (file.exists(paste("./raw/",basefiles[i],".csv",sep="")) 
      & file.exists(paste("./raw/",repfiles[i],".csv",sep=""))
      )   processData(basefiles[i],repfiles[i],i,metadata)
      
    
            
########################################################################################################################
            
##########  create multiyear file #######################################################################################

# if all 3 yearly analytic data sets exist call multiyear creation
# multiyear file

if (file.exists("hh2017_analys.csv") & file.exists("hh2013_analys.csv") & file.exists("hh2011_analys.csv") & file.exists("hh2009_analys.csv") & file.exists("hh2015_analys.csv"))
{

hh2017<-read.csv("hh2017_analys.csv",na.strings=".")
hh2015<-read.csv("hh2015_analys.csv",na.strings=".")
hh2013<-read.csv("hh2013_analys.csv",na.strings=".")
hh2011<-read.csv("hh2011_analys.csv",na.strings=".")
hh2009<-read.csv("hh2009_analys.csv",na.strings=".")

#
# if all 3 final csvs exicts merge into multiyear file without base var and only derived variables 
###########   read in meta data file and create a multiyear file with only anayltic variables
# VariableName	VarLabel	           Code	Value
# pbaseint	     Base Interview - person	1	Interviewed in base supplement
# change logic to keep all vars in metadata except those starting w hes/pes

n<-tolower(levels(metadata$VariableName))
n<-c(n[!grepl("hes|pes",n)], names(hh2017[,grepl("repwgt.*",names(hh2017))]),'hrhhid','hrhhid2','pulineno','hhinc_cont','hefaminc','p_hrs_wrk','p_hrs_wrk2','p_weekly_earn','prinusyr')
 n=n[!n %in% c("hanslstq" ,"hansppq" , "hbaseint")]

hh2017[,!grepl("wgt|id|num|age",names(hh2017))]<-as.data.frame(lapply(hh2017[,!grepl("wgt|id|num|age",names(hh2017))],as.integer))
hh2015[,!grepl("wgt|id|num|age",names(hh2015))]<-as.data.frame(lapply(hh2015[,!grepl("wgt|id|num|age",names(hh2015))],as.integer))
hh2013[,!grepl("wgt|id|num|age",names(hh2013))]<-as.data.frame(lapply(hh2013[,!grepl("wgt|id|num|age",names(hh2013))],as.integer))
hh2011[,!grepl("wgt|id|num|age",names(hh2011))]<-as.data.frame(lapply(hh2011[,!grepl("wgt|id|num|age",names(hh2011))],as.integer))
hh2009[,!grepl("wgt|id|num|age",names(hh2009))]<-as.data.frame(lapply(hh2009[,!grepl("wgt|id|num|age",names(hh2009))],as.integer))

gc()


multiyear<- rbind(hh2013[n],hh2011[n])
#rm(hh2013)
#rm(hh2011)
gc()

multiyear<- rbind(multiyear,hh2009[n])

#rm(hh2009)
gc()
multiyear<- rbind(multiyear,hh2015[n])

multiyear<- rbind(multiyear,hh2017[n])


#####   drop repwgts in multiyear      ######################
multiyear=multiyear[,!grepl("repwgt",names(multiyear))]


write.table(multiyear,file="hh_multiyear_analys.csv",row.names=FALSE,sep=",",na=".")





createSummaryStats(multiyear,"multiyear",metadata)

# create single year replicate weights files to merge into multiyear data set
#setwd('.')

rep09<-read.csv("./raw/jan09rep.csv")
rep11<-read.csv("./raw/jun11rep.csv")
rep13<-read.csv("./raw/jun13rep.csv")
rep15<-read.csv("./raw/jun15rep.csv")
rep17<-read.csv("./raw/jun17rep.csv")


rep09$hryear4=2009
rep11$hryear4=2011
rep13$hryear4=2013
rep15$hryear4=2015
rep17$hryear4=2017

single_yr_rep<-rbind(rep09,rep11,rep13,rep15,rep17)
single_yr_rep[,grepl("repwgt.*",names(single_yr_rep))]<-single_yr_rep[,grepl("repwgt.*",names(single_yr_rep))]/10000000
write.csv(single_yr_rep,file='../single_yr_rep.csv')



}
##########################################################################################################################


#########################################################################################################################
##########################           sas catalog files           ########################################################
########################################################################################################################
# create a cntlin version for sas formatting from data FMTNAME ,START, LABEL
formatData<-sqldf('select distinct VariableName FMTNAME , Code START, Value LABEL from metadata where code is not null')
formatData$FMTNAME<-paste(formatData$FMTNAME,"X",sep="")
formatData$FMTNAME <-ifelse(nchar(formatData[,1])>10,substring(formatData[,1],nchar(formatData[,1])-9,nchar(formatData[,1])),formatData[,1])
formatData$FMTNAME<-gsub("^[0-9]*","",formatData$FMTNAME)
formatData<-unique(formatData)

formatData2<-subset(formatData,is.na(formatData$START)==FALSE)
write.table(formatData2,file="formatData.csv",row.names=FALSE,sep=",",na="")

########################################################################################################
# now loop through meta data and create labels VariableName	VarLabel  and assign formats for sas data sets

vars<-sqldf('select distinct VariableName VariableName, VarLabel from metadata')
vars$valx<-paste(vars[,1],"X",sep="")
vars$val<-ifelse(nchar(as.character(vars[,3]))>10,substring(as.character(vars[,3]),nchar(as.character(vars[,3]))-9,nchar(as.character(vars[,3]))),as.character(vars[,3]))
vars$val<-gsub("^[0-9]*","",vars$val)
vars<-sqldf('select distinct * from metadata')

 sink("format_hh_survey.sas")
#print labels
for (v in 1:nrow(vars))
{
 cat(paste("LABEL",tolower(vars[v,1]),"=","'",vars[v,2],"';"))
 cat("\n")
}

vars<-sqldf('select distinct VariableName VariableName, VarLabel from metadata where code is not null')
vars$valx<-paste(vars[,1],"X",sep="")
vars$val<-ifelse(nchar(as.character(vars[,3]))>10,substring(as.character(vars[,3]),nchar(as.character(vars[,3]))-9,nchar(as.character(vars[,3]))),as.character(vars[,3]))
vars$val<-gsub("^[0-9]*","",vars$val)
vars<-sqldf('select distinct * from vars')

 cat("FORMAT\n")
#print assignment of formats
for (v in 1:nrow(vars))
{

 cat(paste(tolower(vars[v,1]),paste(vars[v,4],".",sep="") ))
 cat("\n")
}
cat(";\n")

 sink()

##############################################

vars<-sqldf('select distinct VariableName VariableName, VarLabel from metadata')
vars$valx<-paste(vars[,1],"X",sep="")
vars$val<-ifelse(nchar(as.character(vars[,3]))>10,substring(as.character(vars[,3]),nchar(as.character(vars[,3]))-9,nchar(as.character(vars[,3]))),as.character(vars[,3]))
vars$val<-gsub("^[0-9]*","",vars$val)
vars<-sqldf('select distinct * from vars')

#vars<-vars[levels(vars$VariableName) %in% n,]
 sink("multiyear_format_hh_survey.sas")
#print labels
for (v in 1:nrow(vars))
{
 cat(paste("LABEL",tolower(vars[v,1]),"=","'",vars[v,2],"';"))
 cat("\n")
}

vars<-sqldf('select distinct VariableName VariableName, VarLabel from metadata where code is not null')
vars$valx<-paste(vars[,1],"X",sep="")
vars$val<-ifelse(nchar(as.character(vars[,3]))>10,substring(as.character(vars[,3]),nchar(as.character(vars[,3]))-9,nchar(as.character(vars[,3]))),as.character(vars[,3]))
vars$val<-gsub("^[0-9]*","",vars$val)

vars<-sqldf('select distinct * from vars')

 cat("FORMAT\n")
#print assignment of formats
for (v in 1:nrow(vars))
{

 cat(paste(tolower(vars[v,1]),paste(vars[v,4],".",sep="") ))
 cat("\n")
}
cat(";\n")

 sink()


################################################# stata do labels #############################################
options(warn=-1)

sink('stata_labels.do')


metadata$VariableName=tolower(metadata$VariableName)

varsc<-sqldf('select distinct VariableName VariableName, VarLabel from metadata where code is null')

varsc$VariableName=tolower(varsc$VariableName)


for (v in 1:nrow(varsc))
{

cat(paste('label variable',varsc[v,]$VariableName, paste('"',varsc[v,]$VarLabel,'"',sep='')))
cat("\n")
}

vars<-sqldf('select distinct VariableName VariableName, VarLabel from metadata where code is not null')


vars$VariableName=tolower(vars$VariableName)

for (v in 1:nrow(vars))
{


cat(paste('label variable',vars[v,]$VariableName, paste('"',vars[v,]$VarLabel,'"',sep='')))
cat("\n")

l_beg=paste("label define ",paste(vars[v,]$VariableName,'nl',sep=''))

z=subset(metadata,VariableName==vars[v,]$VariableName)
str=""
for (i in 1:nrow(z)) str=paste(str,z[i,]$Code,paste('"',z[i,]$Value,'"',sep=''),sep=' ')

cat(paste(l_beg,str))
cat("\n")


cat(paste("label values ",vars[v,]$VariableName,paste('"',vars[v,]$VariableName,'nl"',sep='')))
cat("\n")
}
sink()
#################################################################################################################






######################  ##auto generate label, format, and assignment blocks  formathh.sas;
############################################################################################################################
### move to create data set step ##############
#multiyear<-read.csv("hh_multiyear_analys.csv",na.strings=".")

setwd("../src/")