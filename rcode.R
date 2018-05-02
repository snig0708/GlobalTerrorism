## clearing workspace
rm(list =ls())

## importing packages
install.packages("magrittr") #for pipeline functions
install.packages("dplyr")   # for data manipulation 
install.packages("tidyr")  #for data wrangling
install.packages("ggplot2") #data visualization
install.packages("ggthemes")
install.packages("mice")
install.packages("VIM")
install.packages("parsedate")

#loading the packages
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(mice)
library(VIM)
library(parsedate)

##importing the original dataset
original_data <- read.csv("~/Downloads/globalterrorismdb_0617dist.csv")
## plotting the missing value count for each column
options(repr.plot.width=6, repr.plot.height=8)
missing_data <- original_data %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "blue", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_few()+ 
  ggtitle("Missing values in the dataset") + labs(x = "variables", y = "percentage of missing values")

#####************************ DATA CLEANING***************************************************************
#####removing columns
##deleting columns that have more than 10% of missing data
originaldata_cleaned <- original_data[, -which(colMeans(is.na(original_data)) > 0.10)]
dim(originaldata_cleaned)
#OUTPUT: [1] 170350     91
colSums(is.na(originaldata_cleaned))
## we are removing 'nwound' because there is high percentage of missing data
grep("nwound",colnames(originaldata_cleaned)) # to find the index of column: nwound
originaldata_cleaned <- originaldata_cleaned[-73]#removing 'nwound'
## we are removing 'nkill' because there is high percentage of missing data
grep("nkill",colnames(originaldata_cleaned))#to find the index of column
originaldata_cleaned <- originaldata_cleaned[-72]# removing 'nkill'

###data imputation 
md.pattern(originaldata_cleaned)
aggr_plot <- aggr(originaldata_cleaned, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(originaldata_cleaned), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
colSums(is.na(originaldata_cleaned))
#targetsubtype1--- replacing with 0
originaldata_cleaned$targsubtype1[which(is.na(originaldata_cleaned$targsubtype1))] <- 0
any(is.na(originaldata_cleaned$targsubtype1))#output: [1] FALSE
## natlty1--- replacing with 0 (as an unknown nationality)
originaldata_cleaned$natlty1[which(is.na(originaldata_cleaned$natlty1))] <- 0
any(is.na(originaldata_cleaned$natlty1)) #output: [1] FALSE

##### ********* takes a lot of time to run: produces good results for small dataset ********************
## guncertain1 --- imputing values using logistic regression model
#originaldata_cleaned$ishostkid <- as.factor(originaldata_cleaned$ishostkid)
#class(originaldata_cleaned$ishostkid) # output: [1] "factor"

#impData <- originaldata_cleaned[c(1:170350),c(75,89,79,46)]
#imp <- mice(impData, meth = c("logreg", "", "", ""))
#completedData <- complete(imp,1)
##******************************************************************************************************

## 'guncertain1' :imputing the 'guncertain1' missing cases with 0
originaldata_cleaned$guncertain1[which(is.na(originaldata_cleaned$guncertain1))] <- 0
## ishostkid: imputing with 0
originaldata_cleaned$ishostkid[which(is.na(originaldata_cleaned$ishostkid))] <- 0

##import the data (dataset that is partially cleaned manually using EXCEL)
data <- read.csv("~/Desktop/new_terrorism_data.csv")


## exploring and summarising missing values
head(data)
tail(data)
any(is.na(data)) 
#OUTPUT:[1] TRUE.... MISSING VALUES PRESENT

sum(is.na(data)) # total number of missing values in the dataset
colSums(is.na(data)) # tital missing cases in each column shown
#OUTPUT: we can see that there are missing values in Subtargettype_num, Latitude, Longitude, TArget_corp, Target, 
# Nationality_num, Killed_num, Killer_num, Wounded_num


## correcting the data type of each variable
str(data)
originaldata_cleaned$resolution <- as.Date(originaldata_cleaned$resolution)
originaldata_cleaned$country <- as.factor(originaldata_cleaned$country)
originaldata_cleaned$region <- as.factor(originaldata_cleaned$region)
originaldata_cleaned$specificity <- as.factor(originaldata_cleaned$specificity)
originaldata_cleaned$vicinity <- as.factor(originaldata_cleaned$vicinity)
originaldata_cleaned$crit1 <- as.factor(originaldata_cleaned$crit1)
originaldata_cleaned$crit2 <- as.factor(originaldata_cleaned$crit2)
originaldata_cleaned$crit3 <- as.factor(originaldata_cleaned$crit3)
originaldata_cleaned$doubtterr <- as.factor(originaldata_cleaned$doubtterr)
originaldata_cleaned$multiple <- as.factor(originaldata_cleaned$multiple)
originaldata_cleaned$success <- as.factor(originaldata_cleaned$success)
originaldata_cleaned$suicide <- as.factor(originaldata_cleaned$suicide)
originaldata_cleaned$attacktype1 <- as.factor(originaldata_cleaned$attacktype1)
originaldata_cleaned$attacktype1_txt <- as.factor(originaldata_cleaned$attacktype1_txt)
originaldata_cleaned$attacktype2_txt <- as.factor(originaldata_cleaned$attacktype2_txt)
originaldata_cleaned$attacktype3_txt <- as.factor(originaldata_cleaned$attacktype3_txt)
originaldata_cleaned$targtype1 <- as.factor(originaldata_cleaned$targtype1)
originaldata_cleaned$targtype1_txt <- as.factor(originaldata_cleaned$targtype1_txt)
originaldata_cleaned$targsubtype1 <- as.factor(originaldata_cleaned$targsubtype1)
originaldata_cleaned$natlty1 <- as.factor(originaldata_cleaned$natlty1)
originaldata_cleaned$gname <- as.character(originaldata_cleaned$gname)
originaldata_cleaned$gsubname <- as.character(originaldata_cleaned$gsubname)
originaldata_cleaned$gname2 <- as.character(originaldata_cleaned$gname2)
originaldata_cleaned$gsubname2 <- as.character(originaldata_cleaned$gsubname2)
originaldata_cleaned$gname3 <- as.character(originaldata_cleaned$gname3)
originaldata_cleaned$gsubname3 <- as.character(originaldata_cleaned$gsubname3)
originaldata_cleaned$motive <- as.character(originaldata_cleaned$motive)
originaldata_cleaned$guncertain1 <- as.factor(originaldata_cleaned$guncertain1)
originaldata_cleaned$individual <- as.factor(originaldata_cleaned$individual)
originaldata_cleaned$weaptype1 <- as.factor(originaldata_cleaned$weaptype1)
originaldata_cleaned$weapdetail <- as.character(originaldata_cleaned$weapdetail)
originaldata_cleaned$property <- as.factor(originaldata_cleaned$property)
originaldata_cleaned$ishostkid <- as.factor(originaldata_cleaned$ishostkid)
originaldata_cleaned$divert <- as.character(originaldata_cleaned$divert)
originaldata_cleaned$kidhijcountry <- as.character(originaldata_cleaned$kidhijcountry)
originaldata_cleaned$ransomnote <- as.character(originaldata_cleaned$ransomnote)
originaldata_cleaned$addnotes <- as.character(originaldata_cleaned$addnotes)
originaldata_cleaned$scite1 <- as.character(originaldata_cleaned$scite1)
originaldata_cleaned$scite2 <- as.character(originaldata_cleaned$scite2)
originaldata_cleaned$scite3 <- as.character(originaldata_cleaned$scite3)
originaldata_cleaned$INT_LOG <- as.factor(originaldata_cleaned$INT_LOG)
originaldata_cleaned$INT_IDEO <- as.factor(originaldata_cleaned$INT_IDEO)
originaldata_cleaned$INT_ANY <- as.factor(originaldata_cleaned$INT_ANY)
originaldata_cleaned$INT_MISC <- as.factor(originaldata_cleaned$INT_MISC)
originaldata_cleaned$related <- as.character(originaldata_cleaned$related)


#### correcting date fields
originaldata_cleaned$approxdate <- parse_date(originaldata_cleaned$approxdate)
originaldata_cleaned$resolution <- parse_date(originaldata_cleaned$resolution)



##********************************* DATA PREPARATION ******************************************************

ggplot(originaldata_cleaned, aes(success, ..count..)) + geom_bar(aes(fill = success), position = "dodge")
head(originaldata_cleaned)


#********************************** Exploratory data analysis********************************************
options(repr.plot.width=5, repr.plot.height=3)
## Analysis about regional success rate
library(ggplot2)
ggplot(originaldata_cleaned, aes(x = region, y =success, fill = region)) + geom_bar() 
mosaicplot(data= originaldata_cleaned, region, success, cex = 0.75)


install.packages("visdat")
library(visdat)
vis_miss(original_data, warn_large_data = FALSE)



install.packages("Amelia")
library(Amelia)
missmap(airquality)