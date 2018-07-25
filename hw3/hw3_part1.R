sample_submission = read.csv("./house_price_data/sample_submission.csv")
test <- read.csv("./house_price_data/test.csv")
train = read.csv("./house_price_data/train.csv")

#The response variable; SalePrice
##As you can see, the sale prices are right skewed. This was expected as few people can afford very expensive houses. I will keep this in mind, and take measures before modeling.

library(ggplot2)
library(ggrepel)
library(knitr)
library(dplyr)
library(plyr)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(randomForest)
library(psych)
library(xgboost)

test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train, test)

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000))



#The most important numeric predictors
##The character variables need some work before I can use them. To get a feel for the dataset, I decided to first see which numeric variables have a high correlation with the SalePrice.

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
## There are 37 numeric variables
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
library(corrplot)
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")


#Overall Quality
##Overall Quality has the highest correlation with SalePrice among the numeric variables (0.79). It rates the overall material and finish of the house on a scale from 1 (very poor) to 10 (very excellent).

ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))


#Above Grade (Ground) Living Area (square feet)
##The numeric variable with the second highest correlation with SalesPrice is the Above Grade Living Area. This make a lot of sense; big houses are generally more expensive.

ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000)) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))

all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]
##      SalePrice GrLivArea OverallQual
## 524     184750      4676          10
## 1299    160000      5642          10

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
##       PoolQC  MiscFeature        Alley        Fence    SalePrice 
##         2909         2814         2721         2348         1459 
##  FireplaceQu  LotFrontage  GarageYrBlt GarageFinish   GarageQual 
##         1420          486          159          159          159 
##   GarageCond   GarageType     BsmtCond BsmtExposure     BsmtQual 
##          159          157           82           82           81 
## BsmtFinType2 BsmtFinType1   MasVnrType   MasVnrArea     MSZoning 
##           80           79           24           23            4 
##    Utilities BsmtFullBath BsmtHalfBath   Functional  Exterior1st 
##            2            2            2            2            1 
##  Exterior2nd   BsmtFinSF1   BsmtFinSF2    BsmtUnfSF  TotalBsmtSF 
##            1            1            1            1            1 
##   Electrical  KitchenQual   GarageCars   GarageArea     SaleType 
##            1            1            1            1            1
cat('There are', length(NAcol), 'columns with missing values')
## There are 35 columns with missing values

all$PoolQC[is.na(all$PoolQC)] <- 'None'
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)
all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)
##5.3 Label encoding/factorizing the remaining character variables
Charcol <- names(all[,sapply(all, is.character)])
Charcol
cat('There are', length(Charcol), 'remaining columns with character values')

#Ex   Excellent
#Gd   Good
#TA   Average/Typical
#Fa   Fair
#NA   No Pool
