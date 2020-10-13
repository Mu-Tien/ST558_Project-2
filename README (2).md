ST558 Project 2
================
Mu-Tien, Lee

-   [Introduction](#introduction)
-   [Require package](#require-package)
-   [Read in data](#read-in-data)
-   [Summary of training data](#summary-of-training-data)
-   [Modeling](#modeling)
    -   [Tree-based model](#tree-based-model)
    -   [Boosted tree model](#boosted-tree-model)

Introduction
============

This project, I'm going to use the bike sharing data from \[UCI machine Learning Repository\] <https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset>. The main purpose of the project is to build a model that can predict the count of rantal of bikes. I am going to use the method.

Require package
===============

``` r
#install.packages("rpart")
library(MuMIn)
library(tidyverse)
library(caret)
library(corrplot)
library(readxl)
library(caret)
library(ggiraphExtra)
library(knitr)
library(ggplot2)
library(ggpubr)
library(rpart.plot)
```

Read in data
============

``` r
setwd("C:\\Users\\LeeSF\\Desktop\\2020 fall\\ST558")

#read in hour data
HourData <- read.csv("hour.csv")
HourData<- HourData %>% select(-casual, -registered)
HourData$yr <- as.factor(HourData$yr)
HourData$holiday <- as.factor(HourData$holiday)
HourData$workingday <- as.factor(HourData$workingday)
#separate HourData by weekday
HourData <-HourData %>% filter(weekday==1)
HourData<- HourData %>% select(-weekday, -workingday)


#read in day data
DayData <- read.csv("day.csv")
DayData<- DayData %>% select(-casual, -registered)
DayData$yr <- as.factor(DayData$yr)
DayData$holiday <- as.factor(DayData$holiday)
DayData$workingday <- as.factor(DayData$workingday)
#separate DayData by weekday
DayData <-DayData %>% filter(weekday==1)
DayData<- DayData %>% select(-weekday, -workingday)

#Separate dataset into train and test data set
set.seed(1997)
train <- sample(1:nrow(HourData), size = nrow(HourData)*0.7)
test <- dplyr::setdiff(1:nrow(HourData), train)
HourDataTrain <- HourData[train, ]
HourDataTest <- HourData[test, ]
```

Summary of training data
========================

``` r
#prin out summary table for tempature humidity and windspeed
sum <- HourDataTrain%>% select(c(temp, atemp, hum, windspeed))
kable(apply(sum, 2,summary))
```

|         |       temp|      atemp|        hum|  windspeed|
|:--------|----------:|----------:|----------:|----------:|
| Min.    |  0.0200000|  0.0303000|  0.2100000|  0.0000000|
| 1st Qu. |  0.3400000|  0.3333000|  0.4900000|  0.1045000|
| Median  |  0.5200000|  0.5000000|  0.6400000|  0.1940000|
| Mean    |  0.4961614|  0.4763708|  0.6329395|  0.1894927|
| 3rd Qu. |  0.6600000|  0.6212000|  0.7800000|  0.2537000|
| Max.    |  0.9200000|  0.8485000|  1.0000000|  0.6866000|

``` r
#plot the boxplot of tempature humidity and windspeed (not genralized amount)
#plot base
boxplot <- ggplot(data = HourDataTrain, aes(x=season))
#adding 4 variables
tem <-boxplot+geom_boxplot(aes(y=temp*41, group=season))+labs(y="Tempature (c)")
fetem <-boxplot+geom_boxplot(aes(y=atemp*50, group=season))+labs(y="Feeling Tempature (c)")
hum <-boxplot+geom_boxplot(aes(y=hum*100, group=season))+labs(y="Humidity")
wind <-boxplot+geom_boxplot(aes(y=windspeed*67, group=season))+labs(y="Wind Speed")
ggarrange(tem, fetem, hum , wind, ncol = 2, nrow = 2)
```

![](ST558-project2_files/figure-markdown_github/summarizing%20data-1.png)

``` r
# plot the count distribution among time and weather
# by time
barplot1<-ggplot(data = HourDataTrain, aes(x=hr))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot1 <- barplot1+labs(x="time", y="Rental Count", title="Retal count distribution" )
barplot1+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](ST558-project2_files/figure-markdown_github/summarizing%20data-2.png)

``` r
# by weather
barplot2 <-ggplot(data = HourDataTrain, aes(x=weathersit))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot2 <- barplot2+labs(x="season", y="Rental Count", title="Retal count distribution" )
barplot2+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](ST558-project2_files/figure-markdown_github/summarizing%20data-3.png)

Modeling
========

Here I use two different method, First one useing a tree-based models using leave one out cross validation. Second, I use the boosted tree model also with cross validation.

Tree-based model
----------------

Since our response variables is continuous response, I choose to use Regression tree with leave one out cross validation (LOOCV) to pick up my final model.

``` r
# set up training control, using repeated cross validation with 10 folder repeated 20 times.
set.seed(615)
trctrl <- trainControl(method = "LOOCV", number = 1)

#getModelInfo("rpart")
# training using regression tree models with default cp

model1 <- cnt~season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed

RegTree_fit1 <- train(model1, data = HourDataTrain, method = "rpart",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength=40
                 )

# show the training result
RegTree_fit1
```

    ## CART 
    ## 
    ## 1735 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (10), scaled (10) 
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 1734, 1734, 1734, 1734, 1734, 1734, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp           RMSE       Rsquared     MAE      
    ##   0.001274010   77.60590  0.813999097   49.13824
    ##   0.001503610   77.66519  0.813649849   49.26629
    ##   0.001857951   79.02077  0.807418419   50.31139
    ##   0.001860388   79.01732  0.807434206   50.26314
    ##   0.001978055   79.39079  0.805624975   50.23399
    ##   0.002089646   79.78105  0.803710670   50.55910
    ##   0.002298521   81.97229  0.792568042   52.98393
    ##   0.002323297   82.24042  0.791193744   53.57338
    ##   0.002363108   82.42798  0.790145740   53.33727
    ##   0.002370245   82.40037  0.790283011   53.28758
    ##   0.002404571   82.10974  0.791736311   52.96614
    ##   0.002720782   82.73458  0.788384423   53.31944
    ##   0.002757648   83.05953  0.786741754   53.62395
    ##   0.002842221   83.99670  0.781892382   54.51930
    ##   0.002885936   84.36179  0.779946881   55.04919
    ##   0.003187067   84.35056  0.780171093   54.36669
    ##   0.003523397   84.66842  0.778561682   55.25125
    ##   0.004525537   86.61451  0.768651133   56.15581
    ##   0.004954105   88.62920  0.757391153   57.30141
    ##   0.005110243   89.76510  0.750834021   58.41085
    ##   0.005266633   90.34312  0.747988495   59.66679
    ##   0.005290709   90.58557  0.746203998   61.14940
    ##   0.005759970   90.30163  0.747711772   60.05523
    ##   0.006108316   91.85761  0.739096059   61.39660
    ##   0.006432456   93.99274  0.726840713   64.13671
    ##   0.006444206   94.00666  0.726755362   64.20883
    ##   0.007340255   95.13724  0.720430246   63.98144
    ##   0.007988249   95.84996  0.716149262   64.38960
    ##   0.008475771   98.34106  0.701320440   65.95465
    ##   0.008820124  100.17125  0.690267932   67.19823
    ##   0.009326248  102.05478  0.678167264   69.30807
    ##   0.009787155  100.74290  0.686170902   67.94116
    ##   0.010744583  103.01571  0.671989985   70.46249
    ##   0.014681520  103.64039  0.667910047   70.52111
    ##   0.017147672  107.00076  0.646014966   76.32647
    ##   0.023230763  117.69133  0.573317696   80.10754
    ##   0.030492328  127.45118  0.499159450   89.76886
    ##   0.054606382  134.33116  0.443972631   92.51634
    ##   0.090075086  161.05452  0.223024297  115.23671
    ##   0.307363623  184.04583  0.002041053  157.69796
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.00127401.

``` r
plot(RegTree_fit1)
```

![](ST558-project2_files/figure-markdown_github/tree%20based%20model-1.png)

``` r
rpart.plot(RegTree_fit1$finalModel)
```

![](ST558-project2_files/figure-markdown_github/tree%20based%20model-2.png)

Boosted tree model
------------------

``` r
# set up training control, using repeated cross validation with 10 folder
set.seed(615)
trctrl <- trainControl(method = "cv", number = 10)

#getModelInfo("rpart")
# training using regression tree models with default cp

model2 <- cnt~season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed

RegTree_fit2 <- train(model2, data = HourDataTrain, method = "bstTree",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid=expand.grid(mstop=c(100,200,300,500,1000),maxdepth=1:5, nu=1)
                 )

# show the training result
RegTree_fit2
```

    ## Boosted Tree 
    ## 
    ## 1735 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (10), scaled (10) 
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 1561, 1562, 1560, 1562, 1561, 1562, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   maxdepth  mstop  RMSE      Rsquared   MAE     
    ##   1          100   84.34529  0.7821506  60.63393
    ##   1          200   83.32922  0.7876925  59.84177
    ##   1          300   83.25700  0.7883070  59.79343
    ##   1          500   83.43448  0.7872840  59.89831
    ##   1         1000   83.72710  0.7859825  60.15395
    ##   2          100   66.29058  0.8659459  46.19104
    ##   2          200   62.01264  0.8833427  42.92020
    ##   2          300   60.08437  0.8906210  41.65170
    ##   2          500   58.19321  0.8973572  40.17142
    ##   2         1000   56.88630  0.9021549  39.35518
    ##   3          100   56.11468  0.9036264  39.21034
    ##   3          200   55.04294  0.9073998  38.18712
    ##   3          300   54.82439  0.9082893  37.84113
    ##   3          500   54.69637  0.9082613  38.07273
    ##   3         1000   55.42654  0.9061254  38.63849
    ##   4          100   59.51751  0.8935320  41.66776
    ##   4          200   60.00046  0.8921121  41.92145
    ##   4          300   59.93246  0.8922653  42.08637
    ##   4          500   60.07680  0.8920906  42.09818
    ##   4         1000   60.23639  0.8915435  42.21259
    ##   5          100   61.83802  0.8860639  43.35495
    ##   5          200   61.99179  0.8856133  43.56506
    ##   5          300   62.19596  0.8849249  43.67661
    ##   5          500   62.31025  0.8846061  43.76732
    ##   5         1000   62.33637  0.8845258  43.77523
    ## 
    ## Tuning parameter 'nu' was held constant at a value of 1
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were mstop = 500, maxdepth = 3 and nu = 1.

``` r
plot(RegTree_fit2)
```

![](ST558-project2_files/figure-markdown_github/boosted%20tree%20model-1.png)
