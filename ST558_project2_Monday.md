ST558 Project 2
================
Mu-Tien, Lee

-   [Require package](#require-package)
-   [Read in data](#read-in-data)
-   [Summarize the training data](#summarize-the-training-data)
-   [Training Model](#training-model)
    -   [Tree-based model](#tree-based-model)
    -   [Boosted tree model](#boosted-tree-model)
-   [Predicting using the best tree-base model](#predicting-using-the-best-tree-base-model)
-   [Predicting using the best boosted-tree model](#predicting-using-the-best-boosted-tree-model)

Require package
===============

``` r
#install.packages("")
library(knitr)
library(rmarkdown)
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
library(rpart)
library(DT)
```

Read in data
============

``` r
#read in hour data
HourData <- read.csv("hour.csv")
HourData<- HourData %>% select(-casual, -registered)
HourData$yr <- as.factor(HourData$yr)
HourData$holiday <- as.factor(HourData$holiday)
HourData$workingday <- as.factor(HourData$workingday)

#filter data by weekday
HourData <-HourData %>% filter(weekday==params$w)
#showing data
HourData <-HourData %>% select(-weekday, -workingday,-instant)
tbl_df(HourData)
```

    ## # A tibble: 2,479 x 12
    ##    dteday season yr     mnth    hr holiday weathersit  temp atemp   hum
    ##    <chr>   <int> <fct> <int> <int> <fct>        <int> <dbl> <dbl> <dbl>
    ##  1 2011-~      1 0         1     0 0                1  0.22 0.197  0.44
    ##  2 2011-~      1 0         1     1 0                1  0.2  0.167  0.44
    ##  3 2011-~      1 0         1     4 0                1  0.16 0.136  0.47
    ##  4 2011-~      1 0         1     5 0                1  0.16 0.136  0.47
    ##  5 2011-~      1 0         1     6 0                1  0.14 0.106  0.5 
    ##  6 2011-~      1 0         1     7 0                1  0.14 0.136  0.5 
    ##  7 2011-~      1 0         1     8 0                1  0.14 0.121  0.5 
    ##  8 2011-~      1 0         1     9 0                1  0.16 0.136  0.43
    ##  9 2011-~      1 0         1    10 0                1  0.18 0.167  0.43
    ## 10 2011-~      1 0         1    11 0                1  0.2  0.182  0.4 
    ## # ... with 2,469 more rows, and 2 more variables: windspeed <dbl>, cnt <int>

``` r
#Separate dataset into train (70%) and test (30%) data set
set.seed(1997)
train <- sample(1:nrow(HourData), size = nrow(HourData)*0.7)
test <- dplyr::setdiff(1:nrow(HourData), train)
HourDataTrain <- HourData[train, ]
HourDataTest <- HourData[test, ]
```

Summarize the training data
===========================

``` r
# plot the histogram of rental count
hist <- ggplot(data=HourDataTrain, aes(x=cnt))+geom_histogram(binwidth = 20, aes(color=yr))
hist <-hist+labs(title="Histogram of the retal count", x="rental count")
hist <-hist+scale_fill_discrete(labels=c(2011,2012))
hist
```

![](ST558_project2_Monday_files/figure-markdown_github/summarizing%20data-1.png)

``` r
#prin out summary table for tempature humidity and windspeed
sum <- HourDataTrain%>% select(c(temp, atemp, hum, windspeed))
kable(apply(sum, 2,summary), caption="Numeric Summary for weather measurement", format ="html")
```

<table>
<caption>
Numeric Summary for weather measurement
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
temp
</th>
<th style="text-align:right;">
atemp
</th>
<th style="text-align:right;">
hum
</th>
<th style="text-align:right;">
windspeed
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:right;">
0.0200000
</td>
<td style="text-align:right;">
0.0303000
</td>
<td style="text-align:right;">
0.2100000
</td>
<td style="text-align:right;">
0.0000000
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:right;">
0.3400000
</td>
<td style="text-align:right;">
0.3333000
</td>
<td style="text-align:right;">
0.4900000
</td>
<td style="text-align:right;">
0.1045000
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
0.5200000
</td>
<td style="text-align:right;">
0.5000000
</td>
<td style="text-align:right;">
0.6400000
</td>
<td style="text-align:right;">
0.1940000
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.4961614
</td>
<td style="text-align:right;">
0.4763708
</td>
<td style="text-align:right;">
0.6329395
</td>
<td style="text-align:right;">
0.1894927
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:right;">
0.6600000
</td>
<td style="text-align:right;">
0.6212000
</td>
<td style="text-align:right;">
0.7800000
</td>
<td style="text-align:right;">
0.2537000
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:right;">
0.9200000
</td>
<td style="text-align:right;">
0.8485000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.6866000
</td>
</tr>
</tbody>
</table>
``` r
#plot the boxplot of tempature humidity and windspeed (not genralized amount)
#plot base
boxplot <- ggplot(data = HourDataTrain, aes(x=season))
#adding 4 variables
tem <-boxplot+geom_boxplot(aes(y=temp*41, group=season))+labs(y="Tempature (c)", title = "boxplot for weather measurement")
fetem <-boxplot+geom_boxplot(aes(y=atemp*50, group=season))+labs(y="Feeling Tempature (c)")
hum <-boxplot+geom_boxplot(aes(y=hum*100, group=season))+labs(y="Humidity")
wind <-boxplot+geom_boxplot(aes(y=windspeed*67, group=season))+labs(y="Wind Speed")
#combine 4 plots into 1
ggarrange(tem, fetem, hum , wind, ncol = 2, nrow = 2)
```

![](ST558_project2_Monday_files/figure-markdown_github/summarizing%20data-2.png)

``` r
# plot the count distribution among time and weather
# by time
barplot1<-ggplot(data = HourDataTrain, aes(x=hr))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot1 <- barplot1+labs(x="time", y="Rental Count", title="Retal count distribution by month" )
barplot1+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](ST558_project2_Monday_files/figure-markdown_github/summarizing%20data-3.png)

``` r
# by weather
barplot2 <-ggplot(data = HourDataTrain, aes(x=weathersit))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot2 <- barplot2+labs(x="Weather situation, 1: clear day, 2: misty day, 3:rain or snow", y="Rental Count", title="Retal count distribution by month" )
barplot2+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](ST558_project2_Monday_files/figure-markdown_github/summarizing%20data-4.png)

Training Model
==============

Here I use two different method, First one useing a tree-based models using leave one out cross validation. Second, I use the boosted tree model with cross validation. Both two training are done using the `train` function from `caret` package. The data was cantered and scaled before training.Since our response variables is continuous response, I choose to use Regression tree.

Tree-based model
----------------

``` r
# set up training control, using leave one out cross validation.
set.seed(615)
trctrl <- trainControl(method = "LOOCV", number = 1)

# getModelInfo("rpart")
# training using regression tree models with cp in [0.001, 0.002]
# since the cp seems have to be really small when I used the default cp to train

model1 <- cnt~season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed

RegTree_fit1 <- train(model1, data = HourDataTrain, method = "rpart",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid=expand.grid(cp=seq(0.0001,0.0015,0.00005))
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
    ##   cp       RMSE      Rsquared   MAE     
    ##   0.00010  74.41616  0.8294464  43.44368
    ##   0.00015  74.64025  0.8283598  44.13635
    ##   0.00020  74.74549  0.8278932  44.15232
    ##   0.00025  74.63540  0.8283110  44.09081
    ##   0.00030  74.79066  0.8275371  44.69143
    ##   0.00035  74.72617  0.8278057  44.56357
    ##   0.00040  74.67621  0.8280043  44.67748
    ##   0.00045  74.39497  0.8292049  44.35688
    ##   0.00050  74.55470  0.8285169  44.60506
    ##   0.00055  74.81282  0.8272250  45.52864
    ##   0.00060  74.91312  0.8267853  45.60972
    ##   0.00065  75.52083  0.8240864  45.90723
    ##   0.00070  76.39893  0.8201303  46.66755
    ##   0.00075  77.17672  0.8165459  47.97332
    ##   0.00080  77.51435  0.8149758  48.53120
    ##   0.00085  77.38920  0.8155622  48.20412
    ##   0.00090  77.38915  0.8155430  48.26579
    ##   0.00095  77.75470  0.8137672  48.76968
    ##   0.00100  77.70269  0.8139237  48.75724
    ##   0.00105  78.21642  0.8113911  49.21642
    ##   0.00110  78.31466  0.8108923  49.55236
    ##   0.00115  78.15624  0.8115385  49.60313
    ##   0.00120  78.31714  0.8107211  49.86625
    ##   0.00125  78.00094  0.8121361  49.46892
    ##   0.00130  77.48578  0.8145319  49.00782
    ##   0.00135  77.53546  0.8142192  49.13666
    ##   0.00140  77.49758  0.8143671  49.12732
    ##   0.00145  77.37628  0.8149834  49.04479
    ##   0.00150  77.66461  0.8136522  49.26109
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.00045.

``` r
# plot the RMSE of selected cp
plot(RegTree_fit1)
```

![](ST558_project2_Monday_files/figure-markdown_github/tree%20based%20model-1.png)

``` r
# plot my final tree model
rpart.plot(RegTree_fit1$finalModel)
```

![](ST558_project2_Monday_files/figure-markdown_github/tree%20based%20model-2.png)

Boosted tree model
------------------

``` r
# set up training control, using cross validation with 10 folder
set.seed(615)
trctrl <- trainControl(method = "cv", number = 10)

# training using boosted tree models with boosting interation in [200,400] and try max tree depth 5~9
model2 <- cnt~season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed
RegTree_fit2 <- train(model2, data = HourDataTrain, method = "gbm",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneGrid=expand.grid(n.trees=seq(800,1250,20),
                                     interaction.depth=6:10,
                                     shrinkage=0.1, n.minobsinnode=10)
                 )
```

``` r
# show the training result
RegTree_fit2$bestTune
```

    ##    n.trees interaction.depth shrinkage n.minobsinnode
    ## 80    1000                 9       0.1             10

``` r
# plot the RMSE of selected parameters
plot(RegTree_fit2)
```

![](ST558_project2_Monday_files/figure-markdown_github/boosted%20result-1.png)

Predicting using the best tree-base model
=========================================

``` r
# predict use predict function
tree_pred <- predict(RegTree_fit1, newdata = HourDataTest)

#Calculate the Root MSE
RMSE_tree<- sqrt(mean((tree_pred-HourDataTest$cnt)^2))
label <- paste0("RMSE =", RMSE_tree)

# plot the prediction
count <- data.frame(true_count=HourDataTest$cnt,prediction=tree_pred )
predPlot <- ggplot(data=count, aes(x=true_count,y=prediction))
predPlot <- predPlot+labs(title="Prediction V.s. True Count using tree-base model")+geom_point()
predPlot <- predPlot+geom_smooth(color="orange")+geom_abline(aes(intercept=0,slope=1), color="blue")
predPlot <- predPlot+geom_text(x=200, y=800,label=label, color="brown")
predPlot
```

![](ST558_project2_Monday_files/figure-markdown_github/predicting%20tree%20model-1.png)

Predicting using the best boosted-tree model
============================================

``` r
# predict use predict function
boosted_pred <- predict(RegTree_fit2, newdata = HourDataTest)

#Calculate the Root MSE
RMSE_boosted <- sqrt(mean((boosted_pred-HourDataTest$cnt)^2))
lab <- paste0("RMSE =", RMSE_boosted)
# plot the prediction
count2 <- data.frame(True_count=HourDataTest$cnt,prediction=boosted_pred )
pred_plot <- ggplot(data=count2, aes(x=True_count,y=prediction))
pred_plot <- pred_plot+labs(title="Prediction V.s. True Count using boosted model")+geom_point()
pred_plot <- pred_plot+geom_smooth(color="orange")+geom_abline(aes(intercept=0,slope=1), color="blue")
pred_plot <- pred_plot+geom_text(x=200, y=800,label=lab, color=" brown")
pred_plot
```

![](ST558_project2_Monday_files/figure-markdown_github/predicting%20boosted%20model-1.png)
