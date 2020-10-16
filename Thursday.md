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

    ## # A tibble: 2,471 x 12
    ##    dteday season yr     mnth    hr holiday weathersit  temp atemp   hum
    ##    <chr>   <int> <fct> <int> <int> <fct>        <int> <dbl> <dbl> <dbl>
    ##  1 2011-~      1 0         1     0 0                1  0.18 0.242  0.55
    ##  2 2011-~      1 0         1     1 0                1  0.16 0.227  0.64
    ##  3 2011-~      1 0         1     2 0                1  0.16 0.227  0.64
    ##  4 2011-~      1 0         1     4 0                2  0.16 0.197  0.64
    ##  5 2011-~      1 0         1     5 0                2  0.14 0.182  0.69
    ##  6 2011-~      1 0         1     6 0                2  0.14 0.167  0.63
    ##  7 2011-~      1 0         1     7 0                2  0.16 0.227  0.59
    ##  8 2011-~      1 0         1     8 0                1  0.16 0.227  0.59
    ##  9 2011-~      1 0         1     9 0                2  0.18 0.242  0.51
    ## 10 2011-~      1 0         1    10 0                1  0.2  0.258  0.47
    ## # ... with 2,461 more rows, and 2 more variables: windspeed <dbl>, cnt <int>

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

![](Thursday_files/figure-markdown_github/summarizing%20data-1.png)

``` r
#prin out summary table for tempature humidity and windspeed
sum <- HourDataTrain%>% select(c(temp, atemp, hum, windspeed))
kable(apply(sum, 2,summary), caption="Numeric Summary for weather measurement")
```

|         |       temp|      atemp|        hum|  windspeed|
|:--------|----------:|----------:|----------:|----------:|
| Min.    |  0.1000000|  0.0758000|  0.0000000|  0.0000000|
| 1st Qu. |  0.3400000|  0.3333000|  0.4700000|  0.1045000|
| Median  |  0.5000000|  0.4848000|  0.6100000|  0.1940000|
| Mean    |  0.5064199|  0.4842271|  0.6132678|  0.1907718|
| 3rd Qu. |  0.6600000|  0.6212000|  0.7700000|  0.2537000|
| Max.    |  0.9400000|  0.9242000|  1.0000000|  0.6866000|

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

![](Thursday_files/figure-markdown_github/plotting%20data-1.png)

``` r
# plot the count distribution among time and weather
# by time
barplot1<-ggplot(data = HourDataTrain, aes(x=hr))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot1 <- barplot1+labs(x="time", y="Rental Count", title="Retal count distribution by month" )
barplot1+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](Thursday_files/figure-markdown_github/plotting%20data-2.png)

``` r
# by weather
barplot2 <-ggplot(data = HourDataTrain, aes(x=weathersit))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot2 <- barplot2+labs(x="Weather situation, 1: clear day, 2: misty day, 3:rain or snow", y="Rental Count", title="Retal count distribution by month" )
barplot2+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](Thursday_files/figure-markdown_github/plotting%20data-3.png)

Training Model
==============

Here I use two different method to train my model. First method is using a tree-based models with leave one out cross validation. For the second method, I use the boosted tree model with cross validation. Both two training are done using the `train` function from `caret` package. The data was cantered and scaled before training. Moreover, since our response variable is continuous responses, I choose to use Regression tree.

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
                 tuneGrid=expand.grid(cp=seq(0.0001,0.0005,0.00004))
)

# show the training result
RegTree_fit1
```

    ## CART 
    ## 
    ## 1729 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (10), scaled (10) 
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 1728, 1728, 1728, 1728, 1728, 1728, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp       RMSE      Rsquared   MAE     
    ##   0.00010  60.86147  0.8970593  36.61289
    ##   0.00014  60.84946  0.8970917  36.89877
    ##   0.00018  60.91024  0.8968519  37.09426
    ##   0.00022  60.92366  0.8967952  37.38067
    ##   0.00026  61.14398  0.8960384  37.95852
    ##   0.00030  61.03935  0.8963359  38.20563
    ##   0.00034  61.04983  0.8962482  38.23389
    ##   0.00038  61.26566  0.8955081  38.32099
    ##   0.00042  61.63042  0.8943369  38.68553
    ##   0.00046  61.60945  0.8944215  38.65511
    ##   0.00050  62.12086  0.8926813  39.15997
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.00014.

``` r
# plot the RMSE of selected cp
plot(RegTree_fit1)
```

![](Thursday_files/figure-markdown_github/tree%20based%20model-1.png)

``` r
# plot my final tree model
rpart.plot(RegTree_fit1$finalModel)
```

![](Thursday_files/figure-markdown_github/tree%20based%20model-2.png)

Boosted tree model
------------------

``` r
# set up training control, using cross validation with 10 folder
set.seed(615)
trctrl <- trainControl(method = "cv", number = 10)

# training using boosted tree models with boosting interation in [700,1250] and try max tree depth 5~9
model2 <- cnt~season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed
RegTree_fit2 <- train(model2, data = HourDataTrain, method = "gbm",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneGrid=expand.grid(n.trees=seq(500,1250,25),
                                     interaction.depth=5:11,
                                     shrinkage=0.1, n.minobsinnode=10)
                 )
```

``` r
# show the training result of boosted tree
RegTree_fit2$bestTune
```

    ##     n.trees interaction.depth shrinkage n.minobsinnode
    ## 191     600                11       0.1             10

``` r
# plot the RMSE of different parameters
plot(RegTree_fit2)
```

![](Thursday_files/figure-markdown_github/boosted%20result-1.png)

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

![](Thursday_files/figure-markdown_github/predicting%20tree%20model-1.png)

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

![](Thursday_files/figure-markdown_github/predicting%20boosted%20model-1.png)
