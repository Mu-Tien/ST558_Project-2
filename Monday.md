ST558 Project 2 (Monday)
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
#install.packages("render")
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
setwd("C:\\Users\\LeeSF\\Desktop\\2020 fall\\ST558")

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
kable(apply(sum, 2,summary), caption="Numeric Summary for weather measurement")
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
                 tuneGrid=expand.grid(cp=seq(0.001,0.002,0.00005))
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
    ##   0.00155  77.82417  0.8129615  49.11446
    ##   0.00160  77.89277  0.8126243  49.12807
    ##   0.00165  78.40299  0.8102618  49.34306
    ##   0.00170  78.47801  0.8099186  49.40792
    ##   0.00175  78.53918  0.8096317  49.53943
    ##   0.00180  78.59359  0.8093984  49.76387
    ##   0.00185  78.95174  0.8077498  50.12080
    ##   0.00190  79.19663  0.8066072  50.09102
    ##   0.00195  79.23748  0.8064052  50.08492
    ##   0.00200  79.32319  0.8059592  50.16656
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.00145.

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
RegTree_fit2 <- train(model2, data = HourDataTrain, method = "bstTree",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneGrid=expand.grid(mstop=seq(200,400,10),
                                     maxdepth=5:9, nu=0.1)
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
    ##   5         200    44.34378  0.9382299  27.49566
    ##   5         210    44.29269  0.9383821  27.48227
    ##   5         220    44.22658  0.9385831  27.43436
    ##   5         230    44.19365  0.9386682  27.42682
    ##   5         240    44.14976  0.9388267  27.43160
    ##   5         250    44.14893  0.9388210  27.37704
    ##   5         260    44.16432  0.9387575  27.38152
    ##   5         270    44.20637  0.9386334  27.41638
    ##   5         280    44.21292  0.9386131  27.43226
    ##   5         290    44.23265  0.9385227  27.46942
    ##   5         300    44.23306  0.9385160  27.48662
    ##   5         310    44.23100  0.9385103  27.47741
    ##   5         320    44.26255  0.9384375  27.52985
    ##   5         330    44.25256  0.9384478  27.54061
    ##   5         340    44.26268  0.9384286  27.53256
    ##   5         350    44.25868  0.9384332  27.51781
    ##   5         360    44.21570  0.9385402  27.51248
    ##   5         370    44.19264  0.9385888  27.49480
    ##   5         380    44.17832  0.9386105  27.50386
    ##   5         390    44.18945  0.9385726  27.52538
    ##   5         400    44.16098  0.9386687  27.52205
    ##   6         200    43.60189  0.9402449  26.83237
    ##   6         210    43.60431  0.9402232  26.84829
    ##   6         220    43.63328  0.9401198  26.87195
    ##   6         230    43.59843  0.9402022  26.87221
    ##   6         240    43.56884  0.9403031  26.86836
    ##   6         250    43.54436  0.9403936  26.86523
    ##   6         260    43.55691  0.9403452  26.85348
    ##   6         270    43.60407  0.9402243  26.89196
    ##   6         280    43.64322  0.9401172  26.91592
    ##   6         290    43.58766  0.9402587  26.89327
    ##   6         300    43.52972  0.9404431  26.86678
    ##   6         310    43.51706  0.9404799  26.89770
    ##   6         320    43.53254  0.9404602  26.89177
    ##   6         330    43.48265  0.9406040  26.88601
    ##   6         340    43.48753  0.9405995  26.90292
    ##   6         350    43.48948  0.9405993  26.91871
    ##   6         360    43.48947  0.9405926  26.93740
    ##   6         370    43.52002  0.9405119  26.97601
    ##   6         380    43.53002  0.9404881  26.98835
    ##   6         390    43.54508  0.9404478  26.99840
    ##   6         400    43.56941  0.9403839  27.01963
    ##   7         200    43.27117  0.9408636  25.81142
    ##   7         210    43.21894  0.9410200  25.75609
    ##   7         220    43.17230  0.9411451  25.75166
    ##   7         230    43.16113  0.9411796  25.76923
    ##   7         240    43.18960  0.9411171  25.80546
    ##   7         250    43.18570  0.9411531  25.83500
    ##   7         260    43.18131  0.9411614  25.84897
    ##   7         270    43.17397  0.9411929  25.85930
    ##   7         280    43.14600  0.9412612  25.88348
    ##   7         290    43.17651  0.9411915  25.90092
    ##   7         300    43.20243  0.9411198  25.92978
    ##   7         310    43.19434  0.9411440  25.93468
    ##   7         320    43.20515  0.9411233  25.94856
    ##   7         330    43.19403  0.9411489  25.95695
    ##   7         340    43.23857  0.9410405  25.96819
    ##   7         350    43.25448  0.9409844  25.96810
    ##   7         360    43.26097  0.9409650  25.97837
    ##   7         370    43.23822  0.9410329  25.95853
    ##   7         380    43.25177  0.9410138  25.98492
    ##   7         390    43.26264  0.9409888  26.00134
    ##   7         400    43.26092  0.9409930  26.00880
    ##   8         200    44.79457  0.9370771  26.56937
    ##   8         210    44.78896  0.9371043  26.59412
    ##   8         220    44.77798  0.9371292  26.58480
    ##   8         230    44.77406  0.9371406  26.59698
    ##   8         240    44.75292  0.9372027  26.58139
    ##   8         250    44.71400  0.9373130  26.56717
    ##   8         260    44.71009  0.9373279  26.56267
    ##   8         270    44.70117  0.9373654  26.52568
    ##   8         280    44.68708  0.9374061  26.49835
    ##   8         290    44.70345  0.9373586  26.51195
    ##   8         300    44.70722  0.9373434  26.53032
    ##   8         310    44.71606  0.9373282  26.54807
    ##   8         320    44.72429  0.9373098  26.55888
    ##   8         330    44.71726  0.9373322  26.56003
    ##   8         340    44.72687  0.9373203  26.56587
    ##   8         350    44.71844  0.9373490  26.56084
    ##   8         360    44.74503  0.9372860  26.58740
    ##   8         370    44.75738  0.9372600  26.59577
    ##   8         380    44.75874  0.9372495  26.60007
    ##   8         390    44.76257  0.9372304  26.60954
    ##   8         400    44.75449  0.9372537  26.60962
    ##   9         200    44.71286  0.9372801  26.65053
    ##   9         210    44.73310  0.9372279  26.67564
    ##   9         220    44.69106  0.9373538  26.67025
    ##   9         230    44.67014  0.9374105  26.66907
    ##   9         240    44.65297  0.9374558  26.66495
    ##   9         250    44.64572  0.9374682  26.65691
    ##   9         260    44.67197  0.9374054  26.67850
    ##   9         270    44.68242  0.9373738  26.68722
    ##   9         280    44.69456  0.9373381  26.70936
    ##   9         290    44.69772  0.9373216  26.71370
    ##   9         300    44.71505  0.9372822  26.72135
    ##   9         310    44.72600  0.9372557  26.71678
    ##   9         320    44.72133  0.9372679  26.70962
    ##   9         330    44.74176  0.9372207  26.72285
    ##   9         340    44.75349  0.9371838  26.73084
    ##   9         350    44.75383  0.9371743  26.73790
    ##   9         360    44.76608  0.9371430  26.74333
    ##   9         370    44.76623  0.9371433  26.74715
    ##   9         380    44.76127  0.9371607  26.75422
    ##   9         390    44.77347  0.9371365  26.76614
    ##   9         400    44.77291  0.9371388  26.76133
    ## 
    ## Tuning parameter 'nu' was held constant at a value of 0.1
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were mstop = 280, maxdepth = 7 and nu = 0.1.

``` r
# plot the RMSE of selected parameters
plot(RegTree_fit2)
```

![](ST558_project2_Monday_files/figure-markdown_github/boosted%20tree%20model-1.png)

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
