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
RegTree_fit2 <- train(model2, data = HourDataTrain, method = "bstTree",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneGrid=expand.grid(mstop=seq(100,400,15),
                                     maxdepth=4:8, nu=0.1)
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
    ##   4         100    49.87005  0.9233877  31.12913
    ##   4         115    49.17329  0.9252649  30.70555
    ##   4         130    48.39015  0.9273693  30.19952
    ##   4         145    48.02095  0.9282838  29.99250
    ##   4         160    47.61484  0.9293724  29.71494
    ##   4         175    47.34984  0.9301121  29.58704
    ##   4         190    47.05294  0.9308442  29.35773
    ##   4         205    46.91222  0.9312101  29.25492
    ##   4         220    46.77823  0.9315765  29.18071
    ##   4         235    46.61695  0.9319918  29.09362
    ##   4         250    46.51357  0.9322614  29.07602
    ##   4         265    46.30582  0.9328558  29.00521
    ##   4         280    46.25513  0.9329857  28.96218
    ##   4         295    46.12718  0.9332932  28.84827
    ##   4         310    46.03235  0.9335345  28.78830
    ##   4         325    45.96425  0.9337491  28.73286
    ##   4         340    45.86041  0.9340690  28.68333
    ##   4         355    45.74189  0.9344080  28.62311
    ##   4         370    45.71420  0.9344380  28.60329
    ##   4         385    45.68086  0.9344943  28.55863
    ##   4         400    45.63207  0.9346206  28.53458
    ##   5         100    45.96814  0.9339917  28.56302
    ##   5         115    45.57687  0.9348894  28.31092
    ##   5         130    45.26160  0.9357012  28.13814
    ##   5         145    44.94606  0.9365121  27.92945
    ##   5         160    44.71372  0.9372040  27.78366
    ##   5         175    44.63290  0.9374031  27.67958
    ##   5         190    44.45785  0.9378813  27.57383
    ##   5         205    44.31899  0.9383099  27.49464
    ##   5         220    44.22658  0.9385831  27.43436
    ##   5         235    44.16965  0.9387486  27.41870
    ##   5         250    44.14893  0.9388210  27.37704
    ##   5         265    44.18064  0.9386909  27.39081
    ##   5         280    44.21292  0.9386131  27.43226
    ##   5         295    44.24694  0.9384840  27.48239
    ##   5         310    44.23100  0.9385103  27.47741
    ##   5         325    44.26743  0.9384130  27.53283
    ##   5         340    44.26268  0.9384286  27.53256
    ##   5         355    44.22621  0.9385221  27.51878
    ##   5         370    44.19264  0.9385888  27.49480
    ##   5         385    44.18802  0.9385891  27.51019
    ##   5         400    44.16098  0.9386687  27.52205
    ##   6         100    44.53098  0.9378800  27.29294
    ##   6         115    44.20658  0.9386445  27.12521
    ##   6         130    44.01552  0.9390827  26.99806
    ##   6         145    43.81740  0.9395864  26.92740
    ##   6         160    43.72837  0.9398658  26.84365
    ##   6         175    43.68219  0.9399855  26.82040
    ##   6         190    43.61654  0.9401781  26.83052
    ##   6         205    43.61716  0.9402082  26.84511
    ##   6         220    43.63328  0.9401198  26.87195
    ##   6         235    43.60646  0.9401887  26.88899
    ##   6         250    43.54436  0.9403936  26.86523
    ##   6         265    43.58573  0.9402706  26.87410
    ##   6         280    43.64322  0.9401172  26.91592
    ##   6         295    43.55132  0.9403654  26.87971
    ##   6         310    43.51706  0.9404799  26.89770
    ##   6         325    43.51007  0.9405326  26.89821
    ##   6         340    43.48753  0.9405995  26.90292
    ##   6         355    43.48340  0.9406048  26.92810
    ##   6         370    43.52002  0.9405119  26.97601
    ##   6         385    43.54538  0.9404455  27.00014
    ##   6         400    43.56941  0.9403839  27.01963
    ##   7         100    44.03518  0.9388414  26.01104
    ##   7         115    43.76090  0.9395449  25.90893
    ##   7         130    43.57405  0.9400532  25.89543
    ##   7         145    43.53460  0.9401770  25.85579
    ##   7         160    43.44825  0.9403958  25.87658
    ##   7         175    43.31800  0.9407473  25.79105
    ##   7         190    43.27387  0.9408632  25.79842
    ##   7         205    43.23776  0.9409449  25.77881
    ##   7         220    43.17230  0.9411451  25.75166
    ##   7         235    43.15948  0.9411917  25.77519
    ##   7         250    43.18570  0.9411531  25.83500
    ##   7         265    43.17541  0.9411769  25.85855
    ##   7         280    43.14600  0.9412612  25.88348
    ##   7         295    43.19565  0.9411368  25.91825
    ##   7         310    43.19434  0.9411440  25.93468
    ##   7         325    43.20011  0.9411351  25.94835
    ##   7         340    43.23857  0.9410405  25.96819
    ##   7         355    43.24205  0.9410147  25.96841
    ##   7         370    43.23822  0.9410329  25.95853
    ##   7         385    43.26171  0.9409887  25.98992
    ##   7         400    43.26092  0.9409930  26.00880
    ##   8         100    45.22595  0.9359816  26.63717
    ##   8         115    45.11354  0.9362703  26.61128
    ##   8         130    45.04345  0.9364448  26.60334
    ##   8         145    44.91707  0.9368046  26.59357
    ##   8         160    44.93553  0.9367379  26.62532
    ##   8         175    44.82559  0.9370018  26.58048
    ##   8         190    44.84828  0.9369264  26.57123
    ##   8         205    44.79900  0.9370722  26.59133
    ##   8         220    44.77798  0.9371292  26.58480
    ##   8         235    44.74387  0.9372225  26.58594
    ##   8         250    44.71400  0.9373130  26.56717
    ##   8         265    44.71560  0.9373156  26.54190
    ##   8         280    44.68708  0.9374061  26.49835
    ##   8         295    44.70335  0.9373553  26.52850
    ##   8         310    44.71606  0.9373282  26.54807
    ##   8         325    44.71375  0.9373402  26.55742
    ##   8         340    44.72687  0.9373203  26.56587
    ##   8         355    44.73412  0.9373114  26.56789
    ##   8         370    44.75738  0.9372600  26.59577
    ##   8         385    44.76166  0.9372367  26.60842
    ##   8         400    44.75449  0.9372537  26.60962
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
