Modeling the rental count of bikes \_ Main code
================
Mu-Tien, Lee

  - [Require package](#require-package)
  - [Read in data](#read-in-data)
  - [Summarize the training data](#summarize-the-training-data)
  - [Training Model](#training-model)
      - [Tree-based model](#tree-based-model)
      - [Boosted tree model](#boosted-tree-model)
  - [Predicting using the best tree-base
    model](#predicting-using-the-best-tree-base-model)
  - [Predicting using the best boosted-tree
    model](#predicting-using-the-best-boosted-tree-model)

# Require package

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

# Read in data

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

    ## # A tibble: 2,487 x 12
    ##    dteday season yr     mnth    hr holiday weathersit  temp atemp   hum
    ##    <chr>   <int> <fct> <int> <int> <fct>        <int> <dbl> <dbl> <dbl>
    ##  1 2011-~      1 0         1     0 0                2  0.2  0.197  0.64
    ##  2 2011-~      1 0         1     1 0                2  0.2  0.197  0.69
    ##  3 2011-~      1 0         1     2 0                2  0.2  0.197  0.69
    ##  4 2011-~      1 0         1     4 0                2  0.2  0.212  0.69
    ##  5 2011-~      1 0         1     5 0                3  0.22 0.273  0.55
    ##  6 2011-~      1 0         1     6 0                2  0.2  0.258  0.69
    ##  7 2011-~      1 0         1     7 0                1  0.2  0.212  0.69
    ##  8 2011-~      1 0         1     8 0                1  0.2  0.197  0.51
    ##  9 2011-~      1 0         1     9 0                1  0.2  0.182  0.47
    ## 10 2011-~      1 0         1    10 0                1  0.22 0.197  0.37
    ## # ... with 2,477 more rows, and 2 more variables: windspeed <dbl>, cnt <int>

``` r
#Separate dataset into train (70%) and test (30%) data set
set.seed(1997)
train <- sample(1:nrow(HourData), size = nrow(HourData)*0.7)
test <- dplyr::setdiff(1:nrow(HourData), train)
HourDataTrain <- HourData[train, ]
HourDataTest <- HourData[test, ]
```

# Summarize the training data

Here I will show you some summary of my training dataset.  
1\. I conduct a histogram of the rental count, since this is my response
variable.  
2\. I built up a summary table of all the weather measurement.  
3\. I also showing the weather summary via a boxplot.  
4\. I plot the rental count distributed by time.  
5\. I plot the rental count distributed by weather situation.

``` r
# plot the histogram of rental count
hist <- ggplot(data=HourDataTrain, aes(x=cnt))+geom_histogram(binwidth = 20, aes(color=yr))
hist <-hist+labs(title="Histogram of the retal count", x="rental count")
hist <-hist+scale_fill_discrete(labels=c(2011,2012))
hist
```

![](Friday_files/figure-gfm/summarizing%20data-1.png)<!-- -->

``` r
#prin out summary table for tempature humidity and windspeed
sum <- HourDataTrain%>% select(c(temp, atemp, hum, windspeed))
kable(apply(sum, 2,summary), caption="Numeric Summary for weather measurement")
```

|         |      temp |     atemp |       hum | windspeed |
| :------ | --------: | --------: | --------: | --------: |
| Min.    | 0.0600000 | 0.0303000 | 0.0800000 |  0.000000 |
| 1st Qu. | 0.3400000 | 0.3182000 | 0.4600000 |  0.104500 |
| Median  | 0.5000000 | 0.4848000 | 0.6100000 |  0.164200 |
| Mean    | 0.4982184 | 0.4727968 | 0.6115805 |  0.185808 |
| 3rd Qu. | 0.6600000 | 0.6212000 | 0.7600000 |  0.253700 |
| Max.    | 0.9600000 | 1.0000000 | 1.0000000 |  0.806000 |

Numeric Summary for weather measurement

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

![](Friday_files/figure-gfm/plotting%20data-1.png)<!-- -->

``` r
# plot the count distribution among time and weather
# by time
barplot1<-ggplot(data = HourDataTrain, aes(x=hr))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot1 <- barplot1+labs(x="time", y="Rental Count", title="Retal count distribution by month" )
barplot1+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](Friday_files/figure-gfm/plotting%20data-2.png)<!-- -->

``` r
# by weather
barplot2 <-ggplot(data = HourDataTrain, aes(x=weathersit))+geom_col(aes(y=cnt, fill=yr))+facet_wrap(~mnth)
barplot2 <- barplot2+labs(x="Weather situation, 1: clear day, 2: misty day, 3:rain or snow", y="Rental Count", title="Retal count distribution by month" )
barplot2+scale_fill_discrete(name="year", labels=c(2011,2012))
```

![](Friday_files/figure-gfm/plotting%20data-3.png)<!-- -->

# Training Model

Here I use two different method to train my model. First method is using
a tree-based models with leave one out cross validation. For the second
method, I use the boosted tree model with cross validation. Both two
training are done using the `train` function from `caret` package. The
data was cantered and scaled before training.

## Tree-based model

Since our respons variable is continuous. I use the regression tree
model to training my data. The `method= "rpart"` was used in `train`
function  
Moreover, because I want to use the leave-one-out cross validation for
this training, therefore,the `method= "LOOCV"` was used in
`trainControl`.  
We can adjust the grid parameter by ourselves. Since the default result
shows that `cp` should be very small to have a lowest RMSE. I set a
range \[0.0001,0.0005\] to fit for every weekday.  
Something to notice, because the `cp` is too small, when I draw my
regression tree, it seems like a mess.

``` r
# set up training control, using leave one out cross validation.
set.seed(615)
trctrl <- trainControl(method = "LOOCV", number = 1)

# getModelInfo("rpart")
# training using regression tree models with cp in [0.0001,0.0005]
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
    ## 1740 samples
    ##   10 predictor
    ## 
    ## Pre-processing: centered (10), scaled (10) 
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 1739, 1739, 1739, 1739, 1739, 1739, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp       RMSE      Rsquared   MAE     
    ##   0.00010  64.77218  0.8633108  38.55849
    ##   0.00014  64.76988  0.8633107  38.53232
    ##   0.00018  64.77336  0.8632758  38.51982
    ##   0.00022  64.97628  0.8623942  39.67933
    ##   0.00026  65.08888  0.8619641  39.65091
    ##   0.00030  65.30849  0.8610317  39.89369
    ##   0.00034  65.56521  0.8599220  40.28419
    ##   0.00038  65.81266  0.8588588  40.70583
    ##   0.00042  65.79123  0.8589248  40.45751
    ##   0.00046  65.93805  0.8583658  40.54614
    ##   0.00050  66.14625  0.8574950  40.86600
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was cp = 0.00014.

``` r
# plot the RMSE of selected cp
plot(RegTree_fit1)
```

![](Friday_files/figure-gfm/tree%20based%20model-1.png)<!-- -->

``` r
# plot my final tree model
rpart.plot(RegTree_fit1$finalModel)
```

![](Friday_files/figure-gfm/tree%20based%20model-2.png)<!-- -->

## Boosted tree model

Here I want to training my data using boosted tree model. The `method=
"gbm"` was used in `train` function  
Because I want to use thecross validation for this training,
therefore,the `method= "cv"` was used in `trainControl`.  
We can adjust the grid parameter by ourselves. I set a range of number
of tree \[100,1250\] and interaction 5\~11 to fit for every weekday.  

``` r
# set up training control, using cross validation with 10 folder
set.seed(615)
trctrl <- trainControl(method = "cv", number = 10)

# training using boosted tree models with boosting interation in [700,1250] and try max tree depth 5~9
model2 <- cnt~season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed
RegTree_fit2 <- train(model2, data = HourDataTrain, method = "gbm",
                trControl=trctrl,
                preProcess = c("center", "scale"),
                tuneGrid=expand.grid(n.trees=seq(100,1250,25),
                                     interaction.depth=5:11,
                                     shrinkage=0.1, n.minobsinnode=10)
                 )
```

``` r
# show the training result of boosted tree
RegTree_fit2$bestTune
```

    ##     n.trees interaction.depth shrinkage n.minobsinnode
    ## 284     125                11       0.1             10

``` r
# plot the RMSE of different parameters
plot(RegTree_fit2)
```

![](Friday_files/figure-gfm/boosted%20result-1.png)<!-- -->

# Predicting using the best tree-base model

Using the best boosted tree model to testing the data.

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
predPlot <- predPlot+geom_text(x=200, y=600,label=label, color="brown")
predPlot
```

![](Friday_files/figure-gfm/predicting%20tree%20model-1.png)<!-- -->

# Predicting using the best boosted-tree model

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
pred_plot <- pred_plot+geom_text(x=200, y=600,label=lab, color=" brown")
pred_plot
```

![](Friday_files/figure-gfm/predicting%20boosted%20model-1.png)<!-- -->

``` r
# create a linear model using repeated cross-validation
linear_mod <- train(cnt~season+yr+mnth+hr+holiday+weathersit+temp+atemp+hum+windspeed,
                    data=HourDataTrain,
                    method='lm',
                    preProcess=c("center", "scale"),
                    metric='RMSE',
                    tuneLength=10,
                    trControl=trainControl(method='repeatedcv', number=10, repeats=3)
                    )

# display the results of the linear model
summary(linear_mod)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -358.66  -89.98  -27.69   53.87  602.77 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 198.1862     3.3160  59.766  < 2e-16 ***
    ## season       16.4403     5.8727   2.799  0.00518 ** 
    ## yr1          42.7597     3.3737  12.675  < 2e-16 ***
    ## mnth          6.1579     5.6341   1.093  0.27456    
    ## hr           54.3184     3.6313  14.958  < 2e-16 ***
    ## holiday1     -1.7758     3.4014  -0.522  0.60169    
    ## weathersit   -0.2167     3.6530  -0.059  0.95270    
    ## temp         51.0533    11.6545   4.381 1.25e-05 ***
    ## atemp        -5.1047    11.5861  -0.441  0.65957    
    ## hum         -32.6591     4.1184  -7.930 3.90e-15 ***
    ## windspeed     3.3709     3.6887   0.914  0.36092    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 138.3 on 1729 degrees of freedom
    ## Multiple R-squared:  0.3776, Adjusted R-squared:  0.374 
    ## F-statistic: 104.9 on 10 and 1729 DF,  p-value: < 2.2e-16

``` r
# compare our linear model to our test data
linear_pred <- predict(linear_mod, newdata=HourDataTest)
postResample(linear_pred, HourDataTest$cnt)
```

    ##        RMSE    Rsquared         MAE 
    ## 131.5847991   0.4165231  96.6972031
