ST558 Project 2
================
Mu-Tien, Lee

-   [Introduction](#introduction)
-   [Analysis](#analysis)
-   [code for Automation](#code-for-automation)

Introduction
============

The main purpose of the project is to build a model that can predict the count of rantal of bikes.
During the project, I'm going to use the bike sharing data from [UCI machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset). This file contain the data of the bicycle rentaling. There are two different dataset. One of them contain the date, season,year, month, holiday or not, weekday, workingday or not, weather situation, temperature, feeling temperature, humidity, windspeed, and the count. It also contain the record to see if the person is a casual user or registered, however, I don't take this into my consideration. The other one contain all same variables but more detail about the time with hour variable that shows what time did the bicycle was rented. Since I personal think the count of the rental will have huge difference between times. I would like to use the dataset with hour variables to do this project.
I am going to use two different method to build my model.First, I will use leave one out cross validation to fit a tree-based regression model. Secondly, I will fit a boosted tree model using cross validation. After that, I will provide some prediction. Most importantly, I will compare these two model, and see which one is better on predicting the rental count of bicycle. Notice that I'm going to separate my data by weekday. That is to say each weekday will have their own best model.

Analysis
========

click below links to check the analysis of each weekday.\
[Monday analysis](ST558_project2_Monday.md)\
[Tuesday analysis](Tuesday.md)\
[Wednesday analysis](Wednesday.md)\
[Thursday analysis](Thursday.md)\
[Friday analysis](Friday.md)\
[Saturday analysis](Saturday.md)\
[Sunday analysis](Sunday.md)

code for Automation
===================

``` r
library(knitr)
library(rmarkdown)
```

``` r
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Tuesday.md",
       params =list(w=2))

render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Wednesday.md",
       params =list(w=3))

render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Thursday.md",
       params =list(w=4))

render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Friday.md",
       params =list(w=5))

render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Saturday.md",
       params =list(w=6))
render("ST558_project2_Monday.Rmd",
       output_format = "github_document",
       output_file = "Sunday.md",
       params =list(w=0))
```

In these analysis, we can see that the boosted tree model has the smaller RMSE on all the training model compare to the tree-base model.Also, boosted model has the lower RMSE when using the testing dataset to predict the rental count.
