ver2\_456
================
Minh Le
2/21/2022

## Introduction

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.2

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.1.2

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

## Data description

Load dataset

``` r
stock <- stock <- read.csv("stock_return.csv", sep = "", dec = ".")
head(stock)
```

    ##       Date   ATT ATT.PctChg.1 Microsoft Microsoft.PctChg.1 Nordstrom
    ## 1 4/1/2010 20.12        35.18     37.34            1186.69        NA
    ## 2 5/3/2010 18.76       -6.76%     34.61             -1.62%     36.05
    ## 3 6/1/2010 18.67       -0.48%     33.89             -2.08%     29.23
    ## 4 7/1/2010 20.37        9.11%     34.66              2.27%     30.88
    ## 5 8/2/2010 21.23        4.22%     32.76             -5.48%     26.44
    ## 6 9/1/2010 22.46        5.79%     38.24             16.73%     34.01
    ##   Nordstrom.PctChg.1  SP_500 SP_500.PctChg.1
    ## 1                         NA                
    ## 2             -3.45% 1089.41          -8.20%
    ## 3            -18.92% 1030.71          -5.39%
    ## 4              5.64% 1101.60           6.88%
    ## 5            -14.38% 1049.33          -4.74%
    ## 6             28.63% 1141.20           8.76%

``` r
str(stock)
```

    ## 'data.frame':    60 obs. of  9 variables:
    ##  $ Date              : chr  "4/1/2010" "5/3/2010" "6/1/2010" "7/1/2010" ...
    ##  $ ATT               : num  20.1 18.8 18.7 20.4 21.2 ...
    ##  $ ATT.PctChg.1      : chr  "35.18" "-6.76%" "-0.48%" "9.11%" ...
    ##  $ Microsoft         : num  37.3 34.6 33.9 34.7 32.8 ...
    ##  $ Microsoft.PctChg.1: chr  "1186.69" "-1.62%" "-2.08%" "2.27%" ...
    ##  $ Nordstrom         : num  NA 36 29.2 30.9 26.4 ...
    ##  $ Nordstrom.PctChg.1: chr  "" "-3.45%" "-18.92%" "5.64%" ...
    ##  $ SP_500            : num  NA 1089 1031 1102 1049 ...
    ##  $ SP_500.PctChg.1   : chr  "" "-8.20%" "-5.39%" "6.88%" ...

The predictor for our simple linear regression model will be Date and
the response variable will be SP\_500.

We will visualize the relationship among date and SP\_500

Firstly, the Date is “chr” data type so we will change it to “Date” data
type.

``` r
stock$Date <- as.Date(stock$Date, format = "%m/%d/%Y")

ggplot(stock, aes(Date, SP_500)) + geom_point()
```

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Title456_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Now we will find the difference between months, setting the first date
as the beginning of our timeline, get elapsed time as our x - axis.

``` r
begin <- stock$Date[1]

stock$diff_month <- sapply(stock$Date, FUN = function(x){interval(begin,x)/months(1)})

ggplot(stock, aes(diff_month, SP_500)) + geom_point() + stat_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Title456_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Analysis

Now, we will clean our data by checking if there are missing values in
both predictors and responses.

``` r
summary(stock$SP_500)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1031    1301    1416    1532    1827    2104       1

``` r
which(is.na(stock$SP_500))
```

    ## [1] 1

``` r
sum(is.na(stock$diff_month))
```

    ## [1] 0

Hence, there is only one missing value in our response SP\_500 which is
from the first observation

Next, we will divide our dataset to build model and predictions into 75%
for training and 25% for testing. First, we will exclude the missing
observation and bring it to our testing set.

``` r
set.seed(1)
inTrain <- createDataPartition(y = stock[-1, ]$SP_500, p = 0.75, list = FALSE)

training <- stock[inTrain, ]

testing <- stock[-inTrain,]
```

## Model evaluation

Now we will use linear regression for our training set to build our
model for prediction

``` r
train.fit <- lm(SP_500 ~ diff_month, data = training)
summary(train.fit)
```

    ## 
    ## Call:
    ## lm(formula = SP_500 ~ diff_month, data = training)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -158.81  -67.04   10.59   66.85  139.13 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 983.3662    23.8370   41.25   <2e-16 ***
    ## diff_month   18.0510     0.7061   25.57   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 79.73 on 44 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9369, Adjusted R-squared:  0.9355 
    ## F-statistic: 653.6 on 1 and 44 DF,  p-value: < 2.2e-16

``` r
plot(stock$diff_month, stock$SP_500)
abline(train.fit)
```

![](Title456_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
summary(train.fit)
```

    ## 
    ## Call:
    ## lm(formula = SP_500 ~ diff_month, data = training)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -158.81  -67.04   10.59   66.85  139.13 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 983.3662    23.8370   41.25   <2e-16 ***
    ## diff_month   18.0510     0.7061   25.57   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 79.73 on 44 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9369, Adjusted R-squared:  0.9355 
    ## F-statistic: 653.6 on 1 and 44 DF,  p-value: < 2.2e-16

``` r
par(mfrow=c(2,2))
plot(train.fit)
```

![](Title456_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
par(mfrow=c(1,1))
```

From our summary fitting model, the coefficient is significant since
both values have p-value much less than 0.05. The residual standard
error is 79.73, R-Squared is 0.9369, meaning that our model fits the
training set. Also, its F-statistic is 653.6, indicating that we can
accept that our model fits the data better than the model with no
independent variables The residuals follow are fairly normal
distributed. Here, since no point falls outside of Cook’s distance (the
red dashed lines) then no point is considered to be influential
observation.

Now we will use this model to predict the testing set.

``` r
fit.test <- predict(train.fit, testing)

plot(testing$SP_500, fit.test)
```

![](Title456_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Here is the actual response SP\_500 vs the fitted value SP\_500

``` r
{plot(testing$diff_month,testing$SP_500)
abline(train.fit)
}
```

![](Title456_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Reference

Link to the dataset
[Link](https://regressit.com/Stock_returns_with_analysis.xlsx)
