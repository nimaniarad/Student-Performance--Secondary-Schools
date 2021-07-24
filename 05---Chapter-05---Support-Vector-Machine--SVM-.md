05 - Chapter 05 - Support Vector Machine (SVM)
================
Nima Niarad
7/24/2021

<style> body {text-align: justify} </style>

<!-- Justify text. -->

### **Loading the Data…**

``` r
library(readr)
MStud=read.table("C:/Nima/Rstudio/Git/Student Performance, Secondary Schools/Student-Performance--Secondary-Schools/student-mat.csv",sep=";",header=TRUE)
```

### **Introduction**

**SVM; Support Vector Machine** It is a new supervised method used for
classification data. SVMs are applied for the binary classification with
two or more classes.

So, first diveding the G3 into two groups; Fail: Below 10, Pass: Above
10.

``` r
MStud$G3= ifelse(MStud$G3>=10 ,"Pass","Fail")

MStud$G3=as.factor(MStud$G3)
```

**Making Test and Train Dataset**

``` r
set.seed(1)
train = sample(1:nrow(MStud), nrow(MStud)/2)
MStud.test = MStud[-train, ]
```

``` r
library(e1071)
set.seed (1)
MStud = na.omit(MStud)
svmfit = svm(G3 ~ +school+sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+guardian+traveltime+
  studytime+failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+
  freetime+goout+Dalc+Walc+health+absences-G1-G2, 
  data = MStud,subset = train, cost = 1,kernel="linear", scale=T)
summary(svmfit)
```

    ## 
    ## Call:
    ## svm(formula = G3 ~ +school + sex + age + address + famsize + Pstatus + 
    ##     Medu + Fedu + Mjob + Fjob + reason + guardian + traveltime + 
    ##     studytime + failures + schoolsup + famsup + paid + activities + 
    ##     nursery + higher + internet + romantic + famrel + freetime + 
    ##     goout + Dalc + Walc + health + absences - G1 - G2, data = MStud, 
    ##     cost = 1, kernel = "linear", subset = train, scale = T)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  linear 
    ##        cost:  1 
    ## 
    ## Number of Support Vectors:  106
    ## 
    ##  ( 59 47 )
    ## 
    ## 
    ## Number of Classes:  2 
    ## 
    ## Levels: 
    ##  Fail Pass

-   Performing on the Training Set: There are 125 support vectors; 69 on
    one side and 56 on the other.

**The Test Error Rate**

``` r
ypred = predict(svmfit , MStud.test)
table(MStud.test$G3 , ypred)
```

    ##       ypred
    ##        Fail Pass
    ##   Fail   20   54
    ##   Pass   14  110

``` r
print("Test Error Rate"); print((14+54)/(20+54+110+14))
```

    ## [1] "Test Error Rate"

    ## [1] 0.3434343

About 34 percent of test observations are misclassified by this SVM!

Given that knowing the important variables, lets try and see to reduce
the test error rate.

``` r
svmfit = svm(G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2, 
  data = MStud,subset = train, cost = 1,kernel="linear", scale=T)

ypred = predict(svmfit , MStud.test)
table(MStud.test$G3 , ypred)
```

    ##       ypred
    ##        Fail Pass
    ##   Fail   16   58
    ##   Pass    6  118

``` r
print("Test Error Rate"); print((6+58)/(16+58+6+118))
```

    ## [1] "Test Error Rate"

    ## [1] 0.3232323

The test error rate is improving a little bit by about 3 percent.

-   Identifying the Optimal cost

``` r
set.seed (1)
tune.out = tune(svm ,G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2, 
                data = MStud, kernel ="linear",scale = T,
                ranges =list (cost=c(0.001 , 0.01 , 0.1, 1 ,5 ,10 ,100)))
summary(tune.out)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  cost
    ##    10
    ## 
    ## - best performance: 0.2785256 
    ## 
    ## - Detailed performance results:
    ##    cost     error dispersion
    ## 1 1e-03 0.3291667 0.07701932
    ## 2 1e-02 0.2810897 0.08555725
    ## 3 1e-01 0.2861538 0.09474572
    ## 4 1e+00 0.2937179 0.08939329
    ## 5 5e+00 0.2913462 0.07663065
    ## 6 1e+01 0.2785256 0.07515796
    ## 7 1e+02 0.2887821 0.07716797

``` r
svmfit = svm(G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2,
             data = MStud, subset = train, kernel="linear", 
             cost = 10  , scale=T)

ypred = predict(svmfit , MStud.test)
table(MStud.test$G3 , ypred)
```

    ##       ypred
    ##        Fail Pass
    ##   Fail   15   59
    ##   Pass    8  116

``` r
print("Test Error Rate"); print((59+8)/(15+59+8+116))
```

    ## [1] "Test Error Rate"

    ## [1] 0.3383838

It is the time to go for radial and polynomial kernels.

**Radial**

``` r
tune.out = tune(svm ,G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2, 
                data = MStud, kernel ="radial",scale = T,
                ranges =list (cost=c(0.001 , 0.01 , 0.1, 1 ,5 ,10 ,100)))
summary(tune.out)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  cost
    ##     1
    ## 
    ## - best performance: 0.2882051 
    ## 
    ## - Detailed performance results:
    ##    cost     error dispersion
    ## 1 1e-03 0.3290385 0.08238284
    ## 2 1e-02 0.3290385 0.08238284
    ## 3 1e-01 0.3264744 0.08214029
    ## 4 1e+00 0.2882051 0.07394657
    ## 5 5e+00 0.3033974 0.08156129
    ## 6 1e+01 0.3159615 0.07590221
    ## 7 1e+02 0.3920513 0.05685945

``` r
svmfit = svm(G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2,
             data = MStud, subset = train, kernel="radial", 
             cost = 1, scale=T)

ypred = predict(svmfit , MStud.test)
table(MStud.test$G3 , ypred)
```

    ##       ypred
    ##        Fail Pass
    ##   Fail   13   61
    ##   Pass    6  118

``` r
print("Radial - Test Error Rate"); print((6+61)/(13+61+6+118))
```

    ## [1] "Radial - Test Error Rate"

    ## [1] 0.3383838

It is almost the same!

**Polynomial Kernel**

``` r
tune.out = tune(svm ,G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2, 
                data = MStud, kernel="polynomial", d = 2, scale = T,
                ranges =list (cost=c(0.001 , 0.01 , 0.1, 1 ,5 ,10 ,100)))
summary(tune.out)
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: 10-fold cross validation 
    ## 
    ## - best parameters:
    ##  cost
    ##     1
    ## 
    ## - best performance: 0.2813462 
    ## 
    ## - Detailed performance results:
    ##    cost     error dispersion
    ## 1 1e-03 0.3298077 0.09912469
    ## 2 1e-02 0.3298077 0.09912469
    ## 3 1e-01 0.3119872 0.07996653
    ## 4 1e+00 0.2813462 0.06299937
    ## 5 5e+00 0.2864103 0.06930973
    ## 6 1e+01 0.2889744 0.06747253
    ## 7 1e+02 0.3014103 0.05748125

``` r
svmfit = svm(G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2,
             data = MStud, subset = train, kernel="polynomial", d = 2, 
             cost = 5, scale=T)

ypred = predict(svmfit , MStud.test)
table(MStud.test$G3 , ypred)
```

    ##       ypred
    ##        Fail Pass
    ##   Fail   14   60
    ##   Pass    4  120

``` r
print("Radial - Test Error Rate"); print((4+60)/(14+60+4+120))
```

    ## [1] "Radial - Test Error Rate"

    ## [1] 0.3232323

It is almost the same as the linear kernel.

Overall, the support vector classifier (Linear Kernel) is working better
than the other.

### **Multiple Classes**

Dividing the G3 into three classes: Failed students (Below 10), Pass
students(Greater than equals 10) and Top ones (greater than 15).

``` r
library(e1071)

MStud=read.table("C:/Nima/Data/student-mat.csv",sep=";",header=TRUE)
attach(MStud)
set.seed(2)

train = sample(1:nrow(MStud), nrow(MStud)/2)
MStud.test = MStud[-train, ]

MStud$G3 = as.factor(ifelse(G3>15, 2, ifelse(G3<10 & G3>10,0, 1)))
table(MStud$G3.level) 
```

    ## < table of extent 0 >

``` r
svmfit.radial.2 = svm(G3 ~ Medu+Mjob+studytime+failures+famsup+romantic+goout+absences-G1-G2
                      ,data = MStud, kernel = "radial", gamma=1, cost=100)

ypred = predict(svmfit.radial.2 , MStud.test)
table(MStud.test$G3 , ypred)
```

    ##     ypred
    ##       1  2
    ##   0  15  0
    ##   5   5  0
    ##   6   6  0
    ##   7   7  0
    ##   8  19  0
    ##   9  12  0
    ##   10 27  0
    ##   11 22  0
    ##   12 11  0
    ##   13 16  0
    ##   14 13  0
    ##   15 21  0
    ##   16  1  8
    ##   17  0  4
    ##   18  0  7
    ##   19  0  3
    ##   20  0  1

``` r
print("Test Error Rate"); print((1+12+19+7+6+5+15)/(15+5+6+7+19+12+27+22+11+16+13+21+1+8+4+7+3+1))
```

    ## [1] "Test Error Rate"

    ## [1] 0.3282828
