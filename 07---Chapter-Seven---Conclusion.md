07 - Chapter Seven - Conclusion
================
Nima Niarad
7/24/2021

<style> body {text-align: justify} </style>

<!-- Justify text. -->

### Conclusion

**Logistic Regression, BSS, & Forward and Backward Stepwise Selections**

They are all like an alternative to least squares method but yield
better prediction accuracy. One of worst error rate that I got in this
project is from logistic regression method.

According to the cp, RSS, and adjusted R squared in Best Subset
Selection (BSS), I should include 15 variables in the model but I know
as the number of predictors goes up, the lower error rate would
decrease. So, I cannot just trust this and go for 15 variables.

There is a same result from Forward and Backward Stepwise Selections.
That is why I used ggplot to have some graphs to remove the variables
that I believe they cannot impact the response. “sex” is the main one
that I can definitely remove from our model.

I cannot get a good result out of “failures” plot to include it in the
model or not. It needs to be investigated more by other methods.

I can consider these variables in the model: “Mjob”,” Mjob”, “failures”,
“studytime”, “school”, “higher”, “romantic”, “freetime”, “gout”,
“health”, and “absences”.

**SVM (Support Vector Machine)**

It is a supervised method that can work well in binary and
classification situations. However, it has some pros and cons. The
margin among classed should be clear if I would like to have a good
result. If there are many overlapping among them, SVM can make it even
worse. It cannot perform well on the large data sets.

First, I applied support vector classifier which appears to work on the
data better than the other two ones that are non-linear.

When I use all predictors, I found it hard to compute, so that is why I
got help from last chapter and used the only ones that can have an
effect on the response. It also turned out that I were right about
improving the test error rate by reducing the number of variables.
However, there is a 32 percent of misclassification that I would like to
improve for sure.

**Decision Tree**

Regarding the using variables that have a greater impact on the
response, I can say I were almost right from the beginning. Bagging and
Random Forest suggestion are: “failures”, “absences”, “goout”,
“freetime”, “Mjob”, “age”, and “Health”. DT added some more: “freetime”,
“studytime”, “Walc”, and “Health”. Bagging and DT can predict about 71
and 72 percent of data correctly.

**The Best Model**

The best model belongs to DT as it has the lowest test error (29
percent) which is very important in selecting the best approach to use
for analyzing and predictions and more importantly trusting the model.
So, the non-linear methods which is SVM (Radial and Polynomial Kernel)
are outperformed by Decision Tree approach.

**DT: Explaining the Predictors and Response Relationships**

![image
info](C:/Nima/Rstudio/Git/Student%20Performance,%20Secondary%20Schools/Student-Performance--Secondary-Schools/06---Chapter-Six---Decision-Tree_files/figure-gfm/unnamed-chunk-8-1.png)

Students who experienced one or more failures in their past are more
likely to get failed if they are absent in more than 13 cases. However,
there are some students who even do not have a single absent but they
could not pass the final exam.

The majority of students who do not have any absent and manage their
free time to study greater than 1.5 hour per week and not too many times
hanging with their friends after school are more likely to pass the
final exams if they have a healthy lifestyle.

The interesting result is the ones who do not have a healthy life and
cannot manage their time properly and spend a lot of it with their
friends studied more than the average student in a week! They are
probably going to fail in their final exam. I do not have a data
regarding this, but the main reason could be lack of enough
concentrations.

Like always, naggging to have more data but it is beyond the scope of
this project. However, bringing this analysis may help future researches
to collect data in a more efficient way.

**Further and Beyond**

Psychologically, the human mind, like muscles, needs constant training.
If a lot of time is devoted to having fun, the human mind thinks that
the main task of a person is to have a time of joy and happiness and
avoids focusing on more difficult tasks. For this reason, the human mind
gradually thinks that studying is useless and that it should do the
right thing, which is having a good time. At the end, the person can
lose their concentrations in many important occasions.
