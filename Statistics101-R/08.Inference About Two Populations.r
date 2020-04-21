#Inference About Two Populations

#Population Mean Between Two Matched Samples

#Assuming that the data in immer follows the normal distribution, find the 95% confidence interval estimate of the difference between the mean barley yields between years 1931 and 1932.
library (MASS)
head(immer)
t.test(immer$Y1, immer$Y2, paired=TRUE) 

#Population Mean Between Two Independent Samples

mtcars$mpg
mtcars$am

#Assuming that the data in mtcars follows the normal distribution, find the 95% confidence interval estimate of the difference between the mean gas mileage of manual and automatic transmissions.


L = mtcars$am == 0 
mpg.auto = mtcars[L,]$mpg 
mpg.auto      


mpg.manual = mtcars[!L,]$mpg 
mpg.manual                  # manual transmission mileage 


t.test(mpg.auto, mpg.manual) 

#Alternative Solution
#We can model the response variable mtcars$mpg by the predictor mtcars$am, and then apply the t.test function to estimate the difference of the population means.

t.test(mpg ~ am, data=mtcars) 

#Comparison of Two Population Proportions
table(quine$Eth, quine$Sex) 

#Assuming that the data in quine follows the normal distribution, find the 95% confidence interval estimate of the difference between the female proportion of Aboriginal students and the female proportion of Non-Aboriginal students, each within their own ethnic group.


prop.test(table(quine$Eth, quine$Sex), correct=FALSE) 
