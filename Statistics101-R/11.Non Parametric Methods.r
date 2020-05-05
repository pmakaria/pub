#Non-parametric Methods

#Sign Test
# 
# A sign test is used to decide whether a binomial distribution has the equal chance of success and failure.
# 
# Example
# A soft drink company has invented a new drink, and would like to find out if it will be as popular as the existing favorite drink. For this purpose, its research department arranges 18 participants for taste testing. Each participant tries both drinks in random order before giving his or her opinion.
# 
# Problem
# It turns out that 5 of the participants like the new drink better, and the rest prefer the old one. At .05 significance level, can we reject the notion that the two drinks are equally popular?

binom.test(5, 18) 



#Wilcoxon Signed-Rank Test

library(MASS)
head(immer)
wilcox.test(immer$Y1, immer$Y2, paired=TRUE) 


#Mann-Whitney-Wilcoxon Test

mtcars$mpg 
mtcars$am 
wilcox.test(mpg ~ am, data=mtcars) 


#Kruskal-Wallis Test

head(airquality) 
kruskal.test(Ozone ~ Month, data = airquality) 




