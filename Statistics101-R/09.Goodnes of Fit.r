#Goodness of Fit


#Multinomial Goodness of Fit

library(MASS)
levels(survey$Smoke) 
smoke.freq = table(survey$Smoke) 
smoke.freq 

#Suppose the campus smoking statistics is as below. Determine whether the sample data in survey supports it at .05 significance level.

smoke.prob = c(.045, .795, .085, .075) 
chisq.test(smoke.freq, p=smoke.prob) 

#Chi-squared Test of Independence

tbl = table(survey$Smoke, survey$Exer) 
tbl 
#Test the hypothesis whether the students smoking habit is independent of their exercise level at .05 significance level.

chisq.test(tbl) 

#The warning message found in the solution above is due to the small cell values in the contingency table. To avoid such warning, we combine the second and third columns of tbl, and save it in a new table named ctbl. Then we apply the chisq.test function against ctbl instead.

ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) 
ctbl 

chisq.test(ctbl) 
