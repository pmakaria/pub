#Type II Error in Lower Tail Test of Population Mean with Known Variance

#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. 
#Assume actual mean light bulb lifetime is 9,950 hours and the population standard deviation is 120 hours. At .05 significance level, what is the probability of having type II error for a sample size of 30 light bulb?

#Calculate SEM
n = 30                # sample size 
sigma = 120           # population standard deviation 
sem = sigma/sqrt(n); sem   # standard error 

alpha = .05           # significance level 
mu0 = 10000           # hypothetical lower bound 
q = qnorm(alpha, mean=mu0, sd=sem); q 


mu = 9950             # assumed actual mean 
pnorm(q, mean=mu, sd=sem, lower.tail=FALSE) 


#Type II Error in Upper Tail Test of Population Mean with Known Variance

#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie.
#Assume the actual mean amount of saturated fat per cookie is 2.09 grams, and the population standard deviation is 0.25 grams.
#At .05 significance level, what is the probability of having type II error for a sample size of 35 cookies?

#Calculate SEM

n = 35                # sample size 
sigma = 0.25          # population standard deviation 
sem = sigma/sqrt(n); sem   # standard error 

alpha = .05           # significance level 
mu0 = 2               # hypothetical upper bound 
q = qnorm(alpha, mean=mu0, sd=sem, lower.tail=FALSE); q 


mu = 2.09             # assumed actual mean 
pnorm(q, mean=mu, sd=sem) 


#Type II Error in Two-Tailed Test of Population Mean with Known Variance

#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. Assume the actual mean population weight is 15.1 kg, and the population standard deviation is 2.5 kg. 
#At .05 significance level, what is the probability of having type II error for a sample size of 35 penguins?

n = 35                # sample size 
sigma = 2.5           # population standard deviation 
sem = sigma/sqrt(n); sem   # standard error 


alpha = .05           # significance level 
mu0 = 15.4            # hypothetical mean 
I = c(alpha/2, 1-alpha/2) 
q = qnorm(I, mean=mu0, sd=sem); q 

mu = 15.1             # assumed actual mean 
p = pnorm(q, mean=mu, sd=sem); p 

#Finally, the probability of type II error is the probability between the two end points.

diff(p)               # p[2]-p[1] 


#Type II Error in Lower Tail Test of Population Mean with Unknown Variance

#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours.
#Assume in a random sample of 30 light bulbs, the standard deviation of the lifetime is 125 hours. If actual mean light bulb lifetime is 9,950 hours, what is the probability of type II error for a hypothesis test at .05 significance level?

n = 30                # sample size 
s = 125               # sample standard deviation 
SE = s/sqrt(n); SE    # standard error estimate 


alpha = .05           # significance level 
mu0 = 10000           # hypothetical lower bound 
q = mu0 + qt(alpha, df=n-1) * SE; q 

mu = 9950             # assumed actual mean 
pt((q - mu)/SE, df=n-1, lower.tail=FALSE) 


#Type II Error in Upper Tail Test of Population Mean with Unknown Variance

#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. Assume in a random sample of 35 cookies, the standard deviation of saturated fat is 0.3 grams. 
#If actual mean amount of saturated fat per cookie is 2.09 grams, what is the probability of type II error for a hypothesis test at .05 significance level?

n = 35                # sample size 
s = 0.3               # sample standard deviation 
SE = s/sqrt(n); SE    # standard error estimate 


alpha = .05           # significance level 
mu0 = 2               # hypothetical upper bound 
q = mu0 + qt(alpha, df=n-1, lower.tail=FALSE) * SE; q 



mu = 2.09             # assumed actual mean 
pt((q - mu)/SE, df=n-1) 


#Type II Error in Two-Tailed Test of Population Mean with Unknown Variance

#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. Assume in a random sample 35 penguins, the standard deviation of the weight is 2.5 kg. 
#If actual mean penguin weight is 15.1 kg, what is the probability of type II error for a hypothesis test at .05 significance level?


n = 35                # sample size 
s = 2.5               # sample standard deviation 
SE = s/sqrt(n); SE    # standard error estimate 

alpha = .05           # significance level 
mu0 = 15.4            # hypothetical mean 
I = c(alpha/2, 1-alpha/2) 
q = mu0 + qt(I, df=n-1) * SE; q 

mu = 15.1             # assumed actual mean 
p = pt((q - mu)/SE, df=n-1); p 
diff(p)  
