#Hypothesis Testing
#Lower Tail Test of Population Mean with Known Variance

#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. Assume the population standard deviation is 120 hours. 
#At .05 significance level, can we reject the claim by the manufacturer?

xbar = 9900            # sample mean 
mu0 = 10000            # hypothesized value 
sigma = 120            # population standard deviation 
n = 30                 # sample size 
z = (xbar−mu0)/(sigma/sqrt(n)) 
z                      # test statistic 
#We then compute the critical value at .05 significance level.

alpha = .05 
z.alpha = qnorm(1−alpha) 
−z.alpha               # critical value 

#Alternative Solution
#Instead of using the critical value, we apply the pnorm function to compute the lower tail p-value of the test statistic. 
#As it turns out to be less than the .05 significance level, we reject the null hypothesis that μ ≥ 10000.

pval = pnorm(z) 
pval                   # lower tail p−value 

#Upper Tail Test of Population Mean with Known Variance
#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. 
#In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the population standard deviation is 0.25 grams. At .05 significance level, can we reject the claim on food label?

xbar = 2.1             # sample mean 
mu0 = 2                # hypothesized value 
sigma = 0.25           # population standard deviation 
n = 35                 # sample size 
z = (xbar−mu0)/(sigma/sqrt(n)) 
z                      # test statistic 

alpha = .05 
z.alpha = qnorm(1−alpha) 
z.alpha                # critical value 

#Alternative Solution
pval = pnorm(z, lower.tail=FALSE) 
pval    

#Two-Tailed Test of Population Mean with Known Variance
#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the population standard deviation is 2.5 kg. 
#At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?

xbar = 14.6            # sample mean 
mu0 = 15.4             # hypothesized value 
sigma = 2.5            # population standard deviation 
n = 35                 # sample size 
z = (xbar−mu0)/(sigma/sqrt(n)) 
z                      # test statistic 
alpha = .05 
z.half.alpha = qnorm(1−alpha/2) 
c(−z.half.alpha, z.half.alpha) 


#Alternative Solution
pval = 2 ∗ pnorm(z)    # lower tail 
pval 

#Lower Tail Test of Population Mean with Unknown Variance
#Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours. In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. Assume the sample standard deviation is 125 hours.
#At .05 significance level, can we reject the claim by the manufacturer?

xbar = 9900            # sample mean 
mu0 = 10000            # hypothesized value 
s = 125                # sample standard deviation 
n = 30                 # sample size 
t = (xbar−mu0)/(s/sqrt(n)) 
t         


alpha = .05 
t.alpha = qt(1−alpha, df=n−1) 
−t.alpha    

#Alternative Solution
pval = pt(t, df=n−1) 
pval 


#Upper Tail Test of Population Mean with Unknown Variance
#Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. In a sample of 35 cookies, it is found that the mean amount of saturated fat per cookie is 2.1 grams. 
#Assume that the sample standard deviation is 0.3 gram. At .05 significance level, can we reject the claim on food label?

xbar = 2.1             # sample mean 
mu0 = 2                # hypothesized value 
s = 0.3                # sample standard deviation 
n = 35                 # sample size 
t = (xbar−mu0)/(s/sqrt(n)) 
t                      # test statistic 

alpha = .05 
t.alpha = qt(1−alpha, df=n−1) 
t.alpha 

#Alternative Solution
pval = pt(t, df=n−1, lower.tail=FALSE) 
pval 

#Two-Tailed Test of Population Mean with Unknown Variance
#Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. In a sample of 35 penguins same time this year in the same colony, the mean penguin weight is 14.6 kg. Assume the sample standard deviation is 2.5 kg. 
#At .05 significance level, can we reject the null hypothesis that the mean penguin weight does not differ from last year?

xbar = 14.6            # sample mean 
mu0 = 15.4             # hypothesized value 
s = 2.5                # sample standard deviation 
n = 35                 # sample size 
t = (xbar−mu0)/(s/sqrt(n)) 
t            

alpha = .05 
t.half.alpha = qt(1−alpha/2, df=n−1) 
c(−t.half.alpha, t.half.alpha) 

#Alternative Solution
pval = 2 ∗ pt(t, df=n−1)  # lower tail 
pval    

#Lower Tail Test of Population Proportion

#Suppose 60% of citizens voted in last election. 85 out of 148 people in a telephone survey said that they voted in current election. 
#At 0.5 significance level, can we reject the null hypothesis that the proportion of voters in the population is above 60% this year?

pbar = 85/148          # sample proportion 
p0 = .6                # hypothesized value 
n = 148                # sample size 
z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 
z    


alpha = .05 
z.alpha = qnorm(1−alpha) 
−z.alpha               # critical value 

#Alternative Solution 1
pval = pnorm(z) 
pval                   # lower tail p−value 

#Alternative Solution 2
prop.test(85, 148, p=.6, alt="less", correct=FALSE) 

#Upper Tail Test of Population Proportion
#Suppose that 12% of apples harvested in an orchard last year was rotten. 30 out of 214 apples in a harvest sample this year turns out to be rotten.
#At .05 significance level, can we reject the null hypothesis that the proportion of rotten apples in harvest stays below 12% this year?

pbar = 30/214          # sample proportion 
p0 = .12               # hypothesized value 
n = 214                # sample size 
z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 
z   

alpha = .05 
z.alpha = qnorm(1−alpha) 
z.alpha                # critical value 
#Alternative Solution 1

pval = pnorm(z, lower.tail=FALSE) 
pval    


#Alternative Solution 2
prop.test(30, 214, p=.12, alt="greater", correct=FALSE) 


#Two-Tailed Test of Population Proportion

#Suppose a coin toss turns up 12 heads out of 20 trials. At .05 significance level, can one reject the null hypothesis that the coin toss is fair?

pbar = 12/20           # sample proportion 
p0 = .5                # hypothesized value 
n = 20                 # sample size 
z = (pbar−p0)/sqrt(p0∗(1−p0)/n) 
z                      # test statistic 

alpha = .05 
z.half.alpha = qnorm(1−alpha/2) 
c(−z.half.alpha, z.half.alpha) 

#Alternative Solution 1
pval = 2 ∗ pnorm(z, lower.tail=FALSE)  # upper tail 
pval 


#Alternative Solution 2
prop.test(12, 20, p=0.5, correct=FALSE) 
