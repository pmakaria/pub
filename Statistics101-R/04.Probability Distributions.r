#Binomial Distribution
#The binomial distribution is a discrete probability distribution. It describes the outcome of n independent trials in an experiment. Each trial is assumed to have only two outcomes, either success or failure. 


#Suppose there are twelve multiple choice questions in an English class quiz. Each question has five possible answers, and only one of them is correct. Find the probability of having four or less correct answers if a student attempts to answer every question at random.
#Since only one out of five possible answers is correct, the probability of answering a question correctly by random is 1/5=0.2. We can find the probability of having exactly 4 correct answers by random attempts as follows.

dbinom(4, size=12, prob=0.2) 
#To find the probability of having four or less correct answers by random attempts, we apply the function dbinom with x = 0,…,4.
dbinom(0, size=12, prob=0.2) + 
  + dbinom(1, size=12, prob=0.2) + 
  + dbinom(2, size=12, prob=0.2) + 
  + dbinom(3, size=12, prob=0.2) + 
  + dbinom(4, size=12, prob=0.2) 

#Alternatively, we can use the cumulative probability function for binomial distribution pbinom.

pbinom(4, size=12, prob=0.2) 

#Poisson Distribution
#The Poisson distribution is the probability distribution of independent event occurrences in an interval.

#If there are twelve cars crossing a bridge per minute on average, find the probability of having seventeen or more cars crossing the bridge in a particular minute.

#The probability of having sixteen or less cars crossing the bridge in a particular minute is given by the function ppois.

 ppois(16, lambda=12)   # lower tail 

 #Hence the probability of having seventeen or more cars crossing the bridge in a minute is in the upper tail of the probability density function.

ppois(16, lambda=12, lower=FALSE)   # upper tail 

#Continuous Uniform Distribution
#The continuous uniform distribution is the probability distribution of random number selection from the continuous interval between a and b

#Select ten random numbers between one and three.
runif(10, min=1, max=3) 

#Exponential Distribution
#The exponential distribution describes the arrival time of a randomly recurring independent event sequence.

#Suppose the mean checkout time of a supermarket cashier is three minutes. Find the probability of a customer checkout being completed by the cashier in less than two minutes.
#The checkout processing rate is equals to one divided by the mean checkout completion time. Hence the processing rate is 1/3 checkouts per minute. We then apply the function pexp of the exponential distribution with rate=1/3.
pexp(2, rate=1/3) 

#Normal Distribution

#Assume that the test scores of a college entrance exam fits a normal distribution. Furthermore, the mean test score is 72, and the standard deviation is 15.2.
#What is the percentage of students scoring 84 or more in the exam?

#We apply the function pnorm of the normal distribution with mean 72 and standard deviation 15.2.
#Since we are looking for the percentage of students scoring higher than 84, we are interested in the upper tail of the normal distribution.

pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 

#calculate z value for 95% confidence interval, 2-tailed
qnorm(c(0.025,0.975))

#Chi-squared Distribution
#Find the 95th percentile of the Chi-Squared distribution with 7 degrees of freedom.

qchisq(.95, df=7) 

#Student t Distribution
#Find the 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom.

#We apply the quantile function qt of the Student t distribution against the decimal values 0.025 and 0.975.

qt(c(.025, .975), df=5)   # 5 degrees of freedom 

#F Distribution
#Find the 95th percentile of the F distribution with (5, 2) degrees of freedom.


#We apply the quantile function qf of the F distribution against the decimal value 0.95.

 qf(.95, df1=5, df2=2) 
 