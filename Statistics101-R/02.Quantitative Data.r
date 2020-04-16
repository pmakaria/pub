head(faithful) 
duration = faithful$eruptions 
#Frequency Distribution of Quantitative Data
range(duration) 
breaks = seq(1.5, 5.5, by=0.5)    # half-integer sequence 
breaks 
duration.cut = cut(duration, breaks, right=FALSE)  #Classify the eruption durations according to the half-unit-length sub-intervals with cut. As the intervals are to be closed on the left, and open on the right, we set the right argument as FALSE.
duration.freq = table(duration.cut)
cbind(duration.freq) 
#Histogram
hist(duration, right=FALSE)
colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
hist(duration,right=FALSE,  col=colors,  main="Old Faithful Eruptions",  xlab="Duration minutes")   

#Relative Frequency Distrribution
duration.relfreq = duration.freq / nrow(faithful)
cbind(duration.freq, duration.relfreq) 

#Cumulative Frequency Distribution

duration.cumfreq = cumsum(duration.freq)
duration.cumrelfreq =duration.cumfreq/nrow(faithful)
old = options(digits=2) 
cbind(duration.cumfreq, duration.cumrelfreq) 
options(old)




#add a starting zero element, and plot the graph.
cumfreq0 = c(0, duration.cumfreq) 
plot(breaks, cumfreq0, main="Old Faithful Eruptions", xlab="Duration minutes",ylab="Cumulative eruptions")
lines(breaks, cumfreq0)           # join the points





#Alternative Solution
#We create an interpolate function Fn with the built-in function ecdf. Then we plot Fn right away. 
#There is no need to compute the cumulative frequency distribution a priori.
Fn = ecdf(duration) 
plot(Fn,  main="Old Faithful Eruptions",  xlab="Duration minutes", ylab="Cumulative eruption proportion")

#Stem-and-Leaf Plot

stem(duration) 


#Scatter Plot

waiting = faithful$waiting         # the waiting interval 
plot(duration, waiting, xlab="Eruption duration", ylab="Time waited")              # y−axis label

#Enhanced Solution
#We can generate a linear regression model of the two variables with the lm function, and then draw a trend line with abline.
abline(lm(waiting ~ duration))
