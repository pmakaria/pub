library(MASS)  # load the MASS package 
painters

painters$School 


help(painters)

#Frequency Distribution of Qualitative Data

 school = painters$School      # the painter schools 
 school.freq = table(school)   # apply the table function
 school.freq  
 cbind(school.freq)  
 
#Relative Frequency Distribution of Qualitative Data
school.relfreq = school.freq / nrow(painters)

school.relfreq
old = options(digits=1) 
school.relfreq 

cbind(school.relfreq) 
 
options(old)
 
barplot(school.freq) 
barplot(school.relfreq) 

colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 

barplot(school.freq,col=colors)  
pie(school.freq, col=colors)  

#Category Statistics

c_school = school == "C"      # the logical index vector
c_painters = painters[c_school, ] 
mean(c_painters$Composition)
tapply(painters$Composition, painters$School, mean) 

