#Completely Randomized Design

# Example
# A fast food franchise is test marketing 3 new menu items. To find out if they the same popularity, 18 franchisee restaurants are randomly chosen for participation in the study. In accordance with the completely randomized design, 6 of the restaurants are randomly chosen to test market the first new menu item, another 6 for the second menu item, and the remaining 6 for the last menu item.
# 
# Problem
# Suppose the following table represents the sales figures of the 3 new menu items in the 18 restaurants after a week of test marketing. At .05 level of significance, test whether the mean sales volume for the 3 new menu items are all equal.

df1 = read.table("fastfood-1.txt", header=TRUE); df1 

r = c(t(as.matrix(df1))) # response data 

f = c("Item1", "Item2", "Item3")   # treatment levels 
k = 3                    # number of treatment levels 
n = 6                    # observations per treatment

tm = gl(k, 1, n*k, factor(f))   # matching treatments 
tm 

av = aov(r ~ tm)

summary(av) 


#Randomized Block Design

# Example
# A fast food franchise is test marketing 3 new menu items. To find out if they have the same popularity, 6 franchisee restaurants are randomly chosen for participation in the study. In accordance with the randomized block design, each restaurant will be test marketing all 3 new menu items. Furthermore, a restaurant will test market only one menu item per week, and it takes 3 weeks to test market all menu items. The testing order of the menu items for each restaurant is randomly assigned as well.
# 
# Problem
# Suppose each row in the following table represents the sales figures of the 3 new menu in a restaurant after a week of test marketing. At .05 level of significance, test whether the mean sales volume for the 3 new menu items are all equal.


df2 = read.table("fastfood-2.txt", header=TRUE); df2 

r = c(t(as.matrix(df2))) # response data 
r


f = c("Item1", "Item2", "Item3")   # treatment levels 
k = 3                    # number of treatment levels 
n = 6                    # number of control blocks

tm = gl(k, 1, n*k, factor(f))   # matching treatment 
tm

blk = gl(n, k, k*n)             # blocking factor 
blk
av = aov(r ~ tm + blk)

summary(av) 


#Factorial Design

# Example
# A fast food franchise is test marketing 3 new menu items in both East and West Coasts of continental United States. To find out if they the same popularity, 12 franchisee restaurants from each Coast are randomly chosen for participation in the study. In accordance with the factorial design, within the 12 restaurants from East Coast, 4 are randomly chosen to test market the first new menu item, another 4 for the second menu item, and the remaining 4 for the last menu item. The 12 restaurants from the West Coast are arranged likewise.
# 
# Problem
# Suppose the following tables represent the sales figures of the 3 new menu items after a week of test marketing. Each row in the upper table represents the sales figures of 3 different East Coast restaurants. The lower half represents West Coast restaurants. At .05 level of significance, test whether the mean sales volume for the new menu items are all equal. Decide also whether the mean sales volume of the two coastal regions differs.
# 

df3 = read.csv("fastfood-3.csv")

r = c(t(as.matrix(df3))) # response data 
r 

f1 = c("Item1", "Item2", "Item3") # 1st factor levels 
f2 = c("East", "West")            # 2nd factor levels 
k1 = length(f1)          # number of 1st factors 
k2 = length(f2)          # number of 2nd factors 
n = 4                    # observations per treatment

tm1 = gl(k1, 1, n*k1*k2, factor(f1)) 
tm1 


tm2 = gl(k2, n*k1, n*k1*k2, factor(f2)) 
tm2 


av = aov(r ~ tm1 * tm2)  # include interaction

summary(av)




# Answer
# Since the p-value of 0.0015 for the menu items is less than the .05 significance level, we reject the null hypothesis that the mean sales volume of the new menu items are all equal. Moreover, the p-value of 1.2e-05 for the east-west coasts comparison is also less than the .05 significance level. It shows there is a difference in overall sales volume between the coasts. Finally, the last p-value of 0.0113 (< 0.05) indicates that there is a possible interaction between the menu item and coast location factors, i.e., customers from different coastal regions have different tastes.