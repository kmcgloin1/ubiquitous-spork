# imports data
mydata = read.csv("halal.csv")

# make sure the data looks correct
head(mydata)

# attach data
attach(mydata)

Mouslim <- factor(Mouslim)
levels(Mouslim) <- c("low", "medium", "high")
tapply(Price, Mouslim, mean)
tapply(Price, Mouslim, sd)

# scatterplot of the population of Muslims in a certain country versus the production price of Halal
plot(Mouslim, Price, main="Muslim Population vs. Production Price", 
     xlab="Population of Muslims in Country", ylab="Production Price")

# create single vectors for different countries
thailand <- c(1145.1, 1040.2, 1074.3, 119.6, 1588.2, 1671.1, 1396.2, 1227.2, 1200.4, 1036.9, 1051.4, 968.8, 1030, 1118.6, 1417.5, 1323, 1548.9, 1770.1, 1766.6)

malaysia <- c(1625.4, 1722.6, 1874.4, 1778.8, 2131.4, 1351.4, 1235.3, 1029, 1052.6, 1019.7, 1163.2, 1102.6, 1181.6, 1186.9, 1359.9, 1382.4, 1695.5, 1793.7, 1586.6)

indonesia <- c(1826.6, 1832.2, 1681.8, 1807.2, 1717.1, 2006.2, 1614.1, 773.5, 1512.8, 1335.2, 1180.6, 1431.1, 1566.1, 1593.7, 2163, 2455.8, 2607.2, 313.5, 3689.4)

# check to see if data set follows a normal distribution using a normal probability plot
par(mfrow=c(3,1))
qqnorm(thailand)
qqnorm(malaysia)
qqnorm(indonesia)


# Note: for the most part our data appears to be normal aside from a few points indicating that we have fat tails

# We will perform the Anderson-Darling normality test to check for normality in our tails

# import package of normality tests
require(nortest)

# perform Anderson-Darling normality test
ad.test(thailand)
ad.test(malaysia)
ad.test(indonesia)

# Since all of the p-values are greater than our statistical significance level, \alpha = 0.5, we can assume 
# that our data is normally distributed. 

# perform ANOVA test
model <- lm(Price ~ Mouslim)
a1 <- anova(model)
# Our p-value in our anova one-way test indicates that the country is significant, but we do not know which pairs of 
# Mouslim levels are significantly different from each other.

# This will require three tests: Thailand vs. Malaysia, Thailand vs. Indonesia, and Malaysia vs. Indonesia. 

# perform pairwise t-tests
pairwise.t.test(Price, Mouslim, p.adj = "none")

pairwise.t.test(Price, Mouslim, p.adj = "bonf")

pairwise.t.test(Price, Mouslim, p.adj = "holm")

# These adjustments indicate that the Thailand-Indonesia pair is significantly different at alpha = 0.5.

# perform Tukey's HSD test
aov1 <- aov(Price ~ Mouslim)
tukey.test <- TukeyHSD(aov1)
plot(tukey.test)

# Similar to our pairwise t-tests performed above, Tukey's Honest Significant Difference test indicates that 
# there is a statistically significant difference in the average production price of Halal between Thailand and Indonesia. 


