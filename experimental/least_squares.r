library(ggplot2)
library(reshape2)

set.seed(1)

# Explore the influence of distribution difference on lm results
a<-rnorm(10000, mean = 0, sd = 1)
b<-0.01*a + runif(10000, min = -1.5, max = 1.5)
c<-0.01*a + runif(10000, min = -0.5, max = 0.5)  # changing the spread clearly changes the p value
d<-data.frame(a,b,c)
d.melt<-melt(d)

ggplot(d.melt, aes(x=value, fill=variable))+geom_density()

ggplot(d) +
  geom_point(aes(x=b, y=a, col="red")) + geom_smooth(aes(x=b, y=a, col="red"), method = "lm") +
  geom_point(aes(x=c, y=a, col="green")) + geom_smooth(aes(x=c, y=a, col="green"), method = "lm") +
  scale_colour_discrete(name="variable", labels=c("c", "b")) +
  theme(axis.title = element_blank())

mb<-lm(a~b, data=d)
mc<-lm(a~c, data=d)

summary(mb)
summary(mc)

