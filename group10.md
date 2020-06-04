# SWA-2020
if(require("pacman")){
  library(pacman)
}else{
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly, tidyverse, reticulate, UsingR, Rmpfr,
               swirl, corrplot, gridExtra, mise, latex2exp, tree, rpart, lattice,
               coin, primes, epitools, maps, clipr, ggmap, twitteR, ROAuth,
               tm, rtweet, base64enc, httpuv, SnowballC, RColorBrewer, wordcloud, ggwordcloud)

#1/Use rtweet library to search and download 1000 tweets written in English mention your Company's name. Ignore retweets while searching.
n <- 1000
tweets.company <- search_tweets(q = 'Bethesda', n = n, token = tk,
                                include_rts = FALSE)
save(tweets.company, file = "~/Documents/WSU2020/Social web/Download_1.Rdata")

#2/Find the follower count (save in variable x) and friend count (save in variable y) of each twitter user in (1)
class(tweets.company)

names(tweets.company)
class(tweets.company$followers_count)
library(tidyverse)
name  <- tweets.company$name
users <- unique(name)


x <- tweets.company$followers_count
(x     <- x[!duplicated(name)]) %>% length()
y <- tweets.company$friends_count
(y     <- y[!duplicated(name)]) %>% length()



#3/Calculate the average followers_count ( save as xbar) and the average friends_count (save as. ybar) of your sample

#find the average of followers count
xbar <- mean(x)
xbar

#find the average of friends count
ybar <- mean(y)
ybar

#4/ greater than average

#which follwers count are greater than average 
px_hat <- x[which(x > mean(x))]
px_hat

#which friends count are greater than average 
py_hat <- y[which(y> mean(y))]
py_hat

#5 

#follers count
#5a
sample.x = sample(x,1000, replace = TRUE)
sample.x
#5b
hist(sample.x)
#5c
x.lower = quantile(sample.x,0.015)
x.upper = quantile(sample.x, 0.985)
print (c(x.lower, x.upper))

#friends count
#5a
sample.y = sample(y,1000, replace = TRUE)
sample.y
#5b
hist(sample.y)
#5d
y.lower = quantile(sample.y,0.015)
y.upper = quantile(sample.y, 0.985)
print (c(y.lower, y.upper))

#6) Use a 97% confidence to estimate the proportion of the users in the population who have
#higher friends_count than the average count


y97 = y[which(y>y.lower & y<y.upper)]
y97

#which are higer in 97%friends count higer than average
y97.greateraverage = y97[which(y97<ybar)]
y97.greateraverage

#7
#(a&b)
#arrange specific follower count data into specific variable 
x.tens <- nrow(as.matrix(c(x[which(x<100)])))
x.thousands <- nrow(as.matrix(c(x[which(x>=1000 & x<2000)])))
x.twothousands = nrow(as.matrix(c(x[which(x>=2000 & x<3000)])))                    
x.threethousands = nrow(as.matrix(c(x[which(x>=3000 & x<4000)])))
x.fourthousands = nrow(as.matrix(c(x[which(x>=4000 & x<5000)])))
x.thousandsOrMore = nrow(as.matrix(c(x[which(x>5000)])))

#arrange specific friends count data into specific variable 
y.tens <- nrow(as.matrix(c(y[which(y<100)])))
y.thousands <- nrow(as.matrix(c(y[which(y>=1000 & y<2000)])))
y.twothousands = nrow(as.matrix(c(y[which(y>=2000 & y<3000)])))                    
y.threethousands = nrow(as.matrix(c(y[which(y>=3000 & y<4000)])))
y.fourthousands = nrow(as.matrix(c(y[which(y>=4000 & y<5000)])))
y.thousandsOrMore = nrow(as.matrix(c(y[which(y>5000)])))

#create a matrix
fre.m <- matrix(ncol = 6 ,nrow =2)
rownames(fre.m) <- c("follower_count","friend_count")
colnames(fre.m) <- c("tens","thousands","twothousands","threethousands","fourthousands","fivethousandOrMore" )

#insert follower count variable into the matrix 
fre.m[1,1] = x.tens
fre.m[1,2] = x.thousands
fre.m[1,3] = x.twothousands
fre.m[1,4] = x.threethousands
fre.m[1,5] = x.fourthousands
fre.m[1,6] = x.thousandsOrMore

#insert friends count variable into the matrix 
fre.m[2,1] <- y.tens
fre.m[2,2] <- y.thousands
fre.m[2,3] <- y.twothousands
fre.m[2,4] <- y.threethousands
fre.m[2,5] <- y.fourthousands
fre.m[2,6] <- y.thousandsOrMore

fre.m


#(c) independence test

expectedIndependent = function(fre.m) {
  n = sum(fre.m)                              
  n
  p = rowSums(fre.m)/sum(fre.m)                
  print(p)
  q = colSums(fre.m)/sum(fre.m)                  
  yy <-p%o%q            
  zz<-yy * n            
  return (zz)
}
E = expectedIndependent(fre.m) 

#(d) 
#This is the end of result
E

