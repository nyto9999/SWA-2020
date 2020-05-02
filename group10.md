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

# Set up Tokens ===========================================================

options(RCurlOptions = list(
  verbose = FALSE, 
  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), 
  ssl.verifypeer = FALSE))

setup_twitter_oauth(
  consumer_key = "JlZd7ZVFiKREFJLvRpl1wmgMm",
  consumer_secret = "e0hFsUmCf84ISyZ9tZzdi78e9pQCnJzXWZmNghPR6JDP8TbpNx",
  access_token = "1251344379186262016-QWjDYXV75enCF3J2Qvi3yZG8pux0Qi",
  access_secret = "mSTOVQfA1S1mALIaWB4LYjLrR3EgYYFITu70Ny6SFNcgC")

# rtweet ==================================================================
tk <-    rtweet::create_token(
  app = "nhiphamtwitter_reasearch",
  consumer_key    = "JlZd7ZVFiKREFJLvRpl1wmgMm",
  consumer_secret = "e0hFsUmCf84ISyZ9tZzdi78e9pQCnJzXWZmNghPR6JDP8TbpNx",
  access_token    = "1251344379186262016-QWjDYXV75enCF3J2Qvi3yZG8pux0Qi",
  access_secret   = "mSTOVQfA1S1mALIaWB4LYjLrR3EgYYFITu70Ny6SFNcgC",
  set_renv        = FALSE)
#1/Use rtweet library to search and download 1000 tweets written in English mention your Company’s name. Ignore retweets while searching.
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


#3/Calculate the average followers_count ( save as xbar) and the average friends_count (save as. ybar) of your sample
