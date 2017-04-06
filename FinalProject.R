library(twitteR)
library(RCurl)
##Check other PC for extra packages###
library("tseries")
library(twitteR)
setup_twitter_oauth("k2VRd8b9WOwP3bCPnzOJhY5BJ","zsfQh55pU4MwjDk9pcwuHu4dFrGpzWu9Yzr7nY3jvkkniIZsUN","793230364718215168-kYebJPAsXvcoW94PE8aS4rdj43ex5zJ","zYCgHZcuOYqmggErfm4Niy6fikCHzBwqkE78Wy5BSBfV3")
library("tseries")
snpxom<-get.hist.quote('XOM',start="2017-03-30",end="2017-04-05",quote="Close")
length(snpxom)
snprexom<-log(lag(snpxom))-log(snpxom)
length(snprexom)
snpvolxom<-sd(snprexom)*sqrt(250)*100
snpvolxom
volxom<-function(d,logrets)
{
  var=0
  lam=0
  varlist<-c()
  for(r in logrets) {
    lam=lam*(1-1/d)+1
    var=(1-1/lam)*var+(1/lam)*r^2
    varlist<-c(varlist,var)
  }
  sqrt(varlist)}
volesxom<-volxom(10,snprexom)
volesxom2<-volxom(30,snprexom)
volesxom3<-volxom(100,snprexom)
plot(volesxom,type="l")
lines(volesxom2,type="l",col="red")
lines(volesxom3,type="l",col="blue")
#Twitter Portion
searchTwitter("xom",n=100,lang="en",since="2017-03-30",until="2017-04-05")
