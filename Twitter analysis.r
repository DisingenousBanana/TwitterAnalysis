
samplemode <- function(x) {
  uni <- unique(x) # find list of unique values in x
  uni[which(tabulate(match(x, uni)) == max(tabulate(match(x, uni))))] # identify mode(s)
}
## generates sample mode 

ds <- dataset20873866

personal <- subset(ds,subset=(username %in% c("@kevinolearytv","@SimuLiu","@b0rk")))

table(factor(personal$day.of.week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday")))

barplot(table(factor(personal$day.of.week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday","Friday","Saturday","Sunday"))),xlab="Day of the week",ylab="Number of Tweets",las=1,main="Number of tweets by Simu Liu by day",ylim=c(0,100),,col=c("dodgerblue3", "darkorchid2", "forestgreen", "orange","red","green","pink"),density=28,angle=c(45, 90, 135, 180,12,32,17))

table(factor(personal$media, levels = c(0, 1, 2, 3,4)))

barplot(table(factor(personal$media, levels = c(0, 1, 2, 3,4))),xlab="Number of media items",ylab="Frequency",las=1,main="Number of media per tweet",ylim=c(0,450),,col=c("dodgerblue3", "orange","red","green","pink"),density=28,angle=c(45, 90, 135, 180,12))

media.mean <- mean(personal$media) 
media.mode <- samplemode(personal$media) 
media.median <- median(personal$media) 
media.sd <- sd(personal$media)

time.of.day.hour <- personal$time.of.day/3600

time.mean <- mean(time.of.day.hour) 
time.median <- median(time.of.day.hour) 
time.mode <- samplemode(time.of.day.hour) 
time.sd <- sd(time.of.day.hour)
time.kurtosis <- kurtosis(time.of.day.hour)
time.skewness <- skewness(time.of.day.hour)

truehist(time.of.day.hour, xlab = "Hours since midnight", ylab = "Relative Frequency", main = "Relative frequency histogram of B0rk posts", ylim = c(0,0.0035), las = 1, col = "red", density = 25, angle = 45)
curve(dnorm(x, mean(time.of.day.hour), sd(time.of.day.hour)), col = "blue", add = TRUE,lwd = 3)

boxplot(time.of.day.hour[ds$day.of.week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")],time.of.day.hour[ds$day.of.week %in% c("Saturday", "Sunday")],xlab = "Day of week", ylab = "Time posted after midnight (in hours)", names = c("Weekday ","Weekend"), main = "B0rk tweet times compared (weekends and weekdays)")
_________________________________
personal <- subset(ds,subset=(username %in% c("@kevinolearytv","@SimuLiu","@b0rk")))
table(personal$media.binary)
at.least.one.media <- 176/530

summary(personal$retweets[personal$media.binary == 1])
summary(personal$retweets[personal$media.binary == 0])
## split tweets by whether or not they have any media

tweet.gap.hour <- personal$tweet.gap/3600
samplemode <- function(x) {
       uni <- unique(x) # find list of unique values in x
       uni[which(tabulate(match(x, uni)) == max(tabulate(match(x, uni))))] # identify mode(s)
       }
       
mean.tweetgap <- mean(tweet.gap.hour)
median.tweetgap <- median(tweet.gap.hour)
sd.tweetgap <- sd(tweet.gap.hour)
kurt.tweetgap <- kurtosis(tweet.gap.hour)
skew.tweetgap <- skewness(tweet.gap.hour)
## mean and median differ substantially, and skewness and kurtosis aren't near 0 and 3 respectively, so not normally distributed

truehist(tweet.gap.hour,main="Frequency of retweets",xlab="Number of retweets",ylab="Frequency of retweets",las=1,nbins=20,ylim=c(0,0.03),col=c("red","blue","green","yellow","dodgerblue3","navy","pink","purple","brown","darkgray","firebrick","forestgreen","gold","darkturquoise","khaki","indianred3","midnightblue","maroon3","plum4","salmon"),density=30,angle=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
curve(dexp(x,1/mean(tweet.gap.hour)),add=TRUE,lwd=3)

plot(ecdf(tweet.gap.hour),xlab="Time between Tweets (in hours)", ylab = "Cumilative Probability",las=1,main="ECDF of Tweet gap (hours)",pch=NA,col="indianred3",lwd=3)
curve(pexp(x,1/mean(tweet.gap.hour)),add=TRUE,lwd=3,col="midnightblue",lty=2)

tweet.gap.hour.nf <- personal$tweet.gap[personal$first.tweet != 0]/3600

mean.tweetgapnf <- mean(tweet.gap.hour.nf)
median.tweegaptnf <- median(tweet.gap.hour.nf)
sd.tweetgapnf <- sd(tweet.gap.hour.nf)
kurt.tweetgapnf <- kurtosis(tweet.gap.hour.nf)
skew.tweetgapnf <- skewness(tweet.gap.hour.nf)

truehist(tweet.gap.hour.nf,main="Frequency of retweets not where it isn't the first tweet of the day",xlab="Number of retweets",ylab="Frequency of retweets",las=1,nbins=20,ylim=c(0,0.03),col=c("red","blue","green","yellow","dodgerblue3","navy","pink","purple","brown","darkgray","firebrick","forestgreen","gold","darkturquoise","khaki","indianred3","midnightblue","maroon3","plum4","salmon"),density=30,angle=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))
curve(dexp(x,1/mean(tweet.gap.hour.nf)),add=TRUE,lwd=3)

plot(ecdf(tweet.gap.hour.nf),xlab="Time between Tweets (in hours)", ylab = "Cumilative Probability",las=1,main="ECDF of Tweet gap (hours)",pch=NA,col="indianred3",lwd=3)
curve(pexp(x,1/mean(tweet.gap.hour.nf)),add=TRUE,lwd=3,col="midnightblue",lty=2)

table(personal$long.words)

mean.long.words <- mean(personal$long.words)
median.long.words <- median(personal$long.words)
mode.long.words <- samplemode(personal$long.words)
sd.long.words <- sd(personal$long.words)

PoiRLF <- function(theta,n,thetahat) {((theta^(n*thetahat))*exp(-n*theta))/((thetahat^(n*thetahat))*exp(-n*thetahat))}
plot(theta,PoiRLF(theta,n,thetahat),xlab=expression("theta"),ylab=expression(paste("R(", theta, ")")), main = "Poisson relative likelyhood function",type="l",lwd=4,las=1,xlim=c(0.87,1.2),col="forestgreen")
## Plots the relative likelyhood function. The maximum likelyhood estimate is between 1 and 1.05, as to be expected since it the maximum likelyhood estimate for a Poisson distribution is the mean

summary(personal$length)
summary(personal$words)
cor(personal$words,personal$length)
## High Correlation suggests that these 2 are related

plot(personal$length,personal$words,main="Tweet length and number of words", xlab= "Lengh of Tweet", ylab="Number of Words in Tweet",pch=1,cex=0.5,col="midnightblue")

personal <- subset(ds,subset=(username %in% c("@kevinolearytv","@SimuLiu","@b0rk")))

personal$long.words.grouped <- personal$long.words 
personal$long.words.grouped[personal$long.words >= 5] <- "5+"
table(personal$long.words.grouped)
## generating a 5+ category

thetahat <-mean.long.words

expected <- dpois(0:4, thetahat) * 530
round(expected, 2)

e5 <- 530 - sum(expected)

expected <- append(expected, e5)
personal$long.words[personal$long.words >= 5] <- "5+" 
observed <- table(personal$long.words)
cbind(observed, expected)

barplot(table(personal$long.words.grouped), las = 1, xlab = "Long words", ylab = "Tweets",
                 ylim = c(0, 280), col = "dodgerblue3", main = "Frequency of the number of long.words in tweets")

PoiRLF <- function(theta,n,thetahat) {((theta^(n*thetahat))*exp(-n*theta))/((thetahat^(n*thetahat))*exp(-n*thetahat))}

plot(theta,PoiRLF(theta,n,thetahat),xlab=expression("theta"),ylab=expression(paste("R(", theta, ")")), main = "Poisson relative likelyhood function",type="l",lwd=4,las=1,xlim=c(0.87,1.2),col="forestgreen")

abline(h = 0.15, col = "red", lwd = 4)

uniroot(function(x) PoiRLF(x, n, thetahat) - 0.15, lower = 0.9, upper = 1)$root

uniroot(function(x) PoiRLF(x, n, thetahat) - 0.15, lower = 1.1, upper = 1.15)$root
## Calculating a 15% likelyhood interval. This comes out to be [0.9484,1.1204]

PoiLOGRLF <- function(theta,n,thetahat) {log(((theta^(n*thetahat))*exp(-n*theta)))-log(((thetahat^(n*thetahat))*exp(-n*thetahat)))}

plot(theta,PoiLOGRLF(theta,n,thetahat),xlab=expression("theta"),ylab=expression(paste("R(", theta, ")")), main = "Poisson relative likelyhood function",type="l",lwd=4,las=1,xlim=c(0.87,1.2),col="forestgreen",ylim=c(-5,0.1))
## Visually this looks different but the interval which we get is the same

abline(h = log(0.15), col = "red", lwd = 4)

truehist(time.of.day.hour, xlab = "Hours since midnight", ylab = "Relative Frequency", main = "Relative frequency histogram of posts", ylim = c(0,0.1), las = 1, col = "red", density = 25, angle = 45)

curve(dnorm(x, mean(time.of.day.hour), sd(time.of.day.hour)), col = "blue", add = TRUE,lwd = 3)

truehist(tweet.gap.hour,main="Frequency of retweets",xlab="Number of retweets",ylab="Frequency of retweets",las=1,nbins=20,ylim=c(0,0.03),col=c("red","blue","green","yellow","dodgerblue3","navy","pink","purple","brown","darkgray","firebrick","forestgreen","gold","darkturquoise","khaki","indianred3","midnightblue","maroon3","plum4","salmon"),density=30,angle=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95))

curve(dnorm(x, mean(time.of.day.hour), sd(time.of.day.hour)), col = "blue", add = TRUE,lwd = 3)

qqnorm(y,main = "Time of tweet QQ plot",col = "dodgerblue3")
qqline(y,lwd=4)
## This qqplot suggests that the data doesn't lie reasonably along a straight line, and therefore is not Gaussian

personal <- subset(ds,subset=(username %in% c("@kevinolearytv","@SimuLiu","@b0rk")))

org <- subset(ds,subset=(username %in% c("@ONThealth","@ROWPublicHealth")))

Totals <- table(c(personal$long.words,org$long.words))
personal$long.words.grouped <- personal$long.words
org$long.words.grouped <- org$long.words
personal$long.words.grouped[personal$long.words >= 8] <- "8+"
org$long.words.grouped[org$long.words >= 8] <- "8+"
Organizationals<- table(org$long.words.grouped)
Personals<- table(personal$long.words.grouped)

personal$long.words[personal$long.words >= 8] <- "8+" 
org$long.words[org$long.words >= 8] <- "8+" 
with.totals <- cbind(Organizationals, Personals,Totals)
no.totals <- cbind(Organizationals, Personals)

barplot(rbind(Personals, Organizationals), beside = T, col = c("dodgerblue3","darkorchid"), density = 75, angle = c(45, 135), ylim = c(0,270), las = 1, xlab = "Long words", ylab = "Tweets",main = "Number of long words per tweet by account")
legend("topright", legend = c("Personal accounts", "Organizational accounts"), fill = c("dodgerblue3","darkorchid"), density = 75, angle = c(45, 135))
## Personal accounts appear more likely to post shorter tweets, whereas organizational tweet length is somewhere in the middle

personal1 <- subset(ds,subset=(username %in% c("@kevinolearytv","@SimuLiu","@b0rk")))

org1 <- subset(ds,subset=(username %in% c("@ONThealth","@ROWPublicHealth")))

org.mean.lw <- mean(org1$long.words)
org.median.lw <- median(org1$long.words)
org.sd.lw <- sd(org1$long.words)
org.mode.lw <- 2

personal.mean.lw <- mean(personal1$long.words)
personal.median.lw <- median(personal1$long.words)
personal.sd.lw <- sd(personal1$long.words)
personal.mode.lw <- 0

i<- 346
That <- org.mean.lw
Seq <- seq(2.3,3.1,0.001)
plot(Seq, PoisRLF(Seq, i, That), xlab = expression(theta), ylab = expression(paste("R(", theta, ")")), type = "l", lwd = 2,main = "Organizational Poisson relative likelihood function", las = 1,col = "navy")
abline(h = 0.15, col = "red", lwd = 3)

i1 <- 530
That1 <-personal.mean.lw
Seq1 <- seq(0.83,1.23,0.001)
plot(Seq1, PoisRLF(Seq1, i1, That1), xlab = expression(theta), ylab = expression(paste("R(", theta, ")")), type = "l", lwd = 2,main = "Personal Poisson relative likelihood function", las = 1,col = "dodgerblue3")
abline(h = 0.15, col = "green", lwd = 3)

personal.mean.lw - 1.96*sqrt(personal.mean.lw/530)
personal.mean.lw + 1.96*sqrt(personal.mean.lw/530)

org.mean.lw - 1.96*sqrt(org.mean.lw/346)
org.mean.lw + 1.96*sqrt(org.mean.lw/346)

## A 15% likelyhood interval for number of long words for personal accounts (the number of words 10 characters or longer, barring urls, hashtags and mentions of other users) is [0.9484,1.1204]
## A 15% likelyhood interval for number of long words for organizational accounts (the number of words 10 characters or longer, barring urls, hashtags and mentions of other users) is [2.5254,2.8665]

org.time.of.day.hour <- org$time.of.day/3600

personal.time.of.day.hour <- personal$time.of.day/3600

boxplot(org.time.of.day.hour,personal.time.of.day.hour,xlab = "Type of Account", ylab = "Time posted after midnight (in hours)", names = c("Organizational accounts","Personal accounts"), main = "Organizational and Personal tweet times compared",col=c("dodgerblue3","forestgreen"),density=75,angle=c(45,135))

org.todh.mean <- mean(org.time.of.day.hour)
org.todh.sd <- sd(org.time.of.day.hour)
org.todh.sample.size <- 346

personal.todh.mean <- mean(personal.time.of.day.hour)
personal.todh.sd <- sd(personal.time.of.day.hour)
personal.todh.sample.size <- 530

a <- qt(0.975,personal.todh.sample.size-1) ##this is about 1.964459
a1 <- qt(0.975,org.todh.sample.size-1) ##this is about 1.966864

personal.todh.mean-a*((personal.todh.sd)/sqrt(530))
personal.todh.mean+a*((personal.todh.sd)/sqrt(530))
##Interval: [13.09722,14.12756]
##We are 95% confident that the true value of the mean time for personal accounts lies between these 2 numbers

org.todh.mean-a1*((org.todh.sd)/sqrt(346))
org.todh.mean+a1*((org.todh.sd)/sqrt(346))
##Interval: [12.33726,12.83801]
##We are 95% confident that the true value of the mean time for organizational accounts lies between these 2 numbers

b <- qt(0.995,personal.todh.sample.size-1) ##this is about 2.585155
b1 <- qt(0.995,org.todh.sample.size-1) ##this is about 2.590155

personal.todh.mean-b*((personal.todh.sd)/sqrt(530))
personal.todh.mean+b*((personal.todh.sd)/sqrt(530))
##Interval: [12.9344,14.29304]
##We are 99% confident that the true value of the mean time for personal accounts lies between these 2 numbers

org.todh.mean-b1*((org.todh.sd)/sqrt(346))
org.todh.mean+b1*((org.todh.sd)/sqrt(346))
##Interval: [12.25792,12.91375]
##We are 99% confident that the true value of the mean time for organizational accounts lies between these 2 numbers

c <- qchisq(0.05,personal.todh.sample.size-1)
c1 <- qchisq(0.95,personal.todh.sample.size-1)

d <- qchisq(0.05,org.todh.sample.size-1)
d1 <- qchisq(0.95,org.todh.sample.size-1)

sqrt(((530-1)*personal.todh.sd^2)/c1)
sqrt(((530-1)*personal.todh.sd^2)/c)
##Interval: [5.747968,6.360241]
##We are 90% confident that the true value of sigma for time for personal accounts lies between these 2 numbers

sqrt(((346-1)*org.todh.sd^2)/d1)
sqrt(((346-1)*org.todh.sd^2)/d)
##Interval: [2.228999,2.526799]
##We are 90% confident that the true value of sigma for time for organizational accounts lies between these 2 numbers

ds <-dataset20873866

ds$retweets.log <- log(ds$retweets + 1)
ds$likes.log <- log(ds$likes + 1)

mod <- lm(ds$likes.log ~ ds$retweets.log)

coef(mod)
## alpha_hat is approximately 0.7094803
## beta_hat is approximately 1.2485247
## from fitting a linear model to our data

plot(ds$retweets.log, stdres, xlab = "retweets.log", ylab = "Standardized Residuals",
           main = "Std residuals vs. retweets.log", pch = 1, col = "navy", cex = 0.5, las = 1)
 abline(h = 0, lty = 2, col = "red", lwd = 2)

qqnorm(stdres, main = "qqplot of std residuals", xlab = "G(0, 1) Quantiles", ylab = "Standardized Residuals",
               pch = 1, col = "navy", cex = 0.5)
 qqline(stdres, lty = 2, col = "red", lwd = 2)
## The standard residuals don't appear to be randomly scattered. Variability is larger closer to 0, and decreases as retweets increase. Additionally, the qqplot doesn't seem to be roughly along a straight line. Therefore, a GLM is not likely appropriate

 zeromblikes.log <- log(1+ds$likes[ds$media.binary==0])
 
 onemblikes.log <- log(1+ds$likes[ds$media.binary==1])
 
 summary(zeromblikes.log)
 
 summary(onemblikes.log)
 
 t.test(onemblikes.log,zeromblikes.log, var.equal = TRUE)
 ## Testing that the likes log variate for tweets with no media items and one or more media item are equal, we get a 95% confidence interval of [-2.52,-1.82], and so we have reason to believe that these two means are not equal

 likes.binary <- as.numeric(ds$likes > mean(ds$likes))
 
 mean(ds$retweets.log[likes.binary==0])
 
 mean(ds$retweets.log[likes.binary==1])
 
 mod23 <- glm(likes.binary ~ retweets.log, family = 'binomial')
 summary(mod23)