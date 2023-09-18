# 14.10.2019 - erste R-Sitzung

# Beispiel 1

gha <- read.table(file="C:/meinordner/GHA.txt", header=T)
gha <- read.table(url("https://pastebin.com/raw/df3x1d3J"), header=T)

table(gha$Geschlecht, gha$Augenfarbe)
table(gha$Geschlecht, gha$Haarfarbe)
table(gha$Augenfarbe, gha$Haarfarbe)
table(gha)

mytab <- table(gha$Augenfarbe,gha$Haarfarbe)
barplot(mytab, beside=F, legend=T,
        col = c("blue", "brown", "green", "gray"),
        xlab="Haarfarbe", xlim=c(0,6))

# Beispiel 2

table(sample(1:6,100,T)) # 100 mal würfeln

table(sample(1:6,1000,T)) # 1000 mal würfeln

# ein bischen Programmieren:
for(i in seq(100,1000,100)){
  print(table(sample(1:6,i,T))/i)
}

# Beispiel 3

wuerfeln <- table(sample(1:6,10000,T))
chisq.test(wuerfeln, p=rep(1/6,6))

# Beispiel 4

binom.test(x=c(13452,12860), p=0.5 , conf.level=0.95 )

# Beispiel 5

2 * mean(exp(runif(100000,min=-1,max=1)^2))

# Beispiel 6

my.fun <- function(n){
  x.hat <- rep(0,n)
  for (i in 1:n){
    w <- sample(1:6, size=i, replace=T)
    x.hat[i] <- sum(w==6)/i
  }
  x.hat
}
# und jetzt das Ergebnis grafisch darstellen:
t <- my.fun(n=1000)
plot(1:length(t), t, xlab="Anzahl der Würfe",
     ylab="relative Häufigkeit der Sechser",
     lwd=1, col="blue", type="l")
abline(1/6,0, col="red", lwd=2)

# Beispiel 7

install.packages("MASS", dependencies=T); library(MASS)
# package MASS installieren und laden
data(hills) # data hills laden
head(hills); ?hills # data hills anschauen


my.model <- lm(time~dist-1, data=hills)
plot(hills$dist, hills$time)
abline(my.model, lwd = 2, col = "blue")

d <- seq(from = min(hills$dist), to = max(hills$dist), by = 0.01)
data1 <- data.frame(dist = d)
pre <- predict(my.model, newdata = data1, interval = "conf")
lines(d, pre[,2], lty = 2, col = "red")
lines(d, pre[,3], lty = 2, col = "red")
