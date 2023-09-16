# Übungsblatt 6

# Präsensaufgabe 1
bilirubin <- read.csv("Daten/bilirubin.txt", sep=";")
View(bilirubin)
set.seed(123)
rindex <- sample(x=nrow(bilirubin), size=1000, replace = FALSE)
bili.smpl <- bilirubin[rindex,]

# a)
hist(bili.smpl$ALTER, freq=FALSE, ylim=c(0,0.025))
lines(density(x=bili.smpl$ALTER),col="blue",lty=2)

plot(density(x=bili.smpl$Wert),col="blue",lty=2)
hist(bili.smpl$Wert, freq=FALSE, ylim=c(0,0.025), add=TRUE)

boxplot(bili.smpl$ALTER,data=bili.smpl,
        ylab="Alter")

boxplot(bili.smpl$Wert,data=bili.smpl,
        ylab="Bilirubin Werte")

# b)
bili.subsmpl <- split(bili.smpl,bili.smpl$SEX)
op<-par(mfrow = c(1, 2))
# Männer
hist(bili.subsmpl$M$ALTER, freq=FALSE,ylim=c(0,0.03), main="Männer", xlab = "Alter")
lines(density(x=bili.subsmpl$M$ALTER),col="blue",lty=2)
#Frauen
hist(bili.subsmpl$W$ALTER, freq=FALSE,ylim=c(0,0.03), main="Frauen", xlab = "Alter")
lines(density(x=bili.subsmpl$W$ALTER),col="red",lty=2)
par(op)

# c)
boxplot(Wert~SEX,data=bili.smpl,
        col=c("blue","red"),boxwex=0.5,outline=F,
        ylab="Bilirubin-Werte")

#Präsensaufgabe 2
data(iris)
head(iris,10)
# a
op<-par(mfrow = c(2, 2))
for(i in 1:4){
  hist(iris[,i],main=colnames(iris[i]))}
#for(i in 1:4){
#  boxplot(iris[,i],main=colnames(iris[i]))}
par(op)
boxplot(iris[,-5],main="Boxplots für die iris-Daten")
# b
par(mfrow=c(2,2),oma = c(0, 0, 2, 0))
for(i in 1:4){
  boxplot(iris[,i]~iris[,5], xlab="Species", ylab=colnames(iris[i]))}
mtext("Gruppierte Boxplots", outer = TRUE, cex = 1.5)
par(op)

#Präsensaufgabe 3
data(airquality)
head(airquality)
#a
op<-par(mfrow = c(2, 2),oma = c(0, 0, 2, 0))
for(i in 1:4){
  hist(airquality[,i],main="",xlab=colnames(airquality[i]), freq = FALSE)
  lines(density(x=airquality[,i], na.rm=TRUE),col=rainbow(4)[i],lty=2, lwd=2)
}
mtext("Histogramme für airquality", outer = TRUE, cex = 1.5)
par(op)
#range(airquality[,1:4], na.rm=TRUE)
# Range, mean, sd
sapply(airquality[,1:4], function(x) range(x, na.rm=T))
sapply(airquality[,1:4], function(x) mean(x, na.rm=T))
sapply(airquality[,1:4], function(x) sd(x, na.rm=T))

#max(density(x=airquality[,1], na.rm=TRUE)$y, density(x=airquality[,2], na.rm=TRUE)$y,
#    density(x=airquality[,3], na.rm=TRUE)$y, density(x=airquality[,4], na.rm=TRUE)$y)
# maximale Dichte
sapply(airquality[,1:4], function(x) max(density(x, na.rm=TRUE)$y))
# Position der maximalen Dichte (Modalwert)
sapply(airquality[,1:4], function(x) {dens <- density(x, na.rm=TRUE); dens$x[which.max(dens$y)]})

# Dichten gemeinsam plotten -- ist das sinnvoll??
plot(density(x=airquality[,1], na.rm=TRUE),col=rainbow(4)[1],lty=2, xlim = c(0,350), ylim=c(0,0.12), main="")
for(i in 2:4) lines(density(x=airquality[,i], na.rm=TRUE),col=rainbow(4)[i],lty=2)
legend(x="right", legend=names(airquality)[1:4], fill=rainbow(4))

# Einfache Boxplots
op<-par(mfrow = c(2, 2),oma = c(0, 0, 2, 0))
for(i in 1:4){
  boxplot(airquality[,i],xlab=colnames(airquality[i]))}
mtext("Boxplots für airquality", outer = TRUE, cex = 1.5)
par(op)
boxplot(airquality[,1:4],main="Boxplots für die airquality-Daten")
# (bedingte Verteilungen: bedingt auf Monat)
op<-par(mfrow = c(2, 2),oma = c(0, 0, 2, 0))
for(i in 1:4){
  boxplot(airquality[,i]~airquality[,5],xlab=colnames(airquality[5]), ylab=colnames(airquality[i]))}
par(op)
# b
plot(airquality[1:4]) # oder
pairs(airquality[1:4])
# c
cor(airquality[1:4]) # method ="pearson"
cor(airquality[1:4],method="spearman")  
cor(airquality[1:4],method="kendall")
# es treten NAs auf! Also:
cor(na.omit(airquality[1:4])) # method ="pearson"
cor(na.omit(airquality[1:4]),method="spearman")  
cor(na.omit(airquality[1:4]),method="kendall")

# präsenzaufgabe 4
data(m111survey, package = "tigerstats")
head(m111survey)
boxplot(m111survey$fastest~m111survey$sex,main="Gruppierter Boxplot", xlab="Geschlecht", ylab="Höchstgeschwindigkeit [mph]")

# präsenzaufgabe 5
# a
set.seed(123)
dat <- rnorm(1000)
# b
hist(dat, xlab = "Werte", ylab = "Dichte", main = "Histogramm von 1000 standardnormalverteilten Zufallsvariablen", cex.main=1, freq = FALSE)
# c
density(dat)
lines(density(x=dat),col="tomato3",lty=1, lwd=2)
# d
curve(dnorm(x), add=TRUE, col="cornflowerblue",lty=4,lwd=2)

legend(x="topright", legend=c("empirische Dichte","Dichteschätzer","Normalverteilungsdichte"), col=c("black","red", "cornflowerblue"), lty=c(1,1,2), lwd=c(2,2,2))
