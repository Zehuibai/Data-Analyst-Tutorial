# Session 18.11.2019

# Kreis- und Balkendiagramme für ordinal-skalierte Merkmale
# Übung 3.6
set.seed(2017)
Wetter2017 <- sample(c("kalt","mild","warm","heiss"),
                     size = 365, replace = T,
                     prob = c(0.5,0.35,0.12,0.03))
x <- table(Wetter2017); x
prop.table(x)
pie(table(Wetter2017))
barplot(table(Wetter2017))
# falsche Reihenfolge!
wlevels <- c("kalt","mild","warm","heiss")
Wetter2017.ord <- ordered(Wetter2017, levels=wlevels) # bzw.:
Wetter2017.fac <- factor(Wetter2017, levels=wlevels)
# absolute Häugfigkeiten
x <- table(Wetter2017.ord); x
prop.table(x)
# kumulative Häufigkeiten
cumsum(x)
cumsum(prop.table(x))
# Plots
op <- par(mfrow = c(1, 2), mar=rep(1, 4), oma=rep(1,4))
barplot(x,main="Säulendiagramm")
abline(0,0)
pie(x,main="Kreisdiagramm")
par(op)


# Übung 3.7
# Teil a.
install.packages("mixsmsn")
data(bmi, package = "mixsmsn")
# Teil b.
bmi$BMI <- ifelse(bmi$bmi<18.5,"Low",
                  ifelse(bmi$bmi>=18.5 & bmi$bmi<=25,
                         "Normal","High"))
# Teil c.
attach(bmi) # Objekte aus bmi verfügbar machen
BMI.ord <- ordered(BMI,levels=c("Low","Normal","High"))
x <- table(BMI.ord); x
y <- prop.table(x); y
barplot(x, main="BMI-Verteilung", ylab="abs. Häufigkeit")
abline(0,0)
barplot(y, main="BMI-Verteilung", ylab="rel. Häufigkeit")
abline(0,0)
detach(bmi) # attach rückgängig machen

# Charakteristische Maßzahlen (für 1 qualitatives Merkmal)
# Modalwert
data1 <- rep(c("leicht","normal", "schwer"), c(16,40,4))
which.max(table(data1))
set.seed(123)
KlasseB <- sample(c(1,2,3,4,5), size=200, replace=T,
                  prob=c(0.1,0.4,0.3,0.15,0.05))
which.max(table(KlasseB))
# Quantile, Median
quantile(KlasseB, probs=0.5)
?quantile
quantile(x, probs=seq(0, 1, 0.25),type=7,na.rm=FALSE)
y <- c(1, 2, 4, 4, 5, 6, 7, 8)
quantile(y, probs=0.5, type=1)
quantile(y, probs=0.5, type=2)

# Übung 3.8
# a) Führen Sie Folgendes aus und interpretieren Sie:
dim(HairEyeColor)
mtab <- HairEyeColor[, ,1]
wtab <- HairEyeColor[, ,2]
tab1 <- HairEyeColor[1, ,]
tab2 <- HairEyeColor[2, ,]
tab3 <- HairEyeColor[,1,]
tab4 <- HairEyeColor[, 3 ,]
tab5 <- HairEyeColor[1, ,1]
# b)
t1 <- HairEyeColor; t1
col1<-c("black","brown","red","gold")
col2<-c("brown","blue","khaki3","green")
col3<-c("blue","red")
t1[, 1,1] # Braunäugige Männer
pie(t1[, 1,1],col=col1,main="Hair Color distribution",
    sub="for males with brown eyes")
t1[1, ,2] # Schwarzhaarige Frauen
pie(t1[1, ,2],col=col2,main="Eye Color distribution",
    sub="for females with black Hair")
t1[1,1 ,] # Schwarzhaarig und brauäugig
pie(t1[1,1 ,],col=col3 ,main="Sex distribution",
    sub="for black haired people with brown eyes")

# Kreuztabellen manipulieren
HairEyeColor
aperm(HairEyeColor,perm=c(3,2,1))


# „Häufigkeitstabellen“ & Histogramme für quantitative Daten
# Beispiel 3.1
bilirubin <- read.table(file="Daten/bilirubin.txt",
                        head=TRUE, sep=";")
# Übung 3.15: Häufigkeitstabellen, Kreis- und Balkendiagramme.
str(bilirubin)
table(bilirubin[,1])
table(bilirubin[,2])
table(bilirubin[,3])

pie(table(bilirubin[,1]))
pie(table(bilirubin[,2]))
pie(table(bilirubin[,3]))
barplot(table(bilirubin[,1]))
barplot(table(bilirubin[,2]))
barplot(table(bilirubin[,3]))

# Übung 3.16: klassifizierte Häufigkeitstabelle, Kreis- und Balkendiagramm 
breakpoints <- c(0, 9, 19, 29, 39, 49, 59, 69, 79, 89, Inf)
bilirubin$ALTERSGRUPPE <- cut(bilirubin[,1],
                              breaks=breakpoints,
                              include.lowest = TRUE,
                              ordered_result = FALSE)
levels(bilirubin$ALTERSGRUPPE) <- c(paste0((0:8) * 10, "-",
                                           (0:8) * 10+9), ">90")
table(bilirubin$ALTERSGRUPPE)
library(RColorBrewer)
pie(table(bilirubin$ALTERSGRUPPE), main="Altersverteilung",
    col=brewer.pal(n = 10, name = "Spectral"))
barplot(table(bilirubin$ALTERSGRUPPE)/sum(table(bilirubin$ALTERSGRUPPE)),
        xlab="Altersgruppe", ylab="relative Häufigkeit",
        cex.axis=.6, cex.names=.6)

# Beispiel: Histrogramm mit hist()
hist(bilirubin$ALTER)
# eigene Klassenbreite definieren (breaks) :
summary(bilirubin$ALTER)
hist(bilirubin$ALTER, breaks=seq(0,110,10),
     right=F, col="darkgray")
hist(bilirubin$ALTER, breaks=seq(0,110,10),
     right=F,plot=F)
y <- hist(bilirubin$ALTER, breaks=seq(0,110,10),
        right=F,plot=F)
?hist
y$counts # Anzahl Beobachtungen in Zelle
y$mids # Mittelpunkte der Zellen
y$breaks # Grenzen der Zellen
y$density # relative Häufigkeiten/Dichteschätzer

# Histogramme: Übung 3.17
# Bilirubin Daten...
bil.m <- bilirubin[bilirubin[,2]=="M",]
bil.w <- bilirubin[bilirubin[,2]=="W",]
# Je zwei Plots nebeneinander darstellen
op <- par(mfrow = 1:2)
# Histogramme für die Altersverteilung jeweils der
# männlichen und der weiblichen Patienten
hist(bil.w$ALTER, breaks=seq(0,110,10),
     right=FALSE, col="darkgray", main="Frauen",freq=FALSE)
hist(bil.m$ALTER, breaks=seq(0,110,10),
     right=FALSE, col="darkgray", main="Männer",freq=FALSE)
# Histogramme für den Bilirubin-Wert jeweils der
# männlichen und der weiblichen Patienten
breaks1 <- seq(min(bilirubin$Wert), max(bilirubin$Wert), 0.05)
hist(bil.w$Wert, breaks=breaks1,
     right=FALSE, col="darkgray", main="Frauen", freq=FALSE)
hist(bil.m$Wert, breaks=breaks1,
     right=FALSE, col="darkgray",main="Männer",freq=FALSE)
# Histogramme für den Bilirubin-Wert von jeweils
# Männern bis einschließlich 18 J. und Männern über 18 Jahre
bil.m1<-bil.m[bil.m[,1] <= 18 ,]
bil.m2<-bil.m[bil.m[,1] > 18 ,]
hist(bil.m1$Wert, breaks=breaks1,
     right=F, col="darkgray",main="Männer <= 18",freq=F)
hist(bil.m2$Wert, breaks=breaks1,
     right=F, col="darkgray",main="Männer > 18",freq=F)
par(op)

# Boxplots
boxplot(bilirubin[,3])
boxplot(x=bilirubin[,3], outline=FALSE, col="gray90", boxwex=0.5)
boxplot(x = bilirubin[,3], outline=TRUE,
        col="gray90", boxwex=0.5,
        range=1.5) # entspricht default range
boxplot(x=bilirubin[,3], outline=FALSE,
        col="gray90", boxwex=0.5)
boxplot(x=bilirubin[,3], outline=FALSE,
        col="gray90", boxwex=0.5, range=0)
# Boxplots mit ggplot2
library(ggplot2)
p <- ggplot(bilirubin, aes(x="", y=Wert)) +
  geom_boxplot(); p
p + geom_boxplot(outlier.colour="red", outlier.shape=8,
                 outlier.size=4)
p <- ggplot(bilirubin, aes(x=SEX, y=Wert, color=SEX)) +
  geom_boxplot();p
p + scale_color_brewer(palette="Dark2")
p <- ggplot(bilirubin, aes(x=SEX, y=Wert, fill=SEX)) +
  geom_boxplot(); p
p + scale_fill_brewer(palette="Accent")+ coord_flip()

# ...

# Histogramme mit ggplot2
# Einfaches Histogram
ggplot(bil.m, aes(x=Wert)) + geom_histogram()
# Andere Bin-Breite
p <- ggplot(bil.m, aes(x=Wert)) +
  geom_histogram(binwidth=0.2); p
# andere Farben - Histogramm über das Alte geplottet (!)
p + geom_histogram(color="black", fill="white")
# Füllfarben je nach Gruppe
ggplot(bilirubin, aes(x=Wert, fill=SEX, color=SEX)) +
  geom_histogram(position="identity")
# Halbtransparente Füllfarbe
p<-ggplot(bilirubin, aes(x=Wert, fill=SEX, color=SEX)) +
  geom_histogram(position="identity", alpha=0.5)
p

# ...

# (Gruppierte) Dichteschätzer mit ggplot2
# Einfacher Dichteschätzer
  ggplot(bil.w) + geom_density(aes(x = Wert))
# Farbe Rot
ggplot(bil.w) + geom_density(aes(x = Wert), color="red")
# Gruppierte Dichteschätzer
ggplot(bilirubin) +
  geom_density(aes(x = Wert, color = SEX))
# gefüllte, transparente Flächen
ggplot(bilirubin) +
  geom_density(aes(x = Wert, fill = SEX),
               alpha = 0.2)

# ...

# Beispiel zu: density()
# Dichte der metrisch-skalierten Merkmalen aus dem iris-Datensatz
# gruppiert nach dem Merkmal „Species” darstellen:
iris1<-split(iris,iris[,5])
seto <-iris1[[1]]
vers <-iris1[[2]]
virg <-iris1[[3]]
for(i in 1:4){
  plot(density(seto[,i]), main=colnames(seto[i]), col="blue")
  lines(density(vers[,i]), col="red")
  lines(density(virg[,i]), col="green")
}
# Problem mit Range der x-Werte!

for(i in 1:4){
  plot(density(seto[,i]), main=colnames(seto[i]), col="blue",
       xlim=range(seto[,i], vers[,i], virg[,i]))
  lines(density(vers[,i]), col="red")
  lines(density(virg[,i]), col="green")
}
# Problem mit Wertebereich der y-Achse!

for(i in 1:4){
  y1<-density(seto[,i])$y
  y2<-density(vers[,i])$y
  y3<-density(virg[,i])$y
  Y<-max(y1,y2,y3)
  #windows()
  plot(density(seto[,i]),main=colnames(seto[i]),col="blue",
       xlim=range(seto[,i],vers[,i],virg[,i]),ylim=c(0,Y))
  lines(density(vers[,i]),col="red")
  lines(density(virg[,i]),col="green")
}

# ...

# Streudiagramme mit ggplot2
# Einfacher Scatter Plot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
# Grösse und Form ändern
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(size=2, shape=23)
# Gruppierter Scatter Plot - Form der Punkte je nach Species
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width,
                 shape=Species)) + geom_point()
# Form und Farbe ändern
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width,
                 shape=Species, color=Species)) + geom_point()
# Grösse, Farbe und Form ändern
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width,
                 shape=Species, color=Species, size=Species)) +
  geom_point()