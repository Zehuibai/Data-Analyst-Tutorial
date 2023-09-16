########
# PA01 #
########
# a)
pwin <- 1/choose(49,6)
pwin
# b)
# Berechnen über Gegenwahrscheinlichkeit
1-(1-pwin)^(100*52)
# c)
1-(1-pwin)^1000000
# d) 
# Variante 1 (Ausprobieren und Plot zuhilfe nehmen)
jackpot <- function(n) 1-(1-pwin)^n # wie c); WK, dass wenigstens einer von n gewinnt
plot(jackpot, xlim=c(0,10^8))
abline(0.5,0, col="red")
#näher hinschauen:
plot(jackpot, xlim=c(10^7-10^6,10^7))
abline(0.5,0, col="red")
#noch näher hinschauen:
plot(jackpot, xlim=c(9.6*10^6,9.8*10^6))
abline(0.5,0, col="red")
#noch näher hinschauen: schon ziemlich genau...
plot(jackpot, xlim=c(9.6928*10^6,9.6929*10^6))
abline(0.5,0, col="red")
#ganz genau hinschauen:
plot(jackpot, xlim=c(9692840,9692849))
abline(0.5,0, col="red")
# wir lesen ab: 9692843 Beobachtungen nötig

# Variante 2 (Optimieren mithilfe von optim)
jackpot <- function(n) 1-(1-pwin)^n # wie vorhin

ofun <- function(n) abs(jackpot(n)-0.5) # zum Optimieren brauchen wir eine Funktion die an der Lösungsstelle "0" ist
plot(ofun, xlim=c(.5*10^7,1.5*10^7)) 
optimize(ofun, interval = c(0,10^8))
# 9692842
# probe
jackpot(9692842)>0.5
jackpot(9692843)>0.5
jackpot(9692800:9692900)
# 9692843 Beobachtungen nötig

# Variante 3 (theoretisch/mathematisch)
# 1-(1-pwin)^n = 0.5
# (1-pwin)^n = 0.5
n <- log(0.5, base=1-pwin)
n

########
# PA02 #
########

# Einseitiger (oberer) Binomialtest
# H0: p (<)= 0.9, H1: p > 0.9     (p0=0.9; p Trefferwk, also P(w|sexed))
# Versuche: n = 20 
# Treffer:  k = 19
# Signifikanzniveau: alpha = 0.05

# Variante 1 (mit binom.test)
btest <- binom.test(19,n=20,p=0.9, alternative="g")
btest
btest$p.value # 0.391747, keine Ablehnung.
# Beachte auch das "verdächtige" Konfidenzintervall:
btest$conf.int # Rechte Intervallgrenze ist 100%
# Zum Beweis, dass der Test nichts bringt, betrachte folgenden Fall:
binom.test(20,n=20,p=0.9, alternative="g") 
# selbst bei 100% Erfolgsquote keine Signifikanz. Nicht genügend Versuchsobjekte vorhanden!

# Variante 2: manuelles Testen
# kritischer Wert
c=qbinom(p=0.95, size=20, prob=0.9)    # qbinom(p=0.05, size=20, prob=0.9, F)
c # kritischer Wert 20, Abkehnung für k > 20, unmöglich!

# Weitere Begründung:
power = pbinom(q=20, size = 20, prob = 0.9, F)  # bzw.: 1-pbinom(q=20, size = 20, prob = 0.9)
power # Test hat keine Macht/Güte
dbinom(x=20, size=20, prob=0.9)  # größer als alpha!
sum(dbinom(x=1:19, size=20, prob=0.9))
# Versuch mit dieser Stichprobenzahl macht überhaupt keinen Sinn!

########
# PA03 #
########

# Einseitiger (oberer) Binomialtest
# H0: p = 0.7, H1: p > 0.7     (p0=0.7)
# Versuche: n = 30 
# wahres p = 0.8
# Signifikanzniveau: alpha = 0.05

######################
# a) Power bestimmen #
######################
# dazu brauchen wir den kritischen Wert:
c <- qbinom(p = 0.95, size = 30, prob = 0.7) # bzw.: qbinom(p=0.05, size = 30, prob = 0.7, F)
# Probe
1-pbinom(c, size=30, prob=0.7) # kleiner alpha
1-pbinom(c-1, size=30, prob=0.7) # größer alpha
# also kritischer wert c=25
# Ablehnung von H0 für Trefferanzahl k > c=25 

# Fehler 2. Art: irrtümliche Nichtablehnung
# passiert wennn trotz p=0.8 ein k <=  25 gezogen wird
pbinom(q=25, size=30, prob=0.8)

# dann ist die Power:
1-pbinom(q=25, size=30, prob=0.8) # bzw.: pbinom(q=25, size=30, prob=0.8, F)  

############################################
# b) Suche minimales p, sodass Power>=0.95 #
############################################
pwr_p <- function(p) pbinom(q=25, size=30, prob=p, lower.tail=F)
plot( pwr_p, xlab="p", ylab="power" )
abline(0.95,0, col="red")

# man könnte hier bereits anhand des Plots eine approximative Lösung angeben

# Indem man diskretiesiert, bekommt man eine brauchbare Lösung:
p <- seq(0.8,1,0.01)
data.frame(p=p,power=pwr_p(p))
# wenn man feiner diskretisiert, wird die Tabelle unübersichtlich, aber R hilft beim auslesen:
p <- seq(0.8,1,0.0000001)
results <- data.frame(p=p,power=pwr_p(p))
head(results[results$power>0.95,],1)
# für p=0.9319445 wird die gewünschte Power erreicht

# weitere: Variante: wir suchen eine appproximative Lösung mittels optim:
# fürs Optimieren erzeuge Funktion, die am Zielwert minimal ist:
ofun <- function(pr) abs(pwr_p(pr)-0.95); plot(ofun)
opt <- optimize(ofun, interval = c(0,1))
opt$minimum #0.9319315
pwr_p(opt$minimum)
# Approximation passt ungefähr, aber nicht ganz genau:
pwr_p(0.9319); pwr_p(0.932)  #genauer: pwr_p(0.93194441)


############################################
# c) für p=0.71, suche n sodass Power>=0.9 #
############################################
# p=0.71, also delta=0.01
# Funktion aus dem Skript (Defaultwerte geändert)
powerfkt1 <- function(alpha=0.05, p0=0.7, delta=0.01,
                      nstart=25, nend=1000, nstep=25){
  n <- seq(nstart, nend, nstep)
  power<-NULL
  alpha.act<-NULL
  for (i in 1:length(n)){
    k <- qbinom(p=1-alpha, size=n[i], prob=p0)
    alpha.act[i] <- 1-pbinom(k, n[i], p0)
    power[i] <- 1-pbinom(q=k, size=n[i], prob=p0+delta)}
  x <- data.frame(n, power, alpha.act)
  return(x)
}
b<- powerfkt1(nstart=10000, nend=20000, nstep=100)
plot(b[,1:2], type="l") # nicht monoton!!! gemeine Frage
b <- powerfkt1(nstart=17800, nend=18000, nstep=1)
plot(b[,1:2], type="l") # nicht monoton!!! gemeine Frage
abline(0.9,0)
b[which(b[,2]>0.9),] # alle Werte mit Power > 0.9
head(b[which(b[,2]>0.9),], 1) # erster Wert mit Power > 0.9
# 17837, aber d. h. nicht(!), dass die Power für alle n>17837 eingehalten wird

# Ab welchem n wird die gewünschte Power nicht mehr unterschritten?
# Bestimme dazu größtes n, das die Power unterschreitet:
tail(b[which(b[,2]<0.9),], 1)
# Für n >= 17989 wird die Power nicht mehr unterschritten.

# siehe auch die Zusatzaufgabe, welche die Funktion powerfkt1 umschreibt/vereinfacht
