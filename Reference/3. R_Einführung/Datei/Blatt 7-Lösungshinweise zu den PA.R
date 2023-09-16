# Übungsblatt 7 - 
# Kurze Lösungshinweise zu den Präsenzaufgaben

# PA1
#Dichtefunktion von den t-Verteilungen mit 3, 5, 10, 30 und 50 Freiheitsgraden
#und die Dichtefunktion einer Standard-Normalverteilung in einer Abbildung
curve(dnorm(x), from=-3, to=3,
      main="Dichte der t-Verteilungen und N(0,1)-Verteilung",ylab="f(x)")
tdfs <- c(3,5,10,30,50)
cols <- rainbow(5)
for (i in 1:5)
  curve(dt(x,tdfs[i]), from=-3, to=3, col=cols[i],lty=i,lwd=2, add=TRUE)
legend(-3,0.4,
       legend=c(expression(t[3]),expression(t[5]),expression(t[10]),expression(t[30]),expression(t[50]),expression(N(0,1))),
       col = c(cols,"black"), lty =c(1:5,1))

# PA2
#f(x) = pf 1 (x,µ 1 ,σ 1 ) + (1 − p)f 2 (x,µ 2 ,σ 2 )
dgm <- function(p,mu1,mu2 ,sigma1 ,sigma2 ,n) {
  # n Simulationen insgesamt
  # wähle davon k mit Dichte f1 ud n-k mit Dichte f2
  k <- rbinom(1, n, p)
  sample(c(rnorm(k, mu1, sigma1), rnorm(n-k, mu2, sigma2))) #shuffle the values
}
gm.sim <- dgm(p=0.2, mu1=160, sigma1=10,mu2=180,sigma2=10, n=1000)
plot(density(gm.sim))
#curve(density(gm.sim))
gm.dens <- function(x) 0.2*dnorm(x,160,10)+0.8*dnorm(x,180,10)
curve(gm.dens, add = T, col="green")

# PA3
play <- function(n=1000) {
  mein.tip<-c(10,15,20,25,30,35)
  result <- NULL
  for (i in 1:n) {
    gew.woche <- sort(sample(49,6))  # bzw. sample(1:49,6)
    b <- intersect(mein.tip,gew.woche)
    result <- c(result,length(b))
  }
  return(table(result))
}
play()

#PA4
#a
2*mean(exp(runif(1000000,min=-1,max=1)^2))

#allgemein
mcint <- function(ifun, limits, n=1000000) {
  smpl <- runif(n, limits[1], limits[2])
  diff(limits)*mean(ifun(smpl))
}
#i
ifun <- function(x) exp(x^2)
limits <- c(-1,1)
mcint(ifun, limits)
integrate(ifun, limits[1], limits[2]) # zum Vergleich
#2.925303 with absolute error < 3.2e-14

#ii
ifun <- function(x) log(1+tan(x)^2)
limits <- c(0, pi/8)
mcint(ifun, limits)
integrate(ifun, limits[1], limits[2]) # zum Vergleich
#0.02050717 with absolute error < 2.3e-16

#iii
ifun <- function(x) sin(2*x)*cos(x)
limits <- c(-pi/2,pi/2)
mcint(ifun, limits)
integrate(ifun, limits[1], limits[2]) # zum Vergleich
#0 with absolute error < 1.5e-14

#b 
#allgemein
mcint.steps <- function(ifun, limits, n=10000) {
  smpl <- runif(n, limits[1], limits[2])
  diff(limits)*cumsum(ifun(smpl))/seq(1,length(ifun(smpl)))
}
#i
ifun <- function(x) exp(x^2)
limits <- c(-1,1)
plot(mcint.steps(ifun, limits), type="l")
abline(integrate(ifun, limits[1], limits[2])$value,0, col="blue")
#ii
ifun <- function(x) log(1+tan(x)^2)
limits <- c(0, pi/8)
plot(mcint.steps(ifun, limits), type="l")
abline(integrate(ifun, limits[1], limits[2])$value,0, col="blue")
#iii
ifun <- function(x) sin(2*x)*cos(x)
limits <- c(-pi/2,pi/2)
plot(mcint.steps(ifun, limits), type="l")
abline(integrate(ifun, limits[1], limits[2])$value,0, col="blue")

# PA5 - pi berechnen
my.pi <- function(B=1000) {
  x.B <- runif(B, min=-1,max=1)
  y.B <- runif(B, min=-1,max=1)
  inCircle <- sqrt(x.B^2+y.B^2)<1
  4 * sum(inCircle)/B # 4 ist die Fläche des Rechtecks
}
my.pi(10000000)
pi # zum Vergleich den echten Wert ausgeben
