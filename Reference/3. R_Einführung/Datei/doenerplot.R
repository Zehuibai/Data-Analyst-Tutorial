read.num <- function(prompt="Enter an integer: ")
{ 
  n <- readline(prompt)
  n <- as.numeric(n)
  if (is.na(n)){
    n <- read.num(prompt="Bitte ganze Zahl eingeben.")
  }
  return(n)
}

startkapital <- 2000
geld <- startkapital
maxkunden <- 500
bekanntheit <- 2/maxkunden
fixkosten <- 5*600/30
zutaten.kosten <- NULL

hist.beliebt <- 0
hist.bekannt <- bekanntheit
hist.geld <- geld
hist.verkauft <- 0

startinfo <- function() {
  print("/\\/\\/\\/\\-Dönerbude-/\\/\\/\\/\\")
  print(paste("Sie betreiben einen neuen Dönerstand. Ihnen entstehen",
              fixkosten,
              "Euro an Fixkosten pro Tag durch Standmiete, Strom, etc. und",
              "Sie haben ein Vermögen von",geld,"Euro.",
              "Verdoppeln Sie Ihr Vermögen um zu gewinnen!"))
}
repeat {
  if (is.null(zutaten.kosten)) startinfo()
  zutaten.kosten <- read.num("Wie viel Euro wollen Sie in die Produktion eines Döners investieren? ")
  vk.preis <- read.num("Für wie viel Euro wollen Sie einen Döner verkaufen? ")
  beliebtheit <- min(1,min(zutaten.kosten,10)/vk.preis)
  bekanntheit <- min(1,bekanntheit*(0.5+1.5*beliebtheit)^1)
  geld <- geld - fixkosten
  n.verkauft <- round(maxkunden*min(1,bekanntheit*sqrt(beliebtheit)))
  geld <- geld + n.verkauft*(vk.preis-zutaten.kosten)
  
  hist.beliebt <- c(hist.beliebt, beliebtheit)
  hist.bekannt <- c(hist.bekannt, bekanntheit)
  hist.geld <- c(hist.geld, geld)
  hist.verkauft <- c(hist.verkauft, n.verkauft)
  layout(matrix(1:4,nrow=2))
  plot(hist.geld, type="l", main="Geschäftsentwicklung")
  plot(hist.verkauft, type="l", main="Verkaufte Einheiten")
  plot(hist.beliebt, type="l", main="Beliebtheit")
  plot(hist.bekannt, type="l", main="Bekanntheit")
  
  print(paste("Sie haben",n.verkauft,"Döner verkauft.",
              "Ihr Vermögen beträgt jetzt",geld))
  if (geld<0) {
    print("Sie sind pleite!")
    break
  } else if (geld>2*startkapital) {
    print("Sie haben gewonnen!")
    break
  }
}
