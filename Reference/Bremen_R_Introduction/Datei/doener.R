read.int <- function(prompt="Enter an integer: ")
{ 
  n <- readline(prompt)
  n <- as.integer(n)
  if (is.na(n)){
    n <- read.int(prompt="Bitte ganze Zahl eingeben.")
  }
  return(n)
}

startkapital <- 2000
geld <- startkapital
maxkunden <- 500
bekanntheit <- 2/maxkunden
fixkosten <- 600/30
zutaten.kosten <- NULL

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
  zutaten.kosten <- read.int("Wie viel Euro wollen Sie in die Produktion eines Döners investieren? ")
  vk.preis <- read.int("Für wie viel Euro wollen Sie einen Döner verkaufen? ")
  beliebtheit <- zutaten.kosten/vk.preis
  bekanntheit <- min(1,bekanntheit*(0.5+beliebtheit))
  geld <- geld - fixkosten
  n.verkauft <- round(maxkunden*min(1,bekanntheit*beliebtheit))
  geld <- geld + n.verkauft*(vk.preis-zutaten.kosten)
  
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
