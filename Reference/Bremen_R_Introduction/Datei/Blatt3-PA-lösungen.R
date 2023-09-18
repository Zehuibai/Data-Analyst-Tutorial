# Präsensaufgabe 1
data("airquality")

air3 <- na.omit(airquality) # oder:
air3 <- airquality[complete.cases(airquality),]

air4 <- airquality[!is.na(airquality[,1]), ]

air5 <- airquality[!is.na(airquality[,1])&!is.na(airquality[,2]), ]

maxwind <- which.max(airquality$Wind)
airquality[maxwind, c("Month","Day")]
maxtemp <- which.max(airquality$Temp)
airquality[maxtemp, c("Month","Day")]
maxozone <- which.max(airquality$Ozone)
airquality[maxozone, c("Month","Day")]
maxsol <- which.max(airquality$Solar.R)
airquality[maxsol, c("Month","Day")]

# oder ganz elegant:
maxind <- apply(airquality[,c(3,4,1,2)], 2, which.max)
maxval <- airquality[maxind,]
rownames(maxval) <- paste("max",names(maxind))
maxval[, c("Month","Day")]

# Präsenzaufgabe 2

# Vorübung: 3 und 4. Wurzel berechnen
roots34<-function(x)
{
  root3 <- x^(1/3)
  root4 <- x^(1/4)
  return(list(root3, root4))
}
roots34(x=c(16,32,64))

mnroots<-function(x, m,n)
{
  mroot <- x^(1/m)
  nroot <- x^(1/n)
  return(list(m.root=mroot,n.root=nroot))
}
mnroots(x=c(16,32,64), m=2, n=3)

# Präsensaufgabe 3
# a)
findNA1 <- function(x) {
  x[1] <- ifelse(x[1]<0 | x[1]==999, NA, x[1])
  x
}

findNA1(-1)
findNA1(999)
findNA1(9993)
findNA1(c(-1,999,9993))

# b)
findNA2 <- function(x) {
  ifelse(x<0 | x==999, NA, x)
}

findNA2(-1)
findNA2(999)
findNA2(9993)
findNA2(c(-1,999,9993))

# alternativ:
findNA3 <- function(x) {
  x[x<0 | x==999] <- NA
  return(x)
}
findNA3(c(-1,999,9993))

# Präsensaufgabe 4
# a) Ozone1
data("airquality")

airquality$Ozone1 <- ifelse(is.na( airquality$Ozone1 ), -99, airquality$Ozone1)

# b) Solar.R1
airquality$Solar.R1 <- airquality$Solar.R
airquality$Solar.R1[is.na( airquality$Solar.R1 )] <- -99


# Präsensaufgabe 5
x <- sample(c(NA,-3:3), size=8)
ifelse(x>=0, x^2, ifelse(x<0, x^-2,0)) # geht nicht!
ifelse(is.na(x), 0, ifelse(x<0, x^-2, x^2)) # so gehts!
