# Einführung in R
# 2. Aufgabenblatt

# Präsenzaufgabe 1

#a)
my_mean <- function(x) sum(x)/length(x)

# Test: kommt das gleiche Ergebis heraus, wie mit der Fuunktion mean()?
mean(1:10)
my_mean(1:10)
smpl <- rnorm(1000,3,10)
mean(smpl)
my_mean(smpl)

#b)
my_range <- function(x) c(min(x),max(x))

# Test:
range(smpl)
my_range(smpl)

# Präsenzaufgabe 2

#a)
x <- 1:50
y <- seq(0,10,0.2)
# Ausgabe von x, y:
x; y


x^(1/3)
log(y)
x*y # Warnung, da beide Vektoren nicht gleich lang sind.

#b)
rep(1:3,40)

rep(1:100, times=2:101)

rep(rep(1:100, times=2:101),4)

# Alternativlösung für 2. und 3. Vektor:
repx <- function(x) rep(x,x+1)
repx(1:100)
rep(repx(1:100),4)

#c)
z1 <- seq(from=0.5, to=10, by=0.5)
z2 <- c(rep(3,10),rep(0,10))       # alternativ mit ifelse: z2 <- ifelse(1:20>10,0,3)
z3 <- rep(1:2,10)
z4 <- rep(1:6, times=1:6)[1:20]  
z5 <- numeric(20)
z5[1:14] <- c(1:2,1:3,1:4,1:5) 
z6 <- c(1:10,10:1)
z7 <- c(rep(1:4, times=4),5:8)
M <- rbind(z1,z2,z3,z4,z5,z6,z7)

# Präsenzaufgabe 3

#a)
(-2+3i)^2  # (-2+3i)*(-2+3i)=4-6i-6i-9=-5-12i

(5i)^4

sqrt(-1) #fail
sqrt(-1+0i)

#b)
(-1+2i)^(1/6)

sqrt(1i)

(1+1i)/(1-1i)

# Zusatzaufgabe 1

#a) Unterschied von "=" und "<-"

y <- 5 # Zuweisung an (neue) Variable y

x = y # Zuweisung des aktuellen Werts von y  an x

median(x = 1:10) # nur Parameterübergabe, keine Zuweisung: Es wird keine Variable x erzeugt!

median(x <- 1:10) # Zuweisung und Übergabe. Es wird eine Variable x (neu) erzeugt!

#b)

x <- y <- 5 
# in Präfix-Notation:
"<-"(x, "<-"(y, 5))

x = y = 5
# in Präfix-Notation:
"="(x, "="(y, 5))

x = y <- 5
# in Präfix-Notation:
"="(x, "<-"(y, 5))

x <- y = 5
# falsche Vermutung für analoge Präfix-Notation:
"<-"(x, "="(y, 5))

#c)

# Der Parser interpretiert x <- y <- 5 als
#"<-"(x, "<-"(y, 5))
# aber x <- y = 5  wird nicht interpretiert als
# "<-"(x, "="(y, 5))
# sondern entspricht mit Klammernsetzung eher
(x <- y) = 5 #, also
"="("<-"(x, y), 5)
# Grund: <- hat Vorrang ggü. =, vgl. Hilfe ?Syntax
# Weitere Details:
# https://stackoverflow.com/questions/1741820/what-are-the-differences-between-and-assignment-operators-in-r
# Kapitel 8.2.26 in http://www.burns-stat.com/pages/Tutor/R_inferno.pdf

# Zusatzaufgabe 2

#a)
(-8)^(1/3) # NaN
(-8+0i)^(1/3) #geht, gibt aber nicht die reelle Lösung aus!
# gemeint mit abs und sign war vermutlich:
a <- -8
sign(a)*(abs(a))^(1/3) # das ist die reelle Lösung für z^3 = -8, aber es ist ungleich z = (-8)^(1/3)

#b) die weiteren komplexen Lösungen ergeben sich durch:
# (a+bi)^3 = -8
# a^3 - 3ab^2 + i(3a^2b-b^3) = -8
# komplexe Lösungen für b<>0 => b^2 = 3a^2 einsetzen in a^3 - 3ab^2 ergibt a=1, b=+-sqrt(3) 
1+sqrt(3)*1i
1-sqrt(3)*1i
