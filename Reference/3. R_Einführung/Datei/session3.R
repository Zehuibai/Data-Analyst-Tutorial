# R-Session 3 - 28.10.2019

#  Rechnen mit Vektoren
character(5)
numeric(5)
c(1,2,3,4) ** 2
c(1,2,3,4) ^ 2
c(1,2,3,4)-1
c(1,2,3,4) * c(1,-1,0,2)
c(1,2,3,4) * c(3,-1)
c(1,2,3,4) * c(1,2,3)
x<-c(10,11,15); length(x)
sum(x)

# seq und rep
seq(5,7)
seq(10,15,by=2)
seq(1,10,len=5)
rep(4,3)
rep(c(1,0),3)
rep(c("M","W"),3)
c(rep(2,3),rep("M",3))
rep(c(1,0),times=3)
rep(c(1,0),each=3)
rep(c(1,0),times=c(2,3))
rep(c(1,0),each=2,times=3)

# Vektor-Indices
x <- c(12,18,23,5,9); x[2]
x[3:4]
x[c(1,5)]
x[c(1,length(x))]
x[-c(1,5)]
x[3] <- 99; x
x[3] * log(2)

# Elemente eines Vektors benennen:
x <- c(A=5, B=4); x
x["A"]
x[1]
names(x) <- c("1.Element", "2.Element"); x

# Datentypen ermitteln und ändern
x1 <- c(12,18,23,5,9)
is.numeric(x1)
x2 <- c(rep("M",2),rep("W",2))
is.numeric(x2)
is.factor(x2)
is.character(x2)
x3 <- as.factor(x2)
x3
is.factor(x3)

# Matrizen definieren
X1 <- matrix(1:4,nrow=2,ncol=2); X1
X2 <- matrix(1:4,nrow=3);   X2
X3 <- matrix(c(1,0,1,0),nrow=2,byrow=TRUE); X3
X4 <- matrix(c(1,0,1,0),nrow=2,byrow=FALSE); X4
X5 <- matrix(c(1,2,3, 10,20,30),
             nrow = 3, ncol=2, byrow=FALSE,
             dimnames = list(c("row1", "row2", "row3"),
                             c("col1", "col2"))); X5
# Spalten/Zeilen nachträglich umbenennen
colnames(X5) <- c("col1", "col2")
rownames(X5) <- c("row1", "row2", "row3")
# oder in einem Schritt mit:
dimnames(X5) <- list(c("row1", "row2", "row3"),
                     c("col1", "col2"))

# Matrix-Indices
X <- matrix(c(1,2,3, 10,20,30, 100,200,300), nrow=3)
X[2,2]
X[2,]
X[,3]
X[1:2,1:2]
X[,-c(1,3)]
X[,-c(1,3), drop=FALSE]
# Indizierung mit Namen
(X5 <- matrix(c(1,2,3, 10,20,30), nrow = 3, ncol=2,
              dimnames = list(c("row1", "row2", "row3"),c("col1", "col2"))))
X5["row1",]
X5[,"col2"]
X5["row1","col2"]
  

# Teilmengen von Vektoren und Matrizen
x <- c(5,12,26,-2.1); x
X <- matrix(c(1,2,3, 10,20,30), nrow = 3); X
x[x > 0]
x[x > 0 & x < 20]
X[X[,2] > 10,]
X[X[,1] < 2,]

# Arrays
XX <- array(1:30, dim=c(2,5,3))
XX

# Listen
x <- c(1,2)
X <- matrix(c(1,2,3, 10,20,30), nrow = 3)
XX <- array(1:30, dim=c(2,5,3))
Y <- list(x,X,XX)
Y

# Listen-Indices
Y[[1]]
Y[[2]][2,]
Y[[3]][, , 2]
Y[[3]][1, , 3]

# Listen: Namen als Indices
x <- c(1,2)
X <- matrix(c(1,2,3, 10,20,30), nrow = 3)
XX <- array(1:30, dim=c(2,5,3))
Y <- list(vek=x,mat=X,arr=XX)
Y[["vek"]] # oder analog:
  Y$vek
Y[["mat"]] # oder analog:
  Y$mat
names(Y) # Namen auslesen
names(Y) <- c("Aa","Be","Ce") # umbenennen

# Beispiel zu Listen:
# Funktion mit mehreren Ausgaben:
myvarianz1 <- function(x) {
    b1 <- (x-mean(x))^ 2
    V1 <-
      sum(b1)/(length(x)-1)
    V2 <- var(x)
    return(list(V1,V2))
}
data <- c(1,2,3,4,2,1,3)
myvarianz1(x=data)
# Alternativ mit benannten Listenobjekten: 
myvarianz2 <- function(x) {
    b1 <- (x-mean(x))^ 2
    V1 <- sum(b1)/(length(x)-1)
    V2 <- var(x)
    return(list(myvarianz=V1,Rvarianz=V2))
}
myvarianz2(x=data)

#  data.frames
?data.frame
# data.frame definieren
stud <- data.frame(Geschl = c(rep("M",3),rep("W",2)),
                   Gew = c(70,65,80,66,59), 
                   Gr = c(168, 172, 186, 172, 170),
                   Alt = c(20,19,24,19,22))
# oder schrittweise definieren:
a <- c(rep("M",3),rep("W",2))
b <- c(70,65,80,66,59)
c <- c(168, 172, 186, 172, 170)
d <- c(20,19,24,19,22)
stud <- data.frame(a,b,c,d)
colnames(stud) <- c("Geschl","Gew","Gr","Alt")
str(stud) # gibt die Struktur der Daten aus

# data.frame: wichtige Befehle
stud[,2]        # 2. Spalte auswählen
stud[,"Gew"]    # Spalte "Gew" auswählen
stud$Gew        # Spalte "Gew" auswählen
stud[,c(1,3)]   # 1. und 3. Spalte auswählen
stud[-c(1,4),]  # alle Spalten ohne 1. und 4. Zeilen
names(stud)     # Spaltennamen ausgeben
dim(stud)       # Dimensionen bestimmen
mean(stud$Gew)  # Mittelwert der Spalte "Gew"
mean(stud[,2])  # Mittelwert der 2. Spalte

# data.frame: Teilmenge bilden
# mit subset(Datensatzname, logischer Ausdruck)
?subset
stud.leicht <- subset(stud, stud$Gew <= 60)
stud.schwer <- subset(stud, Gew > 70)
stud.m <- subset(stud, Geschl=="M")
stud.m.gross <- subset(stud, Geschl=="M" & Gr > 180)
stud.in <- subset(stud, Alt %in% c(19,20))
# oder direkt: Datensatzname[logischer Ausdruck, ]
stud.leicht <- stud[stud$Gew <= 60 ,]
stud.schwer <- stud[stud[,2] > 70 ,]
stud.m <- stud[stud$Geschl=="M" ,]
stud.m.gross <- stud[stud[,1]=="M" & stud[,3] > 180 ,]
stud.in <- stud[stud$ Alt %in% c(19,20) ,]

stud1 <- subset(stud, subset=stud$Gew <= 60)
stud2 <- subset(stud, subset=(Geschl=="M" & Gr > 180))
stud3 <- subset(stud, subset=(Alt %in% c(19,20)))
stud4 <- subset(stud, subset=(Geschl=="M"), select=c("Gew","Alt"))
stud5 <- subset(stud, subset=(Geschl=="M"), select=-c(1,3))
identical(stud4,stud5)

# data.frame: Daten ordnen
?order
stud1 <- stud[order(stud[,2]),] # oder
stud1 <- stud[order(stud[,"Gew"]),] # oder
stud1 <- stud[order(stud$Gew),]
stud2 <- stud[order(stud$Gr),]
stud3 <- stud[order(stud$Alt),]
stud4 <- stud[order(stud$Gr, stud$Gew),]
stud5 <- stud[order(stud$Gr, -stud$Gew),]

# Umwandlung verschiedener Datenstrukturen
X <- matrix(c(1,2,3, 10,20,30), nrow = 3)
is.matrix(X); is.data.frame(X)
X1 <- as.data.frame(X)
X2 <- as.matrix(X1)
X3 <- as.list(X1)
X4 <- as.data.frame(X3)

# Weitere logische Operationen zur Indizierung
stud <- data.frame(Geschl = c(rep("M",3),rep("W",2)),
                   Gew = c(70,65,80,66,59),
                   Gr=c(168, 172, 186, 172, 170),
                   Alt=c(20,19,24,19,22))
any(stud$Gew > 100)
all(stud$Alt <= 24)
which(stud$Geschl == "W")
stud[which(stud[,3] <= 170),]
stud[which(stud[,1] != "M"),]
stud[which.max(stud[,3]),]
x<-c(1,4,NA)
x[!is.na(x)]
