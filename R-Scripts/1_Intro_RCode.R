#######################
### Intro
# von Julien P. Irmer


##### Daten einlesen und verarbeiten ---
load(url("https://github.com/jpirmer/MSc1_FEI/blob/master/data/Schulleistungen.rda?raw=true"))

##### Ein Überblick über die Daten ---
head(Schulleistungen)

# Namen der Variablen abfragen
names(Schulleistungen)

# Anzahl der Zeilen
nrow(Schulleistungen)
# Anzahl der Spalten
ncol(Schulleistungen)

# Anzahl der Zeilen und Spalten kombiniert
dim(Schulleistungen)

# Struktur des Datensatzes - Informationen zu Variablentypen
str(Schulleistungen)

# Indizierung
Schulleistungen$IQ


##### Einfache Deskriptivstatistiken ---
# Summe
sum(Schulleistungen$IQ)

# Mittelwert
mean(Schulleistungen$IQ)
1/100*sum(Schulleistungen$IQ) # auch der Mittelwert

# Varianz
var(Schulleistungen$IQ)

# SD
sd(Schulleistungen$IQ)
sqrt(var(Schulleistungen$IQ)) # die Wurzel aus der Varianz ist die SD, hier: sqrt() ist die Wurzel Funktion

# Die Summary
summary(Schulleistungen$IQ)

# Spaltenmittelwerte
colMeans(Schulleistungen)

# Zeilenmittelwerte
rowMeans(Schulleistungen)


##### Simulieren von Daten ---
set.seed(1234567) # Replizierbarkeit

# Standardnormalverteilung
X <- rnorm(n = 1000, mean = 0, sd = 1) # Standardnormalverteilung mit n = 1000
hist(X)
mean(X)
sd(X)

# der t-Test
set.seed(2) # Replizierbarkeit
Y <- rnorm(n = 1000, mean = 0, sd = 1)
ttest <- t.test(X, Y, var.equal = T)
ttest

names(ttest)    # alle möglichen Argumente, die wir diesem Objekt entlocken können
ttest$statistic # (empirischer) t-Wert
ttest$p.value   # zugehöriger p-Wert


##### Verstöße gegen die Modellannahmen ---
set.seed(1)
par(mfrow = c(1,2)) # zwei Grafiken neben einander

X <- -rexp(1000, 1)
X <- X + 1
Y <- rexp(1000, 2)
Y <- Y - 1/2
hist(X); hist(Y)

par(mfrow = c(1,1)) # Grafikeinstellung zurücksetzen


##### Appendix ---
### Appendix A ---

#### Verteilung unter $H_0$
ts <- c(); ps <- c() # wir brauchen zunächst Vektoren, in die wir die t-Werte und die p-Werte hineinschreiben können
for(i in 1:10000)
{
    X <- rnorm(n = 1000, mean = 0, sd = 1)
    Y <- rnorm(n = 1000, mean = 0, sd = 1)
    ttest <- t.test(X, Y, var.equal = T)
    ts <- c(ts, ttest$statistic) # nehme den Vektor ts und verlängere ihn um den neuen t-Wert
    ps <- c(ps, ttest$p.value)   # nehme den Vektor ps und verlängere ihn um den neuen p-Wert
}

hist(ts, main = "(empirische) t-Werte nach 10000 Replikationen unter H0", xlab = "T", freq = F)
lines(x = seq(-4,4,0.01), dt(x = seq(-4,4,0.01), df = ttest$parameter), lwd = 3)
hist(ps, main = "p-Werte nach 10000 Replikationen unter H0", xlab = "p", freq = F)
abline(a = 1, b = 0, lwd = 3)


#### Verteilung unter $H_1$
ts <- c(); ps <- c() # wir brauchen zunächst Vektoren, in die wir die t-Werte und die p-Werte hineinschreiben können
for(i in 1:10000)
{
    X <- rnorm(n = 1000, mean = 0, sd = 1)
    Y <- -0.1 + rnorm(n = 1000, mean = 0, sd = 1) # Mittelwertsdifferenz ist 0.1
    ttest <- t.test(X, Y, var.equal = T)
    ts <- c(ts, ttest$statistic) # nehme den Vektor ts und verlängere ihn um den neuen t-Wert
    ps <- c(ps, ttest$p.value)   # nehme den Vektor ps und verlängere ihn um den neuen p-Wert
}

hist(ts, main = "(empirische) t-Werte nach 10000 Replikationen unter H1", xlab = "T", freq = F)
lines(x = seq(-4,4,0.01), dt(x = seq(-4,4,0.01), df = ttest$parameter), lwd = 3)
hist(ps, main = "p-Werte nach 10000 Replikationen unter H1", xlab = "p", freq = F)
abline(a = 1, b = 0, lwd = 3)


#### Verteilung unter $H_0$ mit Modellverstößen
set.seed(1)
ts <- c(); ps <- c() # wir brauchen zunächst Vektoren, in die wir die t-Werte und die p-Werte hineinschreiben können
for(i in 1:10000)
{
       X <- -rexp(n = 5, rate = 1) # simuliere Exponentialverteilung zur Rate 1 mit n = 5
        X <- X + 1 # zentriere, sodass der Populationsmittelwert wieder 0 ist
        Y <- rexp(n = 5, rate = 2) # simuliere Exponentialverteilung zur Rate 2 mit n = 5
        Y <- Y - 1/2 # zentriere, sodass der Populationsmittelwert wieder 0 ist
        ttest <- t.test(X, Y, var.equal = T)
        ts <- c(ts, ttest$statistic) # nehme den Vektor ts und verlängere ihn um den neuen t-Wert
        ps <- c(ps, ttest$p.value)   # nehme den Vektor ps und verlängere ihn um den neuen p-Wert
}

hist(ts, main = "t-Werte nach 10000 Replikationen unter Modellverstöße\n für kleine Stichproben", xlab = "t", freq = F)
lines(x = seq(-4,4,0.01), dt(x = seq(-4,4,0.01), df = ttest$parameter), lwd = 3)
hist(ps, main = "p-Werte nach 10000 Replikationen unter Modellverstößen\n für kleine Stichproben", xlab = "p", freq = F)
abline(a = 1, b = 0, lwd = 3)


#### Verteilung unter $H_0$ mit Modellverstößen: Welch-Test
set.seed(1)
ts <- c(); ps <- c() # wir brauchen zunächst Vektoren, in die wir die t-Werte und die p-Werte hineinschreiben können
for(i in 1:10000)
{
        X <- -rexp(n = 5, rate = 1) # simuliere Exponentialverteilung zur Rate 1 mit n = 5
        X <- X + 1 # zentriere, sodass der Populationsmittelwert wieder 0 ist
        Y <- rexp(n = 5, rate = 2) # simuliere Exponentialverteilung zur Rate 2 mit n = 5
        Y <- Y - 1/2 # zentriere, sodass der Populationsmittelwert wieder 0 ist
        ttest <- t.test(X, Y) # Welch Test
        ts <- c(ts, ttest$statistic) # nehme den Vektor ts und verlängere ihn um den neuen t-Wert
        ps <- c(ps, ttest$p.value)   # nehme den Vektor ps und verlängere ihn um den neuen p-Wert
}

hist(ts, main = "t-Werte (des Welch t-Tests) nach 10000 Replikationen\n unter Modellverstöße für kleine Stichproben", xlab = "t", freq = F)
lines(x = seq(-4,4,0.01), dt(x = seq(-4,4,0.01), df = ttest$parameter), lwd = 3)
hist(ps, main = "p-Werte (des Welch t-Tests) nach 10000 Replikationen\n unter Modellverstößen für kleine Stichproben", xlab = "p", freq = F)
abline(a = 1, b = 0, lwd = 3)


#### Verteilung unter $H_0$ mit Modellverstößen mit größerer Stichprobe: Welch-Test
set.seed(1234)
ts <- c(); ps <- c() # wir brauchen zunächst Vektoren, in die wir die t-Werte und die p-Werte hineinschreiben können
for(i in 1:10000)
{
        X <- -rexp(n = 100, rate = 1) # simuliere Exponentialverteilung zur Rate 1 mit n = 100
        X <- X + 1 # zentriere, sodass der Populationsmittelwert wieder 0 ist
        Y <- rexp(n = 100, rate = 2) # simuliere Exponentialverteilung zur Rate 2 mit n = 100
        Y <- Y - 1/2 # zentriere, sodass der Populationsmittelwert wieder 0 ist
        ttest <- t.test(X, Y) # Welch Test
        ts <- c(ts, ttest$statistic) # nehme den Vektor ts und verlängere ihn um den neuen t-Wert
        ps <- c(ps, ttest$p.value)   # nehme den Vektor ps und verlängere ihn um den neuen p-Wert
}

hist(ts, main = "t-Werte (des Welch t-Tests) nach 10000 Replikationen\n unter Modellverstöße für kleine Stichproben", xlab = "t", freq = F)
lines(x = seq(-4,4,0.01), dt(x = seq(-4,4,0.01), df = ttest$parameter), lwd = 3)
hist(ps, main = "p-Werte (des Welch t-Tests) nach 10000 Replikationen\n unter Modellverstößen für kleine Stichproben", xlab = "p", freq = F)
abline(a = 1, b = 0, lwd = 3)


### Große Grafik
load(url("https://github.com/jpirmer/MSc1_FEI/blob/master/data/Erg.RData?raw=true"))
library(ggplot2) # muss vorher installiert sein!
ggplot(data = Erg, aes(x = d, y = Power, col = n, group = n))+
     geom_line()+
     geom_abline(slope = 0,intercept = .05, lty = 3)+
     geom_abline(slope = 0,intercept = .8, lty = 2) + 
     scale_colour_gradientn(colours=rainbow(4))+
     ggtitle("Power vs. d and n")



##### Appendix B ---
## Matrixalgebra

##### Vektoren
X <- c(1, 2, 3)
Y <- c(10, 8, 6)

# Indizieren
Y[2]

# Einfache Operationen
X + Y  # Addition
X - Y  # Subtraktion
3*X    # Skalare Multiplikation
1/2*X

Z <- c(1:6) # Zahlen 1 bis 6
Z + Y       # Addition ungleicher Längen

# Multiplikation
X*Y # elementenweise Multiplikation



#### Matrizen
# mache X zu Spaltenvektor (Matrix)
as.matrix(X)

# Vektoren zu Matrizen zusammenfassen
A <- cbind(X, Y)
A
B <- rbind(X, Y)
B

# Indizierung
A[1, 2] # Eintrag 1. Zeile 2. Spalte in A
A[1, ] # 1. Zeile
A[, 2] # 2. Spalte

# Transponieren
A
t(A)
B

# Addition (funktioniert nicht!)
A + B

# Skalarmultiplikation
A * 2

# Matrixprodukte
A %*% B # Matrixprodukt A*B
B %*% A # Matrixprodukt B*A

#### Spezielle Matrizen

diag(3) # Einheitsmatrix 3x3
diag(1:3) # Diagonalmatrix mit Elementen 1,2,3 auf der Diagonale


C <- matrix(data = c(1:9), nrow = 3, ncol = 3, byrow = T)
C

# Diagonalelemente von C
diag(C)



##### Determinanten und Invertierung
solve(C) # Inverse -> nicht möglich

det(C)
round(det(C), 14) # Determinante ist 0 (nur numerische Abweichungen)

# Lineare Abhängigkeit in C
2*C[, 2] - C[, 1]     # 2*2.Spalte - 1. Spalte rechnen ist gleich
C[, 3]               # 3. Spalte

# Elementenweise Invertierung ist NICHT die Inverse
C^-1
C^-1 %*% C # ist nicht die Einheitsmatrix
C^-1 * C   # elementenweise ergibt überall 1 - ist immer noch nicht die Einheitsmatrix!

# Eine invertierbare Matrix D
D <- matrix(c(1, 0, 0,
              1, 1, 1,
              2, 4, 5), 3, 3, byrow = T)
det(D)

solve(D) # Inverse von D
D %*% solve(D)
solve(D) %*% D

