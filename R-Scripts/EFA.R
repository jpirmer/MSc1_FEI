#######################
### EFA
# von Julien P. Irmer


#### Vorbereitung ----

library(corrplot) # Korrelationsmatrix grafisch darstellen
library(psych) # EFA durchführen
library(GPArotation) # EFA Lösung rotieren


# Datensatz laden
load(url("https://pandar.netlify.app/post/Big5_EFA.rda"))
head(Big5, n = 10) # gebe die ersten 10 Zeilen aus
dim(Big5)

# Französische Stichprobe herausholen und Dimensionen des Datensatzes betrachten
data_France <- Big5[Big5$country == "FR", ]
dim(data_France)


# Weiter kürzen
dataFR <- data_France[, -c(1:4)] # entferne demografische Daten und speichere als "dataFR"

#### Visualisierte Korrelationsmatrix in dataFR
corrplot(corr = cor(dataFR), # Korrelationsmatrix (Datengrundlage)
         method = "color", # Zeichne die Ausprägung der Korrelation farblich kodiert
         addCoef.col = "black", # schreibe die Korrelationskoeffizienten in schwarz in die Grafik
         number.cex = 0.7) # Stelle die Schriftgröße der Koeffizienten ein




##### 2-faktorieller Subdatensatz -----
dataFR2 <- dataFR[,1:6] # Zunächst wählen wir die ersten 6 Items: E1 bis E3 und N1 bis N3
head(dataFR2)

# zum gleichen Ergebnis würde auch Folgendes kommen (besonders von Relevanz,
# wenn wir bspw. nicht die Position sondern nur die Namen der Variablen kennen!):
head(dataFR[, c("E1", "E2", "E3", "N1", "N2", "N3")])

# Visualisierte Korrelationsmatrix
corrplot(corr = cor(dataFR2), # Korrelationsmatrix (Datengrundlage)
         method = "color", # Zeichne die Ausprägung der Korrelation farblich kodiert
         addCoef.col = "black", # schreibe die Korrelationskoeffizienten in schwarz in die Grafik
         number.cex = 1) # Stelle die Schriftgröße der Koeffizienten ein




## Parallelanalysen
fa.parallel(dataFR2)
fa.parallel(dataFR2, fa = "fa")



## Orthogonale EFA ---
fa(dataFR2, nfactors = 2, rotate = "varimax")
two_factor <- fa(dataFR2, nfactors = 2, rotate = "varimax")
names(two_factor)

# Faktorladungen
two_factor$loadings
two_factor$loadings[,] # ohne seltsames Runden

# Strukturmatrix
two_factor$Structure[,]



## Oblique EFA ---
two_factor_oblimin <- fa(dataFR2, nfactors = 2, rotate = "oblimin")

# Eigenwerte
two_factor$Vaccounted
two_factor_oblimin$Vaccounted

# Faktorladungen
two_factor_oblimin$loadings[,] # Ladungsmatrix

# Phi
two_factor_oblimin$Phi # Korrelationsmatrix der Faktoren


two_factor_oblimin$loadings[,]
# Strukturmatrix
two_factor_oblimin$Structure[,]


### ML-EFA ---
two_factor_ML <- fa(dataFR2, nfactors = 2, rotate = "oblimin", fm = "ml")
two_factor_ML

two_factor_ML$STATISTIC # Likelihood basierter Chi^2-Wert
two_factor_ML$PVAL # p-Wert


# Passt auch eines mit 1 Faktor?
one_factor_ML <- fa(dataFR2, nfactors = 1, rotate = "oblimin", fm = "ml")
one_factor_ML$STATISTIC # Chi-Quadratwert
one_factor_ML$PVAL # p-Wert

anova(one_factor_ML, two_factor_ML)


# Passt auch eines mit 3 Faktor?
three_factor_ML <- fa(dataFR2, nfactors = 3, rotate = "oblimin", fm = "ml")
three_factor_ML$STATISTIC # Chi-Quadratwert
three_factor_ML$PVAL # p-Wert

anova(two_factor_ML, three_factor_ML)



#### Appendix B -----------
# Orthogonaler Fall
two_factor$loadings[1, 1] # volle Formel für ersten Eintrag in Strukutrmatrix, da Kovarianz der Faktoren = 0
two_factor$Structure[1, 1] # erster Eintrag in der Strukturmatrix

# Obliquer Fall
two_factor_oblimin$loadings[1, 1] # erste Faktorladung im obliquen Modell (unterscheidet sich von dem ersten Eintrag der Strukturmatrix)
two_factor_oblimin$loadings[1, 1] + two_factor_oblimin$loadings[1, 2]*two_factor_oblimin$Phi[2, 1] # volle Formel für ersten Eintrag in Strukutrmatrix
two_factor_oblimin$Structure[1, 1] # erster Eintrag in der Strukturmatrix


# in Matrixnotation
two_factor_oblimin$loadings[,] %*% two_factor_oblimin$Phi[,] # Matrixprodukt
two_factor_oblimin$Structure[,] # Strukturmatrix


## Mehrdimensionale Fall
five_factor_ML$loadings[,] %*% five_factor_ML$Phi[,] # Matrixprodukt
five_factor_ML$Structure[,] # Strukturmatrix
five_factor_ML$loadings[,] %*% five_factor_ML$Phi[,] - five_factor_ML$Structure[,]



#### Appendix C -----------
# Berechnen von Eigenwerten und Kommunalitäten mit Hilfe von Lambda und Phi

# Kommunalitäten
two_factor_ML$communality
diag(two_factor_ML$loadings[,] %*% two_factor_ML$Phi[,] %*% t(two_factor_ML$loadings[,]))
diag(two_factor_ML$Structure[,] %*% t(two_factor_ML$loadings[,]))

# Eigenwerte
two_factor_ML$Vaccounted # Eigenwerte nach Rotation und Extraktion in SS loadings
diag(two_factor_ML$Phi[,] %*% t(two_factor_ML$loadings[,])  %*% two_factor_ML$loadings[,])
diag(t(two_factor_ML$Structure[,]) %*% two_factor_ML$loadings[,])




#### Appendix D -----------
# Mahalanobisdistanz
Mahalanobis_Distanz <- mahalanobis(x = dataFR, cov = cov(dataFR), center = apply(X = dataFR, MARGIN = 2, FUN = mean)) # Berechnen der Mahalanobisdistanz
hist(Mahalanobis_Distanz, col = "skyblue", border = "blue", freq = F, breaks = 15) # Histogramm
lines(x = seq(0, max(Mahalanobis_Distanz), 0.01), y = dchisq(x = seq(0, max(Mahalanobis_Distanz), 0.01), df = 15), col = "darkblue", lwd = 4) # Einzeichnen der Dichte

# Mittelwerte
colMeans(dataFR)

# Mardias Test
library(MVN)
mvn(data = dataFR, mvnTest = "mardia")


#### Appendix D -----------
# ML-EFA für den gesamten (gekürtzten) Datensatz

# Parallelanalyse
fa.parallel(x = dataFR,fa = "fa")

## ML-EFA
five_factor_ML <- fa(dataFR, nfactors = 5, rotate = "oblimin", fm = "ml")
five_factor_ML$STATISTIC
five_factor_ML$PVAL # Modell wird durch die Daten nicht verworfen

# Faktorladungen
five_factor_ML$loadings # auch nochmal ohne [,] um die Ausblendehilfe von psych als Unterstützung für die Zuordnung zu nutzen
five_factor_ML$loadings[,] # alle Dezimalstellen anzeigen


# Orthogonale Lösung
fa(dataFR, nfactors = 5, rotate = "varimax", fm = "ml")$loadings[,]

round(five_factor_ML$Phi, 2) # runde auf 2 Nachkommastellen
fa(dataFR, nfactors = 5, rotate = "varimax", fm = "ml")$Phi
diag(5) # Einheitsmatrix der Dimension 5x5 (Phi der orthognalen Lsg)


### ML-EFA Modellvergleiche
# Passen auch 4 Faktoren?
four_factor_ML <- fa(dataFR, nfactors = 4, rotate = "oblimin", fm = "ml")
four_factor_ML$STATISTIC
four_factor_ML$PVAL

anova(four_factor_ML, five_factor_ML)


# Passen auch 6 Faktoren?
six_factor_ML <- fa(dataFR, nfactors = 6, rotate = "oblimin", fm = "ml")
six_factor_ML$STATISTIC
six_factor_ML$PVAL # Modell wird durch die Daten nicht verworfen

anova(five_factor_ML, six_factor_ML)

anova(four_factor_ML, five_factor_ML, six_factor_ML) # alle Modellvergleiche in einem

