#######################
### Principal Component Analysis
# von Julien P. Irmer




### Daten und Pakete laden --- 
load(url("https://github.com/jpirmer/MSc1_FEI/blob/master/data/PCA.RData?raw=true"))

library(psych)     # Datenaufbereitung und -diagnostik
library(corrplot)  # Korrelationsmatrixgrafiken


### Überblick über die Daten und einfache Deskriptivstatistiken
head(data)
head(dataUV)

round(head(data),2) # noch ein Überblick: diesmal auf 2 Nachkommastellen gerundet
# Mittelwerte der Daten
round(apply(X = data, MARGIN = 2, FUN = mean), 10) # identisch zu "colMeans(data)"
# SD der Daten
round(apply(X = data, MARGIN = 2, FUN = sd), 10)   

## Berechnen der Korrelationsmatrix
cor(dataUV) # Korrrelationsmatrix
cov(dataUV) # Kovarianzmatrix

R_UV <- cor(data[,1:6])


### Grafische Veranschaulichung der Korrelationsmatrix
corrplot(R_UV)

## PCA mit der Funktion `pca` des psych-Pakets
PCA1 <- pca(r = R_UV, nfactors = 6, rotate = "none")
PCA1 

names(PCA1)

PCA1$Vaccounted

PCA1$loadings
          
PCA1$loadings[,] # um alle zu sehen

round(PCA1$loadings, 1)
PCA1$Vaccounted

diag(t(PCA1$loadings[,]) %*% PCA1$loadings[,]) 

(PCA1$loadings[,]) %*% t(PCA1$loadings[,]) # Resultiernde Korrelationsmatrix (hier ursprüngliche)


PCA1$weights

round(t(PCA1$weights) %*% PCA1$weights, 3) 

round(t(PCA1$weights) %*% PCA1$weights %*% diag(PCA1$values), 10)


## Bestimmung der Komponentenzahl ##
fa.parallel(dataUV, fa = "pc") 
# Eigenwerte größer 1?
abline(h = 1) 

## PCA mit zwei 2 Hauptkomponenten ohne Rotation ##
PCA2 <- pca(r = R_UV, nfactors = 2, rotate = "none")
PCA2


### Grafische Veranschaulichungen des Ladungsmusters
barplot(PCA2$loadings, beside = T, names.arg = rep(colnames(dataUV),2), 
        xlab = "PC1                   -                    PC2", 
        main  = "Ladungsmuster der Variablen \n auf den Hauptkomponenten")


plot(PCA2, pch = 1)

## PCA mit zwei 2 Hauptkomponenten mit Varimax-Rotation ##
PCA3 <- pca(r = R_UV, nfactors = 2, rotate = "varimax")
PCA3



### Grafische Veranschaulichungen des Ladungsmusters
barplot(PCA3$loadings, beside = T, names.arg = rep(colnames(dataUV),2), 
        xlab = "PC1                   -                    PC2", 
        main  = "Ladungsmuster der Variablen \n auf den Hauptkomponenten")

plot(PCA3, cex = 2)

fa.diagram(PCA3)

### Unrotierte und rotierte Ladungen in einem Plot
plot(PCA3, xlim = c(-1,1),ylim = c(-1,1), cex  = 2)
par(new=TRUE)
plot(PCA2, xaxt = "n", yaxt = "n", ylab = "", xlab = "", xlim = c(-1,1),ylim = c(-1,1), pch = 1)


## Bilden der Hauptkomponenten als neue Variablen
PCs <- as.matrix(dataUV) %*% PCA3$weights

cor(dataUV, PCs)
PCA3$loadings[,]

plot(PCs)
round(cor(PCs), 10)



## Nutzung der Hauptkomponenten als Prädiktoren in der Regression
mx <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = data)
summary(mx)

mpca <- lm(data$y ~ PCs[,1] + PCs[,2])
summary(mpca)


## Appendix
### Appendix A {#AppendixA}
corrplot(R_UV, method = "color",tl.col = "black", addCoef.col = "black",
         col=colorRampPalette(c("red","white","blue"))(100))


corrplot(R_UV, method = "color",tl.col = "black", addCoef.col = "black",
         col=colorRampPalette(c("blue","white","blue"))(100))


barplot(PCA2$loadings, beside = T, names.arg = rep(colnames(dataUV),2), 
        xlab = "PC1                   -                    PC2", 
        main  = "Ladungsmuster der Variablen \n auf den Hauptkomponenten")



barplot(PCA3$loadings, beside = T, names.arg = rep(colnames(dataUV),2), 
        xlab = "PC1                   -                    PC2", 
        main  = "Ladungsmuster der Variablen \n auf den Hauptkomponenten")


plot(PCA3, xlim = c(-1,1),ylim = c(-1,1), cex  = 2)
par(new=TRUE)
plot(PCA2, xaxt = "n", yaxt = "n", ylab = "", xlab = "", xlim = c(-1,1),ylim = c(-1,1), pch = 1)



### Appendix B {#AppendixB}
#### PCA zu Fuß {#PCAzuFuss}
eigen(cor(dataUV))

Gamma <- eigen(R_UV)$vectors # Eigenvektoren
theta <- eigen(R_UV)$values # Eigenwerte

# Plot der Eigenwerte
plot(theta, type="l", ylab = "Eigenwert", xlab = "Hauptkomponente")

Lambda <- Gamma %*% diag(sqrt(theta)) # sqrt zieht die Wurzel!
Lambda
PCA1$loadings[,] # Vergleich mit den Ladungen aus der pca-Funktion




### Exkurs: Was passiert bei linearen Abhängigkeiten?
dataUV$X <- rowMeans(dataUV)
R2 <- cor(dataUV)
round(R2,2)

eigen(R2)$values
plot(eigen(R2)$values, type="l", ylab = "Eigenwert", xlab = "Hauptkomponente")
abline(h=0, col="red")
solve(R2)


