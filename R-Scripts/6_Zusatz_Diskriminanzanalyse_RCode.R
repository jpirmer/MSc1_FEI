#######################
###  Diskriminanzanalyse
# von Julien P. Irmer


### Daten laden ---
load(url("https://pandar.netlify.app/post/Therapy.rda"))

### Übersicht über die Daten ---
head(Therapy)
levels(Therapy$Intervention)
levels(Therapy$Geschlecht)

colnames(Therapy) # Spaltennamen ansehen
colnames(Therapy) <- c("LZ", "AB", "Dep", "AZ", "Intervention", "Geschlecht") # Spaltennamen neu zuordnen
head(Therapy)


### Pakete laden ---
library(MASS)
library(ggplot2)

### Analysen ---
model_DA <- lda(Intervention ~ LZ + Dep + AB + AZ, Therapy)
model_DA
model_DA$scaling # Koeffizienten


head(predict(model_DA)$posterior) # Wahrscheinlichkeit in jeweiliger Gruppe zu landen
head(predict(model_DA)$class)     # Vorhergesagte Klasse
head(predict(model_DA)$x)         # vorhergesagte Ausprägung auf jeweiliger Diskriminanzachse


# abspeichern der DAs
Therapy$DA1 <- predict(model_DA)$x[, 1] # erste DA
Therapy$DA2 <- predict(model_DA)$x[, 2] # zweite DA

ggplot(data = Therapy, aes(x = DA1, y = DA2, color = Intervention)) +
        geom_point()+
        geom_hline(yintercept = 0, lty = 3)+
        geom_vline(xintercept = 0, lty = 3)+
        ggtitle(label = "Diskriminanzachsen", subtitle = "mit Trennlinien")

plot(model_DA)

plot(model_DA, col = c(rep("red", 30), rep("gold3", 30), rep("blue", 30)))
abline(v = 0, lty = 3)
abline(h = 0, lty = 3)


# Mittelwerte auf den DAs
Means <- aggregate(cbind(DA1, DA2) ~ Intervention, data = Therapy, FUN = mean)

# Mittelwerte auf DA1
mDA1_K <- Means[1,2] # Kontrollgruppenmittelwert auf DA1
mDA1_V <- Means[2,2] # Mittelwert VT auf DA1
mDA1_VG <- Means[3,2] # Mittelwert VT + Gruppenuebung auf DA1

# Mittelwerte auf DA2
mDA2_K <- Means[1,3] # Kontrollgruppenmittelwert auf DA2
mDA2_V <- Means[2,3] # Mittelwert VT auf DA2
mDA2_VG <- Means[3,3] # Mittelwert VT + Gruppenuebung auf DA2

ggplot(data = Therapy, aes(x = DA1, y = DA2, color = Intervention)) + geom_point()+
        geom_hline(yintercept = mDA2_K, lty = 2, col = "red")+
        geom_hline(yintercept = mDA2_V, lty = 2, col = "gold3")+
        geom_hline(yintercept = mDA2_VG, lty = 2, col = "blue")+
        geom_hline(yintercept = (mDA2_VG+mDA2_K)/2, lty = 1, col = "black", lwd = 0.2)+
        geom_vline(xintercept = mDA1_K, lty = 2, col = "red")+
        geom_vline(xintercept = mDA1_V, lty = 2, col = "gold3")+
        geom_vline(xintercept = mDA1_VG, lty = 2, col = "blue")+
        geom_vline(xintercept = (mDA1_VG+mDA1_K)/2, lty = 1, col = "black", lwd = 0.2)+
        ggtitle(label = "Diskriminanzachsen", subtitle = "mit Mittelwerten pro Gruppe")


### Wie gut ist unsere Gruppenzuordnung? ---
Therapy$predict_class <- predict(model_DA)$class
table(Therapy$predict_class, Therapy$Intervention)
table(Therapy$predict_class, Therapy$Intervention)/30


mean(Therapy$predict_class == Therapy$Intervention)*100





### Trennlinien in ursprünglichen Variablenkoordinatensystem ---
model_DA2 <- lda(Intervention ~ LZ + AZ, data = Therapy)
model_DA2
model_DA2$scaling # Koeffizienten

# Ein Koordinatensystem erstellen von 0 bis 12 auf den beiden Variablen
contour_data <- expand.grid(LZ = seq(0,12, 0.01), AZ = seq(0,12,0.01))
contour_data

# Für das Koordinatensystem für jeden Punkt die Gruppenzugehörigkeit bestimmen
contour_data$Intervention <- as.numeric(predict(object = model_DA2, newdata = contour_data)$class)

head(contour_data$Intervention)

# Gruppenzugehörigkeiten in Originalkoordinatensystem einzeichnen
ggplot(data = Therapy, mapping = aes(x = LZ, y = AZ, color = Intervention))+
     geom_point()+
     stat_contour(aes(x = LZ, y = AZ, z = Intervention), data = contour_data)+
     ggtitle("Lebenszufriedenheit vs Arbeitszufriedenheit", subtitle = "inklusive retransformierter Entscheidungslinien\nabgeleitet von den Diskriminanzachsen")
