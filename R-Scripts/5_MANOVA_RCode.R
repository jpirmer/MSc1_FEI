#######################
###  MANOVA
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
library(heplots) # für Box-M Test für Kovarianzhomogenität

### Hypothese 1 ---
# Kovarianzhomogenität
boxM(cbind(LZ, AB, Dep, AZ) ~ Intervention, data = Therapy)

round(cov(Therapy[Therapy$Intervention == "Kontrollgruppe", 1:4]),2)
round(cov(Therapy[Therapy$Intervention == "VT Coaching", 1:4]),2)
round(cov(Therapy[Therapy$Intervention == "VT Coaching + Gruppenuebung", 1:4]),2)


manova1 <- manova(cbind(LZ, AB, Dep, AZ) ~ Intervention, 
                  data = Therapy)
summary(manova1, test = "Wilks")

# Wilks Lambda zu Fuß
sum_manova1 <- summary(manova1, test = "Wilks")
names(sum_manova1) # mögliche Argumente 

names(sum_manova1$SS) # mögliche Argumente
sum_manova1$SS # B und W !
B <- sum_manova1$SS$Intervention # B-Matrix
W <- sum_manova1$SS$Residuals  # W-Matrix

det(W)/(det(B + W)) # Wilks Lambda

#### Wie sehen die Mittelwerte aus?
aggregate(cbind(LZ, AB, Dep, AZ) ~ Intervention, 
          data = Therapy, 
          FUN = mean)



### Hypothese 2 ---

summary.aov(manova1) # post hoc anovas

anovaLZ <- aov(LZ ~ Intervention, data = Therapy)
summary(anovaLZ)

## t-Tests
pairwise.t.test(x = Therapy$LZ, g = Therapy$Intervention, p.adjust.method = "none")
pairwise.t.test(x = Therapy$Dep, g = Therapy$Intervention, p.adjust.method = "none")
pairwise.t.test(x = Therapy$AZ, g = Therapy$Intervention, p.adjust.method = "none")

## Tuckey HSD
TukeyHSD(aov(LZ ~ Intervention, data = Therapy)) # Tukey HSD für LZ
# als Plot
tukeyLZ <- TukeyHSD(aov(LZ ~ Intervention, data = Therapy))
plot(tukeyLZ, las = 1)
TukeyHSD(aov(Dep ~ Intervention, data = Therapy)) # Tukey HSD für Dep
plot(TukeyHSD(aov(Dep ~ Intervention, data = Therapy)), las = 1) # Tukey HSD-Plot für Dep
TukeyHSD(aov(AZ ~ Intervention, data = Therapy)) # Tukey HSD für AZ
plot(TukeyHSD(aov(AZ ~ Intervention, data = Therapy)), las = 1) # Tukey HSD-Plot für AZ

 
### Normalverteilung der Residuen ---
MD <- mahalanobis(resid(manova1), center = colMeans(resid(manova1)), cov = cov(resid(manova1)))
hist(MD, breaks = 20, col = "skyblue", border = "blue", freq = F, main = "Mahalnobisdistanz vs Chi2(4) Verteilung",
     xlab = "Mahalanobisdistanz")
xWerte <- seq(from = min(MD), to = max(MD), by = 0.01)
lines(x = xWerte, y = dchisq(x = xWerte, df = 4), lwd = 3, col = "blue")



### Hypothese 3 ---
manova3 <- manova(cbind(LZ, AB, Dep, AZ) ~ Intervention + Geschlecht, 
                  data = Therapy)
summary(manova3, test = "Wilks")

aggregate(cbind(LZ, AB, Dep, AZ) ~ Intervention + Geschlecht, 
          data = Therapy,
          FUN = mean)

# Interaktion
manova3b <- manova(cbind(LZ, AB, Dep, AZ) ~ Intervention + Geschlecht + Intervention:Geschlecht, 
                   data = Therapy)
summary(manova3b, test = "Wilks")

# Post Hoc ANOVAS
summary.aov(manova3)



## Appendix ---
### Appendix A: `R`-Code zu den Grafiken 
library(ggplot2)
Therapy_long <- reshape(data = Therapy, varying = names(Therapy)[1:4],idvar = names(Therapy)[5:6],
                        direction = "long", v.names = "AVs", timevar = "Variable", new.row.names = 1:360)

Therapy_long$Variable[Therapy_long$Variable == 1] <- "Lebenszufriedenheit"
Therapy_long$Variable[Therapy_long$Variable == 2] <- "Arbeitsbeanspruchung"
Therapy_long$Variable[Therapy_long$Variable == 3] <- "Depressivitaet"
Therapy_long$Variable[Therapy_long$Variable == 4] <- "Arbeitszufriedenheit"


ggplot(Therapy_long, aes(x = Intervention, y = AVs,  group = Variable, col = Variable))+ 
     stat_summary(fun.data = mean_se)+stat_summary(fun.data = mean_se, geom = c("line"))



               