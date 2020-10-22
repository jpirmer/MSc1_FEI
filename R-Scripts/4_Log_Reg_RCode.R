


### Daten laden ---
load(url("https://pandar.netlify.app/post/Titanic.rda"))

### Übersicht über die Daten
head(Titanic)

### Hypothese 1: Alter als Prädiktor --- 
#### Lineares Modell: Einfache Regressionsanalyse ---

reg_model <- lm(survived ~ 1 + age, data = Titanic)
summary(reg_model)


# Diagnostika
library(car) # nötiges Paket laden
avPlots(model = reg_model, pch = 16)
residualPlots(model = reg_model, pch = 16)

library(MASS)# nötiges Paket laden
res <- studres(reg_model) # Studentisierte Residuen als Objekt speichern
hist(res, freq = F)
xWerte <- seq(from = min(res), to = max(res), by = 0.01)
lines(x = xWerte, y = dnorm(x = xWerte, mean = mean(res), sd = sd(res)), lwd = 3)


### Generalisiertes Lineares Modell: Logistische Regressionsanalyse ---
m1 <- glm(survived ~ 1 + age, family = "binomial", data = Titanic)
summary(m1)


### Beispielgrafiken: Alterseffekt ---
AltersWerte <- seq(-500, 500, 0.1)
logit <- m1$coefficients[1] + m1$coefficients[2]*AltersWerte
plot(x = AltersWerte, y = logit, type = "l", col = "blue", lwd = 3)

odds <- exp(logit)
plot(x = AltersWerte, y = odds, type = "l", col = "blue", lwd = 3)

p <- odds/(1 + odds)
plot(x = AltersWerte, y = p, type = "l", col = "blue", lwd = 3)

### Hypothese 2 ---
is.factor(Titanic$sex)

table(Titanic$survived, Titanic$sex)

m2 <-  glm(survived ~ 1 + age + sex, family = "binomial", data = Titanic)
summary(m2)

Titanic$sex
levels(Titanic$sex)

#### Ergebnisinterpretation
exp(m2$coefficients) # Odds-Ratios

#### Grafische Veranschaulichung mit ggplot2
library(ggplot2)

logit_m2 <- predict(m2)
odds_m2 <- exp(logit_m2)
p_m2 <- odds_m2/(1 + odds_m2)

# dem Datensatz anhängen:
Titanic$logit_m2 <- logit_m2
Titanic$odds_m2 <- odds_m2
Titanic$p_m2 <- p_m2

head(Titanic)

ggplot(data = Titanic, mapping = aes(x = age, y = logit_m2, col = sex)) + 
     geom_line(lwd = 2) + 
     ggtitle("Logit vs Age and Sex")

ggplot(data = Titanic, mapping = aes(x = age, y = odds_m2, col = sex)) + 
     geom_line(lwd = 2) + 
     ggtitle("Odds vs Age and Sex")

ggplot(data = Titanic, mapping = aes(x = age, y = p_m2, col = sex)) + 
     geom_line(lwd = 2) + 
     ggtitle("P vs Age and Sex")


### Hypothese 3 ---
levels(Titanic$pclass)

m3 <-  glm(survived ~ 1 + age + sex + pclass, family = "binomial", data = Titanic)
summary(m3)


#### Modellvergleich
anova(m2, m3, test = "LRT")

#### Ergebnisinterpretation
exp(m3$coefficients) # Odds-Ratio

#### Grafische Veranschaulichung

logit_m3 <- predict(m3)
odds_m3 <- exp(logit_m3)
p_m3 <- odds_m3/(1 + odds_m3)

# dem Datensatz anhängen:
Titanic$logit_m3 <- logit_m3
Titanic$odds_m3 <- odds_m3
Titanic$p_m3 <- p_m3

head(Titanic)


# Trick 17: Kodierung
class_sex <- as.numeric(as.character(Titanic$sex))*100 + as.numeric(as.character(Titanic$pclass))
Titanic$class_sex <- as.factor(class_sex) # als Faktor dem Datensatz hinzufügen
head(Titanic)

ggplot(data = Titanic, mapping = aes(x = age, y = logit_m3, col = class_sex)) + 
     geom_line(lwd = 2) + 
     ggtitle("Logit vs Age, Sex and Class")

ggplot(data = Titanic, mapping = aes(x = age, y = odds_m3, col = class_sex)) + 
     geom_line(lwd = 2) + 
     ggtitle("Odds vs Age, Sex and Class")

ggplot(data = Titanic, mapping = aes(x = age, y = p_m3, col = class_sex)) + 
     geom_line(lwd = 2) + 
     ggtitle("P vs Age, Sex and Class")

     
## Appendix ---
### Appendix A: Parametereinflüsse 
Logistic_functions <- function(beta0 = 0, beta1 = 1)
{
     par(mfrow=c(2,2)) # 4 Grafiken in einer
     
     xWerte <- seq(-5, 5, 0.1)
     logit <- beta0 + beta1*xWerte
     plot(x = xWerte, y = logit, type = "l", col = "blue", lwd = 3, main = "Logit vs X", xlab = "X")
     lines(xWerte, xWerte, col = "skyblue")
     abline(h = 0, lty = 3); abline(v = 0, lty = 3)
     
     odds <- exp(logit)
     plot(x = xWerte, y = odds, type = "l", col = "blue", lwd = 3, main = "Odds vs X", xlab = "X")
     abline(h = 0, lty = 3); abline(v = 0, lty = 3)
     lines(xWerte, exp(xWerte), col = "skyblue")
     
     
     
     p <- odds/(1 + odds)
     plot(x = xWerte, y = p, type = "l", col = "blue", lwd = 3, main = "P vs X", ylim = c(0,1), xlab = "X")  
     lines(xWerte, exp(xWerte)/(1 + exp(xWerte)), col = "skyblue")
     abline(h = 0, lty = 3); abline(v = 0, lty = 3)
     
     set.seed(1234) # Vergleichbarkeit
     Y <- rbinom(n = length(xWerte), size = 1, prob = p)
     plot(x = xWerte, y = p, type = "l", col = "blue", lwd = 3, main = "P vs X und zufällige Realisierungen",
          ylim = c(0,1), xlab = "X", ylab = "P und Y")
     abline(h = 0, lty = 3); abline(v = 0, lty = 3)
     points(x = xWerte, y = Y, pch = 16, cex = .5, col = "red")
}


Logistic_functions(beta0 = 1, beta1 = -.5)



 
          