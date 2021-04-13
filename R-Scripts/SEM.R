#######################
### Pfadanalyse und SEM
# von Julien P. Irmer

# Pakete laden ----
library(lavaan)
library(semPlot)


# Daten laden und Überblick verschaffen ----
load(url("https://pandar.netlify.app/post/StressAtWork.rda"))
head(StressAtWork)
names(StressAtWork)
dim(StressAtWork)

# Pfadanalyse ----
## Skalenmittelwerte bilden
StressAtWork$ZDs <- rowMeans(StressAtWork[,paste0("zd",c(1, 2, 6))])
StressAtWork$BOEEs <- rowMeans(StressAtWork[,paste0("bo",c(1, 6, 12, 19))])
StressAtWork$BFs <- rowMeans(StressAtWork[,paste0("bf",1:20)])

# Modelle in sem und lavaan Schreibweise
model_paths <- '
BOEEs ~ ZDs
BFs ~  BOEEs + ZDs
'

model_paths_lavaan <- '
BOEEs ~ ZDs
BFs ~  BOEEs + ZDs
BOEEs ~~ BOEEs
BFs ~~ BFs
'

# Pfadmodell schätzen mit sem
fit_paths <- sem(model_paths, data = StressAtWork)
summary(fit_paths, rsq = T, fit.measures = T)

### Grafische Veranschaulichung des Modells und der Ergebnisse
semPaths(fit_paths)
semPaths(fit_paths, what = "est")

semPaths(object = fit_paths, what = "model", layout = "tree2", rotation = 2,
         col = list(man = "skyblue"),  edge.label.cex=1, sizeMan = 5)

semPaths(object = fit_paths, what = "est", layout = "tree2", rotation = 2,
         col = list(man = "skyblue"),  edge.label.cex=1, sizeMan = 5)

### Berechnen und Testen des indirekten Effektes von Zeitdruck über emotionale Erschöpfung auf psychosomatische Beschwerden
model_paths_abc <- '
BOEEs ~ a*ZDs
BFs ~  b*BOEEs + c*ZDs
'
fit_paths_abc <- sem(model_paths_abc, data = StressAtWork)
summary(fit_paths_abc)

# totalen und indirekten Effekt hinzufügen
model_paths_IE_TE <- '
BOEEs ~ a*ZDs
BFs ~  b*BOEEs + c*ZDs

# Neue Parameter
IE := a*b
TE := IE + c
'
fit_paths_IE_TE <- sem(model_paths_IE_TE, data = StressAtWork)
summary(fit_paths_IE_TE)

### Bootstrapping
set.seed(1234)
fit_paths_IE_TE_boot  <- sem(model_paths_IE_TE, data = StressAtWork, se = "boot", bootstrap = 1000)
parameterEstimates(fit_paths_IE_TE_boot, ci = TRUE)

semPaths(object = fit_paths_IE_TE, what = "model", layout = "tree2", rotation = 2,
         col = list(man = "skyblue"),  edge.label.cex=1)


# Strukturgleichungsmodelle ----
model_sem_IE_TE <- '
# Messmodelle
ZD =~ zd1 + zd2 + zd6
BOEE =~ bo1 + bo6 + bo12 + bo19

# Strukturmodell
BOEE ~ a*ZD
BFs ~  b*BOEE + c*ZD

# Neue Parameter
IE := a*b
TE := IE + c
'

fit_sem_IE_TE <- sem(model_sem_IE_TE, StressAtWork)

### Berechnen und Testen des indirekten Effektes von Zeitdruck über emotionale Erschöpfung auf psychosomatische Beschwerden im SEM
set.seed(12345)
fit_sem_IE_TE_boot  <- sem(model_sem_IE_TE, data = StressAtWork, se = "boot", bootstrap = 1000)
parameterEstimates(fit_sem_IE_TE_boot, ci = TRUE)

### Grafische Repräsentation
semPaths(object = fit_sem_IE_TE,  what = "model", layout = "tree2",
         rotation = 2, curve = T, col = list(man = "skyblue", lat = "yellow"),
         curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)


semPaths(object = fit_sem_IE_TE, what = "est", layout = "tree2",
         rotation = 2, curve = T, col = list(man = "skyblue", lat = "yellow"),
         curvePivot = T,  edge.label.cex=1.2, sizeMan = 5, sizeLat = 8)

### Modellpassung
library(ezCutoffs)
ezCutoffs(model = model_sem, data = StressAtWork)