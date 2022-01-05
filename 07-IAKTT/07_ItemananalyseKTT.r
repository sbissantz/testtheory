############### Itemanalyse nach der KTT #################
## WiSe2021_22

# Pakete installieren ####
## Hinweis/Wiederholung: Pakete installieren brauchen Sie in der Regel nur
## einmal pro PC, danach sind sie installiert,
## und müssen nur noch geladen werden (s. nächster Punkt)

# install.packages("psych")
# install.packages("car")
# install.packages("dplyr")

# Pakete laden ####
## Hinweis: Es ist immer sinnvoll zu Beginn seiner Analyse alle Pakete zu laden, 
## die für eine Analyse erforderlich sind

library(psych) ;  library(car) ; library(dplyr)

# Itemanalyse ####
## 0. Vorarbeit: Kordierung der Itemantworten ####
## In dem Paket psych ist ein Datensatz bereits vorhanden (bfi)
## Was wurde gemessen? Schauen Sie sich die Variablenbezeichnungen an
## oder rufen sie unter help "bfi" auf.
bfi <- psych::bfi 
head(bfi)

## Wir fokussieren uns nur auf die Items E1, E2, E3, E4, E5 (Extraversion)
(extra <- subset(bfi, select = E1:E5)) 
# View(extra)

### Negativ gepolte Items rekodieren ####
##Gibt es negativ gepolte Items?
psych::alpha(extra, check.keys = TRUE)

extra$E1_r <- 7 - extra$E1 
# cbind(extra$E1, extra$E1_r, extra$E1 + extra$E1_r)

extra$E2_r <- 7 - extra$E2 
# cbind(extra$E1, extra$E1_r, extra$E1 + extra$E1_r)

#psych::alpha(extra[, -(1:2)], check.keys = TRUE)

### Alternativen zum Rekodieren, die Sie zu Hause ausprobieren können ###
## Eine Alternative mit dem package "car"
## Denken Sie daran, das Package car vorher zu laden (zu Beginn des Skripts ist es auskommentiert)
# extra$E1_r <- dplyr::recode(extra$E1, '1 = 6; 2 = 5; 3 = 4; 4 = 5; 5 = 6')
# extra$E2_r <- dplyr::recode(extra$E2, '1 = 6; 2 = 5; 3 = 4; 4 = 5; 5 = 6')

## Eine weitere Alternative (für simultane Rekodierung) mit einer Funktion aus dem Package dplyr
# Denken Sie daran, das Package dplyr vorher zu laden (zu Beginn des Skripts ist es auskommentiert)
#extra_r <- extra %>% dplyr::mutate_at(c("E1", "E2"), funs(recode(., '1' = 6, '2'=5, '3'=4, '4'=3, '5'=2, '6'=1, .default = NaN)))

## 1 Itemverteilung ####
## 1.1 Graphisch: Säulendiagramm und Box-Plot

## Histogramme & Boxplots für einzelne E-Items
## Welche Antwortalternative wurde am häufigsten und welche am seltensten gewählt?
hist(extra$E1, main = "E1: Ich bin jemand, der nicht viel spricht.")
boxplot(extra$E3, col = "darkcyan", main = "E3: Ich bin jemand, der es versteht, Menschen zu begeistern.")
# plot(density(na.omit(extra$E1)))

# par(mfrow=c(1,2))
hist(extra$E3, main = "E1: Ich bin jemand, der nicht viel spricht.")
boxplot(extra$E3, col = "darkcyan", main = "E3: Ich bin jemand, der es versteht, Menschen zu begeistern.")
# par(mfrow=c(1,1))

# par(mfrow=c(1,3))
hist(extra$E1_r, col = "darkcyan", main = "E1_r: Ich bin jemand, der nicht viel spricht.")
# plot(density(na.omit(extra$E1_r)), add=TRUE)
boxplot(extra$E1_r)
# par(mfrow=c(1,1))

hist(extra$E5, col = "darkcyan", main = "Ich bin jemand, der Verantwortung übernimmt.")
boxplot(extra$E5)
# plot(density(na.omit(extra$E5)))

## Boxplots für einzelne E-Items und für alle 5 Items gemeinsam
boxplot(extra)

## 1.2 Statistisch: Maße der zentralen Tendenz + Streeungsmaße
## Wir bezeiehn ab hier nur noch die rekodierten Extraversions-Items ein 
## (und entfernen die urprünglichen E-Items E1 und E2)
extra <- subset(extra, select = c("E1_r", "E2_r", "E3", "E4", "E5"))
head(extra)

## Übersicht ueber die deskriptiv-statistischen Maße
summary(extra) 
psych::describe(extra, quant=c(.25,.75))

## Funktionen fuer separate Berechnungen der statistischen Kennwerte
# Varianz
var(extra$E1, na.rm = TRUE) 
# sapply(extra, var, na.rm=TRUE)

# Mean 
mean(extra$E1, na.rm = TRUE)
# sapply(extra, mean, na.rm=TRUE)

# Median
median(extra$E1, na.rm = TRUE)
# sapply(extra, median, na.rm=TRUE)

# Standardabweichung 
sd(extra$E1, na.rm = TRUE)
# sapply(extra, sd, na.rm=TRUE)

# Modus
# which.max(table(extra$E1)) 
(ftab <- table(extra$E1))
max(ftab)

# (2) Itemschwierigkeit
### bei Rating Skalen
### Mittelwert als Schwierigkeitsindex bei Persoenlichkeitsfrageboegen
(schwierigkeit_mw <- colMeans(extra, na.rm =TRUE))

## (3) Trennscharfe
(results <- alpha(extra, na.rm = TRUE))
## Hinsweis: Wenn Sie sich nur die Trennschaerfekoeffizienten ausgeben lassen wollen
# Übersetzung: Zeige die Itemstatistiken (item.stats) raw.r und r.drop 
# results$item.stats[c("raw.r","r.drop")]                        

## Add noise
# set.seed(123)
# noise <- runif(100,0,6)
# alpha(cbind.data.frame(extra, noise), check.keys = TRUE)

## (4) Faktorladungen (Erinnerung an Sitzung zur EFA): 
(extra_efa <- fa(extra, fm="pa", nfactors = 1, rotate="oblimin"))
extra_efa