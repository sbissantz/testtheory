####### Itemanalysen nach der IRT ########
#Pakete laden
library(mirt)

#Arbeitsverzeichnis
setwd("~/testtheory/08-IAIRT/")

#Datensatz einlesen
data <- foreign::read.spss("WichertsBakker2012.sav", to.data.frame = T)

#Ueberblick verschaffen
head(data) 
# str(data)
# View(data)

#nur die Variablen: VP-Nummer, Geschlecht, Sprache, Ravens Matrizentest
dat <- subset(data, select = c(subjnr, sex, language, rav1:rav36))
head(dat)
# str(dat)
# View(dat)

## Itemverteilung
summary(dat)
# lapply(dat, mean, na.rm=TRUE)
#Mittelwerte der Items plotten 
x_axis <- 1:36 ; y_axis <- colMeans(dat[,4:39], na.rm = TRUE)
plot(x_axis, y_axis, pch=19, ylim=c(-.1,1.1),  
     xlab="Raven-Items",ylab="Itemmittelwerte")
abline(h=c(0,1),lty="dotted")
lbl <- colnames(dat[,4:39]) ; 
# Spielerei
text(x_axis, y_axis + 0.05, lbl, cex=.5)

# Rasch-Modell schaetzen
rm1 <- mirt::mirt(data = dat[,4:39], model = 1, itemtype = "Rasch")
#! Fehlermeldung beachten !#

#haben Personen kein einziges Item bearbeitet?
(pat <- rowSums(is.na(dat[,4:39]))<36)
df <- subset(dat, subset =  pat)

# Rasch-Modell erneut schaetzen
rm1 <- mirt(data = df[,4:39], model = 1, itemtype = "Rasch")
#! Fehlermeldung beachten !#

# Rasch-Modell erneut ohne Item 36 schaetzen
rm1 <- mirt(data = df[,4:38], model = 1, itemtype = "Rasch")
# Keine Fehlermeldung?! - Juhuu

# Trotzdem: Konvergenz ueberpruefen
print(rm1)

## Itemschwierigkeit (1.Konzept)
coef(object = rm1, simplify = TRUE, IRTpars = TRUE)$items
# rav1: easiest -- rav35:hardest - occular inspection:
itemplot(rm1, item = 1, type = "trace") #ICC
itemplot(rm1, item = 35, type = "trace") #ICC
# ..and another one
itemplot(rm1, item = 26, type = "trace") #ICC

## Iteminformation (2.Konzept)
itemplot(rm1, item = 1, type = "info")
itemplot(rm1, item = 35, type = "info")
# ..and another one
itemplot(rm1, item = 26, type = "info") #ICC

## Testinformation (3.Konzept)
theta_range <- seq(-6, 6, .1)
testinfo <- testinfo(rm1, theta_range)
plot(theta_range, testinfo, type = 'l', main = 'Testinformation', col="blue")

# Joke: the perfect test
curve(dunif(x, min=-6, max=6), from=-6.2, to=6.2, lwd=2, yaxt="n",
      xlab="theta_range", ylab="testinfo")

## Itemfit
#   Vorgehen:
#    #1#  Bildung von Gruppen (Median, Geschlecht, Alter, ...)
#    #2#  RM in jeder Gruppe schaetzen
#    #3#  Fuer alle Items: Vergleich der Itemparameter zwischen den Gruppen
#         Nullhypothese: Die Itemparameter unterscheiden sich zwischen den Gruppen nicht.

#Rasch-Modell mit dem eRm Paket schaetzen
(rm2 <- eRm::RM(df[,4:38], se=TRUE))

#Gruppenbildung: Sprache (Matrizentests sind sprachfreie IQ-Tests)
table(df$language) #Native Dutch Speaker?

eRm::Waldtest(rm2, splitcr = df$language)
# Visualize!
# wald_test <- eRm::Waldtest(rm2, splitcr = df$language)
# p_values <- wald_test$coef.table[,2]
# plot(seq(p_values), p_values, xaxt="n",xlab="item", pch=20)
# axis(side=1, at=seq(p_values), cex.axis=.7)
# abline(h=0.05, col="red", lty=2)
# lbl <- colnames(dat[,4:39])
# text(seq(p_values), p_values + 0.04, lbl, cex=.5)

