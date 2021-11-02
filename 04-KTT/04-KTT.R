###################################
# R Skript zum Übungskurs: 04 KTT # 
##################################ä

# Übungsaufgabe 1 ---------------------------------------------------------

# Alter
#

# Setup
# set.seed(123)
# rm(list = ls())
#
# Wahres Alter
true_age <- 28 #<------------------------------------------ verändere mich!

add_privacy <- function(true_age){
  hot <- sample(c(0,1), 1, replace = TRUE)
  ifelse(hot==1, true_age+1, true_age-1)
}

# Anzahl der Testwiederholungen 
M <- 10 #<------------------------------------------------ verändere mich!
reps <- replicate(M, add_privacy(true_age))
# Erwartungswert von X ; E(X) = T 
(E_X <- mean(reps))

# Waage
#

# Setup
# set.seed(123)
# rm(list = ls())
#
true_weight <- 67 #<---------------------------------------- verändere mich!
crazy_scale <- function(true_weight){
  # 5kg +/- 
  rnorm(1, true_weight, 5)
}
# Anzahl der Messwiederholungen
M <- 10 #<------------------------------------------ verändere mich!
reps <- replicate(M, crazy_scale(true_weight))
mean(reps)

# Übungsaufgabe 2 ---------------------------------------------------------

# Setup
# set.seed(123)
# rm(list = ls()) 
#
# Anzahl der Testwiederholungen 
M <- 10000 #<---------------------------------- verändere mich!
# True Score
T <- 100 
# Messfehler: E(E) = 0
E <- rnorm(M, 0, 10)
# Beobachtungswert 
X <- T + E 
# (Imaginäre) Wiederholung des Test 
reps <- replicate(M, sample(X, 1))

# Output(s)
#
# Intraindividuelle Merkmalsverteilung
hist(reps, 
     main = "Intraindividuelle Merkmalsverteilung (M = 10.000)", 
     xlab = "Testwerte", 
     ylab = "Häufigkeiten")
#     
# Erwartungswert des Individuums bei M Wiederholungen
# E(X) = T = 100 (?)
(E_X <- mean(reps))
#
# Ein zufällig gezogener Beobachtungswert
# .. aus der intraindividuellen Merkmalsverteilung
# .. ein Schätzer für den wahren Wert: 100
(X_i <- sample(X, 1))
cat("Beobachtungswert", X_i, "True Score", T)

# Übungsaufgabe 3 ---------------------------------------------------------

# Setup 
# set.seed(123)
# rm(list = ls())
#
# Populationsgröße 
N <- 10000 #<----------------------------------------------- verändere mich!
# Generierung der True Scores
# 100: Mittlere Intelligenz in der Population
# 30: Abweichungen vom Populationnsmittelwert
T <- round(rnorm(N, 100, 30), digits = 0)
# Anzahl der Testwiederholungen
M <- 5000 #<-------------------------------------------------- verändere mich!
X <- lapply(T, function(T) rnorm(M, T , 5)) ; names(X) <- T

# Outputs
#

# "Verteilung der wahren aber unbekannten Intelligenzwerte 
# ...in der Population
#
hist(T, 
     main = "Verteilung der True Scores", 
     sub =  paste("(N =", N, ")"),
     xlab = "True Scores", 
     ylab = "Häufigkeiten"
     )

# Interindividuelle Merkmalsverteilung
#
hist(rapply(X, mean), 
     main = "Interindividuelle Merkmalsverteilung", 
     sub = paste0("(N=", N, ", M=", M, ")"), 
     xlab = "Erwartungswerte",
     ylab = "Häufigkeiten")

# Mittelwert unserer Population
# ..zur Erinnerung: T = 100
#
mean(rapply(X, mean)) 

# Intraindividuelle Merkmalsverteilung 
# ..ziehe zufällig ein (i=1) Individuum aus der Population
#
i <- 1 
X_i <- sample(X, i)
hist(X_i[[1]],
     main = "Intraindividuelle Merkmalsverteilung",
     sub = paste0("(M=", M, ")"), 
     xlab = "Testwert",
     ylab = "Häufigkeitsverteilung der Erwartungswerte")

# Ziehe zufällig ein Testwert dieser Person 
# ..aus der intraindividuellen Merkmalsverteilung
(X_ij <- sample(X_i[[1]], 1)) #<--------------------------- mehrfach ausführen!
cat("Observed Score:", X_ij, "True Score:", names(X_i))

# Funktion 
# set.seed(123)
# rm(list = ls())
#
# Ziehe zufällig ein Testwert dieser Person 
# ..aus der intraindividuellen Merkmalsverteilung
rsample_i <- function(){
  # Ziehe zufällig ein Individuum 
  (X_i <- sample(X, 1)) 
  # Ziehe zufällig einen Testwert dieses Individuums 
  (X_ij <- sample(X_i[[1]], 1))
  cat(" True Score (T):", names(X_i), "\n", 
      "Testwert (X):", round(X_ij), "\n", 
      "Messfehler (E):", 
      abs(round(X_ij) - as.numeric(names(X_i))))}

# Ziehe zufällig ein Individuum aus der Population
rsample_i() #<------------------------------------------ mehrfach ausführen!

# Übungsaufgabe 5 ---------------------------------------------------------

# Setup
# set.seed(123)
# rm(list = ls())
#
# Populaitonsgröße
N <- 10000
tau <- rnorm(N, mean = 100, 30)
var_epsilon <- 25 # <-------------------------------------- verändere mich!
epsilon <- rnorm(N, 0, var_epsilon) 

# Funktion
# 
reliab <- function(tau, epsilon){
  rel <- var(tau) / var(tau + epsilon)
  cat("Reliabilität der Messung:", rel)
}

reliab(tau, epsilon) #<------------------------------- mehrfach ausführen!

# Exkurs ------------------------------------------------------------------

# Auswkirungen system. Störeinflüsse
# Korrelationen
# ...über alle Fälle hinweg

# Setup
# set.seed(123)
#
n <- 100
x <- rnorm(n)
y <- rnorm(x)
cor(x, y) ; cor(x+3, y) ; cor(x, y + 3) ; cor(x + 3, y + 3)

# Auswikrungen system. Störeinflüsse
# Korrelationen
# ...auf einige (die ersten 50) Probanden  

# Setup
# set.seed(123)
#
x[1:50] <- x[1:50] + 3
cor(x, y) 

# Zusätzlicher Messfehler
x[1:50] <- x[1:50] + 6
cor(x, y) 

# Auswirkungen system. Störeinflüsse
# Regressionsgewichte 
# ...über alle Fälle hinweg

# Setup
# set.seed(123)
#
n <- 100
x <- rnorm(n)
y <- rnorm(x)
# Original result
lm(y ~ x)$coef[[2]] 
# Veränderter Prediktor 
beta_ast <- lm(y ~ I(x+3))$coef[[2]] 
# Manipuliertes Ergebnis
lm(I(y+3) ~ I(x))$coef[[2]] 

# Auswirkungen system. Störeinflüsse
# Regressionsgewichte 
# ...auf einige (die ersten 50) Probanden  

# Setup
# set.seed(123)
#
n <- 100
x <- rnorm(n)
y <- rnorm(x)
x[1:50] <- x[1:50] + 3
beta <- lm(y ~ x)$coef[[2]] 
# Veränderter Prediktor 
x[1:50] <- x[1:50] + 6
# Manipuliertes Ergebnis
lm(y ~ x)$coef[[2]] 
