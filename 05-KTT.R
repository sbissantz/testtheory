###################################
# R Skript zum Übungskurs: 05 KTT #
##################################ä

# Übungsaufgabe 1 ---------------------------------------------------------

# Setup
# set.seed(123)
# rm(list = ls())
#
# Ihr wahres Alter
true_age <- 28 #<------------------------------------------ Verändere mich!
# Sicherheitsfeature
add_privacy <- function(true_age){
  # heads or tails
  hot <- sample(c(0,1), 1, replace = TRUE)
  ifelse(hot==1, true_age+1, true_age-1)
}
# Anzahl der Testwiederholungen 
M <- 10 #<------------------------------------------------ Verändere mich!
# Wiederholung der Messung: M mal 
reps <- replicate(M, add_privacy(true_age))
# Erwartungswert von X: 
# ..es sollte gelten: E(X) = T
(E_X <- mean(reps))

# Übungsaufgabe 2 ---------------------------------------------------------

# Setup
# set.seed(123)
# rm(list = ls()) 
#
# unzählige Male
M <- 1e4 #<-------------------------------------------------- Verändere mich!
# True Score: i (z.B. Intelligenztest)
T <- 100 
# random error; Schwankungsbreite +/- 10
E <- rnorm(M, 0, 10)
# Measurements
X <- T + E 
# Imaginäre Wdh der Messung 
reps <- replicate(M, sample(X, 1))

# Output(s)
#
# Intraindividuelle Merkmalsausprägung 
hist(reps, 
     main = "Intraindividuelle Merkmalsverteilung", 
     xlab="Messwiederholungen", 
     ylab="Häufigkeiten: Erwartungswerte")
#
# Ein zufällig gezogener Beobachtungswert 
(X_i <- sample(X, 1))
#
# Erwartungswert des Individuums bei M Wiederholungen
# ..es sollt gelten: T = 100 = E(X) 
(E_X <- mean(reps))

# Übungsaufgabe 3 ---------------------------------------------------------

# Setup 
# set.seed(123)
# rm(list = ls())
#
# Populationsgröße 
N <- 1e4 #<-------------------------------------------------- Verändere mich!
# Generierung der True Scores
# 100: Mittlere Intelligenz in der Population
# 30: Abweichungen vom Populationnsmittelwert
T <- round(rnorm(N, 100, 30), digits = 0)
# Anzahl der Testwiederholungen
M <- 5000 #<-------------------------------------------------- Verändere mich!
X <- lapply(T, function(T) rnorm(M, T , 5)) ; names(X) <- T

# Outputs
#

# "Verteilung der wahren aber unbekannten Intelligenzwerte in der Population
hist(T, 
     main = "Verteilung der True Scores", 
     xlab = "True Scores", 
     ylab = "Häufigkeiten: True Scores"
     )

# Interindividuelle Merkmalsverteilung
hist(rapply(X, mean), 
     main = "Interindividuelle Merkmalsverteilung", 
     xlab = "EW bei M Intelligenztests",
     ylab = "Häufigkeiten: Erwartungswerte")

# Mittelwert unserer Population
# ..zur Erinnerung T war 100
mean(rapply(X, mean)) 

# Intraindividuelle Merkmalsverteilung 
# ..ziehe zufällig ein Individuum aus der Population
X_i <- sample(X, 1) 
hist(X_i[[1]],
     main = "Intraindividuelle Merkmalsverteilung",
     xlab = "EW bei M Intelligenztests",
     ylab = "Häufigkeiten: Erwartungswerte")

# Ziehe zufällig ein Testwert dieser Person 
# ..aus der intraindividuellen Merkmalsverteilung
X_ij <- sample(X_i[[1]], 1)
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
rsample_i() #<-------------------------------------------------- Ausführen!

# Übungsaufgabe 5 & 6 -----------------------------------------------------

## empty

# Übungsaufgabe 7 ---------------------------------------------------------

# Setup
# set.seed(123)
# rm(list = ls())
#
# Populaitonsgröße
N <- 1e4
tau <- rnorm(N, mean = 100, 30)
var_epsilon <- 25 # <-------------------------------------- Verändere mich!
epsilon <- rnorm(N, 0, var_epsilon) 

# Funktion
# 
reliab <- function(tau, epsilon){
  rel <- var(tau) / var(tau + epsilon)
  cat("Reliabilität der Messung:", rel)
}

reliab(tau, epsilon) #<--------------------------------------- Ausführen!
