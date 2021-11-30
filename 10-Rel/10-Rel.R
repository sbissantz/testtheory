###################################
# R Skript zum Übungskurs: 10 Rel #
##################################ä

# Notwendige Packages laden 
# (Ein/Auskommentieren R-Studio: Strg + Shift + C)
#
# if(!requireNamespace("MASS", quietly = TRUE)) {
#   msg <- "'MASS' is not installed, want to install it? Type 'yes' or 'no'."
#   answer <- readline(prompt = message(msg))
#   no_msg <- "Did not install the package `MASS`."
#   switch(answer,
#          yes = install.packages("MASS"),
#          no = stop(no_msg, call. = FALSE),
#          stop("Please answer 'yes' or 'no' (omit quotes!)" ))
# } else {
#   message("`MASS is already installed!") ; Sys.sleep(1)
#   message("Time to rock!\n(*weird guitar sound*)")
# }

# Übung 2 -----------------------------------------------------------------

# Generativer Prozess
# (simuliert)
#
set.seed(123)
# Tau-parallel
M <- 8
mu <- c(5,4,3,4,5,3,5,4)
# Kovarianzmatrix
Sigma <- matrix(
  c(.8, .5, .5, .5, .5, .5, .5, .5,
    .5, .8, .5, .5, .5, .5, .5, .5,
    .5, .5, .8, .5, .5, .5, .5, .5,
    .5, .5, .5, .8, .5, .5, .5, .5,
    .5, .5, .5, .5, .8, .5, .5, .5,
    .5, .5, .5, .5, .5, .8, .5, .5,
    .5, .5, .5, .5, .5, .5, .8, .5,
    .5, .5, .5, .5, .5, .5, .5, .8),
  M,M)
N <- 1e3
# Multivariate Half-Normal Distribution
X <- data.frame(abs(MASS::mvrnorm(N, mu, Sigma)))

# Zufallsaufteilung
#
m <- length(X) ; seq <- seq(m)
rseq <- sample(seq, m, replace=FALSE)
X_p <- X[,rseq[1:4]]
X_q <- X[,rseq[5:8]]

# Summencores
#
rsx_p <- rowSums(X_p)
rsx_q <- rowSums(X_q)

# Halbtestreliabilität 
#
(Rel_halb <- cor(rsx_p, rsx_q))

# Hilfsfunktion: Spearman Brown Korrektur 
#
Rel_SBK <- function(X_p, X_q) {
  # Summenscores berechnen
  rs_p <- rowSums(X_p)
  rs_q <- rowSums(X_q)
  # Korrelation der Testhälften
  r <- cor(rs_p, rs_q)
  # Spearman-Brown-Korrektur
  2 * r / (1+r)
}



# Überprüfung
#
Rel_SBK(X_p, X_q)

# Übung 3 -----------------------------------------------------------------

# Daten simulieren
#
set.seed(123)
# Tau-parallel
M <- 8
mu <- c(5,4,3,4,5,3,5,4)
# Kovarianzmatrix
Sigma <- matrix(
  c(.8, .5, .5, .5, .5, .5, .5, .5,
    .5, .8, .5, .5, .5, .5, .5, .5,
    .5, .5, .8, .5, .5, .5, .5, .5,
    .5, .5, .5, .8, .5, .5, .5, .5,
    .5, .5, .5, .5, .8, .5, .5, .5,
    .5, .5, .5, .5, .5, .8, .5, .5,
    .5, .5, .5, .5, .5, .5, .8, .5,
    .5, .5, .5, .5, .5, .5, .5, .8),
  M,M)
N <- 1e2
X <- data.frame(MASS::mvrnorm(N, mu, Sigma))

# Kennwerte
# 
# Varianz-Kovarianz-Matrix
VCOV <- cov(X)
# Summe der Varianzen
V_items <- diag(VCOV)
# Gesamtvarianz
V_X <- sum(VCOV)

# Aufgabenstellung
#
list("Gesamtvarianz" = round(V_X, 2),
     "Itemvarianz" = round(V_items,2))

# Hilfsfunktion: Cronbach's Alpha 
#
alpha <- function(X) {
  # Varianz-Kovarianz-Matrix
  VCOV <- cov(X) ; m <- length(X)
  # Gesamtvarianz 
  V_x <- sum(VCOV)
  # Varianz der Summen
  V_xi <- sum(diag(VCOV))
  # Cronbachs Alpha
  m/(m-1) * (1-(V_xi/V_x))
}

# Überprüfung 
#
alpha(X)

# Psych Package
#
psych::alpha(X)

# Übung 4 -----------------------------------------------------------------

# Hilfsfunktion
#
calc_k <- function(Rel_ast, Rel) {
  (Rel_ast * (1-Rel)) / (Rel * (1 -Rel_ast))
}

# Überprüfung
#
calc_k(Rel_ast=0.9, Rel=0.45)

# -------------------------------------------------------------------------

########################
# Übungen aus dem Kurs # 
########################

# Übung 2 aus dem Kurs ----------------------------------------------------

set.seed(123)
# Tau-parallel
M <- 8
mu <- c(5,4,3,4,5,3,5,4)
# Kovarianzmatrix
Sigma <- matrix(
  c(.8, .5, .5, .5, .5, .5, .5, .5,
    .5, .8, .5, .5, .5, .5, .5, .5,
    .5, .5, .8, .5, .5, .5, .5, .5,
    .5, .5, .5, .8, .5, .5, .5, .5,
    .5, .5, .5, .5, .8, .5, .5, .5,
    .5, .5, .5, .5, .5, .8, .5, .5,
    .5, .5, .5, .5, .5, .5, .8, .5,
    .5, .5, .5, .5, .5, .5, .5, .8),
  M,M)
N <- 1e3
# Multivariate Half-Normal Distribution
X <- data.frame(abs(MASS::mvrnorm(N, mu, Sigma)))
# Odds-Even Zuteilung
even <- seq(1,8, by=2)
uneven <- seq(2,8, by=2)
rsx_even <- rowSums(X[,even])
rsx_uneven <- rowSums(X[,uneven])
# Halbtestreliabilität
(Rel_halb <- cor(rsx_even, rsx_uneven))

# Hilfsfunktion
#
Rel_SBK <- function(X_p, X_q) {
  rs_p <- rowSums(X_p)
  rs_q <- rowSums(X_q)
  r <- cor(rs_p, rs_q)
  2 * r / (1+r)
}

# Spearman Brown Korrektur
#
X_p <- X[, even]
X_q <- X[, uneven]
Rel_SBK(X_p, X_q)

# Übung 3 aus dem Kurs ----------------------------------------------------

# Daten simulieren
#
set.seed(123)
# Essenziell Tau-Äquivalent
M <- 8
mu <- c(5,4,3,4,5,3,5,4)
# Kovarianzmatrix
Sigma <- matrix(
  c(1.8, .5, .5, .5, .5, .5, .5, .5,
    .5, 1.7, .5, .5, .5, .5, .5, .5,
    .5, .5, 1.8, .5, .5, .5, .5, .5,
    .5, .5, .5, 1.6, .5, .5, .5, .5,
    .5, .5, .5, .5, 1.6, .5, .5, .5,
    .5, .5, .5, .5, .5, 1.7, .5, .5,
    .5, .5, .5, .5, .5, .5, 1.8, .5,
    .5, .5, .5, .5, .5, .5, .5, 1.8),
  M,M)
N <- 1e3
# Multivariate Half-Normal Distribution
X <- data.frame(abs(MASS::mvrnorm(N, mu, Sigma)))

# Kennwerte
#
VCOV <- cov(X)
V_items <- diag(VCOV)
V_X <- sum(VCOV)
list("Gesamtvarianz" = round(V_X, 2),
     "Itemvarianz" = round(V_items,2))

# Hilfsfunktion: alpha()
#
alpha <- function(X) {
  # Varianz-Kovarianz-Matrix
  VCOV <- cov(X) ; m <- length(X)
  # Gesamtvarianz 
  V_x <- sum(VCOV)
  # Varianz der Summen
  V_xi <- sum(diag(VCOV))
  # Cronbachs Alpha
  m/(m-1) * (1-(V_xi/V_x))
}

# Crpnbach's Alpha 
# 
alpha(X)

# Psych Package
#
psych::alpha(X)

# Übung 4 aus dem Kurs ----------------------------------------------------

set.seed(123)
# Essenziell Tau-Äquivalent
M <- 6
mu <- c(5,4,3,4,5,3)
# Kovarianzmatrix
Sigma <- matrix(
  c(.8, .4, .4, .4, .4, .4,
    .4, .7, .4, .4, .4, .4,
    .4, .4, .8, .4, .4, .4,
    .4, .4, .4, .7, .4, .4,
    .4, .4, .4, .4, .8, .4,
    .4, .4, .4, .4, .4, .7),
  M,M)
N <- 1e3
# Multivariate Half-Normal Distribution
X <- data.frame(abs(MASS::mvrnorm(N, mu, Sigma)))

# Cronbach's Alpha
#
alpha(X)

# Hilfsfunktion: calc_k()
#
calc_k <- function(Rel_ast, Rel) {
  (Rel_ast * (1-Rel)) / (Rel * (1 -Rel_ast))
}
Rel_ast <- 0.99 ; Rel <- 0.87
calc_k(Rel_ast, Rel)


