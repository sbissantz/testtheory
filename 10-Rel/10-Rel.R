###################################
# R Skript zum Übungskurs: 10 Rel #
##################################ä

# Notwendige Packages laden 
# (Ein/Auskommentieren R-Studio: Strg/Cmd + Shift + C)
#
if(!requireNamespace("MASS", quietly = TRUE)) {
  msg <- "'MASS' is not installed, want to install it? Type 'yes' or 'no'."
  answer <- readline(prompt = message(msg))
  no_msg <- "Did not install the package `MASS`."
  switch(answer,
         yes = install.packages("MASS"),
         no = stop(no_msg, call. = FALSE),
         stop("Please answer 'yes' or 'no' (omit quotes!)" ))
} else {
  message("`MASS is already installed!") ; Sys.sleep(1)
  message("Time to rock! (*weird guitar sound*)")
}

# Übung 1 -----------------------------------------------------------------

# Generativer Prozess
# (simuliert)
#
# TIPP: 
# Wenn Sie für die Klausur noch geringfügig andere Werte zum Üben generieren wollen
# dann löschen Sie set.seed(123) und führen den nachfolgenden Code erneut aus.
# Alternativ können Sie auch den Wert verändern, z.B. set.seed(007), und dann
# den Code erneut ausführen.
set.seed(123)
# Tau-parallel
M <- 8
mu <- c(4,4,4,4,4,4,4,4)
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
N <- 100 
# Multivariate Half-Normal Distribution
X <- data.frame(abs(MASS::mvrnorm(N, mu, Sigma)))

# Zufallsaufteilung
#
m <- length(X) ; seq <- seq(m)
rseq <- sample(seq, m, replace=FALSE)
X_p <- rowSums(X[,rseq[1:4]])
X_q <- rowSums(X[,rseq[5:8]])

# Halbtestreliabilität 
#
(Rel_halb <- cor(X_p, X_q))

# Hilfsfunktion: Spearman Brown Korrektur 
#
Rel_SBK <- function(X_p, X_q) {
  # Korrelation der Testhälften
  r <- cor(X_p, X_q)
  # Spearman-Brown-Korrektur
  2 * r / (1+r)
}

# Überprüfung
#
Rel_SBK(X_p, X_q)

# Übung 2 -----------------------------------------------------------------

# Daten simulieren
#
# TIPP: Wenn Sie für die Klausur noch leicht andere Werte zum üben haben wollen,
# dann löschen Sie set.seed(123) und führen den nachfolgenden Code erneut aus.
# Alternativ können Sie auch den Wert verändern, z.B. set.seed(007), und dann
# den Code erneut ausführen. Zum Schluss können sie zusätzlich(!) N kleiner
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
N <- 100
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
item_stats <- psych::alpha(X)
# Cronbach's alpha als Einzelergebnis
item_stats$total[["raw_alpha"]]
# alpha$total[1]

# Übung 3 -----------------------------------------------------------------

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

# Übung 1 aus dem Kurs ----------------------------------------------------

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
N <- 1000 
# Multivariate Half-Normal Distribution
X <- data.frame(abs(MASS::mvrnorm(N, mu, Sigma)))

# Odds-Even Zuteilung
even <- seq(2,8, by=2)
uneven <- seq(1,8, by=2)
rsx_even <- rowSums(X[,even])
rsx_uneven <- rowSums(X[,uneven])
# Halbtestreliabilität
(Rel_halb <- cor(rsx_even, rsx_uneven))

# Hilfsfunktion
#
Rel_SBK <- function(X_p, X_q) {
  r <- cor(X_p, X_q)
  2 * r / (1+r)
}

# Spearman Brown Korrektur
#
X_p <- rowSums(X[, even])
X_q <- rowSums(X[, uneven])
Rel_SBK(X_p, X_q)

# Übung 2 aus dem Kurs ----------------------------------------------------

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
N <- 1000 
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

# Übung 3 aus dem Kurs ----------------------------------------------------

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
N <- 1000 
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

# Messmodell Routlette ----------------------------------------------------

messmodell_roulette <- function() {
  ## Paralleles Messmodell 
  parallel <- function() { 
    M <- 4
    mu <- rep(5,M)
    Sigma <- matrix( 
      c(.8, .5, .5, .5, 
        .5, .8, .5, .5, 
        .5, .5, .8, .5, 
        .5, .5, .5, .8), 
      M,M) 
    N <- 1e5 
    X <- data.frame(MASS::mvrnorm(N, mu, Sigma))
    print(list( 
      "Spaletenmittelwerte" = round(colMeans(X), digits = 1) , 
      "Kovarianzmatrix" = round(cov(X), digits = 1) 
    ))
    invisible("a")
    }
  ## Essenziell paralleles Messmodell
  essential_parallel <- function() {
    M <- 4
    mu <- c(5,4,3,4)
    Sigma <- matrix( 
      c(.8, .5, .5, .5,
        .5, .8, .5, .5,
        .5, .5, .8, .5,
        .5, .5, .5, .8),
      M,M)
    N <- 1e5
    X <- data.frame(MASS::mvrnorm(N, mu, Sigma))
    print(list( 
      "Spaletenmittelwerte" = round(colMeans(X), digits = 1) , 
      "Kovarianzmatrix" = round(cov(X), digits = 1) 
    ))
    invisible("b")
    }
  ## Tau-äquivalentes Messmodell
  tau_equivalent <- function() {
    M <- 4
    mu <- rep(5,M)
    Sigma <- matrix(
      c(.7, .5, .5, .5,
        .5, .8, .5, .5,
        .5, .5, .7, .5,
        .5, .5, .5, .6),
      M,M)
    N <- 1e5
    X <- data.frame(MASS::mvrnorm(N, mu, Sigma))
    print(list( 
      "Spaletenmittelwerte" = round(colMeans(X), digits = 1) , 
      "Kovarianzmatrix" = round(cov(X), digits = 1) 
    ))
    invisible("c")
    }
    ## Essenziell tau-äquivales Messmodell
    essential_tau_equivalent <- function() {
    M <- 4
    mu <- c(5,4,3,4)
    Sigma <- matrix(
      c(.7, .5, .5, .5,
        .5, .8, .5, .5,
        .5, .5, .7, .5,
        .5, .5, .5, .6),
      M,M)
    N <- 1e5
    X <- data.frame(MASS::mvrnorm(N, mu, Sigma))
    print(list( 
      "Spaletenmittelwerte" = round(colMeans(X), digits = 1) , 
      "Kovarianzmatrix" = round(cov(X), digits = 1) 
    ))
    invisible("d")
    }
    tau_congeneric <- function() {
    ## Tau-kongenerisches Messmodell
    M <- 4
    mu <- c(5,4,3,4)
    # Kovarianzmatrix
    Sigma <- matrix(
      c(.7, .5, .6, .7,
        .5, .8, .5, .6,
        .6, .5, .7, .5,
        .7, .6, .5, .8),
      M,M)
    N <- 1e5
    X <- data.frame(MASS::mvrnorm(N, mu, Sigma))
    print(list( 
      "Spaletenmittelwerte" = round(colMeans(X), digits = 1) , 
      "Kovarianzmatrix" = round(cov(X), digits = 1) 
    ))
    invisible("e")
    }
smpl <- sample(letters[1:5], 1)
output <- switch(smpl,
                 a = parallel(),
                 b = essential_parallel(),
                 c = tau_equivalent(),
                 d = essential_tau_equivalent(),
                 e = tau_congeneric(), 
                 stop("Please answer 'yes' or 'no' (omit quotes!)"))
prompt_msg <- paste0("Um welches Messmodell handelt es sich hier? Antwortalternativen: \n (a) parallel \n (b) essenziell-parallel \n (c) tau-aequivalent \n (d) essenziell-tau-aequivalent \n (e) tau-kongenerisch \n Tippen Sie in die Konsole bitte den entsprechenden Buchstaben ein; z.B.: a")
input <- readline(prompt = message(prompt_msg))
true_msg <- paste0("(", output, ") is die richtige Antwort! Klasse, weiter so!")
false_msg <- paste0("Das war leider nicht korrekt. (", output, ") wäre richtig gewesen!")
ifelse(input == output, true_msg, false_msg)
}  
  
# Klausurübung
#
messmodell_roulette()










