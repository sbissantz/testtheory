# Übungsaufgabe 1 ---------------------------------------------------------

# Übungsaufgabe 2 ---------------------------------------------------------

# Setup
#
# Faktorladungen; 8 items
load_F1 <- c(0.6, -0.3, 0.5, 0.7, 0.1, 0.2, 0.2, 0.3) #<-------- Verändere mich!
load_F2 <- c(-0.1, 0.1, 0.1, 0.1, -0.7, 0.5, -0.6, 0.7) #<------ Verändere mich!
fx <- cbind(load_F1, load_F2)
# Zwischenfaktorkorrelation(en) 
phi <- diag(rep(1, 2)) ; phi[1, 2] <- phi[2, 1] <- 0.6 #<------- Verändere mich!
# Struktur erstellen
S <- psych::sim.structure(fx, phi, n=1000)
# Korrelations- (R) und Datenmatrix (X)
R <- S$model ; X <- S$observed  # alternativ: R <- cor(X)

## Output (grafisch): zwei latente Variablen
#
old.par <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
  # Visuelle Darsetellung ihrer Schöpfung
  psych::structure.diagram(fx, phi, cut=FALSE)
  # Visuelle Darsetellung ihrer Korrelationsmatrix 
  corrplot::corrplot.mixed(R, number.cex=.7)
par(old.par)

# Übungsaufgabe 3 ---------------------------------------------------------

# Old School! (stats)
# dino_pca <- princomp(X, cor=TRUE) ; summary(dino_pca, cor=TRUE)

# New School
(fit_pca <- psych::principal(R, nfactors = 2, rotate = "none"))
## Komponentenladungen
fit_pca$loadings
## Einzigartigkeit 
fit_pca$uniquenesses
## Kommunalitäten 
fit_pca$communality

# Übungsaufgabe 4 ---------------------------------------------------------

(fit_paf <- psych::fa(R, nfactors=2,  rotate="none",  fm="pa"))
# Kommunalitäten
fit_paf$communality
# Eigenwerte
fit_paf$e.values
# Einzigartigkeit
fit_paf$uniquenesses
# Quadrierte multiple Korrelation
fit_paf$R2

# Übungsaufgabe 5 ---------------------------------------------------------
# Übungsaufgabe 6 ---------------------------------------------------------

(fit_mlf <- psych::fa(R, nfactors=2,  rotate="none",  fm="ml"))
# Kommunalitäten
fit_mlf$communality
# Eigenwerte
fit_mlf$e.values
# Einzigartigkeit
fit_mlf$uniquenesses
# Quadrierte multiple Korrelation
fit_mlf$R2

# Übungsaufgabe 7 ---------------------------------------------------------

(fit_vmax <- psych::fa(R, nfactors=2,  rotate="varimax",  fm="ml"))
# Kommunalitäten
fit_vmax$communality
# Eigenwerte
fit_vmax$e.values
# Einzigartigkeit
fit_vmax$uniquenesses
# Quadrierte multiple Korrelation
fit_vmax$R2

# Übungsaufgabe 8 ---------------------------------------------------------

# Scree Test und K1
#
# Barfuß
ev <- eigen(R)$values
plot(ev, main="Eigenwerteplot", xlab="Eigenwerte",
     ylab = "Komponentenzahl", type="b")
abline(h=1, lty=2)

# psych package
psych::scree(R, factors = FALSE)
psych::scree(R, factors = TRUE)

# Parallel Analyse 
#
# psych::fa.parallel(X, fa = "pc", fm = "ml")
# Lange Laufzeit 
psych::fa.parallel(X, fa = "pc", fm = "ml")

# Alternative
#
# Für Komponenten
paran::paran(X)
# Für Faktoren
paran::paran(X, cfa=TRUE)

# Übungsaufgabe 9 ---------------------------------------------------------

# Datenmatrix (X)
# psych::cortest.bartlett(X)
# Korrelationsmatrix (R)
psych::cortest.bartlett(R, n=nrow(X))
psych::KMO(X)

# Selbststudium 1 ---------------------------------------------------------

find_RSS <- function(noi){
  resid <- psych::fa(R, fm="pa", nfactors = 2, rotate = "none",
                     residuals = TRUE, max.iter=noi)$residual
  sum(resid[upper.tri(resid, diag = FALSE)])^2
}
seq <- 1:10
(RSS <- sapply(seq, find_RSS))
plot(NULL, xlim=c(-0.1, 11), ylim= c(0, 0.025),
     ylab="Residuenquadratsumme", xlab="Anzahl der Iterationen")
points(x=seq, y=RSS, type = "b")

# Selbststudium 2 ---------------------------------------------------------

# Black Box
pca_fit <- princomp(X, cor = TRUE)

# Barfuß
R_EVD <- cor(X)
# EVD von R ; Eigenvektoren ; Eigenwerte
EVD_R <- eigen(R_EVD)
# Komponentenladungen
loadings <- EVD_R$vectors
# Gleich?
pca_fit$loadings ; print(loadings, digits=3)

# Selbststudium 3 ---------------------------------------------------------

# Black Box
pca_fit <- prcomp(X, scale. = TRUE)

# Barfuß
n <- nrow(X)
SVD_X <- svd(scale(X)/sqrt(n-1))
U <- SVD_X$u ; D <- SVD_X$d
loadings <- U %*% diag(D) * sqrt(n-1)
# Gleich?
pca_fit$x ; loadings

# Selbststudium 4 ---------------------------------------------------------

grep_commu <- function(noi) {
  psych::fa(R, fm="pa", nfactors = 2, max.iter = noi)$communality
}
iters <- 1:8
commus <- lapply(iters, grep_commu)
calc_cormat <- function(commu, R){
  diag(R) <- commu ; round(R, digits = 2)
}
Rs <- lapply(commus, calc_cormat, R=R) ;
names(Rs) <- paste0("iteration ", seq(iters))
Rs

# Selbststudium 5 ---------------------------------------------------------

# Basisalgorithmus
#
# EVD of R
EVD_R <- eigen(R)
# Loadings
F <- EVD_R$vectors
# Residual matrix
R_ast <- R - F %*% t(F)
# Squared uniqness matrix
U2 <- diag(R_ast)
# Replace on diagonals
(diag(R) <- 1 - U2)
R

# 1 Iteration
#
EVD_R <- eigen(R) # EVD of R
F <- EVD_R$vectors # Loadings
k <- 5 # k largest PC
# Pre reproduction
Fk <- F[,seq(k)] 
Z <- matrix(0, ncol = ncol(F) - k,  nrow(F))
Fk <- cbind(Fk, Z)
# Residual matrix
R_ast <- R - Fk %*% t(Fk)
# Squared uniqness matrix
U2 <- diag(R_ast)
# Replace on diagonals
diag(R) <- 1 - U2
#...Repeat
round(R, digits = 3)





