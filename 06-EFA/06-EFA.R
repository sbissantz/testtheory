
# Übung 1 -----------------------------------------------------------------

# Setup
#

# Anzahl der Items
N <- 8
# Faktorladungen
F1 <- c(0.6, -0.3, 0.5, 0.7, 0.1, 0.2, 0.2, 0.3)
F2 <- c(-0.1, 0.1, 0.1, 0.1, -0.7, 0.5, -0.6, 0.7)
(fx <- cbind(F1, F2))
# Hauptdiagonale der Korrelationsmatrix (i.d.R.: 1) 
phi <- diag(rep(1, 2))
# Zwischenfaktorkorrelationen 
phi[1, 2] <- phi[2, 1] <- 0.6
# Struktur simulieren
S <- psych::sim.structure(fx, phi, n = 1000)
# Korrelarionsmatrix
R <- S$model
# Datenmatrix
X <- S$observed

# Output
#

# Visuelle Darsetellung ihrer Kreatur
psych::structure.diagram(fx, phi, cut = FALSE) 
# Visuelle Darsetellung ihrer Korrelationsmatrix 
corrplot::corrplot.mixed(R, number.cex=.7)

# Übung 2 -----------------------------------------------------------------

# Übung 3 -----------------------------------------------------------------

# Old School!
dino_pca <- princomp(X, cor=TRUE) ; summary(dino_pca, cor=TRUE)
# New School
pca_fit <- psych::principal(R, nfactors = 2, rotate = "none")
## Komponentenladungen
pca_fit$loadings
## Uniqueness 
pca_fit$uniquenesses
## Kommunalitäten 
pca_fit$communality

# Übung 4 -----------------------------------------------------------------





