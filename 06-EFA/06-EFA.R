
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
# Korrelationsmatrix simulieren 
R_zz <- psych::sim.structure(fx, phi)$model

# Output
#
psych::structure.diagram(fx, phi, cut = FALSE) 
corrplot::corrplot.mixed(R_zz, number.cex=.7)






