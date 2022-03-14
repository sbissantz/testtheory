##########
# 05-IRT #
##########

# Data
#
data(zareki) 

# Data wrangling
#
(cols <- grep("subtr", colnames(zareki)))
data <- zareki[, cols]

#
# Unidimensionality
#

# Princals
#
princal <- Gifi::princals(data)
plot(princal, main="Zareki Loadings") #subtr5

# EFA
#

# Minimum Rank Factor Analysis
#
EFA.MRFA::hullEFA(data, maxQ=4, extr = "ML", 
                  index_hull = "CAF", display = TRUE, 
                  graph = TRUE, details = TRUE) # 1 Factor

# Minimum Rank Factor Analysis
#
EFA.MRFA::parallelMRFA(data, graph=TRUE) # 1 Factor

# Very simple structure
#
R_poly <- psych::polychoric(data)$rho
n.obs <- nrow(R_poly)
psych::vss(R_poly, fm="ml", n.obs = n.obs) # 1 to 2

# IFA
#
fit_fa.1 <- mirt::mirt(data, 1, verbose = FALSE)
fit_fa.2 <- mirt::mirt(data, 2, verbose = FALSE)
mirt::anova(fit_fa.1, fit_fa.2, verbose=FALSE)$X2 # 1 Factor

# Assumptions
# (unidimensionality, Parallel ICCs, local independence)
# Note: if the data fit the Rasch model all assumptions are met!
#
(fit_rasch.1 <- eRm::RM(data)) # eta: item parameter (normally: beta_i)

# LR-Test
# Crux: Measurement invariance
# ..a statistical property of measurement that indicates that the same construct
# is being measured across some specified groups
#
median_split <- (zareki$time <= median(zareki$time))
timecat <- factor(median_split)
(fit_LR <- eRm::LRtest(fit_rasch.1, timecat)) #not fit? (Hyp: subtr5)

# Waldtest
#
eRm::Waldtest(fit_rasch.1, timecat) #subtr5

# Visualize LR-test 
# (confidence bands)
#
(fit_rasch.2 <- eRm::RM(data[,-5])) # eta: item parameter (normally: beta_i)
eRm::plotGOF(fit_LR, ctrline = list(col="grey"), conf = list())

# LR-Test (2)
#
(fit_LR.2 <- eRm::LRtest(fit_rasch.2, timecat)) #not fit? (Hyp: subtr5)

# Local independence
# (local & global)
set.seed(123)

# local
#
eRm::NPtest(as.matrix(data[,-5]), n = 1e3, method = "T1")

# global 
#
eRm::NPtest(as.matrix(data[,-5]), n = 1e3, method = "T11")

# easiness parameters
round(sort(fit_rasch.2$betapar), 3)
# difficulty parameters
round(sort(-fit_rasch.2$betapar), 3)

# ICCs 
# Item repsonse functions
#
eRm::plotjointICC(fit_rasch.2, xlab="Subtraction Trait", 
                  main="ICCs Subtraction Items")

# Person parameters
#
perpars <- eRm::person.parameter(fit_rasch.2)
zareki$theta <- perpars$theta.table[,1]
summary(aov(theta ~ class, data = zareki))

