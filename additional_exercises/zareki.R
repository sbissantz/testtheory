# Prüfe, ob benötigte Packages geladen sind
#
if(!requireNamespace("MPsychoR", quietly = TRUE)) { 
  msg <- "'MPsychoR' is not installed, want to install it? Type 'yes' or 'no'."
  answer <- readline(prompt = message(msg))
  no_msg <- "Did not install the package `MPsychoR`."
  switch(answer,
         yes = install.packages("MPsychoR"),
         no = stop(no_msg, call. = FALSE),
         stop("Please answer 'yes' or 'no' (omit quotes!)" ))
} else {
  message("`MPsychoR is already installed!") ; Sys.sleep(1)
  message("Time to rock! (*weird guitar sound*)")
}

# Package laden
#
library(MPsychoR)

# Datensatz laden
#
data("zareki")

# Instruktionen für Datensatz 
#
?zareki

# Data wrangling
#
dat <- zareki[,1:16]
dat$sex <- sample(c("male", "female"), nrow(zareki), replace=TRUE)
dat$sex.f <- factor(dat$sex)
dat$sex.num <- as.numeric(factor(dat$sex))
dat$age <- sample(6:10, nrow(zareki), replace=TRUE)
rm(zareki)

# Bitte arbeiten Sie mit dem Datensatzobjekt "dat" weiter
# str(dat) ; View(dat) ; head(dat)