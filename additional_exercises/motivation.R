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
data("Rmotivation")
?Rmotivation

# Instruktionen für Datensatz 
#
?Rmotivation

# Data wrangling
#
dat <- Rmotivation

# Bitte arbeiten Sie mit dem Datensatzobjekt "dat" weiter
# str(dat) ; View(dat) ; head(dat)
