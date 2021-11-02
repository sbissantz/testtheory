path <- "./members.xlsx"
members <- readxl::read_excel(path, col_names = TRUE)
nms <- members$Nachname
cncpts <- c(
# Hauptgütekriterien
#
"Objektivität",
"Durchführungsobjektivität",
"Interpreationsobjektivität",
"Auswertungsobjektivität",

"Reliabilität",
"Paralleltest-Reliabilität",
"Testhalbierungs-Reliabilität",
"Retest-Reliabilität",
"Interne Konsistenz",

"Validität",
"Inhaltsvalidität",
"Augescheinvalidität",
"Kriteriumsvalidität",
"Vorhersagevalidität",
"Retrospektive Validität",
"Übereinstimmunsvalidität",
"Inkrementelle Validität",

"Konstruktvalidität",
"Konvergente Validität",
"Diskriminante Validität",
"Faktorielle/Strukturelle Validität",

# Nebengütekriterien,
"Skalierung",
"Eichung",
"Ökonomie",
"Nützlichkeit",
"Zumutbarkeit",
"Unverfälschbarkeit",
"Fairness")

nms_len <- length(nms) ; cncpts_len <- length(cncpts)
if(nms_len > cncpts_len){
 d <- nms_len - cncpts_len 
 cbind( 
   sample(nms, nms_len),
   sort(c(cncpts, rep("Findling", d))))
}



