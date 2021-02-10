clean_names <- function(x, firstupper = TRUE, lvls = TRUE) {
  gsub("(ECI|[cP])_|_?TRUE.?|_X|TypeOf", "", x) %>%
  {gsub("_",       " ",           .)} %>%
  {gsub("SexMale", "Male sex",    .)} %>%
  {gsub("DiaGrp",  "Diagnosis: ",   .)} %>%
  {gsub("ASA",     "ASA class: ", .)} %>%
  {gsub("BMI",     "BMI: ",       .)} %>%
  {gsub("aids",    "AIDS/HIV",    .)} %>%
  {gsub("cns",     "CNS",         .)} %>%
  {gsub("\\.",     " ",           .)} %>%
  {gsub("  ",      " ",           .)} %>%
  {gsub("AVN",     "\\(AVN\\)",   .)} %>%
  {gsub("Rheumatidisease", "Rheumatic disease",   .)} %>%
  {paste0(toupper(substr(., 1, 1)), substring(., 2))} %>%
  trimws()
}


model_names <- function(x, age = FALSE) {
  if (!age)
    x <- gsub(" \\(age as main effect\\)", "", x)

  x <- gsub("ASA", "ASA class", x)

  gsub("BRL reduced", "Reduced model", x) %>%
    {gsub("BRL", "Main model", .)}
}
