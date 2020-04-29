clean_names <- function(x, firstupper = TRUE, lvls = TRUE) {
  x <-
    gsub("(ECI|[cP])_|_?TRUE.?|_X|TypeOf", "", x) %>%
    tolower() %>%
    {gsub("_", " ", .)} %>%
    {gsub("asa2", "ASA grade = 2", .)} %>%
    {gsub("asa3", "ASA grade = 3", .)} %>%
    {gsub("gender ?man", "male sex", .)} %>%
    {gsub("diagrp", "Diagnos", .)}


  if (!lvls) {
    x <- unique(gsub("ASA grade = [23]", "ASA grade", x))
    x <- gsub("male sex", "sex", x)
  }
  if (firstupper) {
    x <- paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  }

  x <-
    ifelse(
      grepl("bmi|asa|eci|cci", x, ignore.case = TRUE),
      toupper(x),
      x
    )

  x <- ifelse(grepl("aids", x, ignore.case = TRUE), "AIDS/HIV", x)
  x <- ifelse(grepl("cns", x, ignore.case = TRUE), "CNS disease", x)
  gsub("GRADE", "class", x, ignore.case = TRUE)
}


model_names <- function(x, age = FALSE) {
  if (!age)
    x <- gsub(" \\(age as main effect\\)", "", x)

  x <- gsub("ASA", "ASA class", x)

  gsub("BRL reduced", "Reduced model", x) %>%
    {gsub("BRL", "Main model", .)}
}
