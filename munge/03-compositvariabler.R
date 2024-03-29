# F�rbered kompisitvariabler som ev kan ers�tta enskilda diagnoser

# Fulfix!
# Tidigare Fel i coder-paketet d�pte "malignancy" till "malingnancy".
# Nu �tg�rdat i paketet men fortf fel i samk�rningsdatabsen
df <- rename(df, CCI_malignancy = CCI_malingnancy)


# Make mutate statments from Excel-file -----------------------------------

categorization.Blad1 <- readxl::read_excel("data/categorization.xlsx")
cache("categorization.Blad1")

# Define it!
categorization <-
  categorization.Blad1 %>%
  mutate_all(zoo::na.locf) %>%
  filter(from != "Rx") %>%                        # NOTE!!!!!!!!!!!! TO TRY WITHOUT
  mutate_at(vars(new, old), ~ gsub("/| ", "_", tolower(.))) %>%
  mutate(
    new = paste0("c_", new),
  ) %>%
  unite("old", from, old) %>%
  group_by(new) %>%
  summarise(old = paste(old, collapse = " | ")) %>%
  ungroup() %>%
  mutate(
    new = syms(new),
    old = parse_exprs(old)
  )

cache("categorization")

# Do it!
c_cols <- bind_cols(pmap(categorization, ~transmute(df, !!.x := !!.y)))


# Combine some existing columns with new ones --------------------------------

df <-
  df %>%
  select(
    everything(),
    -matches("^([CE]CI)|(Rx)_"),
    matches("index")
  ) %>%
  bind_cols(c_cols) %>%
  select(-c_obesity)

cache("df")
