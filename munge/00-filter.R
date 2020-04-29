# Pre-filter: some pat ids have same hip primarly operated twice!
dubbles <-
  df_shpr_orig %>%
  select(LopNr, P_Side, P_SurgDate) %>%
  add_count(LopNr, P_Side, sort = TRUE) %>%
  filter(n > 1)


# Define filter steps -----------------------------------------------------

# Make tibble with one row per filter step and columns:
# step: Number identifying inclusion box.
#       Does not need to be unique if several exlusions are combined.
# incl: Description of included cases
# excl: Description of cases to exclude
# expr: quosure defining filter step
# data: The data set to be filtered. Only required for initial step (first row).

filters <-
  bind_rows(
    tibble(
      step = 0,
      incl = "all",
      data = list(df_shpr_orig)
    ),

    tibble(
      step = 1,
      excl = "Hemi- and surface prosthesis",
      incl = "THA 2008-2015",
      expr = list(quo(
        # 2007-2008 previously included to identify THAs within two years
        P_SurgDate >= as.Date("2008-01-01") &
        P_ProstType == "Totalprotes" &
        (is.na(P_KVA1) | P_KVA1 != "NFB62 - Primär total ytersättningspr")
      ))
    ),


    tibble(
      step = 2,
      excl = "First THA for bilateral cases",
      incl = "Last operated hip",
      expr = list(quo(op_last == 1))
    ),

    tibble(
      step = 2,
      excl = "Died within 2 years of THA",
      incl = "Last operated hip",
      expr = list(quo(
        is.na(DateOfDeath) | as.numeric(DateOfDeath - P_SurgDate) > 2 * 365.241
        ))
    ),

    tibble(
      step = 3,
      excl = "Diagnosis: Fracture/tumour/unspecified/unknown.",
      incl = "Elective THA",
      expr = list(quo(!P_DiaGrp %in% c(
        "Akut trauma, höftfraktur",
        "Akut trauma, övriga",
        "Tumör",
        "Övrigt",
        NA)))
    ),

    tibble(
      step = 4,
      excl = "Age < 18 or > 100",
      incl = "Total study population",
      expr = list(quo(between(P_Age, 18, 100)))
    ),

    tibble(
      step = 4,
      excl = "BMI missing",
      incl = "Total study population",
      expr = list(quo(!is.na(P_BMI)))
    ),

    tibble(
      step = 4,
      excl = "BMI > 50",
      incl = "Total study population",
      expr = list(quo(P_BMI <= 50))
    ),

    tibble(
      step = 4,
      excl = "ASA missing",
      incl = "Total study population",
      expr = list(quo(!is.na(P_ASA)))
    ),

    tibble(
      step = 4,
      excl = "ASA > 3",
      incl = "Total study population",
      expr = list(quo(P_ASA <= 3))
    ),

    tibble(
      step = 4,
      excl = "Missing education",
      incl = "Total study population",
      expr = list(quo(!is.na(education)))
    ),

    tibble(
      step = 4,
      excl = "Missing civil status",
      incl = "Total study population",
      expr = list(quo(!is.na(civil_status)))
    ),

    tibble(
      step = 4,
      excl = "Missing type of hospital",
      incl = "Total study population",
      expr = list(quo(!is.na(P_TypeOfHospital)))
    ),

    tibble(
      step = 4,
      excl = "Missing cementation of stem",
      incl = "Total study population",
      expr = list(quo(!is.na(P_FemStemCemMix)))
    ),

    tibble(
      step = 4,
      excl = "Missing cementation of cup",
      incl = "Total study population",
      expr = list(quo(!is.na(P_AcetCupCemMix)))
    )
  )


# Filter out cases row by row --------------------------------------------------
for (r in 2:nrow(filters)) {
  filters$data[[r]] <- filter(filters$data[[r - 1]], !!filters$expr[[r]])
}

# Save final data set as df
df_shpr <- filters$data[[r]]


# Format table with flowchart data ---------------------------------------------

filters <-
  filters %>%
  mutate(
    N        = map_int(data, ~ n_distinct(.$LopNr)),
    N_s      = prettyNum(N, big.mark = ",", preserve.width = "none"),
    N_excl   = lag(N) - N,
    N_excl_s = prettyNum(N_excl, big.mark = ",", preserve.width = "none")
  ) %>%
  filter(N_excl > 0) %>%
  group_by(step) %>%
  mutate(
    excl_text =
      if (n() == 1) {
        sprintf("%s\\l(N = %s)\\l", excl, N_excl_s)
      } else {
        sprintf(
          "Exclusion of (N = %s):\\l  - %s\\l",
          format(sum(N_excl), big.mark = ","),
          paste(sprintf("%s (N = %s)", excl, N_excl_s), collapse = "\\l  - ")
        )
      },
    incl_text  = sprintf("%s\\l (N = %s)\\l", incl, N_s)
  ) %>%
  ungroup() %>%
  mutate(excl_next = lead(excl_text)) %>%
  select(step, incl_text, excl_next) %>%
  distinct(excl_next, .keep_all = TRUE)


# Define nodes for graph --------------------------------------------------

nodes <-
  filters %>%
  pivot_longer(-step) %>%
  filter(!is.na(value)) %>%
  unite("node", step, name, remove = FALSE) %>%
  add_rowindex() %>%
  rename(
    id    = .row,
    label = value,
    rank  = step
  ) %>%
  mutate(
    shape     = "rectangle",
    width     = if_else(startsWith(name, "incl"), 1.75, 3.25),
    color     = "Black",
    fillcolor = "White",
    fontcolor = "Black"
  )


# Define edges for graph --------------------------------------------------

incl_edges <-
  nodes %>%
  filter(startsWith(name, "incl"))

excl_edges <-
  nodes %>%
  filter(startsWith(name, "excl"))

edges <-
  tibble(
    from = incl_edges$id,
    to1 = lead(incl_edges$id),
    to2 = c(excl_edges$id, NA)
  ) %>%
  pivot_longer(-from, values_to = "to") %>%
  select(-name) %>%
  arrange(from, to) %>%
  filter(!is.na(to)) %>%
  {create_edge_df(
    .$from, .$to,
    color    = "black",
    penwidth = 2,
    len      = 1
  )}


# Make graph --------------------------------------------------------------

graph <-
  create_graph(
    nodes,
    edges
  ) %>%
  add_global_graph_attrs("layout", "dot", "graph") %>% # Apply cluster ranking
  add_global_graph_attrs("fixedsize", "FALSE", "node") # Nice edge height

export_graph(graph, "graphs/flowchart.png", "png", width = 1024)
# render_graph(graph)

