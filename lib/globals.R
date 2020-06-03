# con <- shar_linkage_con()

# General settings
memory.limit(1e10)
set.seed(132456798)
future::plan(sequential)
# future::plan("multiprocess", workers = 2) # future::availableCores()

options(
  digits    = 2,
  na.action = "na.fail",
  scipen    = 999,
  repos     = "https://mran.microsoft.com/snapshot/2020-06-02"
)

# Add any project specific configuration here.
add.config(
  apply.override = FALSE
)

# Add project specific configuration that can be overridden from load.project()
add.config(
  N_bots         = 100,
  Bmax           = 100,     # No of external reruns for BRLasso
  apply.override = TRUE
)
