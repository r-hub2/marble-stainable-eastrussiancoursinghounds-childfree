.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("childfree")
  packageStartupMessage("N   \U2587\U2500\U252C\U2500O   childfree v",local_version)
  packageStartupMessage(" O    \U2506     CITE: Neal, Z. P. and Neal, J. W., (2024). Childfree: An R package to access")
  packageStartupMessage("  K   \U2587\U2500\U252C\U2500O and harmonize childfree demographic data. CRAN.")
  packageStartupMessage("   I    \U2506   HELP: type vignette(\"childfree\"); email zpneal@msu.edu; github zpneal/childfree")
  packageStartupMessage("    D   \U2573   BETA: type devtools::install_github(\"zpneal/childfree\", ref = \"devel\")")
}
