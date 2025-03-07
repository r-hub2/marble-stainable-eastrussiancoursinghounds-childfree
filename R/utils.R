.onAttach <- function(lib,pkg) {
  local_version <- utils::packageVersion("childfree")
  packageStartupMessage("N   \U2587\U2500\U252C\U2500O   childfree v",local_version)
  packageStartupMessage(" O    \U2506     CITE: Neal, Z. P. and Neal, J. W., (2024). childfree: An R package to access and")
  packageStartupMessage("  K   \U2587\U2500\U252C\U2500O harmonize childfree demographic data. CRAN. https://doi.org/10.32614/CRAN.package.childfree")
  packageStartupMessage("   I    \U2506   HELP: type vignette(\"childfree\"); email zpneal@msu.edu; github zpneal/childfree")
  packageStartupMessage("    D   \U2573   BETA: type devtools::install_github(\"zpneal/childfree\", ref = \"devel\")")
}
