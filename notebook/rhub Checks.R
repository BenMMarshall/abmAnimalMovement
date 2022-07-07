library(rhub)
cran_prep <- check_for_cran(platforms = "macos-highsierra-release-cran")
cran_prep$cran_summary()
