library(rhub)
cran_prep <- check_for_cran(platforms = "macos-highsierra-release-cran")
cran_prep$cran_summary()

platforms()
check(platform = "solaris-x86-patched")

citation("abmAnimalMovement")

simResultsList$KINGCOBRA$inputs$in_moveMatrix

plot_landscapeLayersList(
  list("shelter" = simResultsList$KINGCOBRA$inputs$in_shelterMatrix,
      "forage"  = simResultsList$KINGCOBRA$inputs$in_forageMatrix,
      "movement"  = simResultsList$KINGCOBRA$inputs$in_moveMatrix
       ))


# ❯ checking files in ‘vignettes’ ... WARNING
# Files in the 'vignettes' directory but no files in 'inst/doc':
#   ‘Agent-based_model_walkthrough.pdf’
