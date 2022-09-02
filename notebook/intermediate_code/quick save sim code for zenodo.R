dir.create(here::here("notebook",
                      "manuscript",
                      "eg_simdata"))

for(species in vecSpecies){

  write.csv(simResultsList[[species]],
            file = here::here("notebook",
                              "manuscript",
                              "eg_simdata",
                              paste0("eg_simdata", species, ".csv")),
            row.names = FALSE)
}

saveRDS(simResultsList,
        file = here::here("notebook",
                          "manuscript",
                          "eg_simdata",
                          paste0("eg_simdata_completelist.RDS")),
        compress = TRUE)

testrds <- readRDS(here::here("notebook",
                              "manuscript",
                              "eg_simdata",
                              paste0("eg_simdata_completelist.RDS")))
