

row = 200; col = 200

gf1 <- suppressMessages(NLMR::nlm_gaussianfield(ncol = col,
                                                nrow = row,
                                                resolution = 1,
                                                autocorr_range = 40,
                                                mag_var = 5,
                                                nug = 0.2,
                                                mean = 0.5,
                                                user_seed = seed,
                                                rescale = TRUE))

forageQual <- gf1

forageQual[forageQual[] < 0.4] <- 0
# set min 0 max 1, normalise the values between 1 and 0
forageQual[] <- (forageQual[] - min(forageQual[], na.rm = TRUE)) /
  (max(forageQual[], na.rm = TRUE) - min(forageQual[], na.rm = TRUE))

moveQual <- gf1
# areas with high resources are accessible (> 0.6, increased by 0.5), but the
# fastest least resistance routes are actually edge habitat areas (0.6 to 0.3,
# increased by 1). Core areas of low resrouce are also difficult to move
# through.
moveQual[moveQual[] > 0.6] <- moveQual[moveQual[] > 0.6] + 0.5
moveQual[moveQual[] < 0.6 & moveQual[] > 0.3] <-
  moveQual[moveQual[] < 0.6 & moveQual[] > 0.3] + 1
moveQual[] <- (moveQual[] - min(moveQual[], na.rm = TRUE)) /
  (max(moveQual[], na.rm = TRUE) - min(moveQual[], na.rm = TRUE))


shelterQual <- gf1
# shelter sites are best found near the edge of high resource areas, but deeper than the best movement routes
shelterQual[shelterQual[] < 0.7 & shelterQual[] > 0.5] <-
  shelterQual[shelterQual[] < 0.7 & shelterQual[] > 0.5] + 1
shelterQual[] <- (shelterQual[] - min(shelterQual[], na.rm = TRUE)) /
  (max(shelterQual[], na.rm = TRUE) - min(shelterQual[], na.rm = TRUE))

landscapeLayersList <- list(
  "shelter" = matrix(data = raster::getValues(shelterQual),
                     nrow = row,
                     ncol = col),
  "forage" = matrix(data = raster::getValues(forageQual),
                    nrow = row,
                    ncol = col),
  "movement" = matrix(data = raster::getValues(moveQual),
                      nrow = row,
                      ncol = col))

save(landscapeLayersList,
     file = here::here("data", "landscapeLayersList.rda"),
     compress = TRUE)

