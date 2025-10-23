# abmAnimalMovement

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6951937.svg)](https://doi.org/10.5281/zenodo.6951937)

--------------------------------------------------------------------------------

The *abmAnimalMovement* simulates animal movement use a discrete time agent-based model, programmed in C++ via the Rcpp package. The simulations include a number of key internal and external movement influences, as well as parameters for navigation and mobility capacity of the animal.

A more complete description of the package, alongside a demonstration can be found at DOI: [https://doi.org/10.12688/f1000research.124810.1](https://doi.org/10.12688/f1000research.124810.1). Or a draft version of that manuscript within the package Github [here](https://github.com/BenMMarshall/abmAnimalMovement/blob/main/notebook/manuscript/Agent-based_model_walkthrough.pdf).

--------------------------------------------------------------------------------

## Installation

**Install with GitHub**

To install the development versions of the package from GitHub, use the `install_github` function from the `devtools` library. Ensure that build_vignettes = TRUE if you want to use the minimally reproducible example code.

```
install.packages("devtools")
devtools::install_github("BenMMarshall/abmAnimalMovement", dependencies = TRUE, build_vignettes = TRUE)
```

**Install from CRAN**

*TBC*

<!-- 
To install [this package](https://CRAN.R-project.org/package=abmAnimalMovement) from CRAN.

```
install.packages("abmAnimalMovement")

```
-->

## Core simulation function

The `abm_simulate()` function is the main purpose of the package. Guidance on how to parametrise the simulation can be found in accompanying vignette documentation; however, a more detailed walk-through can be found at: [https://doi.org/10.12688/f1000research.124810.1](https://doi.org/10.12688/f1000research.124810.1).
