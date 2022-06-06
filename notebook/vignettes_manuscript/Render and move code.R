
rmarkdown::render(input = here::here("notebook",
                               "vignettes_manuscript",
                               "Agent-based_model_walkthrough.Rmd"),
                  output_format = "bookdown::pdf_document2")

file.copy(from = here::here("notebook",
                      "vignettes_manuscript",
                      "Agent-based_model_walkthrough.pdf"),
          to = here::here("vignettes", "Agent-based_model_walkthrough.pdf"),
          overwrite = TRUE)
# file.remove(here("Agent-based_model_walkthrough.pdf"))
