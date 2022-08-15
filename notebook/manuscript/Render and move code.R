
rmarkdown::render(input = here::here("notebook",
                                     "manuscript",
                                     "Agent-based_model_walkthrough.Rmd"),
                  output_format = "bookdown::pdf_document2")

# For in-development packages
# cffr::cff_write()
cffr::cff_gha_update()


# file.copy(from = here::here("notebook",
#                             "manuscript",
#                             "Agent-based_model_walkthrough.pdf"),
#           to = here::here("vignettes", "Agent-based_model_walkthrough.pdf"),
#           overwrite = TRUE)
# file.remove(here("Agent-based_model_walkthrough.pdf"))

# pdftools::pdf_subset(here::here("notebook",
#                                 "manuscript",
#                                 "Agent-based_model_walkthrough.pdf"),
#                      pages = 18:25,
#                      output = here::here("notebook",
#                                          "manuscript",
#                                          "Extended Data - Supplementary Material.pdf"))
#
# pdftools::pdf_subset(here::here("notebook",
#                                 "manuscript",
#                                 "Agent-based_model_walkthrough.pdf"),
#                      pages = 1:17,
#                      output = here::here("notebook",
#                                          "manuscript",
#                                          "Agent-based_model_walkthrough FRONT.pdf"))
# pdftools::pdf_subset(here::here("notebook",
#                                 "manuscript",
#                                 "Agent-based_model_walkthrough.pdf"),
#                      pages = 26:31,
#                      output = here::here("notebook",
#                                          "manuscript",
#                                          "Agent-based_model_walkthrough BACK.pdf"))
#
# pdftools::pdf_combine(c(
#   here::here("notebook",
#              "manuscript",
#              "Agent-based_model_walkthrough FRONT.pdf"),
#   here::here("notebook",
#              "manuscript",
#              "Agent-based_model_walkthrough BACK.pdf")),
#   output = here::here("notebook",
#                       "manuscript",
#                       "Agent-based_model_walkthrough FULL.pdf"))
#
# file.remove(here::here("notebook",
#                        "manuscript",
#                        "Agent-based_model_walkthrough FRONT.pdf"))
# file.remove(here::here("notebook",
#                        "manuscript",
#                        "Agent-based_model_walkthrough BACK.pdf"))

#### REMOVE THE FOLLOWING FROM THE TEX FILE

# \clearpage
# \setcounter{table}{0}  \renewcommand{\thetable}{S\arabic{table}} \setcounter{figure}{0} \renewcommand{\thefigure}{S\arabic{figure}}
#
# \hypertarget{supplementary-materials}{%
#   \section{Supplementary Materials}\label{supplementary-materials}}

# \hypertarget{references}{%
#   \section*{References}\label{references}}
# \addcontentsline{toc}{section}{References}
