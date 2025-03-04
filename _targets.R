
library(here)
ProgramName <- basename(rstudioapi::getSourceEditorContext()$path)
here::i_am(ProgramName)

# Test "src/1-intro.Rmd"
rmarkdown::render(here::here("slides", "1-intro.Rmd"), params = list(
  author = c("Waylon Howard"),
  date = format(Sys.time(), '%B %d, %Y'),
  location = "Webinar"))

pagedown::chrome_print(here::here("slides", "1-intro.html"), 
                       output = here::here("slides", "1-intro.pdf") )

#devtools::install_github("jirilukavsky/pdf2pptx")
# Convert the PDF to PowerPoint
pdf2pptx::pdf2pptx(here::here("slides", "1-intro.pdf"),
         here::here("slides", "1-intro.pptx") )


# Create zip file
#utils::zip(
#  zipfile = "materials.zip",
#  files = c("prereqs.R",
#            "slide_code.R",
#            "slide_deck.pdf",
#            "tidyverse-tools.Rproj")
#)

rmarkdown::render(here::here("slides", "2-estimation.Rmd"), params = list(
  author = c("Waylon Howard"),
  date = format(Sys.time(), '%B %d, %Y'),
  location = "Webinar"))

pagedown::chrome_print(here::here("slides", "2-estimation.html"), 
                       output = here::here("slides", "2-estimation.pdf") )

#devtools::install_github("jirilukavsky/pdf2pptx")
# Convert the PDF to PowerPoint
pdf2pptx::pdf2pptx(here::here("slides", "2-estimation.pdf"),
                   here::here("slides", "2-estimation.pptx") )

#- section 3

rmarkdown::render(here::here("slides", "3-software.Rmd"), params = list(
  author = c("Waylon Howard"),
  date = format(Sys.time(), '%B %d, %Y'),
  location = "Webinar"))

pagedown::chrome_print(here::here("slides", "3-software.html"), 
                       output = here::here("slides", "3-software.pdf") )

#devtools::install_github("jirilukavsky/pdf2pptx")
# Convert the PDF to PowerPoint
pdf2pptx::pdf2pptx(here::here("slides", "3-software.pdf"),
                   here::here("slides", "3-software.pptx") )

