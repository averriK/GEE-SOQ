source("_build/utils.R")

projectFolder <- file.path(".") 
buildFolder <- file.path("_build")
markupFolder <- file.path("_markup")
publishFolder <- file.path("_publish")
exportFolder <- file.path("_export")
renderFolder <- file.path("_render")
markup_filename <- "markup.Rds"
index_filename <- "index.qmd"
quarto_filename <- "_quarto.yml"

subdirs <- list.dirs(path = markupFolder, full.names = TRUE, recursive = TRUE)[-1]
unlink(subdirs, recursive = TRUE, force = TRUE)

subdirs <- list.dirs(path = renderFolder, full.names = TRUE, recursive = TRUE)[-1]
unlink(subdirs, recursive = TRUE, force = TRUE)

subdirs <- list.dirs(path = publishFolder, full.names = TRUE, recursive = TRUE)[-1]
unlink(subdirs, recursive = TRUE, force = TRUE)


.encode(language ="EN")
.decode(language="EN")
.buildYAML(language="EN",output_format=c("html","docx"),output_dir=publishFolder)
quarto::quarto_render(input = "index.qmd",output_format = "html",execute_params=list(ext="png",render="none"))

# **

# quarto::quarto_render(input = "index.qmd",output_format = "els-pdf",execute_params=list(ext="png",render="none"))
# quarto::quarto_render(input = "index.qmd",output_format = "docx",execute_params=list(ext="png",render="none"))
# file.remove("_quarto.yml")


# -----------------------------------------------------------------------------

# .translate(source="EN",target="ES")
# .decode(language="ES")
# .buildYAML(language="ES",output_format=c("html","els-pdf","docx"),output_dir=publishFolder)
# quarto::quarto_render(input = "index.qmd", output_format = "html",execute_params=list(ext="png",render="none"))
# quarto::quarto_render(input = "index.qmd",output_format = "els-pdf",execute_params=list(ext="png",render="none"))
# quarto::quarto_render(input = "index.qmd",output_format = "docx",execute_params=list(ext="png",render="none"))
file.remove("_quarto.yml")


# ------------------------------------------------------------------------------
source("_build/postRender.R")



