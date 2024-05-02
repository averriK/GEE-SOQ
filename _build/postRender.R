EXTENSIONS <- c("spl", "bst", "cls", "md", "aux", "log", "tex", "jpg", "sty", "docx", "pdf", "html")
PATTERN <- paste0("\\.(", paste(EXTENSIONS, collapse = "|"), ")$")
LIST <- list.files(path=".",pattern = PATTERN, ignore.case = TRUE)
if (length(LIST) > 0) {
  file.remove(LIST)
} 