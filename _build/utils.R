library(data.table)
library(stringr)
library(brio)
library(yaml)
library(epoxy)
library(httr2)
library(purrr)
library(reticulate)
library(knitr)

.validAuthor <- function(author) {
  # Specify the fields to check for NULL values
  fields_to_check <- c("name", "orcid", "email")
  # Check each field for NULL and return TRUE if any field is not NULL
  for (field in fields_to_check) {
    if (!is.null(author[[field]])) {return(TRUE)}
  }
}

.encode <- function(language="ES"){
  # Codifica los archivos de sourceFolder y los guarda en markupFolder/LANG
  LANG <- language
  # ------------------------------------------------------
  
  IPATH <- file.path(projectFolder) 
  OPATH <- file.path(markupFolder,LANG ) 
  if(!dir.exists(OPATH)) dir.create(OPATH,recursive = TRUE)
  
  DIRS <- list.dirs(IPATH, full.names = TRUE, recursive = TRUE) 
  DIRS <- grep("^_", basename(DIRS), value = TRUE, invert = TRUE)

  FILES <- sapply(DIRS, function(dir) {
    list.files(dir, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)
  }, USE.NAMES = FALSE) |> unlist()
  
  INDEX <- NULL
  for(FILE in FILES){
    SOURCE <- brio::read_lines(FILE) |> paste(collapse = "\n")
    
    if (length(SOURCE) == 0 || nzchar(tail(SOURCE, 1))) {
      # Agrega una nueva línea vacía al contenido
      SOURCE <- c(SOURCE, "")}
    sprintf("Fix headings in %s...",FILE) |> cat(fill=TRUE)
    SOURCE <- gsub(pattern =  "^(#+)([^\\s])",replacement =  "\\1 \\2", SOURCE)
    
    sprintf("Markup source: %s...",FILE) |> cat(fill=TRUE)
    # ------------------------------------------------------
    TYPE <- "captions"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( SOURCE,type=TYPE)
    INDEX <-  rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "includes"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "code"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "math"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    
    # ------------------------------------------------------
    TYPE <- "inline_r"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "references"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "citations"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "figures"
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TYPE <- "equations"
    
    sprintf("> %s",TYPE) |> cat(fill=TRUE)
    TAG <- .markup( TAG$text,type=TYPE)
    INDEX <- rbindlist(list(INDEX,TAG$index))
    # ------------------------------------------------------
    TEXT <- TAG$text
    brio::write_lines(TEXT,path=file.path(OPATH,basename(FILE)))
    
  }
  saveRDS(INDEX,file.path(OPATH,markup_filename))
}

.translate <- function(source_language="ES",target_language="EN",auth_key=Sys.getenv("DEEPL_AUTH_KEY")){
  # Traduce los archivos de markupFolder/SOURCE_LANG y los guarda en markupFolder/TARGET_LANG
  SOURCE_LANG <- source_language
  TARGET_LANG <- target_language
  AUTH_KEY <- auth_key
  # ------------------------------------------------------
  IPATH <- file.path(markupFolder,SOURCE_LANG) 
  stopifnot(dir.exists(IPATH))
  
  # if(!dir.exists(IPATH)){
  #   .encode(language =source_language)
  # }
  
  OPATH <- file.path(markupFolder,TARGET_LANG ) 
  if(!dir.exists(OPATH)) {
    dir.create(OPATH,recursive = TRUE)
  } 
  
  CONTEXT <- "Multivariate linear regression models, earthquake engineering, seismic ground motions, dynamic site response, seismic design, mining infrastructure, ground-motion intensity measures, peak-ground accelerations(PGA), peak-ground velocities (PGV), Arias Intensity (AI)."
  
  FILES <- list.files(IPATH,pattern = "\\.qmd$",full.names = FALSE)
  # ----------------------------------------------------------------------------
  # Build CRC for INDEX
  
  
  FILE <- file.path(IPATH,markup_filename)
  INDEX <- readRDS(FILE)
  
  FILE.CRC <- file.path(OPATH,paste0(".",markup_filename,".crc"))
  CRC <- digest::digest(INDEX,algo="sha512")
  OK <- !file.exists(FILE.CRC) || (file.exists(FILE.CRC) && brio::read_lines(FILE.CRC) != CRC)
  
  
  if(OK){
    sprintf("Translating captions index to %s...",TARGET_LANG) |> cat(fill=TRUE)
    INDEX[TYPE=="captions",VALUE:=deeplr::translate(
      text = VALUE,
      source_lang = SOURCE_LANG,
      target_lang = TARGET_LANG,
      auth_key = AUTH_KEY,
      split_sentences = TRUE,
      # endpoint="https://api.deepl.com/v2/translate",
      # formality="prefer_more",
      preserve_formatting=TRUE,      #context= CONTEXT
      )]
    saveRDS(INDEX,file.path(OPATH,markup_filename))
    # FILE.CRC <- file.path(OPATH,paste0(".",markup_filename,".crc"))
    # CRC <- digest::digest(INDEX,algo="sha512")
    brio::write_lines(text=CRC,path=FILE.CRC)
  }
  
  
  for(FILE in FILES){
    
    SOURCE <- brio::read_lines(path=file.path(IPATH,FILE))
    EOL <- brio::file_line_endings(file.path(IPATH,FILE))
    
    CRC <- digest::digest(SOURCE,algo="sha512")
    FILE.CRC <- file.path(OPATH,paste0(".",FILE,".crc"))
    
    OK <- !file.exists(FILE.CRC) || (file.exists(FILE.CRC) && brio::read_lines(FILE.CRC) != CRC)
    
    if(OK){
      sprintf("Translating source %s to %s...",FILE,TARGET_LANG) |> cat(fill=TRUE)
      
      TEXT <- deeplr::translate(
        text = paste(SOURCE,collapse = "\n"),
        source_lang = SOURCE_LANG,
        target_lang = TARGET_LANG,
        auth_key = Sys.getenv("DEEPL_AUTH_KEY"),
        # endpoint="https://api.deepl.com/v2/translate",
        # formality="prefer_more",
        split_sentences = TRUE,
        preserve_formatting=TRUE,        #context= CONTEXT
        )
      brio::write_lines(TEXT,path=file.path(OPATH,FILE), eol=EOL)
      # CRC <- digest::digest(paste(TEXT,collapse = "\n"),algo="sha512")
      # FILE.CRC <- file.path(OPATH,paste0(".",FILE,".crc"))
      brio::write_lines(text=CRC,path=FILE.CRC)
      
    }
    
    
    
  }
}

.markup <- function(text,type="markdown"){
  REGEX <- list(
    captions = "#\\| (fig|tbl)-cap:\\s+\"(.*?)\"",
    
    math ="\\$\\$\\s*([^$]+?)\\s*\\$\\$|\\$\\s*([^$]+?)\\s*\\$|\\$\\s*([^$]+?)\\s*\\$",
    references = "(?:(?:@fig-|@tbl-|@eq-)\\w+|\\[@(?:fig-|tbl-|eq-)\\w+\\])",
    citations = "\\\\?\\[\\-?@([\\w,]+)\\\\?\\]",
    includes ="\\{\\{.*?\\}\\}",
    figures = "```\\{[^}]*\\}(?:.|\\n)*?```",
    equations = "```\\{[^}]*\\}(?:.|\\n)*?```",
    code = "```\\{r\\}(?:.|\\n)*?```",
    
    inline_r = "`r .*?`",
    footnotes = "\\[\\^\\d+\\]",
    links = "\\[\\[(.*?)\\]\\]",
    images = "!\\[.*?\\]\\((.*?)\\)",
    tables = "```\\{[^}]*\\}(?:.|\\n)*?```",
    bold = "\\*\\*([^*]+?)\\*\\*",
    italic = "\\*([^*]+?)\\*",
    strikethrough = "~~(.*?)~~",
    superscript = "\\^\\^(.*?)\\^\\^",
    subscript = "~\\~(.*?)~\\~",
    underline = "__([^_]+?)__",
    highlight = "==(.*?)==",
    smallcaps = "\\[\\^(\\w+)\\]",
    quotes = "\"(.*?)\"",
    acronyms = "\\b([A-Z]{2,})\\b",
    abbreviations = "\\b([A-Z]{2,})\\b",
    units = "\\b([A-Z]{2,})\\b",
    dates = "\\b(\\d{4}-\\d{2}-\\d{2})\\b",
    times = "\\b(\\d{2}:\\d{2}:\\d{2})\\b",
    numbers = "\\b(\\d+\\.?\\d*)\\b",
    currencies = "\\b(\\d+\\.?\\d*)\\b",
    percentages = "\\b(\\d+\\.?\\d*)\\b",
    emojis = ":[^:]+?:",
    ellipses = "\\.{3}",
    dashes = "--|—",
    arrows = "=|->",
    chemicals = "\\b([A-Z][a-z]?\\d?)\\b",
    cas = "\\b(\\d{2,7}-\\d{2}-\\d)\\b",
    smiles = ":[^:]+?:",
    emails = "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b",
    urls = "\\bhttps?://[^\\s]+\\b",
    hashtags = "#\\w+",
    mentions = "@\\w+",
    telephones = "\\b\\d{3}-\\d{3}-\\d{4}\\b",
    ipaddresses = "\\b\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\b",
    html = "<[^>]+>",
    xml = "<[^>]+>",
    markdown = "```\\{[^}]*\\}(?:.|\\n)*?```",
    rmarkdown = "```\\{r\\}(?:.|\\n)*?```"
  )
  stopifnot(type %in% names(REGEX))
  PATTERN <- REGEX[[type]]
  SOURCE <- text
  TAG <- NULL
  INDEX <- NULL
  TEXT <- paste(SOURCE, collapse = "\n") 
  IDX <- stringr::str_extract_all(TEXT, pattern =  PATTERN) |> as.data.table()  
  # IDX <- re_matches(data=TEXT,pattern = PATTERN,global = TRUE) |> as.data.table() |> na.omit()
  
  if (nrow(IDX) > 0) {
    IDX <- IDX[,.(TYPE=type,VALUE=V1,ID=sapply(V1, function(x){
      paste0("<ID.",toupper(digest::digest(object=x,algo="crc32")),">")}
    ))]
    
    AUX <- TEXT
    for (i in 1:nrow(IDX)) {
      AUX <- gsub(IDX$VALUE[i], IDX$ID[i], AUX, fixed = TRUE)
    }
    INDEX <- data.table::rbindlist(list(INDEX,IDX),use.names = TRUE) |> unique()
    TEXT <- AUX
    
  }
  TAG <- list(source=SOURCE,text=TEXT,index=INDEX)
  return(TAG)
}

.decode <- function(language="ES"){
  LANG <- language
  # ------------------------------------------------------
  IPATH <- file.path(markupFolder,LANG) 
  stopifnot(dir.exists(IPATH))  
  OPATH <- file.path(renderFolder,LANG,"qmd") 
  if(!dir.exists(OPATH)) dir.create(OPATH,recursive = TRUE)
  
  FILES <- list.files(IPATH,pattern = "\\.qmd$",full.names = FALSE)

  INDEX <- readRDS(file.path(IPATH,markup_filename)) |> as.data.table()
  
  # Fix Paths
  INDEX[TYPE=="includes",VALUE:=gsub(x=VALUE,pattern =  "\\{\\{< include article/qmd/([^/]+\\.qmd) >\\}\\}",replacement =  paste0("{{< include ", OPATH, "/\\1 >}}"))]
  
  for(FILE in FILES){
    sprintf("Rebuilding source %s to %s...",FILE,LANG) |> cat(fill=TRUE)
    SOURCE <- brio::read_lines(file.path(IPATH,FILE))
    EOL <- brio::file_line_endings(file.path(IPATH,FILE))
    AUX <- SOURCE
    for (i in 1:nrow(INDEX)) {
      AUX <- gsub(INDEX$ID[i], INDEX$VALUE[i], AUX, fixed = TRUE)
    }
    
    TEXT <- AUX
    sprintf("Rebuilding captions in %s...",LANG) |> cat(fill=TRUE)
    AUX <- TEXT
    for (i in 1:nrow(INDEX)) {
      AUX <- gsub(INDEX$ID[i], INDEX$VALUE[i], AUX, fixed = TRUE)
    }
    
    TEXT <- AUX
    # Write File
    
    brio::write_lines(TEXT,path=file.path(OPATH,FILE), eol=EOL)
    
  }
}

.buildYAML <- function(language="ES",output_format=c("html","docx","els-pdf"),output_dir=exportFolder){
  
  if(!dir.exists(output_dir)) dir.create(output_dir)
  
  LANG <- language
  
  # ---------------------------------------------------------------------------
  # Language-independent stage
  DATA <- list()
  
  FIELD <- list(project=list(
    type="default",
    'output-dir'=output_dir
  ),engine="knitr",jupyter="python3")
  DATA <- c(DATA,FIELD)
  
  FILE <- list.files(file.path(buildFolder),pattern = "_params\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE) ## |> paste(collapse = "\n")
    DATA <- c(DATA,FIELD)
  } 
  FILE <- list.files(file.path(projectFolder) ,pattern = "references\\.bib$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- FILE
    FIELD <- list(bibliography=VAR)
    DATA <- c(DATA,FIELD)
  } 
  
  FILE <- list.files(file.path(buildFolder) ,pattern = "styles\\.docx$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- FILE
    FIELD <- list("reference-doc"=VAR)
    DATA <- c(DATA,FIELD)
  } 
  
  FILE <- list.files(file.path(projectFolder) ,pattern = "_authors\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE) # #|> paste(collapse = "\n")
    FIELD$author <- Filter(.validAuthor, FIELD$author)
    DATA <- c(DATA,FIELD)
  } 
  
  FILE <- list.files(file.path(buildFolder),pattern = "_format\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE) 
    FIELD$format <- FIELD$format[names(FIELD$format) %in% output_format] 
    if("els-pdf" %in% names(FIELD$format) && is.null(FIELD$format$`els-pdf`$journal$name)){
      FIELD$format$`els-pdf`$journal$name <- gsub(DATA$subtitle, pattern="\n",replacement="")
    }
    DATA <- c(DATA,FIELD)
  } 
  
  
  # ------------------------------------------------------
  # Language-dependent stage
  
  stopifnot(dir.exists(file.path(renderFolder,LANG) ))
  LDATA <- list()
  
  FILE <- list.files(file.path(buildFolder) ,pattern = "_crossref\\.yml$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    FIELD <- read_yaml(FILE,readLines.warn=FALSE) ## |> paste(collapse = "\n")
    LDATA <- c(LDATA,FIELD[[LANG]])
  } 
  
  FILE <- list.files(file.path(renderFolder,LANG) ,pattern = "_TITLE\\.qmd$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- brio::read_lines(FILE) |> paste(collapse = "\n")
    FIELD <- list(title=VAR)
    LDATA <- c(LDATA,FIELD)
  }
  
  FILE <- list.files(file.path(renderFolder,LANG) ,pattern = "_SUBTITLE\\.qmd$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- brio::read_lines(FILE) |> paste(collapse = "\n")
    FIELD <- list(subtitle=VAR)
    LDATA <- c(LDATA,FIELD)
  }
  
  FILE <- list.files(file.path(renderFolder,LANG) ,pattern = "_ABSTRACT\\.qmd$",recursive = TRUE,full.names = TRUE)
  if(length(FILE)==1){
    VAR <- brio::read_lines(FILE) |> paste(collapse = "\n")
    FIELD <- list(abstract=VAR)
    LDATA <- c(LDATA,FIELD)
  } 
  
  # --------------------------------------------------------------------------
  FIELD <- list(params=list(
    background= "white",
    render="none",
    ext="png",
    lang=LANG
  ))
  LDATA <- c(LDATA,FIELD)
  # 
  # YAML <- as.yaml(FIELD)
  # # Read index.qmd
  # FILE <- file.path(renderFolder,LANG,"qmd",index_filename)
  # INDEX <- brio::read_lines(path=FILE) |> paste(collapse = "\n")
  # idx <- which(INDEX=="---")
  # if(length(idx)==2) {
  #   # skip YAML lines
  #   INDEX <- INDEX[-idx]
  # }
  # 
  # # Replace with new YAML code
  # TEXT <- paste0("---\n",YAML,"---\n\n\n",INDEX,"\n\n")
  # FILE <- file.path(".",index_filename)
  # brio::write_lines(text=TEXT, path=FILE)
  # ---------------------------------------------------------------------------
  YAML <-as.yaml(c(DATA,LDATA))
  TEXT <- YAML |>  gsub(pattern=":\\s*yes($|\\n)", replacement=": true\\1") |> gsub(pattern=":\\s*no($|\\n)", replacement=": false\\1")
  FILE <- file.path(".",quarto_filename)
  brio::write_lines(text=TEXT, path=FILE)
  
}

.improve <- function(language="ES",auth_key){
  SOURCE_LANG <- source_language
  AUTH_KEY <- auth_key
  # ------------------------------------------------------
  IPATH <- file.path(markupFolder,SOURCE_LANG) 
  stopifnot(dir.exists(IPATH))
  OPATH <- file.path(markupFolder,SOURCE_LANG) 
  
}

.chatCompletion <- function(
    model,
    isAzure=FALSE,
    messages = NULL,
    temperature = 1,
    n = 1,
    stream = FALSE,
    stop = NULL,
    max_tokens = NULL,
    presence_penalty = 0,
    frequency_penalty = 0,
    logit_bias = NULL,
    user = NULL,
    openai_organization = Sys.getenv("OPENAI_ORGANIZATION")
) {
  
  
  if(isAzure==FALSE){
    API_KEY <-  Sys.getenv("OPENAI_API_KEY")
    BASE_URL <- "https://api.openai.com/v1/chat/completions"
  } 
  
  if(isAzure==TRUE){
    API_KEY <-  Sys.getenv("AZURE_API_KEY")
    API_VERSION <-  "2023-08-01-preview"
    RESOURCE_ID <- "na-oai-canadaeast"
    BASE_URL <-  epoxy::epoxy("https://{RESOURCE_ID}.openai.azure.com/openai/deployments/{model}/chat/completions?api-version={API_VERSION}")
  }
  
  
  #---------------------------------------------------------------------------
  # Build path parameters
  HEADERS <- "application/json"
  
  if (!is.null(openai_organization)) {
    HEADERS["OpenAI-Organization"] <- openai_organization
  }
  
  #---------------------------------------------------------------------------
  # Build request body
  
  body <- list()
  body[["model"]] <- model
  body[["messages"]] <- messages
  body[["temperature"]] <- temperature
  body[["n"]] <- n
  body[["stream"]] <- stream
  body[["stop"]] <- stop
  body[["max_tokens"]] <- max_tokens
  body[["presence_penalty"]] <- presence_penalty
  body[["frequency_penalty"]] <- frequency_penalty
  body[["logit_bias"]] <- logit_bias
  body[["user"]] <- user
  
  
  #---------------------------------------------------------------------------
  # Make a request and parse it
  
  if(isAzure==TRUE){
    response <- httr2::request(BASE_URL) |>
      httr2::req_headers("Content-Type" = HEADERS, "api-key" = API_KEY) |>
      httr2::req_user_agent("@averriK") |>
      httr2::req_body_json(body) |> 
      httr2::req_retry(max_tries = 4) |>
      httr2::req_throttle(rate = 15) |>
      httr2::req_perform() 
  } 
  
  if(isAzure==FALSE){
    response <- httr2::request(BASE_URL) |>
      httr2::req_auth_bearer_token(token = API_KEY) |>
      httr2::req_headers("Content-Type" = "application/json") |> 
      httr2::req_user_agent("@averriK") |>
      httr2::req_body_json(body) |> 
      httr2::req_retry(max_tries = 4) |>
      httr2::req_throttle(rate = 40) |> #3500 RPM = 3500/60=60 
      httr2::req_perform() 
  }
  
  
  parsed <- response |> httr2::resp_body_json(simplifyVector = TRUE)
  
  #---------------------------------------------------------------------------
  # Check whether request failed and return parsed
  
  
  if(resp_is_error(response)) {
    paste0(
      "OpenAI API request failed [",
      # httr::status_code(response),
      httr2::resp_status(response),"] - [",
      httr2::resp_status_desc(response),"] - [",
      httr2::resp_check_status(response),
      "]:\n\n",
      parsed$error$message
    ) %>%
      stop(call. = FALSE)
  }
  
  return(parsed)
  
}
