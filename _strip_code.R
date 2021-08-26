breakStr <- "!!!BREAK!!!"
files <- list.files(pattern = ".Rmd$")
directory <- "_codeless_files"

if (!dir.exists(directory))
  dir.create(directory)

for (f in files) {
  fr <- file(f, "r")
  content <- readLines(fr)
  close(fr)
  
  content_chr <- paste(content, collapse = breakStr)
  
  clean_chr <- gsub(
    "(```{r [^}]*})[\\s\\S]+?```", 
    paste0("\\1", breakStr, "```"), 
    content_chr, 
    perl = T
  )
  clean <- stringr::str_split_fixed(clean_chr, breakStr, Inf)
  
  fw <- file(paste0(directory, "/", f), "w")
  writeLines(clean, fw)
  close(fw)
}