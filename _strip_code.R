breakStr <- "!!!BREAK!!!"
files <- list.files(pattern = ".Rmd$")
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
  
  fw <- file(paste0("_codeless_files/", f), "w")
  writeLines(clean, fw)
  close(fw)
}