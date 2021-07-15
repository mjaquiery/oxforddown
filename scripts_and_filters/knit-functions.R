knit_thesis <- function(input, output_format = "pdf", debug = F, ...){
  if (debug) {
    trace(recover, sink)
  }
  # Save to protect against `rm()` calls
  save(output_format, file = "_knit_thesis_output_format.rda")
  
  if ("pdf" %in% output_format){
    bookdown::render_book(input, output_format = "bookdown::pdf_book", ...)
    
    file.remove(list.files(pattern = "*\\.(log|mtc\\d*|maf|aux|bcf|lof|lot|out|toc)$"))
  }
  
  load("_knit_thesis_output_format.rda")
  if ("bs4" %in% output_format){
    bookdown::render_book(input, output_format = "bookdown::bs4_book", ...)
    
    # create a .nojekyll file which is needed to deploy on GitHub
    file.create(here::here("docs", ".nojekyll"))
  }
  
  load("_knit_thesis_output_format.rda")
  if ("gitbook" %in% output_format){
    bookdown::render_book(input, output_format = "bookdown::gitbook", ...)
    
    # create a .nojekyll file which is needed to deploy on GitHub
    file.create(here::here("docs", ".nojekyll"))
  }
  
  load("_knit_thesis_output_format.rda")
  if ("word" %in% output_format){
    bookdown::render_book(input, output_format = "bookdown::word_document2", ...)
  }
  
  # remove the _bookdown_files folder after the build
  unlink("_bookdown_files", recursive = TRUE)
  unlink("_knit_thesis_output_format.rda")
}