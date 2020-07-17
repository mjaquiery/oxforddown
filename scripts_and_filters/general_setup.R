#' Setup script for thesis chapters
#'
#' - Matt Jaquiery, 16/07/2020
#'
#' This script loads various libraries and sets options and themes so that they
#' are consistent in each chapter and will not be modified by changes in
#' previous chapters.


# Clear workspace ---------------------------------------------------------

rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)  # Pipes, tidyselectors, string manipulation, data wrangling, etc.
library(kableExtra) # Pretty formatting of output tables
library(esmData)    # My own data package holding the data for the project (`remotes::install_github('oxacclab/esmData')`)
library(prettyMD)   # My own formatting library for neatly formatting stats output (`remotes::install_github('mjaquiery/prettyMD)`)


# Load specific knitr stuff -----------------------------------------------

##### add chunk options for PDF output ####
# for guidance on how to create your own chunk options see
# https://ulyngs.github.io/blog/posts/2019-02-01-how-to-create-your-own-chunk-options-in-r-markdown/

knitr::knit_hooks$set(vspace_output = function(before, options, envir) {
  if (!before) {
    ## after a chunk has been evaluated
    end <- paste0("\\vspace{", options$vspace_output, "}")
    stringr::str_c(out, end)
  }
})

knitr::knit_hooks$set(quote_author = function(before, options, envir) {
  if (!before) {
    ## after a chunk has been evaluated
    latex_include <- paste0("\\\\qauthor\\{", options$quote_author, "\\}\\1")
    gsub('(\\\\end\\{savequote\\})', latex_include, txt)
  }
})

#' Caption figures with a short caption (up to \newline) and longer caption thereafter
#' Matt Jaquiery
knitr::opts_hooks$set(
  fig.caption = function(options) {
    short <- str_match(options$fig.caption, '(.+?)( {2})')
    if (!is.na(short[1]))
      options$fig.scap <- short[2]
    options$fig.cap <- gsub(' {2}', '\\\\newline ', options$fig.caption)
    options
  }
)

options(
  tinytex.verbose = TRUE,
  bookdown.render.file_scope = FALSE
)

knitr::opts_chunk$set(
  echo = F,
  warnings = F,
  messages = F
)


# Plot theme --------------------------------------------------------------

# ggplot basic theme
theme_set(
  theme_light() +
    theme(
      rect = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'),
      text = element_text(size = 14),
      legend.position = 'top'
    )
)