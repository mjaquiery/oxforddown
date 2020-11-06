#' Setup script for thesis chapters
#'
#' - Matt Jaquiery, 16/07/2020
#'
#' This script loads various libraries and sets options and themes so that they
#' are consistent in each chapter and will not be modified by changes in
#' previous chapters.


# Clear workspace ---------------------------------------------------------

rm(list = ls())

options(
  ESM.recalculate = F # Whether to recalculate the very computationally expensive chunks
)
if (!dir.exists('_cache')) 
  dir.create('_cache')

# Libraries ---------------------------------------------------------------
# Load libraries which are widely shared by the chapter files

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

#' Caption figures with a short caption (up to double-space) and longer caption thereafter
#' Convert any double-spaces to \newline
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
      legend.position = 'top',
      strip.text = element_text(colour = 'black')
    )
)

broken_axis <- theme(axis.line.y = element_line(arrow = arrow(ends = 'both', length = unit(6, 'points'), type = 'closed')))
broken_axis_top <- theme(axis.line.y = element_line(arrow = arrow(ends = 'last', length = unit(6, 'points'), type = 'closed')))
broken_axis_bottom <- theme(axis.line.y = element_line(arrow = arrow(ends = 'first', length = unit(6, 'points'), type = 'closed')))

simulation <- theme(
  plot.background = element_rect(linetype = 'dashed', colour = 'black', size = 1),
  plot.margin = margin(10, 10, 10, 10, 'pt')
)

# For prettyMD ------------------------------------------------------------

# Here is a bunch of stuff for plotting etc. which should eventually go in prettyMD

#' GGplot2 helper function to get offset x coordinates to the left or right
#' depending on whether the x coordinate is to the left or right.
#' @param x vector of x coordinates (typically factors)
#' @param amount amount to nudge in the appropriate direction
#' @param direction to nudge. 'outwards' is towards the extremes, 'inwards' is towards the centre.
nudge <- function(x, amount, direction = 'outwards') {
  if (!is.factor(x)) 
    x <- factor(x)
  x <- as.numeric(x) 
  dir <- if (direction == 'outwards') 1 else -1
  x + sign(x - mean(range(x, na.rm = T))) * dir * amount
}

#' Adapted from https://stackoverflow.com/a/45614547 
#' The split violin shows only one half of the violin plot, 
#' specifically the left half for group = 1 and the right half for group = 2
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * width * (x - xmin), xmaxv = x + violinwidth * width * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- data[1, "x"]
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


#' Compare models within a anovaBF output to get relative likelihood
#' @param x the BFBayesFactor object containing the results
#' @param comparisons list of values for the comparisons. Values can be row
#'   numbers or model strings. Pairs of values will be compared to one another;
#'   individual values will be included directly (compared to null model).
#' @return data frame with columns M1, M2, BF(M1,M2)
marginalBF <- function(x, comparisons) {
  ns <- rownames(x@bayesFactor)
  getIndex <- function(i) if (i %in% ns) which(ns == i) else i
  bf <- function(a, b) exp(a - b)
  out <- NULL
  for (comp in comparisons) {
    if (length(comp) == 1) {
      a <- getIndex(comp[1])
      out <- rbind(out, data.frame(
        M1 = ns[a],
        M2 = x@denominator@longName,
        BF.M1.M2 = exp(x@bayesFactor$bf[a])
      ))
    } else {
      a <- getIndex(comp[1]); b <- getIndex(comp[2]);
      out <- rbind(out, data.frame(
        M1 = ns[a], 
        M2 = ns[b],
        BF.M1.M2 = bf(x@bayesFactor$bf[a], x@bayesFactor$bf[b])
      ))
    }
    
  }
  if (!is.null(names(comparisons)))
    rownames(out) <- names(comparisons)
  out
}


# For esmData -------------------------------------------------------------

# This stuff might belong better in esmData

#' Go through the workspace and remove excluded pids from all tbls with a pid 
#' field, creating backups along the way
#' @param exclusions tbl with a pid column and n>1 logical columns where TRUE indicates that a participant is excluded for columnName reason
#' @param envir environment in which to do modifications
#' @param backup whether to back up previous version of objects as all.[objectName]
do_exclusions <- function(exclusions, envir = .GlobalEnv, backup = T) {
  # Prepare excluded pid list
  exclusions$excluded <- exclusions %>% select(-pid) %>% apply(1, any)
  pids <- exclusions$pid[!exclusions$excluded]
  
  for (o in ls(envir = envir)) {
    if (o == 'exclusions') next()
    if (str_starts(o, 'all\\.')) next()
    obj <- get(o, envir = envir)
    if ('pid' %in% names(obj)) {
      if (backup & paste0('all.', o) %in% ls(envir = envir)) {
        warning(paste0('all.', o, ' already exists: overwriting.'))
      }
      if (backup) {
        assign(paste0('all.', o), obj, envir = envir)
      }
      obj <- obj[obj$pid %in% pids, ]
      assign(o, obj, envir = envir)
    }
  }
}

#' Return the name of an advice type profile
#' @param advisorType vector of advisor types
#' @return vector of type names
advisor_profile_name <- function(advisorType) {
  case_when(
    advisorType == 3 ~ 'Bias sharing',
    advisorType == 4 ~ 'Anti-bias',
    advisorType == 5 ~ 'High accuracy',
    advisorType == 6 ~ 'Low accuracy',
    advisorType == 7 ~ 'High agreement',
    advisorType == 8 ~ 'Low agreement',
    advisorType == 9 ~ 'High accuracy',
    advisorType == 10 ~ 'High agreement',
    T ~ NA_character_
  )
}

#' Return the name of an advice type profile for the Dates task data
#' @param advisor0idDescription vector of advisor descriptions
#' @return vector of nice names
advisor_description_name <- function(advisor0idDescription) {
  case_when(
    advisor0idDescription == 'highAccuracy' ~ 'High accuracy',
    advisor0idDescription == 'lowAccuracy' ~ 'Low accuracy',
    advisor0idDescription == 'highAgreement' ~ 'High agreement',
    advisor0idDescription == 'lowAgreement' ~ 'Low agreement',
    advisor0idDescription == 'Accurate' ~ 'High accuracy',
    advisor0idDescription == 'Agreeing' ~ 'High agreement',
    T ~ NA_character_
  )
}

#' Return a copy of x with the factors of x ordered how we want them to ensure
#' consistency across plots
#' @param x tbl whose factors should be reordered
#' @return \code{x} with reordered factors
order_factors <- function(x) {
  #' Reorder a factor 
  #' Logic is a bit twisted because R kept reversing factors even
  #' when asked nicely not to
  .f <- function(f) {
    if (length(levels(f)) != 2)
      return(f)
    if (
      all(str_detect(levels(f), c('^[hH]igh ?accuracy', 
                                  '^[hH]igh ?agreement'))) ||
      all(str_detect(levels(f), c('^[bB]ias', '^[aA]nti'))) ||
      all(str_detect(levels(f), c('^[hH]igh', '^[lL]ow'))) ||
      all(str_detect(levels(f), c('^[fF]inal', '^[iI]nitial'))) ||
      all(str_detect(levels(f), c('^[fF]eedback', '^[nN]o'))) ||
      all(str_detect(levels(f), c('^[cC]orrect', '^[iI]ncorrect'))) ||
      all(str_detect(levels(f), c('^[aA]gree', '^[dD]isagree'))) ||
      all(str_detect(levels(f), c('^[aA]s planned', '^[aA]nomalous')))
    )
      fct_rev(f)
    else
      f
  }
  mutate(x, across(.cols = where(is.factor), .f))
}


# For new analysis package ------------------------------------------------

#' Calculate the probability that confidence is greater than \code{quantiles}
#' for each quantile. This is used to obtain data for plotting receiver operator
#' characteristic curves.
#' @param df tbl to duplicate
#' @param quantiles vector of quantiles to use for subsetting
p_conf <- function(df, quantiles) {
  map(
    quantiles, 
    ~ transmute(
      df, 
      pConf = mean(ConfidenceScore > .),
      Confidence = .
    ) %>% 
      unique()
  )
}
