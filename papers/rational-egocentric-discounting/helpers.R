save_figure <- function(filename = stop("Filename is required"), ...) {
  ggsave(
    filename = filename,
    width = 800,
    height = 1200,
    dpi = 200,
    units = "px",
    bg = "white",
    ...
  )
}