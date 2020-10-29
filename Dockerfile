# Produce a PDF of the thesis in a container

# Use image with base R included
FROM rstudio/r-base:4.0-focal

# Install system libraries the R packages will depend on
# Also nginx so we can serve the thesis
RUN apt-get update && apt-get install -y \
  git \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libpng-dev \
  libjpeg-dev \
  cargo \
  nginx

# Clear nginx html file
RUN rm -rf /usr/share/nginx/html/*

# # Clone thesis from github
RUN git clone https://github.com/mjaquiery/oxforddown.git
WORKDIR oxforddown
RUN git pull && \
  git checkout --track origin/thesis

# Update packages from renv.lock file
RUN R -e "renv::restore()"

# Knit PDF
RUN rm -f _main.* && \
	Rscript -e 'bookdown::render_book("index.Rmd", output_format = c("bookdown::pdf_book", "bookdown::html_book"), output_dir = "/usr/share/nginx/html/")' && \
	rm -f *.mtc* *.maf *.aux *.bcf *.lof *.lot *.out *.toc front_matter/abbreviations.aux
