# Produce a PDF of the thesis in a container
# NOTE: this builds a smaller version of the thesis for continuous integration
# testing. 
# 
# To build the full version, remove the call to set the R option 'ESM.skip'.

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

# # Clone thesis from github
RUN git clone https://github.com/mjaquiery/oxforddown.git
WORKDIR oxforddown
RUN git pull && \
  git checkout --track origin/dockertest

# nginx setup
RUN cp scripts_and_filters/docker-setup/localhost.conf \
  /etc/nginx/sites-enabled/localhost.conf

# Update packages from renv.lock file
RUN R -e "renv::restore(); tinytex::tlmgr_install('cbfonts-fd')"

# Knit PDF
RUN rm -f _main.* && \
  rm -r docs && \
	Rscript -e 'options(ESM.skip = T); bookdown::render_book("index.Rmd")'

RUN service nginx restart