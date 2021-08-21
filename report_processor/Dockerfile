FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get install -y  --no-install-recommends\
   git-core \
   libcurl4-openssl-dev \
   libgit2-dev \
   libssh2-1-dev \
   libicu-dev \
   libpng-dev \
   libudunits2-dev \
   zlib1g-dev \
   libgdal-dev \
   libproj-dev \
   libgeos-dev \
   libssl-dev \
   libxml2-dev \
   make pandoc \
   pandoc-citeproc \
   libsqlite3-dev \
   libnode-dev \
   bzip2 \
   libfontconfig \
   && apt-get clean \
   && rm -rf /var/lib/apt/lists/*

RUN apt-get update \
      && apt-get install -y --no-install-recommends \
          curl \
      && mkdir /tmp/phantomjs \
      && curl -L https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2 \
              | tar -xj --strip-components=1 -C /tmp/phantomjs \
      && cd /tmp/phantomjs \
      && mv bin/phantomjs /usr/local/bin \
      && cd \
      && apt-get purge --auto-remove -y \
          curl \
      && apt-get clean \
      && rm -rf /tmp/* /var/lib/apt/lists/*
   
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'

RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.6")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("emayili",upgrade="never", version = "0.4.15")'

RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.5")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.30")'
RUN Rscript -e 'remotes::install_version("webshot",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("tinytex",upgrade="never", version = "0.32")'
RUN Rscript -e 'tinytex::install_tinytex()'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.3")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-6")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("RPostgreSQL",upgrade="never", version = "0.6.2")'
RUN Rscript -e 'remotes::install_version("aws.s3",upgrade="never", version = "0.3.21")'

COPY env_variables.R /srv/shiny-server/App/env_variables.R

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
COPY . /srv/shiny-server/App

RUN rm -rf /build_zone
EXPOSE 25250
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/App', host = '0.0.0.0', port = 25250)"]
 
