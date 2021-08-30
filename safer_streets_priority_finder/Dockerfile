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
   sqlite3 \
   libsqlite3-dev \
   libnode-dev \
   bzip2 \
   libfontconfig \
   libsodium-dev \
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
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("htmlwidgets",upgrade="never", version = "1.5.3")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@aaae5c8788802a7b4aef4df23691902a286dd964")'

RUN Rscript -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.4")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "0.9-6")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("rpostgis",upgrade="never", version = "1.4.3")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.2")'
RUN Rscript -e 'remotes::install_version("rgdal",upgrade="never", version = "1.5-18")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("sp",upgrade="never", version = "1.4-4")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.5")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.30")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.6")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.1.3")'
RUN Rscript -e 'remotes::install_version("promises",upgrade="never", version = "1.2.0.1")'
RUN Rscript -e 'remotes::install_version("future",upgrade="never", version = "1.21.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.3")'
RUN Rscript -e 'remotes::install_version("cartography",upgrade="never", version = "3.0.0")'
RUN Rscript -e 'remotes::install_version("osmdata",upgrade="never", version = "0.1.5")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("webshot",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("tinytex",upgrade="never", version = "0.32")'
RUN Rscript -e 'tinytex::install_tinytex()'
RUN Rscript -e 'remotes::install_version("shinydisconnect",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("leafgl",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("emayili",upgrade="never", version = "0.4.15")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.2.2")'
RUN Rscript -e 'remotes::install_version("aws.s3",upgrade="never", version = "0.3.21")'

COPY env_variables.R /srv/shiny-server/App/env_variables.R

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
COPY . /srv/shiny-server/App
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');safer.streets.priority.finder::run_app()"]

