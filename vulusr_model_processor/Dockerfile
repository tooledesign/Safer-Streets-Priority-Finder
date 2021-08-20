FROM rocker/r-ver:4.0.3


RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
  	apt-utils \
  	ed \
  	libnlopt-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/

# Install rstan
RUN install2.r --error --deps TRUE rstan
   
RUN apt-get update && apt-get install -y \
                      git-core \
                      libcurl4-openssl-dev \
                      libgit2-dev \
                      libicu-dev \
                      libssl-dev \
                      libxml2-dev \
                      libpng-dev \
                      make pandoc \
                      pandoc-citeproc \
                      libnode-dev \
                      libpq-dev \
                      && rm -rf /var/lib/apt/lists/*


RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'

RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.16")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.6")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("RPostgreSQL",upgrade="never", version = "0.6.2")'
RUN Rscript -e 'remotes::install_version("emayili",upgrade="never", version = "0.4.15")'

COPY env_variables.R /srv/shiny-server/App/env_variables.R
RUN Rscript -e 'source("/srv/shiny-server/App/env_variables.R")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
COPY . /srv/shiny-server/App

RUN rm -rf /build_zone
EXPOSE 3839
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/App', host = '0.0.0.0', port = 3839)"]
 
