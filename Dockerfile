# Container image for GAMMa. Build from the repo root:
#   docker build -t gamma .
#   docker run --rm -p 3838:3838 gamma
# Uses the exact package versions in renv.lock.
FROM rocker/shiny-verse:4.6.1

RUN apt-get update && apt-get install -y --no-install-recommends \
      libgdal-dev libgeos-dev libproj-dev libudunits2-dev pandoc \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/gamma

# Restore packages first so the layer is cached across app-code changes
COPY renv.lock .renvignore ./
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" \
 && R -e "renv::restore(lockfile = 'renv.lock', library = .libPaths()[1], prompt = FALSE)"

COPY app.R global.R VERSION reportTemplate.Rmd reportTemplate.css ./
COPY R ./R
COPY appData ./appData
COPY www ./www

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/gamma', host = '0.0.0.0', port = 3838)"]
