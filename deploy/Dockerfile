FROM golemdocker_base
COPY renv.lock.prod renv.lock
RUN R -e 'renv::restore()'
COPY golemDocker_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz",upgrade="never")'
RUN rm /app.tar.gz
EXPOSE 5050
CMD R -e "options('shiny.port'=5050,shiny.host='0.0.0.0');library(golemDocker);golemDocker::run_app()"
