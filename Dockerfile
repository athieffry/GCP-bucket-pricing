FROM rocker/shiny-verse:4.0.0

RUN ["install2.r", "shiny", "tidyverse", "magrittr", "reshape2", "shinythemes", "ggplot2"]

COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

COPY app.R /srv/shiny-server/app.R

EXPOSE 8080

RUN sudo chown -R shiny:shiny /srv/shiny-server

CMD ["/usr/bin/shiny-server"]
