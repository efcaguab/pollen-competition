# Includes R 3.5.0, src build tools, rstudio, tidyverse & devtools and tex and publishing-related pacakages. R 3.5.0 was released on April 23
FROM rocker/tidyverse:3.5.0
# Installing texlive though apt-get cause I was having trouble using TinyTex
RUN apt-get update \
  && apt-get -y --no-install-recommends install texlive-full
# R dependences are installed from MRAN repo snapshotted on 2018-06-01, one day before 3.5.1 and a month before they did something weird to MuMIN
RUN R -e "install.packages('drake', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('foreach', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('lme4', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('FD', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('FactoMineR', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('missMDA', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('MuMIn', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('smatr', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('kableExtra', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
RUN R -e "install.packages('bookdown', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-06-01'))"
# Need newer ggplot for plot tags
RUN R -e "install.packages('ggplot2', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-08-01'))"
RUN R -e "install.packages('cowplot', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-08-01'))"
RUN R -e "install.packages('ggridges', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2018-08-01'))"
## This install extra latex packages, should be able to install extra packages in runtime if necessary
# Now using texlive-full, so extra packages are not needed
# RUN apt-get -y install xzdec
# RUN runuser -l rstudio -c "cd ~ && mkdir texmf && tlmgr init-usertree && tlmgr update --self"
# RUN runuser -l rstudio -c "tlmgr install lm ec booktabs titling multirow xcolor wrapfig float tabu varwidth threeparttable threeparttablex environ trimspaces ulem makecell setspace lineno"
RUN apt-get -y --no-install-recommends install pdftk
## Install latest version of ggforce. Date is different as the plots require features just made available in April 2019. Its a bit tricky because it requires a newer Rcpp
RUN apt-get -y --no-install-recommends install libudunits2-dev libgdal-dev libv8-dev
RUN R -e "install.packages(c('ggforce', 'Rcpp', 'concaveman'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-04-24'))"
