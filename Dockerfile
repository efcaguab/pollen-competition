# Includes R 3.5.0, src build tools, rstudio, tidyverse & devtools and tex and publishing-related pacakages. R 3.5.0 was released on April 23
FROM rocker/tidyverse:3.5.0
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
# Updating tinytex 
#RUN R -e "install.packages('tinytex', repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-02-01'))"
#RUN R -e "tinytex::install_tinytex(force = TRUE)"
#RUN R -e "tinytex::tlmgr_install('collection-latexextra')"
#RUN echo "PATH=${PATH}" >> /usr/local/lib/R/etc/Renviron