FROM islasgeci/base
RUN Rscript -e "install.packages(c('survey'), repos='http://cran.rstudio.com')"
COPY . .