library(tidyverse)
library(survey)
library(foreign)

sdem22<-read_csv("data/ocupados_22.csv")



sdem22$eda=as.numeric(as.character(sdem22$eda))

#Poblaci�n total

sdem22 %>% summarise(POBLACION=sum(fac_tri))

get_older_than_15_years <- function(original) {
  older_than_15_years <- original %>% filter(eda>=15 & eda<=98)
  return(older_than_15_years)
}

#Población de 15 años o más
sdem22 %>%
  get_older_than_15_years() %>%
  summarise(POBLACION_15omas=sum(fac_tri))
## Hom
get_men <- function(data) {
  return(data %>% filter(sex==1))
}

sdem22 %>%
  get_older_than_15_years() %>%
  get_men() %>%
  summarise(POBLACION_15omas_H=sum(fac_tri))
## Muj

sdem22 %>% filter(r_def=="00",
                  c_res==1|c_res==3,sex==2,
                  eda>=15 & eda<=98)%>%
  summarise(POBLACION_15omas_M=sum(fac_tri))

