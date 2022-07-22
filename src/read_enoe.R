library(tidyverse)
library(survey)
library(foreign)

sdem22<-read_csv("data/ocupados_22.csv")



sdem22$eda=as.numeric(as.character(sdem22$eda))

#Poblaci�n total

sdem22 %>% filter(r_def=="00", c_res==1|c_res==3)%>% summarise(POBLACION=sum(fac_tri))


#Población de 15 años o más
sdem22 %>% filter(r_def=="00",
                  c_res==1|c_res==3,
                  eda>=15 & eda<=98)%>%
  summarise(POBLACION_15omas=sum(fac_tri))
## Hom

sdem22 %>% filter(r_def=="00",
                  c_res==1|c_res==3,sex==1,
                  eda>=15 & eda<=98)%>%
  summarise(POBLACION_15omas_H=sum(fac_tri))
## Muj

sdem22 %>% filter(r_def=="00",
                  c_res==1|c_res==3,sex==2,
                  eda>=15 & eda<=98)%>%
  summarise(POBLACION_15omas_M=sum(fac_tri))



#Población Economicamente activa
sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 eda>=15 & eda<=98,
                 clase1==1)%>%
  summarise(PEA=sum(fac_tri))

## hom

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 sex==1,eda>=15 & eda<=98,
                 clase1==1)%>%
  summarise(PEA_H=sum(fac_tri))

## Muj

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 sex==2,eda>=15 & eda<=98,
                 clase1==1)%>%
  summarise(PEA_M=sum(fac_tri))


# PNEA
sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 eda>=15 & eda<=98,
                 clase1==2)%>%
  summarise(PNEA=sum(fac_tri))

## Hom

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==1,
                 eda>=15 & eda<=98,
                 clase1==2)%>%
  summarise(PNEA_H=sum(fac_tri))

##Muj

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==2,
                 eda>=15 & eda<=98,
                 clase1==2)%>%
  summarise(PNEA_M=sum(fac_tri))

# PO

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 eda>=15 & eda<=98,
                 clase2==1)%>%
  summarise(PO=sum(fac_tri))

## Hom

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==1,
                 eda>=15 & eda<=98,
                 clase2==1)%>%
  summarise(PO_H=sum(fac_tri))

## Muj

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==2,
                 eda>=15 & eda<=98,
               clase2==1)%>%
  summarise(PO_M=sum(fac_tri))

# PD

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 eda>=15 & eda<=98,
                 clase2==2)%>%
  summarise(PD=sum(fac_tri))
## Hom
sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==1,
                 eda>=15 & eda<=98,
                 clase2==2)%>%
  summarise(PD_H=sum(fac_tri))

## Muj

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==2,
                 eda>=15 & eda<=98,
                 clase2==2)%>%
  summarise(PD_M=sum(fac_tri))


# PDis
sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 eda>=15 & eda<=98,
                 clase2==3)%>%
  summarise(PDis=sum(fac_tri))

##Hom

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==1,
                 eda>=15 & eda<=98,
                 clase2==3)%>%
  summarise(PDis_H=sum(fac_tri))

## MUj

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==2,
                 eda>=15 & eda<=98,
                 clase2==3)%>%
  summarise(PDis_M=sum(fac_tri))



#PnDis

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,
                 eda>=15 & eda<=98,
                 clase2==4)%>%
  summarise(PnDis=sum(fac_tri))

## Hom

sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==1,
                 eda>=15 & eda<=98,
                 clase2==4)%>%
  summarise(PnDis_H=sum(fac_tri))

## Muj
sdem22%>% filter(r_def=="00",
                 c_res==1|c_res==3,sex==2,
                 eda>=15 & eda<=98,
                 clase2==4)%>%
  summarise(PnDis_M=sum(fac_tri))


# Poblaci�n total para Sonora

sdem22 %>% filter(R_DEF=="00", C_RES==1|C_RES==3,ENT==26)%>% 
    summarise(POBLACION_total_SON=sum(FAC_TRI))

#Población de 15 años o más para Sonora
sdem22 %>% filter(R_DEF=="00",
                  C_RES==1|C_RES==3,
                  EDA>=15 & EDA<=98,ENT==26)%>%
  summarise(POBLACION_15omas_SON=sum(FAC_TRI))

## Hom
sdem22 %>% filter(R_DEF=="00",
                  C_RES==1|C_RES==3,
                  EDA>=15 & EDA<=98,ENT==26,SEX==1)%>%
  summarise(POBLACION_15omas_SON_H=sum(FAC_TRI))

## Muj
sdem22 %>% filter(R_DEF=="00",
                  C_RES==1|C_RES==3,
                  EDA>=15 & EDA<=98,ENT==26,SEX==2)%>%
  summarise(POBLACION_15omas_SON_M=sum(FAC_TRI))


#Población Economicamente activa Sonora
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE1==1, ENT==26)%>%
  summarise(PEA_SON=sum(FAC_TRI))

## hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 SEX==1,EDA>=15 & EDA<=98,
                 CLASE1==1,ENT==26)%>%
  summarise(PEA_SON_H=sum(FAC_TRI))

## Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 SEX==2,EDA>=15 & EDA<=98,
                 CLASE1==1,ENT==26)%>%
  summarise(PEA_SON_M=sum(FAC_TRI))

# PNEA Sonora
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE1==2,ENT==26)%>%
  summarise(PNEA_SON=sum(FAC_TRI))

## Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE1==2,ENT==26)%>%
  summarise(PNEA_SON_H=sum(FAC_TRI))

##Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE1==2,ENT==26)%>%
  summarise(PNEA_SON_M=sum(FAC_TRI))

# PO Sonora

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==1,ENT==26)%>%
  summarise(PO_SON=sum(FAC_TRI))

## Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==1,ENT==26)%>%
  summarise(PO_SON_H=sum(FAC_TRI))

## Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==1,ENT==26)%>%
  summarise(PO_SON_M=sum(FAC_TRI))


# PD para Sonora

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==2,ENT==26)%>%
  summarise(PD=sum(FAC_TRI))

## Hom
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==2,ENT==26)%>%
  summarise(PD_SON_H=sum(FAC_TRI))

## Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==2,ENT==26)%>%
  summarise(PD_SON_M=sum(FAC_TRI))


# PDis para Sonora
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==3,ENT==26)%>%
  summarise(PDis_SON=sum(FAC_TRI))

##Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==3,ENT==26)%>%
  summarise(PDis_SON_H=sum(FAC_TRI))

## MUj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==3,ENT==26)%>%
  summarise(PDis_SON_M=sum(FAC_TRI))



#PnDis

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==4,ENT==26)%>%
  summarise(PnDis_SON=sum(FAC_TRI))

## Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==4,ENT==26)%>%
  summarise(PnDis_SON_H=sum(FAC_TRI))

## Muj
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==4,ENT==26)%>%
  summarise(PnDis_SON_M=sum(FAC_TRI))



#Poblaci�n ocupada y remunera

ocupados22=sdem22 %>% 
  filter(CLASE2 == 1,
         ING_X_HRS>0,
         C_RES==1 | C_RES==3,
         EDA>=15 & EDA<=98)%>%
  select(FAC_TRI,EST_D_TRI, UPM,ING_X_HRS,INGOCUP,HRSOCUP, EDA, SEX,ENT, NIV_INS, ANIOS_ESC, SALARIO)









