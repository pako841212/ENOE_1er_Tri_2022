getwd()

library(dplyr)
library(survey)
library(foreign)

sdem22<-read.dbf("ENOEN_SDEMT122.dbf")
class(sdem22)
names(sdem22)
head(sdem22)

sum(sdem22$FAC_TRI)
sdem22$FAC_TRI

sdem22$EDA=as.numeric(as.character(sdem22$EDA))
class(sdem22$EDA)

#Poblaci髇 total

sdem22 %>% filter(R_DEF=="00", C_RES==1|C_RES==3)%>% summarise(POBLACION=sum(FAC_TRI))


#Poblaci贸n de 15 a帽os o m谩s
sdem22 %>% filter(R_DEF=="00",
                  C_RES==1|C_RES==3,
                  EDA>=15 & EDA<=98)%>%
  summarise(POBLACION_15omas=sum(FAC_TRI))
## Hom

sdem22 %>% filter(R_DEF=="00",
                  C_RES==1|C_RES==3,SEX==1,
                  EDA>=15 & EDA<=98)%>%
  summarise(POBLACION_15omas_H=sum(FAC_TRI))
## Muj

sdem22 %>% filter(R_DEF=="00",
                  C_RES==1|C_RES==3,SEX==2,
                  EDA>=15 & EDA<=98)%>%
  summarise(POBLACION_15omas_M=sum(FAC_TRI))



#Poblaci贸n Economicamente activa
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE1==1)%>%
  summarise(PEA=sum(FAC_TRI))

## hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 SEX==1,EDA>=15 & EDA<=98,
                 CLASE1==1)%>%
  summarise(PEA_H=sum(FAC_TRI))

## Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 SEX==2,EDA>=15 & EDA<=98,
                 CLASE1==1)%>%
  summarise(PEA_M=sum(FAC_TRI))


# PNEA
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE1==2)%>%
  summarise(PNEA=sum(FAC_TRI))

## Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE1==2)%>%
  summarise(PNEA_H=sum(FAC_TRI))

##Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE1==2)%>%
  summarise(PNEA_M=sum(FAC_TRI))

# PO

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==1)%>%
  summarise(PO=sum(FAC_TRI))

## Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==1)%>%
  summarise(PO_H=sum(FAC_TRI))

## Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==1)%>%
  summarise(PO_M=sum(FAC_TRI))

# PD

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==2)%>%
  summarise(PD=sum(FAC_TRI))
## Hom
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==2)%>%
  summarise(PD_H=sum(FAC_TRI))

## Muj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==2)%>%
  summarise(PD_M=sum(FAC_TRI))


# PDis
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==3)%>%
  summarise(PDis=sum(FAC_TRI))

##Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==3)%>%
  summarise(PDis_H=sum(FAC_TRI))

## MUj

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==3)%>%
  summarise(PDis_M=sum(FAC_TRI))



#PnDis

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,
                 EDA>=15 & EDA<=98,
                 CLASE2==4)%>%
  summarise(PnDis=sum(FAC_TRI))

## Hom

sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==1,
                 EDA>=15 & EDA<=98,
                 CLASE2==4)%>%
  summarise(PnDis_H=sum(FAC_TRI))

## Muj
sdem22%>% filter(R_DEF=="00",
                 C_RES==1|C_RES==3,SEX==2,
                 EDA>=15 & EDA<=98,
                 CLASE2==4)%>%
  summarise(PnDis_M=sum(FAC_TRI))


# Poblaci髇 total para Sonora

sdem22 %>% filter(R_DEF=="00", C_RES==1|C_RES==3,ENT==26)%>% 
    summarise(POBLACION_total_SON=sum(FAC_TRI))

#Poblaci贸n de 15 a帽os o m谩s para Sonora
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


#Poblaci贸n Economicamente activa Sonora
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



#Poblaci髇 ocupada y remunera

ocupados22=sdem22 %>% 
  filter(CLASE2 == 1,
         ING_X_HRS>0,
         C_RES==1 | C_RES==3,
         EDA>=15 & EDA<=98)%>%
  select(FAC_TRI,EST_D_TRI, UPM,ING_X_HRS,INGOCUP,HRSOCUP, EDA, SEX,ENT, NIV_INS, ANIOS_ESC, SALARIO)









