###Class 03 - Data Management & Visualization###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl)

library(data.table) 

casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE) # stringsAsFalse se recomienda poner en False
casos[,table((Región))]
casos[,.N,by=.(Región)]

casos<-casos[Región=="Metropolitana",] 

saveRDS(casos,"Class_03/casosRM.rds") # para guardar la base de datos en la carpeta

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8') # se recomienda escribir en el archivo csv, ya que es de los más extendidos

writexl::write_xlsx # los :: sirven para entrar a todas las funciona de writexl

library(foreign)

write.dta

casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T) # fread permite leer en muchos tipos de funcionalidades

casosRM[,table(Sexo)]
casosRM[Sexo=="Fememino",Sexo:="Femenino"] # se reemplaza "Fememino" por "Femenino"


# Creating (factor) variables -> factor permite convertir y manipular variables de tipo str que tienen leyendas/etiquetas asociadas

class(casosRM$Sexo)
casosRM[,Sexo:=factor(Sexo)]
casosRM[,Sexo:=factor(Sexo,nmax = 2)]

head(casosRM$Sexo)
head(as.numeric(casosRM$Sexo))
levels(casosRM$Sexo)
labels(casosRM$Sexo)


table(casosRM$Sexo)
casosRM[,.N,by=.(Sexo)]
casosRM[,.N,by=.(Sexo, `Centro de salud`)]

#Collapsing by Centro de Salud  ()

casosRM[,sum(`Casos confirmados`,na.rm = T),by=.(`Centro de salud`)] # esto no está correcto, ya que se suman los casos totales
casosRM[,sum(`Casos confirmados`,na.rm = T),by=.(`Centro de salud`)][,V1/sum(V1)] # % de casos por centro de salud

casosRM[,.N,by=.(`Centro de salud`)] # esto es mejor, porque no suma el total de casos, sino que ve casos por centro de salud
casosRM[,.N,by=.(`Centro de salud`)][,N/sum(N)] # % de casos por centro de salud

casosRM[,mean(Edad,na.rm = T),by=.(`Centro de salud`)]
casosRM[,max(Edad,na.rm = T),by=.(`Centro de salud`)]



# collapsing by average age

A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)] # media de edad por centro de salud

dim(A)
casosRM[,.N,by=.(`Centro de salud`)]

B<-casosRM[,.(Total_centro=.N),by=.(`Centro de salud`)] 

dim(B)

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)]

dim(C)

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)]

dim(D)




#merging data sets

AB<-merge(A,B,by = "Centro de salud",all = T,sort = F)
ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F)
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F)

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro]

# reshaping 

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=.N),by=.(`Centro de salud`,Sexo)]

G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud') # función reshape para reorganizar los datos 

#---- Part 2: Visualization  -------------------

#Scatter plot
#Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`)
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5)

#ggplot2
p1<-ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T)
p1

#plotly
library(plotly)
ggplotly(p1)

# other useful ways to show data

#high charter
# http://jkunst.com/highcharter/index.html


#---- Part 3: Intro to Mapping  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F)

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]

zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",]

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F)

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),]

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F)


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds"))


ggplot(zonas_valparaiso) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 11)

# creating a fake spatial distribution of adult population in space
zonas_valparaiso2<-cbind(zonas_valparaiso[,c("geocodigo","codigo_comuna","codigo_provincia","codigo_region","geometry")],"AdultosMayores"=sample(zonas_valparaiso$AdultosMayores,size = length(zonas_valparaiso$AdultosMayores)))


ggplot(zonas_valparaiso2) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 13)

#comparing histograms of the same variable

hist(zonas_valparaiso$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo") 

hist(zonas_valparaiso2$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")
