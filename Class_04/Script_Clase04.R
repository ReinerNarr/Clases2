###Class 04 - Visualization & Intro to Mapping ###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl)
library(data.table)



casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)

names(casos)
casos<-casos[Región=="Metropolitana",]

saveRDS(casos,"Class_03/casosRM.rds")

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8')

writexl::write_xlsx(casos,path = "Class_03/CasosenExcel.xlsx")

library(foreign)

#write.dta



casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

casosRM[,table(Sexo)]
casosRM[Sexo=="Fememino",Sexo:="Femenino"]

casosRM[`Centro de salud`=="Clínica Alemana",`Centro de salud`:="Clinica Alemana"]
casosRM[,.N,by=.(`Centro de salud`)]

# Creating (factor) variables

class(casosRM$Sexo)

casosRM[,Sexo:=factor(Sexo)]

head(casosRM$Sexo)
head(as.numeric(casosRM$Sexo))

table(casosRM$Sexo)
casosRM[,.N,by=.(Sexo)]
casosRM[,.N,by=.(Sexo,`Centro de salud`)]

#Collapsing by Centro de Salud 

names(casosRM)
obj1<-casosRM[,.N,by=.(`Centro de salud`)]


obj1[,sum(N,na.rm = T)]

obj1[,porc:=N/sum(N,na.rm = T)]

# collapsing (colapsar) by average age


A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)]

B<-casosRM[,.(Total_centro=.N),by=.(`Centro de salud`)]

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)]

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)]

dim(A)
dim(B)
dim(C)
dim(D)


#merging data sets


AB<-merge(A,B,by = "Centro de salud",all = T,sort = F)


ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F)
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F)

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro]


# reshaping

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=.N),by=.(`Centro de salud`,Sexo)]

G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud')

#---- Part 2: Visualization  -------------------

#Scatter plot
#Base R 
plot(x=G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`) # grafica datos en eje x e y
text(x=G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5) # le agrega etiquetas a cada dato

#ggplot2
library(ggplot2)

names(E)
ggplot(data = E,mapping = aes(x = AvAge,y=`Casos confirmados`)) + geom_point() # grafica datos en eje x e y

names(G)
ggplot(data = G,mapping = aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+
  geom_point() # grafica datos en eje x e y

p1<-ggplot(data = G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+
  geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+ # datos de mujeres cambian de tamaño y los hombres de color
  geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T) # le agrega etiquetas a cada dato

ggplot(data = E,mapping = aes(x=AvAge,y=`Casos confirmados`))+
  geom_point()+
  facet_wrap(~Sexo)+ # separa datos (tipo factor o character)entre hombres y mujeres en dos graficos
  geom_smooth(method = 'lm',se=F)+ # me muestra una linea recta de tendencia
  geom_smooth(method = 'loess',col='red',se=F) # me muestra una linea de tendencia que se mueve con los datos


#plotly
#install.packages('plotly')
library(plotly) # graficos mas interactivos
ggplotly(p1)

#histograms

ggplot(data = casos,mapping = aes(x=Edad))+geom_histogram()
ggplot(data = E,mapping = aes(x=AvAge))+geom_histogram()

# Kernel Densities

ggplot(E,aes(x=AvAge))+geom_density()
ggplot(E,aes(x=AvAge,group=Sexo))+geom_density()
ggplot(E,aes(x=AvAge,group=Sexo,colour=Sexo))+geom_density()
ggplot(E,aes(x=AvAge,group=Sexo,colour=Sexo))+geom_density()+facet_wrap(~Sexo)


#looking at the whole country
casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)

ggplot(casos,aes(x=Edad,group=Sexo,fill=Sexo))+geom_histogram()+facet_wrap(~factor(Región))

#como sacamos el "fememino"?
casos[Sexo=='Fememino',Sexo:='Femenino']



ggplot(casos,aes(x=Edad,group=Sexo,fill=Sexo))+geom_histogram()


#high charter
# http://jkunst.com/highcharter/index.html

#https://chilecracia.org 

#---- Part 3: Intro to Mapping (Shapefile) -------------------
#archivos.shp (shapefile) son el archivo principal que contienen las figuras
#archivos.dbf (data base file) son los datos de cada figura
#archivos.prj (projection) son como estan los mapas guardados/proyectados
#archivos.shx son en general los metadatos
#install.packages("chilemapas")
#install.packages("rgdal")
library(rgdal)
library(sp) # paquete clásico (spatial)
library(chilemapas) # para ver lo que contiene busco en help(package=chilemapas )
library(data.table)



# 3.1 Shapefiles as in the `sp` package
help(package='sp')
View(ogrDrivers())

comunas_rm<-readOGR("Class_04/ComunasR13/COMUNA_C17.shp") # readOGR es la función para cargar mapas
class(comunas_rm) # objeto complejo. Mapa de tipo poligono.

comunas_rm@proj4string # proyección de longitud-latitud de la RM y forma de medir elipsoide (GRS80)

View(comunas_rm@data)
plot(comunas_rm)

coordinates(comunas_rm) # coordenadas de latitud y longitud. Muestran los centroides (punto del centro) de los poligonos (de cada comuna).

centroids_rm<-SpatialPoints(coordinates(comunas_rm),proj4string = comunas_rm@proj4string) # se crea un shapefile de puntos en base a las coordenadas de la RM y a su proyección.
plot(comunas_rm) 
plot(centroids_rm,add=T,col='red',lty=1,pch=21,cex=0.1) # se le agregan al mapa de la RM los centroides
lines(coordinates(comunas_rm),col='blue') # lineas que juntan los puntos en orden aleatorio
# se van superponiendo cosas


str(comunas_rm@data) # muestra los tipos de datos, etc.

# 3.2 Shapefiles as in the `sf`(simple features) package
# paquete sf se creó en base al paquete chilemapas

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F)

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]

zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",] # mapa zonas censales de Valparaiso

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F) # le meto los geocodigos

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),] # selecciono 2 comunas 

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F) # agrego población de adultos mayores


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds")) # defino paleta de colores roja



ggplot(zonas_valparaiso) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) + # AdultosMayores mapeados por colores
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") + # le pongo color de paleta
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") + # le pongo titulos
  theme_minimal(base_size = 11) # cambio tamaño de letra

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
