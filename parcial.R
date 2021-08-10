install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("sf")
library(sf)
install.packages("raster")
library(raster)
library(rgdal)
library(dplyr)



#1
tangente_hiperbolica<-seq(-10,10,0.5)
tangente_hiperbolica
tanh<-function(a){
  r<-(exp(2*a)-1)/(exp(2*a)+1)
  return(r)
}
solucion<-tanh(tangente_hiperbolica)
plot(tangente_hiperbolica,solucion,type="l")

#2

ecua_regresion<-function(v1,v2){
  X1<-c()
  Y2<-c()
  for(i in 1:length(v1)){
    x<-log10(v1[i])
    y<-log10(v2[i])
    X1<-c(X1,x)
    Y2<-c(Y2,y)
  }
  multip<-X1*Y2
  X1_alcuadrado<-X1^2
  n<-length(v1)
  B<-((n*sum(multip))-(sum(X1)*sum(Y2)))/((n*sum(X1_alcuadrado))-(sum(X1))^2)
  A<-10^(mean(Y2)-B*mean(X1))
  return(c(A,B))
}
x<-c(2.5,3,4,5,5.5,6,7)
y<-c(12.5,10,7,4.5,4,3,3.5)
ecua_regresion(x,y)

# 3
AyP <- function (l1, l2, l3 ) {
  p <-  sum(l1, l2, l3)
  area <- sqrt(p/2*(p/2-l1)*(p/2-l2)*(p/2-l3))
  return(c(area, p))
  
}
AyP(4,11,8)



# 4
TiemCo <- function ( L , CN , S ) {
  resultado <- ( 0.0136*( L ^ 0.8 )*((( 1000 / CN ) - 9)^0.7))/S^0.5
  return(resultado)
}
TiemCo(6,15,8)

#5
CSSE<-function(v,r){
  m<-matrix(v,ncol = 3)
  ds<-det(m)
  for (n in 1:length(r)) {
    if(n==1){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dx<-det(m)
    }else if(n==2){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dy<-det(m)
    }else{
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dz<-det(m)
    }
  }
  x<-dx/ds
  y<-dy/ds
  z<-dz/ds
  r<-c(x,y,z)
  return(r)
}
variables_xyz<-c(2,5,1,1,-4,-1,3,1,-4)
resul_de_SE<-c(7,-19,4)
CSSE(variables_xyz,resul_de_SE)

#PARTE2

#1)

setwd("D:/R/progra1/parcial")
cuenca<- sf::st_read("uh_datos.shp")
#a)
datos <- readOGR(dsn = ".", layer= "uh_datos")
data_nueva<-datos@data
head(data_nueva)
pp_prom<-data_nueva %>% 
  group_by(AAA) %>% 
  summarise(promedio_AAA=mean(pcp))
View(pp_prom)

ggplot(pp_prom, aes(AAA, promedio_AAA)) +
  geom_point(color = "purple")+
  labs(y="promedio_AAA", x = "AAA")+
  ggtitle("Precipitacion promedio por Autoridad AAA")

#b)

View(pp_prom)
View(data_nueva)
indic_ar<- data_nueva %>% 
  group_by(NOMBRE) %>% 
  summarise(ia=(pcp/pet))
View(indic_ar)

#c)
indic_ar<- data_nueva %>% 
  group_by(NOMBRE) %>% 
  summarise(ia=(pcp/pet))
View(indic_ar)
ia<- indic_ar$ia

clasificacionIA<-c()
for(a in ia){
  if(1<=a){
    f<- print("Húmedo")
  }else if(0.7<=a){
    f<- print("Subhúmedo húmedo")
  }else if(0.5<=a){
    f<- print("Subhúmedo seco")
  }else if(0.2<=a){
    f<- print("semiarido")
  }else if(10.05<=a){
    f<- print("arido")
  }else if(0<=a){
    f<- print("Hiperarido")
  }
  clasificacionIA<-c(f,clasificacionIA)
}
length(clasificacionIA)
clasif<-tibble(indic_ar, clasificacionIA)
view(clasif)

#d)
ggplot(data = data_nueva, mapping = aes(x = AAA, y = pcp)) +
  geom_boxplot()

data_nueva %>%
  filter(AAA== "X")

ggplot(data = data_nueva, mapping = aes(x = AAA, y = pet)) +
  geom_boxplot()+scale_fill_brewer(palette="Dark2")

#2)
codigo <- c("A","B","C","D","E","F","A")
ESTE <- c(272841.7, 272893.6, 272892.5, 272913.8, 272911.2, 272837.5, 272841.7)
NORTE <- c(8666459.9, 8666456.9, 8666446.1, 8666441.5, 8666399.9, 8666407.9, 8666459.9)
df1<-data.frame(codigo,ESTE,NORTE)
head(df1)
poligono<-function(codigo, ESTE, NORTE){
  objeto_sf<- st_as_sf(df1, coords = c("ESTE", "NORTE"),crs = st_crs(32718))
  return(plot(objeto_sf, type= "l"))
}
poligono(df1)









#------------------------------