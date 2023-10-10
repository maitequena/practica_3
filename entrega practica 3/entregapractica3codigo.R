#Ej para entregar practica 3
rm(list=ls())
getwd()
setwd("C:/Users/maiti/OneDrive/Escritorio/labo/practica3/Datos Practica 3")

#cargo losdatos correspondientes 
azul<-read.table("AZUL.txt",col.names=c("Codigo de identificacion","Fecha","Temperatura","Temperatura de rocio","Presion"))
catamarca<-read.table("CATAMARCA.txt",col.names=c("Codigo de identificacion","Fecha","Temperatura","Temperatura de rocio","Presion"))
aeroparque<-read.table("AEROPARQUE.txt",col.names=c("Codigo de identificacion","Fecha","Temperatura","Temperatura de rocio","Presion"))
chilecito<-read.table("CHILECITO.txt",col.names=c("Codigo de identificacion","Fecha","Temperatura","Temperatura de rocio","Presion"))
iguazu<-read.table("IGUAZU.txt",col.names=c("Codigo de identificacion","Fecha","Temperatura","Temperatura de rocio","Presion"))
mendoza<-read.table("MENDOZA.txt",col.names=c("Codigo de identificacion","Fecha","Temperatura","Temperatura de rocio","Presion"))

#tomo los valores 999.9 como dato faltante para convertirlo en NA

dato.faltante<- -999.9
azul[which(azul==dato.faltante)]<-NA
catamarca[which(catamarca==dato.faltante)]<-NA
aeroparque[which(aeroparque==dato.faltante)]<-NA
chilecito[which(chilecito==dato.faltante)]<-NA
iguazu[which(iguazu==dato.faltante)]<-NA
mendoza[which(mendoza==dato.faltante)]<-NA

#convierto temp y temp de rocio de F a °C con una funcion

#fahrenheit a celsius
f_a_c<-function(temp) {
  celsius<-(temp-32)*5/9
  return(celsius)
}

azul$Temperarura<-f_a_c(azul$Temperatura)
azul$Temperatura.de.rocio<-f_a_c(azul$Temperatura.de.rocio)
catamarca$Temperarura<-f_a_c(catamarca$Temperatura)
catamarca$Temperatura.de.rocio<-f_a_c(catamarca$Temperatura.de.rocio)
aeroparque$Temperarura<-f_a_c(aeroparque$Temperatura)
aeroparque$Temperatura.de.rocio<-f_a_c(aeroparque$Temperatura.de.rocio)
chilecito$Temperarura<-f_a_c(chilecito$Temperatura)
chilecito$Temperatura.de.rocio<-f_a_c(chilecito$Temperatura.de.rocio)
iguazu$Temperarura<-f_a_c(iguazu$Temperatura)
iguazu$Temperatura.de.rocio<-f_a_c(iguazu$Temperatura.de.rocio)
mendoza$Temperarura<-f_a_c(mendoza$Temperatura)
mendoza$Temperatura.de.rocio<-f_a_c(mendoza$Temperatura.de.rocio)

estaciones<-as.list(read.table("estaciones.txt"))

x<-c("AZUL","AEROPARQUE","CATAMARCA","CHILECITO","IGUAZU","MENDOZA")
estaciones[[1]]<-x
latitud<-c(-36.8,-34.6,-28.6,-29.2,-25.7,-32.8)
estaciones[[2]]<-latitud
longitud<-c(-59.9,-58.4,-65.8,-67.4,-54.5,-68.8)
estaciones[[3]]<-longitud
altura<-c(146,6,454,1250,270,746)
estaciones[[4]]<-altura
names(estaciones)<-c("Estacion","Latitud","Longitud","Altura")
estaciones<-as.data.frame(estaciones)

#extraigo los datos que quiero

#azul
nombre.azul<-estaciones[1,1]
latitud.azul<-estaciones[1,2]
longitud.azul<-estaciones[1,3]
altura.azul<-estaciones[1,4]
data.azul<-list("Nombre"=nombre.azul,"Latitud"=latitud.azul,"Longitud"=longitud.azul,"Altura"=altura.azul)
lista.azul<-list("Estacion"=data.azul,"Datos"=azul)

#catamarca
nombre.catamarca<-estaciones[2,1]
latitud.catamarca<-estaciones[2,2]
longitud.catamarca<-estaciones[2,3]
altura.catamarca<-estaciones[2,4]
data.catamarca<-list("Nombre"=nombre.catamarca,"Latitud"=latitud.catamarca,"Longitud"=longitud.catamarca,"Altura"=altura.catamarca)
lista.catamarca<-list("Estacion"=data.catamarca,"Datos"=catamarca)

#aeroparque
nombre.aeroparque<-estaciones[3,1]
latitud.aeroparque<-estaciones[3,2]
longitud.aeroparque<-estaciones[3,3]
altura.aeroparque<-estaciones[3,4]
data.aeroparque<-list("Nombre"=nombre.aeroparque,"Latitud"=latitud.aeroparque,"Longitud"=longitud.aeroparque,"Altura"=altura.aeroparque)
lista.aeroparque<-list("Estacion"=data.aeroparque,"Datos"=aeroparque)


#chilecito
nombre.chilecito<-estaciones[4,1]
latitud.chilecito<-estaciones[4,2]
longitud.chilecito<-estaciones[4,3]
altura.chilecito<-estaciones[4,4]
data.chilecito<-list("Nombre"=nombre.chilecito,"Latitud"=latitud.chilecito,"Longitud"=longitud.chilecito,"Altura"=altura.chilecito)
lista.chilecito<-list("Estacion"=data.chilecito,"Datos"=chilecito)

#iguazu
nombre.iguazu<-estaciones[5,1]
latitud.iguazu<-estaciones[5,2]
longitud.iguazu<-estaciones[5,3]
altura.iguazu<-estaciones[5,4]
data.iguazu<-list("Nombre"=nombre.iguazu,"Latitud"=latitud.iguazu,"Longitud"=longitud.iguazu,"Altura"=altura.iguazu)
lista.iguazu<-list("Estacion"=data.iguazu,"Datos"=iguazu)

#mendoza
nombre.mendoza<-estaciones[6,1]
latitud.mendoza<-estaciones[6,2]
longitud.mendoza<-estaciones[6,3]
altura.mendoza<-estaciones[6,4]
data.mendoza<-list("Nombre"=nombre.mendoza,"Latitud"=latitud.mendoza,"Longitud"=longitud.mendoza,"Altura"=altura.mendoza)
lista.mendoza<-list("Estacion"=data.mendoza,"Datos"=mendoza)

lista.estaciones<-list(azul,catamarca,aeroparque,chilecito,iguazu,mendoza)
names(lista.estaciones)<-c("Azul","Catamarca","Aeroparque","Chilecito","Iguazu","Mendoza")

#pongo orden

rm(list="altura.aeroparque","altura.azul","altura.catamarca","altura.chilecito","altura.iguazu","altura.mendoza")
rm(list="latitud.aeroparque","latitud.azul","latitud.catamarca","latitud.chilecito","latitud.iguazu","latitud.mendoza")
rm(list="longitud.aeroparque","longitud.azul","longitud.catamarca","longitud.chilecito","longitud.iguazu","longitud.mendoza")
rm(list="nombre.aeroparque","nombre.azul","nombre.catamarca","nombre.chilecito","nombre.iguazu","nombre.mendoza")
rm(list="altura","x","latitud","longitud")
rm(list="aeroparque","azul","catamarca","chilecito","iguazu","mendoza")
rm(list="lista.aeroparque","lista.azul","lista.catamarca","lista.chilecito","lista.iguazu","lista.mendoza")


#ej 2

#i)

resumen<-function(lista){
  nombres<-c()
  fecha.inicial<-c()
  fecha.final<-c()
  mean.T<-c()
  mean.Tr<-c()
  desv.est.T<-c()
  desv.est.Tr<-c()
  max.T<-c()
  min.T<-c()
  max.Tr<-c()
  min.Tr<-c()
  for (estacion in 1:length(lista)) {
    for (dat.ubic in 1:length(lista[[estacion]] [] )) {
      for (elemento in names(lista [[estacion]] [[dat.ubic]] [] )) {
        if (elemento=="Fecha") {
          fecha.inial[estacion]<-lista[[estacion]][[dat.ubic]][[elemento]][1]
          fecha.final[estacion]<-lista[[estacion]][[dat.ubic]][[elemento]][length(lista [[estacion]] [[dat.ubic]] [[elemento]] [] )]
        } else if (elemento=="Temperatura") {
          mean.T [estacion]<-mean(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
          desv.est.T [estacion]<-sd(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
          max.T [estacion]<-max(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
          min.T [estacion]<-min(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
      } else if (elemento=="Temperatura.de.rocio") {
        mean.Tr [estacion]<-mean(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
        desv.est.Tr [estacion]<-sd(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
        max.Tr [estacion]<-max(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
        min.Tr [estacion]<-min(lista[[estacion]] [[dat.ubic]] [[elemento]] [] ,na.rm=T)
      }
    }
  }
    
resumen.estadistica<-data.frame("Fecha inicial"=fecha.inicial,
                                "Fecha final"=fecha.final,
                                "Media de temperatura"=mean.T,
                                "Desvio estandar de temperatura"=desv.est.T,
                                "Maximo de temperatura"=max.T,
                                "Minimo de temperatura"=min.T,
                                row.names=c("Azul","Catamarca","Aeroparque","Chilecito","Iguazu","Mendoza"))
return(resumen.estadistica)

}

resumen()


#ii)

cercania<-function(lista,lat.min,lat.max,long.min,long.max) {
  for (elemento in names(lista)) {
    if (lista[[elemento]][["Estacion"]][["Latitud"]][]>=lat.min & lista[[elemento]][["Estacion"]][["Latitud"]][]<=lat.max & lista[[elemento]][["Estacion"]][["Longitud"]][]>=long.min & lista[[elemento]][["Estacion"]][["Longitud"]][]<=long.max) {
      print(paste("La estacion", elemento, "tiene estaciones cercanas"))
    } else {
      print(paste("No hay estaciones cercanas"))
    }
  }
}
cercania()

#iii)

guardado<-function (lista) {
 save(lista, file="DatosEstaciones.RData") 
}
guardado(estaciones)

