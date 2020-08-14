# Libreria YAPO#

sacandoPrecio <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, ".offer")
  if(length(nodoBread)>0){
    precio <- html_text(nodoBread)
    precio <- gsub("\\t","",precio)
    precio <- gsub("\\n","",precio)
    precio <- gsub("\\$","",precio)
    precio <- gsub("[.]","",precio)
    precio <- as.numeric(precio)
  }else{
    precio = NA
  }
  return(precio)
}

obtenerCategoria <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, ".breadcrumbs")
  nodoBread <- html_nodes(nodoBread, "strong")
  return(html_text(nodoBread))
}

obtenerComuna <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, "seller-info")
  comuna <- html_attr(nodoBread,"region")
  return(gsub("Región Metropolitana, ","",comuna))
}

obtenerTipoNegocio <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, ".details")
  nodoBread <- html_nodes(nodoBread, "table")
  tabla <- html_table(nodoBread)[[1]]
  subsettn <- unlist(subset(tabla, X1 == 'Tipo')[2])
  if(length(subsettn)>0){
    return(subsettn)
  }else{
    return(NA)
  }
}

obtenerAnioUsuarioYapo <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, "seller-info")
  seniority <- html_attr(nodoBread,"seniority")
  seniority <- gsub("En Yapo desde ","",seniority)
  return(strsplit(seniority," ")[[1]][2])
}

obtenerPublicacionesActivasUsuarioYapo <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, "seller-info")
  actives <- html_attr(nodoBread,"actives")
  return(gsub("[.]","",actives))
}

obtenerPublicacionesTotalesUsuarioYapo <- function(htmlSeg){
  nodoBread <- html_nodes(htmlSeg, "seller-info")
  historical <- html_attr(nodoBread,"historical")
  return(gsub("[.]","",historical))
}

# Yapo Documento 


install.packages("rvest")

# cargar las librerias
library(xml2)
library(rvest)
source('libreriasYapo.R')

fullDatos <- data.frame()
for(numeroPagina in 1:6){
  readHtml <- read_html(paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",numeroPagina,sep = ""))
  print(paste("Descargando pagina nro:",numeroPagina))
  nodeTabla <- html_nodes(readHtml, ".listing_thumbs")
  nodeTabla <- html_nodes(nodeTabla, ".title")
  linksProductos <- html_attr(nodeTabla,"href")
  
  for (urlYapo in linksProductos) {
    htmlSeg <- read_html(urlYapo)
    
    print(paste("Descargando URL ==> ",urlYapo))
    
    textoTipoAviso <- obtenerCategoria(htmlSeg)
    precio <- sacandoPrecio(htmlSeg)
    comuna <- obtenerComuna(htmlSeg)
    tipoNegocio <- obtenerTipoNegocio(htmlSeg)
    anioUsuarioyapo <- obtenerAnioUsuarioYapo(htmlSeg)
    publicacionesactivasusuarioyapo <- obtenerPublicacionesActivasUsuarioYapo(htmlSeg)
    publicacionestotalesusuarioyapo <- obtenerPublicacionesTotalesUsuarioYapo(htmlSeg)
    
    fullDatos <- rbind(fullDatos,data.frame(comuna = comuna, categoria = textoTipoAviso, precio = precio,
                                            tiponegocio = tipoNegocio, aniousuarioyapo = anioUsuarioyapo,
                                            urlyapo = urlYapo ))
  }
}




View(fullDatos)


#Filtrando las variables que no importan para el estudio

fullDatos<-fullDatos[-6]

#Quedamos con las 5 variables restantes

library(ggplot2)

install.packages("datasets")

library(ggplot2)
library(datasets)
library(dplyr)

##Pregunta 1

# Analisis de las variables mediante graficos 

ggplot(fullDatos, aes(x=tiponegocio))+ geom_bar(stat="count", position="dodge")

ggplot(fullDatos, aes(y=comuna))+ geom_bar(stat="count", position="dodge")

ggplot(fullDatos, aes(x=aniousuarioyapo))+ geom_bar(stat="count", position="dodge")

ggplot(fullDatos, aes(y=categoria))+ geom_bar(stat="count", position="dodge")

# Al analizar la informacion que presentan los graficos se decide optar 
# por analizar las variables correspondiente a: "categoria", "tiponegocio"
# y "comuna". 

# Para el desarorollo del estudio se ha considerado como variables de interes 
# la venta de moda y vestuario en las diversas comunas de la Region Metropolitana,
# por lo cual se ha creado un nuevo dataframe denominado "modaComuna".

modaComuna <- filter(fullDatos,categoria == "Moda y vestuario", tiponegocio =="Vendo")

# "modaComuna" permitira analizar las ventas de "moda y vestuario" en las distintas 
# comunas de la Region Metropolitana. 

ggplot(modaComuna,aes(x=comuna))+
  geom_bar(stat="count", position="dodge")+
  coord_flip()+
  geom_text(stat = 'count', hjust = 0, vjust = 0, position = position_dodge(0.9), aes(x = comuna, label=stat(count)))+
  ggtitle(label = "Cantidad de publicaciones de ventas segmentada por comuna de R.M.")




##Pregunta 2

# Analisis

# Podemos observar que una mayor cantidad de publicaciones relacionadas con 
# las Ventas  sobre "moda y vestuario" se encuentran en  las comunas de 
# La Reina y Providencia. Ambas comunas son centricas por lo que cuentan
# con mejor ubicacion comercial y con un nivel socio economico 
# por sobre el promedio, por otro lado , poseen mayor poder 
# adquisitivo en comparacion a comunas populares o de menos recursos como 
# Puente Alto y Quinta Normal. 

# Pregunta 3


## Pregunta 4

#Con R set en la carpeta documentos.

setwd("C:/Users/Lissette/Desktop/Datos Bigdata")
write.table(fullDatos, file = "fullDatos_tabla.csv", sep = ";", row.names = F)


