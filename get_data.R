#espacio urbano Zona centro 
library(RCurl)
library(XML)
setwd("~/R/proyectos/inmobiliaria")

url_zonas <- c("https://www.espaciourbano.com/Apartamentos_arriendo_Zona1.asp?Neg=2&xZona=&Ciudad=Medell%EDn%20Zona%201%20-%20Centro&IdCiudad=10000&offset=0",
             "https://www.espaciourbano.com/Apartamentos_arriendo_Zona2.asp?Neg=2&xZona=&Ciudad=Medell%EDn%20Zona%202%20-%20Poblado&IdCiudad=10027&offset=0",
             "https://www.espaciourbano.com/Apartamentos_arriendo_Zona3.asp?Neg=2&xZona=&Ciudad=Medell%EDn%20Zona%203%20-%20Laureles&IdCiudad=10028&offset=0",
             "https://www.espaciourbano.com/Apartamentos_arriendo_Zona4.asp?Neg=2&xZona=&Ciudad=Medell%EDn%20Zona%202%20-%20Poblado&IdCiudad=10029&offset=0")

total <-data.frame( codigo=character(),
                    lugar=character(), 
                    area=character(),
                    precio=character(),
                    zona=integer(),
                    stringsAsFactors=FALSE) 

for(i in 1:length(url_zonas)){
#for(i in 1:1){
  crudo <- getURL(url_zonas[i])
  doc <- htmlParse(crudo)
  li <- getNodeSet(doc, "//span[@class='style100']//a")
  paginas <- sapply(li, xmlValue)
  ult_pag <- 0
  for(k in 1:length(paginas)){
    valor <- paginas[k]
    cortado <- strsplit(valor," ")
    numero <- as.numeric(cortado[[1]][1])
    if(!is.na(numero))
      ult_pag <- numero
  }
  #######################################################3
  rm(crudo,doc,li,paginas)
  for(j in 1:ult_pag){
  #for(j in 1:2){ 
    valor <- ((j*25) - 25)
    val_offset <- paste("offset=",valor,sep="")
    #####################################################3
    url <- gsub("offset=0",val_offset,url_zonas[i])
    #url <- "https://www.espaciourbano.com/Apartamentos_arriendo_Zona4.asp?Neg=2&xZona=&Ciudad=Medell%EDn%20Zona%202%20-%20Poblado&IdCiudad=10029&offset=50"
    crudo <- getURL(url,.encoding = "ISO-8859-1")
    doc <- htmlParse(crudo,ignoreBlanks = TRUE)
    li <- getNodeSet(doc, "//table[@width='0100%']")
    listado <- lapply(li,function (x) list(trimws(xmlValue(x[[1]][[1]][[1]])),trimws(xmlValue(x[[1]][[3]][[1]])),trimws(xmlValue(x[[1]][[5]][[1]])),trimws(xmlValue(x[[1]][[7]][[1]])),i))
    listado <- do.call(rbind.data.frame,listado)
    colnames(listado) <- c("codigo","lugar","area","precio","zona")
    total <- rbind(total,listado)
  }
  ruta <- paste("output/zona",i,".csv",sep="")
  write.csv2(total,ruta,sep = "|")
  total <-data.frame(codigo=character(),
                     lugar=character(), 
                     area=integer(),
                     precio=character(),
                     zona=integer(),
                     stringsAsFactors=FALSE) 
}
rm(crudo,doc,li,paginas,total)


plot(nuevo[,3], log(nuevo[,4]), main="")
abline(lm(log(nuevo[,4])~ nuevo[,3]), col="yellow")


