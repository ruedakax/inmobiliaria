library(tidyverse)

setwd("~/R/proyectos/inmobiliaria")
my_files <- list.files("output")
total <-data.frame(lugar=character(), 
                   area=integer(),
                   precio=integer(),
                   zona=integer(),
                   stringsAsFactors=FALSE) 

for (file in my_files ){
  if(file != "resultados"){
    df_tmp <- read.csv(paste("output/",file,sep=""),sep = ";")
    df_tmp$codigo <- apply(df_tmp,1,function(x) gsub("ID.","",x[2]))
    df_tmp$codigo <- apply(df_tmp,1,function(x) gsub("Anuncio ","",x[2]))
    #mutate(total,gsub("Anuncio ","",gsub("ID.","",codigo)))
    df_tmp$area <- apply(df_tmp,1,function(x) gsub(" M2.","",x[4]))
    df_tmp$precio <- apply(df_tmp,1,function(x) gsub("[^0-9]", "", x[5]))
    total <- rbind(total,df_tmp)
  }
}

rm(df_tmp,file,my_files)

total$area <- as.integer(total$area)
total$precio <- as.integer(total$precio)
total$precio_m2 <- apply(total,1,function(x) trunc(as.numeric(x[5])/as.numeric(x[4])))
##hacemos de la zona un factor
total$zona <- as.factor(total$zona)
levels(total$zona) <- c("CENTRO","POBLADO","NOROCCIDENTE","SUROCCIDENTE")

summary(total)
####### analisis exploratorio 
## se detecta que el Ã rea minima verosimil es de 10 M2
## se detectan 2 registros con nulo en el precio 
## table(total[total$area >= 300,3]) para detectar el max area verosimil
## la cual se  concluye que es 1000
nuevo  <- total[(total$area >= 10 & total$area <= 1000) & (!is.na(total$precio) & total$precio > 0), c(2,3,4,5,6,7)]
summary(nuevo)
###########################3
# análisis gráfico, qué elementos no tienen una proporción coherente
# de  precio segun el lugar
# se saca este gráfico para cada una de las zonas

test <- nuevo[nuevo$zona == 2,]
ggplot(data = test) + 
  geom_point(mapping = aes(x = lugar, y = precio_m2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
rm(test)
## se detectan y eliminan los anuncios que no son coherentes según 
## el gráfico
nuevo <- subset(nuevo,!(nuevo$codigo %in% c("599464","579250","705523","690301","603142","667397")))
##
## Se vuelve a analizar cada zona en función de la
## relación precio del M2 Vs area
test <- nuevo[nuevo$zona == "CENTRO",]
ggplot(data = test) + 
  geom_point(mapping = aes(x = area, y = precio_m2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
## se detectan y eliminan los anuncios que no son coherentes según 
## el gráfico
nuevo <- subset(nuevo,!(nuevo$codigo %in% c("742325","714155","688077","688058","581789","566926","536975","380667","701860","704475","712867","592599","599462","664643","732001")))
nuevo <- subset(nuevo,!(nuevo$codigo %in% c("741381","742619","736483","697413","660762","595459","506242","495974","741166","737918","751029","731007","716037","678075","675300","667140","652216","567417","532542","474087","327932","744261","685028","637329","683183","669260","652849")))
nuevo <- subset(nuevo,!(nuevo$codigo %in% c("585134","634599","749603","427358","730729","620169","572615","427358")))
##graficas
ggplot(data = nuevo, aes(x = zona)) +
  geom_bar(colour="black", fill="#61d43c", width=.8, stat="count")+
  guides(fill=FALSE) +
  ylab("# Ofertas") + xlab("Zona") +
  ggtitle("Cantidad de ofertas por zona")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..),vjust=2)
##
ggplot(data = filter(nuevo, zona == "CENTRO"),aes(x = lugar)) + 
  geom_bar(colour="black", fill="#00cc66", width=.8, stat="count") +
  guides(fill=FALSE) +
  ylab("# Ofertas")  + xlab("") + 
  ggtitle("Zona Centro") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)
        ,plot.title = element_text(hjust = 0.5)
  )

ggplot(data = filter(nuevo, zona == "POBLADO"),aes(x = lugar)) + 
  geom_bar(colour="black", fill="#00cc66", width=.8, stat="count") +
  guides(fill=FALSE) +
  ylab("# Ofertas") + xlab("") +
  ggtitle("Zona Poblado") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)
        ,plot.title = element_text(hjust = 0.5)
  )

ggplot(data = filter(nuevo, zona == "NOROCCIDENTE"),aes(x = lugar)) + 
  geom_bar(colour="black", fill="#00cc00", width=.8, stat="count") +
  guides(fill=FALSE) +
  ylab("# Ofertas") + xlab("") +
  ggtitle("Zona Noroccidente") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)
        ,plot.title = element_text(hjust = 0.5)
  )

ggplot(data = filter(nuevo, zona == "SUROCCIDENTE"),aes(x = lugar)) + 
  geom_bar(colour="black", fill="#66ff66", width=.8, stat="count") +
  guides(fill=FALSE) +
  ylab("# Ofertas") + xlab("") +
  ggtitle("Zona Suroccidente") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)
        ,plot.title = element_text(hjust = 0.5)
  )

ggplot(data = nuevo) + 
  stat_summary(
    mapping = aes(x = zona,y= area),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median,color = '#008000'
  )+
  guides(fill=FALSE) +
  ylab("Metros cuadrados") + xlab("Zona")

ggplot(data = nuevo) + 
  geom_smooth(mapping = aes(y = precio_m2, x = area ),color = '#008000') +
  facet_wrap(~ zona, nrow = 2) +
  ylab(bquote('Precio '~ m^2)) + 
  xlab("Area")

ggplot(nuevo, aes(area, precio_m2)) +
  geom_point(aes(color = zona)) +
  geom_smooth(se = FALSE)  +
  ylab(bquote('Precio '~ m^2)) + 
  xlab("Area")

ggplot(data = filter(nuevo,precio < 1000000)) +
  geom_histogram(mapping = aes(x = precio),binwidth = 100000,fill='#008000') +
  ylab("Oferta") + 
  xlab("Area")








