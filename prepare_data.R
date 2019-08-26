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

#creamos la columna que genera la relacion precio y area (precio del metro cuadrado)
total$precio_m2 <- apply(total,1,function(x) trunc(as.numeric(x[5])/as.numeric(x[4])))
##hacemos de la zona un factor
total$zona <- as.factor(total$zona)
levels(total$zona) <- c("CENTRO","POBLADO","NOROCCIDENTE","SUROCCIDENTE")

summary(total)

nuevo <- total

####### analisis exploratorio ## 
################################
# graficar y detectar los outliers por zona
ggplot(nuevo, aes(nuevo$zona, nuevo$precio_m2)) +
  geom_boxplot() +
  coord_flip()

## se detectan y eliminan los anuncios que no son coherentes seg�n 
## el gr�fico usando la relacion  precio metro cuadrado 
for(lazona in levels(nuevo$zona)){
  
  test <- nuevo[nuevo$zona == lazona,]  
  
  meanD = mean(test$precio_m2,na.rm = TRUE)
  
  sdD = sd(test$precio_m2,na.rm = TRUE)
  
  outliers = subset(test, (abs(test[, 7] - meanD) > 2 * sdD ))
  
  nuevo <- nuevo[ !(nuevo$codigo %in% outliers$codigo), ]
}
##
## Se vuelve a analizar cada zona en funci�n de la
## relaci�n precio del M2 Vs area
# graficar y detectar los outliers por zona
ggplot(nuevo, aes(nuevo$zona, nuevo$precio_m2)) +
  geom_boxplot() +
  coord_flip()
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

