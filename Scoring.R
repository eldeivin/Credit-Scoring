
############# PARA CARGAR ARCHIVO Y PAQUETES ###################
  
library(readxl)
library(tidyverse)
library(dplyr)
library(stargazer)
library(mlogit)
library(ROCR)
library(ggplot2)
library(rmarkdown)
library(ggpubr)
library(pROC)
library(DescTools)
library(ResourceSelection)
library(stargazer)

# install.packages("stargazer")
# install.packages("ResourceSelection")
# install.packages("pROC")
# install.packages("ggpubr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("stargazer")
# install.packages("mlogit")
# install.packages("ROCR")

## Ejecutar en R (no RStudio):

# install.packages("rmarkdown")
# update.packages()
# install.packages("caTools")
# updateR()
# install.packages("knitr")
# install.packages("tinytex")
# tinytex::install_tinytex()

DATOS <- read_excel("DAVID VARGAS/DATOS.xlsx", sheet = "DATOS")
options(scipen = 999) # Para evitar notacion cientifica en resultados de comandos



############### MODIFICACION DE BASE Y TRANSFORMACION DE VBLES ###############

DATOS <- DATOS %>%
  filter(`Codigo Tipo de Asociado` !=9) %>%   # filtrar quitando tipo de asociado 9 persona juridica
  mutate(PERIODO = as.Date(PERIODO)) %>%   # convierte en formato fecha a la vble
  mutate(`% Cobertura de la Garantia (Cierre)` = 
           ifelse(`% Cobertura de la Garantia (Cierre)` == 70 , 1 , 0)) %>%  # se transfroma a vble dicotomica
  mutate(`Codeudor` =
         ifelse(`Codeudor` == "SI" , 1 , 0 )) %>%  # se transfroma a vble dicotomica
  mutate(`Forma de Pago` = as.factor(`Forma de Pago`)) %>%  # convierte a vble en factor
  mutate(`Sexo` = ifelse(`Sexo` == "F" , 1 , 0)) %>%
  mutate(`Calificacion antes de Arrastre (Cierre)` = 
           ifelse(`Calificacion antes de Arrastre (Cierre)`=="A",1,0)) %>% # se transfroma a vble dicotomica
  mutate(PERIODO = as.Date(PERIODO) , 
         Sueldo = Sueldo/1000000 ,
         `Total Aportes` = `Total Aportes`/1000000 ,
         Monto = Monto/1000000 ,
         `Saldo Capital` = `Saldo Capital`/1000000 ,
         `Valor Cuota` = `Valor Cuota`/1000000) 



table(DATOS$`Codigo Tipo de Asociado`)
table(DATOS$PERIODO)
table(DATOS$`% Cobertura de la Garantia (Cierre)`)
table(DATOS$Codeudor)
table(DATOS$`Forma de Pago`)
table(DATOS$Sexo)
table(DATOS$`Calificacion antes de Arrastre (Cierre)`)


# para crear o modificar una vble en el data frame:

   # 1. datos <- datos %>% mutate(valor = replace(valor, valor == "X", 0))

   # DATOS = DATOS %>% filter(DATOS$`Forma de Pago` == "Ventanilla") # filtrar y modificar base inicial por medio de pago
   # table(DATOS$`Forma de Pago`) 

   #remove(profesion) # eliminar vble o vector


#############################################################################

  # El siguiente es un ejemplo para crear un Data. Frame: 


  # nombre <- c("A","B","C","D","E","F","G","H","I","J")
  # sexo <- c(rep("man",5),rep("woman",5))
  # edad <- c (25,14,25,76,12,90,65,45,56,43)
  # pais <- c(rep("spain",3),rep("italy",3),rep("portugal",4))

  # datos <- data.frame(nombre=nombre,sexo=sexo,edad=edad,pais=pais)
  # head(datos)

##########################################################################


######################## CREACION DE VBLES ##############################

  # as.factor vuelve factor una vble categorica

monto = DATOS$Monto
saldo_capital=DATOS$`Saldo Capital`
dias_vencidos=DATOS$`Dias vencidos`
cuota=DATOS$`Valor Cuota`
num_cuotas=DATOS$`Num.Cuotas`
fecha_nacim=DATOS$`Fecha de Nacimiento`
fecha_desemb=DATOS$`Fecha del desembolso`
fecha_prox_venc=DATOS$`Fecha Proximo Vencimiento`
fecha_ultimo_pago=DATOS$`Fecha de Ultimo Pago`
altura_real=DATOS$`Altura real`
prob_mora=DATOS$`Probabilidad de Mora`
codeudor=as.factor(DATOS$Codeudor)
aportes=DATOS$`Total Aportes`
estado_civil=as.factor(DATOS$`Estado civil`)
sexo=as.factor(DATOS$Sexo)
nivel_educativo=as.factor(DATOS$`Nivel Estudios`)
ingresos=DATOS$Sueldo
profesion=as.factor(DATOS$Profesion)
num_reestructuraciones=DATOS$`Numero de Reestructuraciones`
tipo_contrato=as.factor(DATOS$`Tipo de contrato`)
calificacion=as.factor(DATOS$`Calificacion del Asociado`)
garantia=DATOS$`% Cobertura de la Garantia (Cierre)`
calificacion_credito=as.factor(DATOS$`Calificacion Credito`)
sucursal=DATOS$`Sucursal del Credito`
fecha_ingreso_empresa=DATOS$`Fecha ingreso a la Empresa`
fecha_afiliacion=DATOS$`Fecha ultima afiliacion`
edad=DATOS$Edad
tipo_asociado = DATOS$`Codigo Tipo de Asociado`
periodo = as.Date(DATOS$PERIODO)
table(periodo)
modalidad = as.factor(DATOS$`Nombre de la Modalidad`)


  # datos <- na.omit(datos) Borrar filas con valores nulos

  # Reemplazar valores en vector o vble:

  # 1. datos <- datos %>% mutate(valor = replace(valor, valor == "X", 0))
  # 2. datos_valijas$x <- ifelse(datos_valijas$x == 'SI', 1 , 0)
  # 3. df$x <- replace(df$x, df$x > 4, 50)





######################### DESCRIPTIVOS ###########################


dim(DATOS) # muestra la dimension de la base de datos
summary(tipo_asociado) # resumen de vble
levels(nivel_educativo) # muestra las distintas categorias de la vble

mean(na.omit(ingresos)) # media de la vble. "na.omit" omite ausencia de datos.
sd(na.omit(ingresos)) # sd calcula la desviacion estandar
class(niv_educ) # tipo o clase de dato
length(ingresos) # cantidad de datos 
table(DATOS$`Forma de Pago` , DATOS$`Nombre de la Modalidad`)
with(DATOS , table(`Calificacion antes de Arrastre (Cierre)`))  # crea una tabla de frecuencias



# Decimal o porcentaje del área debajo de la curva, corresponde al nivel explicativo de la variable independiente.
# Una variable es buena predictora, si tiene un coeficiente mayor a 0.7.
a = roc(DATOS$`Calificacion antes de Arrastre (Cierre)` ~ DATOS$Codeudor)
a

# Una mayor area en el grafico, corresponde a un mejor predictor. La linea recta corresponde al 50%
plot(a)



############################ GRAFICAS SELECCIONADAS ################################


DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot()+
  aes(`Calificacion Credito`)+
  geom_bar() +
  facet_wrap(~`Forma de Pago`) +
  geom_text(aes(label = after_stat(count)) , stat = 'count' , vjust = -0.5)+
  labs(title = "Categoria de riesgo", subtitle = "A corte de octubre 2022" , x="Categoria", y="N° asociados")+
  theme_light()


DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() +
  aes(x = `Total Aportes` , y = `Sueldo` , color = Sexo) +
  geom_point() + 
  geom_smooth(method = "lm")+
  labs(x = "Aportes sociales (en millones)" , y = "Ingresos (en millones)" , 
       title = "Propension al ahorro: Ingresos VS Aportes" , 
       subtitle = "A corte octubre de 2022" ) +
  scale_x_continuous(limits = c(0 , 30) , breaks = seq(0 , 30 , 5)) +
  scale_y_continuous(limits = c(0 , 30) , breaks = seq(0 , 30 , 5))

  
  
DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() + 
  aes(x = Edad , y = Sueldo , fill= Sexo) +
  geom_histogram(aes(y =..density..) , bins = 70 , position = 'identity' , alpha = 0.7) +
  scale_x_continuous(breaks = seq(0 , 100 , 10)) +
  labs(x = "Edad" , y = "Frec. Relativa" , 
       title = "Distribucion del ingreso por edad y genero" ,
       subtitle = "A corte octubre de 2022")




DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() + 
  aes(Sueldo , Monto) +
  geom_point(color = "grey25") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,200)) +
  labs(x = "Ingresos (en millones)" , y = "Monto solicitado (en millones)" , 
       title = "Propension del monto solicitado: por nivel de estudios" ,
       subtitle = "A corte octubre de 2022") +
  facet_wrap(~ `Nivel Estudios`)



DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() + 
  aes(Sueldo , Monto) +
  geom_point(color = "grey25") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,200)) +
  labs(x = "Ingresos (en millones)" , y = "Monto solicitado (en millones)" , 
       title = "Propension del monto solicitado: por tipo de contrato laboral" ,
       subtitle = "A corte octubre de 2022") +
  facet_wrap(~ `Tipo de contrato`)



DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() + 
  aes(Sueldo , Monto) +
  geom_point(color = "grey25") +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,200)) +
  labs(x = "Ingresos (en millones)" , y = "Monto solicitado (en millones)" , 
       title = "Propension del monto solicitado: por ocupacion" ,
       subtitle = "A corte octubre de 2022") +
  facet_wrap(~ `Ocupacion`)



DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Antiguedad laboral meses` = (`Antiguedad laboral dias`/30) ,
         `Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
  aes(`Antiguedad laboral meses` , `Meses vencidos`) +
  geom_point(color = "grey25") +
  geom_smooth(method = "glm") +
  scale_x_continuous(limits = c(0,400)) +
  scale_y_continuous(limits = c(0,60)) +
  labs(x = "Antiguedad laboral (meses)" , y = "Altura mora (meses)" , 
       title = "Incidencia de la antiguedad laboral sobre el impago : por tipo de contrato" ,
       subtitle = "A corte octubre de 2022") +
  facet_wrap(~ `Tipo de contrato`)




DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30 ,
         `Tasa Efectiva` = `Tasa Efectiva`*100) %>%
  ggplot() + 
  aes(`Meses vencidos` , `Tasa Efectiva`) +
  geom_point(color = "grey30") +
  geom_smooth(method = "lm") +
  #scale_x_continuous(limits = c(0,400)) +
  scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (meses)" , y = "% Tasa E.A." , 
       title = "Sensibilidad a la tasa de interes: por ocupacion" ,
       subtitle = "A corte octubre de 2022") +
  facet_wrap(~ `Ocupacion`)



DATOS %>%
  mutate(`Tasa Efectiva` = `Tasa Efectiva`*100) %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva` , color = `Forma de Pago`) +
  geom_jitter() + 
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400)) +
  scale_y_continuous(limits = c(5,35)) +
  labs(title = "Elasticidad precio de la demanda del credito" ,
       subtitle = "Enero a octubre del 2022" ,
       x="Monto (en millones)" , 
       y="% Tasa Efectiva") +
  facet_wrap(~ `Forma de Pago`) +
  stat_regline_equation(label.x = 50 , label.y = 30)


  
DATOS %>%
  mutate(`Tasa Efectiva` = `Tasa Efectiva`*100 ,
         `Nombre de la Modalidad` = 
           case_when(`Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO S < 600" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO SC >=600 <=699" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO S >= 700  <799" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO S > 800" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC < 600" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC >= 600  < 699" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC >= 700 < 799" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC > 800" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC < 600" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC >=600  < 700" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC >= 700 < 799" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC > 800" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "INPEC CARRERA SC < 600" ~ "CONVENIO INPEC" ,
                     `Nombre de la Modalidad` == 
                       "INPEC CARRERA SC >= 700 < 799" ~ "CONVENIO INPEC" ,
                     `Nombre de la Modalidad` == 
                       "CASUR < 800" ~ "CASUR" , 
                     `Nombre de la Modalidad` == 
                       "CASUR SC >= 800" ~ "CASUR" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO CONVENIO GRUPOS" ~ "CONVENIO GRUPOS")) %>%
  filter(`Forma de Pago` == "Nomina" , `Nombre de la Modalidad` == c("SECTOR PUBLICO",
                                                                     "COLPENSIONES",
                                                                     "CONVENIO POLICIA",
                                                                     "CONVENIO INPEC",
                                                                     "CASUR",
                                                                     "CONVENIO GRUPOS")) %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva`) +
  geom_jitter(color = "grey30") + 
  geom_smooth(method = "lm") +
  labs(title = "Elasticidad precio de la demanda: modalidades de libranza" ,
       subtitle = "Enero a octubre del 2022" ,
       x="Monto (en millones)" , 
       y="% Tasa Efectiva") +
  facet_wrap(~ `Nombre de la Modalidad`) +
  stat_regline_equation(label.x = 30 , label.y = 30 , color = "blue4")



DATOS %>%
  mutate(`Tasa Efectiva` = `Tasa Efectiva`*100 ,
         `Nombre de la Modalidad` = 
           case_when(`Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO S < 600" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO SC >=600 <=699" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO S >= 700  <799" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO SECTOR PUBLICO S > 800" ~ "SECTOR PUBLICO" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC < 600" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC >= 600  < 699" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC >= 700 < 799" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "COLPENSIONES SC > 800" ~ "COLPENSIONES" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC < 600" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC >=600  < 700" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC >= 700 < 799" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "CONVENIO LIBRANZA POLICIA SC > 800" ~ "CONVENIO POLICIA" ,
                     `Nombre de la Modalidad` == 
                       "INPEC CARRERA SC < 600" ~ "CONVENIO INPEC" ,
                     `Nombre de la Modalidad` == 
                       "INPEC CARRERA SC >= 700 < 799" ~ "CONVENIO INPEC" ,
                     `Nombre de la Modalidad` == 
                       "CASUR < 800" ~ "CASUR" , 
                     `Nombre de la Modalidad` == 
                       "CASUR SC >= 800" ~ "CASUR" ,
                     `Nombre de la Modalidad` == 
                       "CREDI DESCUENTO CONVENIO GRUPOS" ~ "CONVENIO GRUPOS")) %>%
  filter(`Nombre de la Modalidad` == c("SECTOR PUBLICO",
                                        "COLPENSIONES",
                                        "CONVENIO POLICIA",
                                        "CONVENIO INPEC",
                                        "CASUR",
                                        "CONVENIO GRUPOS")) %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva`) +
  geom_jitter(color = "grey30") + 
  geom_smooth(method = "lm") +
  labs(title = "Elasticidad precio de la demanda: modalidades de libranza" ,
       subtitle = "Enero a octubre del 2022" ,
       x="Monto (en millones)" , 
       y="% Tasa Efectiva") +
  facet_wrap(~ `Nombre de la Modalidad` , scales = "free") +
  stat_regline_equation(label.x = 30 , label.y = 30 , color = "blue4")




DATOS %>%
  mutate(`Tasa Efectiva` = `Tasa Efectiva`*100 ,
         `Nombre de la Modalidad` = 
           case_when(`Nombre de la Modalidad` == 
                       "CREDI CONSUMO" ~ "CREDI CONSUMO" ,
                     `Nombre de la Modalidad` == 
                       "FG CREDI CONSUMO" ~ "CREDI CONSUMO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI INICIO" ~ "CREDI CONSUMO" ,
                     `Nombre de la Modalidad` == 
                       "FG CREDI INICIO" ~ "CREDI CONSUMO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI VENTANILLA PLUS" ~ "VENTANILLA PLUS" ,
                     `Nombre de la Modalidad` == 
                       "CREDI VENTANILLA PLUS INICIO" ~ "VENTANILLA PLUS" ,
                     `Nombre de la Modalidad` == 
                       "FG CREDI VENTANILLA PLUS INICIO" ~ "VENTANILLA PLUS" ,
                     `Nombre de la Modalidad` == 
                       "FG CREDI VENTANILLA PLUS" ~ "VENTANILLA PLUS" ,
                     `Nombre de la Modalidad` == 
                       "CREDI SOCIAL ROTATIVO 1" ~ "CREDI SOCIAL" ,
                     `Nombre de la Modalidad` == 
                       "CREDI SOCIAL ROTATIVO 2" ~ "CREDI SOCIAL" ,
                     `Nombre de la Modalidad` == 
                       "CREDI SOCIAL ROTATIVO 3" ~ "CREDI SOCIAL" ,
                     `Nombre de la Modalidad` == 
                       "CREDI SOCIAL" ~ "CREDI SOCIAL" ,
                     `Nombre de la Modalidad` == 
                       "CREDIFLASH COMPRA CARTERA" ~ "COMPRA DE CARTERA" ,
                     `Nombre de la Modalidad` == 
                       "ROTATIVO EDUCATIVO I" ~ "CREDI FORMAR (1,2,3 Y ESP.)" ,
                     `Nombre de la Modalidad` == 
                       "ROTATIVO EDUCATIVO II" ~ "CREDI FORMAR (1,2,3 Y ESP.)" ,
                     `Nombre de la Modalidad` == 
                       "ROTATIVO EDUCATIVO III" ~ "CREDI FORMAR (1,2,3 Y ESP.)" ,
                     `Nombre de la Modalidad` == 
                       "CREDI FORMAR I" ~ "CREDI FORMAR (1,2,3 Y ESP.)" ,
                     `Nombre de la Modalidad` == 
                       "CREDI FORMAR II" ~ "CREDI FORMAR (1,2,3 Y ESP.)" ,
                     `Nombre de la Modalidad` == 
                       "CREDI FORMAR III" ~ "CREDI FORMAR (1,2,3 Y ESP.)" ,
                     `Nombre de la Modalidad` == 
                       "CREDI FORMAR ESPECIALIZACION Y POSGRADOS" ~ "CREDI FORMAR (1,2,3 Y ESP.)",
                     `Nombre de la Modalidad` == 
                       "Credi Flash 7x24 Tarjeta Visa Platino" ~ "TARJETA ROTATIVO" ,
                     `Nombre de la Modalidad` == 
                       "Credi Flash 7x24 Tarjeta Visa Dorada" ~ "TARJETA ROTATIVO" ,
                     `Nombre de la Modalidad` == 
                       "Credi Flash 7x24 Tarjeta Visa Diamante" ~ "TARJETA ROTATIVO" ,
                     `Nombre de la Modalidad` == 
                       "TJ DEBITO CREDI FLASH 7X24" ~ "TARJETA ROTATIVO" ,
                     `Nombre de la Modalidad` == 
                       "CREDI HOGAR" ~ "CREDI HOGAR Y GARANTIA" ,
                     `Nombre de la Modalidad` == 
                       "CREDI HOGAR NO VIS" ~ "CREDI HOGAR Y GARANTIA" ,
                     `Nombre de la Modalidad` == 
                       "CREDI CONSUMO  GARANTIA" ~ "CREDI HOGAR Y GARANTIA")) %>%
  filter(`Nombre de la Modalidad` == c("CREDI CONSUMO",
                                       "VENTANILLA PLUS",
                                       "CREDI SOCIAL",
                                       "COMPRA DE CARTERA",
                                       "CREDI FORMAR (1,2,3 Y ESP.)",
                                       "TARJETA ROTATIVO",
                                       "CREDI HOGAR Y GARANTIA")) %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva`) +
  geom_jitter(color = "grey30") + 
  geom_smooth(method = "lm") +
  labs(title = "Elasticidad precio de la demanda: modalidades de ventanilla" ,
       subtitle = "Enero a octubre del 2022" ,
       x="Monto (en millones)" , 
       y="% Tasa Efectiva") +
  facet_wrap(~ `Nombre de la Modalidad` , scales = "free")





DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
  aes(`Meses vencidos` , Monto , color = Codeudor) +
  geom_jitter() +
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400)) +
  #scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (meses)" , 
       y = "Monto" , 
       title = "Requisito de codeudor: Altura de mora VS Monto" ,
       subtitle = "A corte octubre de 2022") +
  facet_wrap(~ `Codeudor` , scales = "free") +
  stat_regline_equation(label.x = 20) +
  theme(legend.position = "none")




levels(as.factor(DATOS$`Nombre de la Modalidad`))

# .........................................................................................................


#################################  BORRADORES DE GRAFICAS ###################



plot(ingresos , monto ) # grafica de dispersion de puntos
barplot(table(sexo))
table(sexo) # imprime tabla de frecuencias para categorias de la vble
barplot(table(edad)) # incluir "table" para graficar conteo o frecuencias de datos
table(sexo)
barplot(table(DATOS$`Nivel Estudios`))
barplot(table(DATOS$Antigüedad))


DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() + 
  aes(x = Edad , y = Sueldo , fill= Sexo) + # geom_point es para graficar puntos. "mapping" y "aes" deben ir siempre emparejadas si de graficar x y y se trata.
  geom_bar(stat = "identity" , bins = 50) +
  facet_wrap(~Sexo)


DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() + 
  aes(x = Sueldo , fill= Sexo) + # geom_point es para graficar puntos. "mapping" y "aes" deben ir siempre emparejadas si de graficar x y y se trata.
  geom_histogram(aes(y =..density..) , bins = 70 , position = 'identity') +
  scale_x_continuous(limits = c(0,20)) +
  geom_density(aes(linetype = Sexo , colour = Sexo) , alpha=0)



DATOS %>%
  filter(`Forma de Pago` == "Nomina" , `Nombre de la Modalidad` != c(
          "Grand Opening Libranza",
          "CREDI DESCUENTO SECTOR PUBLICO S >= 700  <799",
          "Grand Opening Libranza",
          "CREDI SERVICIOS DESCUENTO NOMINA",
          "CREDI DESCUENTO SECTOR PUBLICO",
          "CREDI DESCUENTO SECTOR PUBLICO S > 800",
          "Grand Opening Libranza",
          "ROTATIVO ANTICIPO NOMINA",
          "CREDI DESCUENTO SECTOR PRIVADO",
          "Credito Libranza")) %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva Colocacion`) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "MONTO VS TASA DE INTERES", x="MONTO", y="TASA E.A.") 
  #facet_wrap (~ `Nombre de la Modalidad`)


DATOS %>%
  filter(`Forma de Pago` == "Ventanilla" , `Nombre de la Modalidad` == c(
    "CREDI CONSUMO",
    "CREDI VENTANILLA PLUS",
    "CREDI CONSUMO  GARANTIA",
    "Credi Flash 7x24 Tarjeta Visa Diamante",
    "Credi Flash 7x24 Tarjeta Visa Dorada",
    "Credi Flash 7x24 Tarjeta Visa Platino",
    "CREDI SOCIAL ROTATIVO 1",
    "CREDI SOCIAL ROTATIVO 2",
    "CREDI SOCIAL ROTATIVO 3",
    "CREDI VENTANILLA PLUS INICIO",
    "CREDIFLASH COMPRA CARTERA",
    "FG CREDI CONSUMO")) %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva`) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "MONTO VS TASA DE INTERES", x="MONTO", y="TASA E.A.") +
  facet_wrap (~ `Nombre de la Modalidad`)




DATOS %>%
  filter(periodo == "2022-10-30") %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva Colocacion`) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "dispersi?n por genero", x="Peso", y="Longitud de la pata")


DATOS %>%
  filter(periodo == "2022-10-30" , modalidad == "CREDI CONSUMO") %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva Colocacion`) +
  geom_point() + 
  geom_smooth(method = "lm")
  

DATOS %>%
  filter(modalidad == "Credi Flash 7x24 Tarjeta Visa Platino") %>%
  ggplot() +
  aes(x = Monto , y = `Tasa Efectiva`) +
  geom_point() + 
  geom_smooth(method = "lm")


DATOS %>%
  filter(PERIODO == "2022-10-30" & `Forma de Pago` == "Ventanilla") %>%
  ggplot() +
  aes(`Antiguedad laboral`) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 60, by = 5))


calificacion <- DATOS %>%
  filter(PERIODO == "2022-10-30") %>%
  group_by(`Calificacion antes de Arrastre (Cierre)`) %>%
  summarise(total = n())

ggplot(calificacion, aes(`Calificacion antes de Arrastre (Cierre)` , 
                         total)) +
  geom_bar(position = "stack" , stat = "identity") +
  geom_text(aes(label = total) ,  vjust = -0.5) +
  labs(title = "Calificaciones - Octubre 2022", x="Categoria", y="N° asociados")





########################## ANALISIS ESTADISTICO ########################


cor(DATOS$Sueldo, DATOS$Antigüedad, use = "complete.obs") #correlacion

DATOS %>%
  lm(formula =  Monto ~ `Tasa Efectiva` + Sueldo + Edad)



x = lm(DATOS$Monto ~ DATOS$`Tasa Efectiva`)
x
plot(x)


# https://es.r4ds.hadley.nz/visualizaci%C3%B3n-de-datos.html#separar-en-facetas
# https://rsanchezs.gitbooks.io/rprogramming/content/chapter9/filter.html


tj_total = DATOS %>% 
  filter(`Nombre de la Modalidad` == c(
    "Credi Flash 7x24 Tarjeta Visa Diamante",
    "Credi Flash 7x24 Tarjeta Visa Dorada",
    "Credi Flash 7x24 Tarjeta Visa Platino"))

table(tj_total$`Nombre de la Modalidad`)


tj_oct = DATOS %>% 
  filter(PERIODO == "2022-10-30" , `Nombre de la Modalidad` == c(
    "Credi Flash 7x24 Tarjeta Visa Diamante",
    "Credi Flash 7x24 Tarjeta Visa Dorada",
    "Credi Flash 7x24 Tarjeta Visa Platino"))

table(tj_oct$`Nombre de la Modalidad`)



#......................REGRESION LOGISTICA SIMPLE............................


# Buen comportamiento menor o igual a 30 dias como calificacion A categoria consumo = 1
# Mal comportamiento mayor a 30 dias calificacion B,C,D,E = 0


DATOS = DATOS %>% filter(DATOS$`Forma de Pago` == "Ventanilla") # filtrar y modificar base inicial por medio de pago

modelo_logistico_1 = glm(`Calificacion antes de Arrastre (Cierre)`~ 
                         Monto + Codeudor + `% Cobertura de la Garantia (Cierre)` +
                         Edad + Sueldo + `Antiguedad laboral` + `Antiguedad asociado` +
                         Sexo + Num.Cuotas + `Dias vencidos` + Ocupacion + 
                          `Nivel Estudios` + `Estado civil` + `Tipo de contrato` ,
                       data = DATOS, family = binomial())

summary(modelo_logistico_1)
modelo_logistico_1$coefficients  # muestra unicamente los coeficientes de la regresion

# Fijarse en criterio AIC. Un menor valor de AIC indica un mejor modelo
# Fijarse en Residual deviance. Un menor valor indica menor ajuste, por tanto mejor resultado.


# Para realizar comparación de mejor modelo. Fijarse en Nagel y coeficiente cercano a 1 o 100% es mejor modelo.
PseudoR2(modelo_logistico_1, c("McFadden" , "Nagel"))
PseudoR2(modelo_logistico_2, c("McFadden" , "Nagel")) # 3er mejor modelo segun Nagel
PseudoR2(modelo_logistico_3, c("McFadden" , "Nagel")) # 2do mejor modelo segun Nagel / 1er mejor modelo segun AIC y deviance
PseudoR2(modelo_logistico_4, c("McFadden" , "Nagel")) # 1er mejor modelo segun Nagel / 2do mejor modelo segun AIC y deviance


# 1. Diferencia de Chi entre modelos
modelChi_1 = modelo_logistico_1$null.deviance - modelo_logistico_1$deviance
modelChi_1

modelChi_2 = modelo_logistico_2$null.deviance - modelo_logistico_2$deviance
modelChi_2

difChi = modelChi_1 - modelChi_2


# 2.Calcular grados de libertad
Glibertad_1 = modelo_logistico_1$df.residual
Glibertad_2 = modelo_logistico_2$df.residual
Glibertad_1
Glibertad_2


# 3. Estimar las diferencias entre grados de libertad
Gliber_total = Glibertad_1 - Glibertad_2   # Se le resta al modelo con mas grados de libertad
Gliber_total 


# 4. Calcular valor P de la diferencia entre modelos
valorP = 1-pchisq(difChi, df=11395) # en df se coloca el valor segun el resultado del paso 3
valorP
# Condiciones:  
Ho : El modelo X NO es significativo si valorP > 0.05
H1 : El modelo X SI es significativo si valorP < 0.05
# Se debe definir cual es el mejor modelo


# 5. Estimar las probabilidades del modelo escogido en el paso 4

pronostico_1 = predict(modelo_logistico_1, type = "response")
pronostico_1 

pronostico_2 = predict(modelo_logistico_2, type = "response")
pronostico_2 

ifelse(pronostico_1 > 0.5 , 1 , 0)

# 6. Grafica de dispersion del modelo 
plot(na.omit(pronostico_1 , DATOS$`Calificacion antes de Arrastre (Cierre)`) , main = "Modelo 1 Logistico Scoring Interno")
plot(na.omit(pronostico_2 , DATOS_VENTANILLA$`Calificacion antes de Arrastre (Cierre)`) , main = "Modelo 2 Logistico Scoring Interno")


# 7. Evaluacion del modelo
resultado_1 = table(na.omit(DATOS$`Calificacion antes de Arrastre (Cierre)` , floor(pronostico_1+0.5)))
resultado_1   # se tabula cantidad de personas con buen pago y mal pago, teniendo en cuenta una probabilidad o puntaje de 50% 

resultado_2 = table(na.omit(DATOS_VENTANILLA$`Calificacion antes de Arrastre (Cierre)` , floor(pronostico_2+0.5)))
resultado_2   # se tabula cantidad de personas con buen pago y mal pago, teniendo en cuenta una probabilidad o puntaje de 50% 

error_1 = sum(resultado_1 , resultado_1) / sum(resultado_1);error_1  # error del modelo, NO SE PARA QUE ES !!!


  
  
  


DATOS <- DATOS %>%
  mutate(Doctorado =
           ifelse(`Nivel Estudios` == "Doctorado" , 1 , 0) ,
         Posgrado = 
           case_when(`Nivel Estudios` == 
                       "Especializacion" ~ 1 ,`Nivel Estudios` == "Maestria" ~ 1 ,`Nivel Estudios` == "Magister" ~ 1) ,
         Posgrado =
           ifelse(is.na(Posgrado) , 0 , 1) ,
         Secundaria =
           ifelse(`Nivel Estudios` == "Secundaria" , 1 , 0) ,
         Universitaria =
           ifelse(`Nivel Estudios` == "Universitaria" , 1 , 0) ,
         Tecnologica =
           ifelse(`Nivel Estudios` == "Tecnológica" , 1 , ifelse(`Nivel Estudios` == "Tecnica" , 1 , 0)) ,
         Casado =
           ifelse(`Estado civil` == "Casado" , 1 , 0) ,
         Divorciado =
           ifelse(`Estado civil` == "Divorciado" , 1 , 0) ,
         Separado =
           ifelse(`Estado civil` == "Separado" , 1 , 0) ,
         Soltero =
           ifelse(`Estado civil` == "Soltero" , 1 , 0) ,
         `Union libre` =
           ifelse(`Estado civil` == "Union libre" , 1 , 0) ,
         Viudo =
           ifelse(`Estado civil` == "Viudo" , 1 , 0) ,
         
         `Obra Labor` =
           ifelse(`Tipo de contrato` == "Obra Labor" , 1 , 0) ,
         `Prestacion de servicios` =
           ifelse(`Tipo de contrato` == "Prestacion de Servicios" , 1 , 0) ,
         `Termino fijo` =
           ifelse(`Tipo de contrato` == "Termino Fijo" , 1 , 0) ,
         `Termino Indefinido` =
           ifelse(`Tipo de contrato` == "Termino Indefinido" , 1 , 0))



modelo_logistico_x = glm(`Calificacion antes de Arrastre (Cierre)`~ 
                           Monto + Codeudor + `% Cobertura de la Garantia (Cierre)` +
                           Edad + Sueldo + `Antiguedad laboral dias` + `Antiguedad asociado dias` +
                           Sexo + Num.Cuotas + `Dias vencidos` + Ocupacion + 
                           Doctorado + 
                           Posgrado +
                           Secundaria +
                           Universitaria +
                           Tecnologica +
                           Casado +
                           Divorciado +
                           Separado +
                           Soltero +
                           `Union libre` +
                           Viudo +
                           `Obra Labor` +
                           `Prestacion de servicios` +
                           `Termino fijo` +
                           `Termino Indefinido` ,
                         family = binomial() , data = DATOS %>% filter(PERIODO == "2022-10-30"))
  
summary(modelo_logistico_x)
exp(coefficients(modelo_logistico_x))
