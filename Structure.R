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

# install.packages("ResourceSelection")
# install.packages("pROC")
# install.packages("ggpubr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("stargazer")
# install.packages("mlogit")
# install.packages("ROCR")



DATOS <- read_excel("C:/Users/ANALISIS_CREDITO/Documents/DAVID VARGAS/DATOS.xlsx" , sheet = "DATOS")
options(scipen = 999) # Para evitar notacion cientifica en resultados de comandos



############### MODIFICACION DE BASE Y TRANSFORMACION DE VBLES: PARA VISUALIZACION DE DATOS ###############


DATOS <- DATOS %>%
  filter(`Codigo Tipo de Asociado` !=9) %>%   # filtrar quitando tipo de asociado 9 persona juridica
  mutate(PERIODO = as.Date(PERIODO) , 
         Sueldo = Sueldo/1000000 ,
         `Total Aportes` = `Total Aportes`/1000000 ,
         Monto = Monto/1000000 ,
         `Saldo Capital` = `Saldo Capital`/1000000 ,
         `Valor Cuota` = `Valor Cuota`/1000000 ,
         `Endeudamiento mensual` = `Endeudamiento mensual`/1000000 ,
         `Endeudamiento total` = `Endeudamiento total`/1000000)


table(DATOS$`Codigo Tipo de Asociado`)
table(DATOS$PERIODO)
table(DATOS$`% Cobertura de la Garantia (Cierre)`)
table(DATOS$Codeudor)
table(DATOS$`Forma de Pago`)
table(DATOS$Sexo)
table(DATOS$`Calificacion antes de Arrastre (Cierre)`)

with(DATOS , table(`Calificacion antes de Arrastre (Cierre)`)) ## tabla de frecuencias




###################################   MODELO REGRESION LOGISTICO SIMPLE   #####################################


# .................................  MODIFICACION DE BASE PARA ANALISIS  ...............................


DATOS <- DATOS %>%
  mutate(`% Cobertura de la Garantia (Cierre)` = 
           ifelse(`% Cobertura de la Garantia (Cierre)` == 70 , 1 , 0)) %>%  
  mutate(`Codeudor` =
           ifelse(`Codeudor` == "SI" , 1 , 0 )) %>%  
  mutate(`Forma de Pago` = as.factor(`Forma de Pago`)) %>%  
  mutate(`Sexo` = ifelse(`Sexo` == "F" , 1 , 0)) %>%
  mutate(`Non Default` = 
           ifelse(`Calificacion antes de Arrastre (Cierre)`=="A",1,0))




# .........................CREACION SUB-BASE SOLO OCTUBRE............................................



  # Incluye todas las variables posibles:

modelo_logistico_1 = glm(`Non Default` ~ 
                           Monto + Codeudor + `% Cobertura de la Garantia (Cierre)` +
                           Edad + Sueldo + `Antiguedad laboral dias` + `Antiguedad asociado dias` +
                           Sexo + Num.Cuotas + `Dias vencidos` + Ocupacion + `Endeudamiento mensual` +
                           `Nivel Estudios` + `Estado civil` + `Tipo de contrato` +
                           `Tasa Efectiva Colocacion` + `Total Aportes` + `Forma de Pago`,
                         family = binomial() , data = DATOS %>% filter(PERIODO == "2022-10-30"))

summary(modelo_logistico_1)
stargazer(modelo_logistico_1 , 
          title = "Modelo 1: incluye todas las variables" , 
          type = "text" , 
          # intercept.bottom = TRUE , # "intercept.bottom = FALSE" para colocar arriba el beta cero
          digits = 2  ,
          covariate.labels=c("Monto" , "Codeudor" ,
                             "Garantia" , "Edad" , "Ingresos" ,
                             "Antiguedad laboral" , "Antiguedad asociado" ,
                             "Sexo" , "Plazo" , "Altura mora"))


  # Se elimina nivel de estudios y ocupación:

modelo_logistico_2 = glm(`Non Default` ~ 
                           Monto + Codeudor + `% Cobertura de la Garantia (Cierre)` +
                           Edad + Sueldo + `Antiguedad laboral dias` + `Antiguedad asociado dias` +
                           Sexo + Num.Cuotas + `Dias vencidos` + `Estado civil` + `Tipo de contrato` + `Endeudamiento mensual` +
                           `Tasa Efectiva Colocacion` + `Total Aportes` + `Forma de Pago`,
                         family = binomial() , data = DATOS %>% filter(PERIODO == "2022-10-30"))

summary(modelo_logistico_2)
stargazer(modelo_logistico_2 , 
          title = "Modelo 2: sin nivel de estudio y ocupacion" , 
          type = "text" , 
          # intercept.bottom = TRUE , # "intercept.bottom = FALSE" para colocar arriba el beta cero
          digits = 2  ,
          covariate.labels=c("Monto" , "Codeudor" ,
                             "Garantia" , "Edad" , "Ingresos" ,
                             "Antiguedad laboral" , "Antiguedad asociado" ,
                             "Sexo" , "Plazo" , "Altura mora"))



  # Se agrega dicótomas de termino indefinido y casado:


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


modelo_logistico_3 = glm(`Non Default` ~ 
                           Monto + Codeudor + `% Cobertura de la Garantia (Cierre)` +
                          Sueldo + `Antiguedad asociado dias` + Num.Cuotas + `Dias vencidos` + 
                           `Termino Indefinido` + `Endeudamiento mensual` +
                            `Tasa Efectiva Colocacion` + `Total Aportes` + `Forma de Pago`,
                         family = binomial() , data = DATOS %>% filter(PERIODO == "2022-10-30"))

summary(modelo_logistico_3)
stargazer(modelo_logistico_3 , 
          title = "Modelo 3" , 
          type = "text" , 
          # intercept.bottom = TRUE , # "intercept.bottom = FALSE" para colocar arriba el beta cero
          digits = 2  ,
          covariate.labels=c("Monto" , "Codeudor" ,
                             "Garantia" , "Edad" , "Ingresos" ,
                             "Antiguedad laboral" , "Antiguedad asociado" ,
                             "Sexo" , "Plazo" , "Altura mora"))




#### Regresion por MCO  ####

  modelo_lineal_4 = lm(`Dias vencidos` ~ 
                             Monto + Codeudor + Sueldo + `Antiguedad asociado dias` +
                              Num.Cuotas + `Termino Indefinido` + `Endeudamiento mensual` +
                             `Tasa nominal Colocacion` + `Total Aportes`,
                           data = DATOS %>% filter(PERIODO == "2022-10-30"))
  
summary(modelo_lineal_4)
stargazer(modelo_lineal_4 , 
          title = "Modelo 3" , 
          type = "text" , 
          # intercept.bottom = TRUE , # "intercept.bottom = FALSE" para colocar arriba el beta cero
          digits = 2  ,
          covariate.labels=c("Monto" , "Codeudor" ,
                             "Garantia" , "Edad" , "Ingresos" ,
                             "Antiguedad laboral" , "Antiguedad asociado" ,
                             "Sexo" , "Plazo" , "Altura mora"))



    ################   VALIDACION VALOR MAX Y MIN EN PESO % PARA SCORE   ###########

  DATOS %>%
    filter(PERIODO == "2022-10-30") %>%
    mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
    ggplot() + 
    aes(`Dias vencidos` , `Forma de Pago`) +
    geom_jitter() +
    geom_smooth(method = "lm" , color = "grey50") +
    #scale_x_continuous(limits = c(0,400)) +
    #scale_y_continuous(limits = c(0,40)) +
    labs(x ="Altura mora (dias)" , 
         y = "Monto" , 
         title = "x" ,
         subtitle = "A corte octubre de 2022") +
    stat_regline_equation() +
    theme(legend.position = "none")



#................................................................................................................


################################# PRUEBAS ESTADISTICAS DE MODELOS ###############################

### 1.1. Chi square para analisis univariante: Dependiente e independiente son dicotomicas

  # p-valor de Chi cuadrado con significancia del 5% cumple condición de asociación significativa
  tabla1 = table(DATOS$Codeudor , DATOS$`Non Default`)
  summary(tabla1)

  # Resultado de la matriz se debe interpretar con fila 1=hay codeudor ; y columna 1=calificacion A
  prop.table(tabla1 , 1)


    tabla2 = table(DATOS$`% Cobertura de la Garantia (Cierre)` , DATOS$`Non Default`)
    summary(tabla2)
    prop.table(tabla2 , 1)


  tabla3 = table(DATOS$Sexo , DATOS$`Non Default`)
  summary(tabla3)
  prop.table(tabla3 , 1)
  
  
  tabla4 = table(DATOS$`Termino Indefinido` , DATOS$`Non Default`)
  summary(tabla4)
  prop.table(tabla4 , 1)
  
  


### 1.2. Dependiente dicotomica e independiente cuantitativa
  
  # Decimal o porcentaje del área debajo de la curva, corresponde al nivel explicativo de la variable independiente.
  # Una variable es buena predictora, si tiene un coeficiente mayor a 0.7.
  a = roc(DATOS$`Non Default` ~ DATOS$`Antiguedad laboral dias` )
  a

  # Una mayor area en el grafico, corresponde a un mejor predictor. La linea recta corresponde al 50%
  plot(a)

  
  b = roc(DATOS$`Non Default` ~ DATOS$`Tasa Efectiva`)
  b
  plot(b)
  
  
### 2. COMPARACION AIC Y DEVIANCE

  # - Fijarse en criterio AIC. Un menor valor de AIC indica un mejor modelo
  # - Fijarse en Residual deviance. Un menor valor indica menor ajuste, por tanto mejor resultado.

modelo_logistico_1  # 1er mejor por residual deviance
modelo_logistico_2  # 2do mejor por AIC y residual deviance
modelo_logistico_3  # 1er mejor por AIC


### 3. Prueba McFadden y Nagel

  # Para realizar comparación de mejor modelo. Fijarse en Nagel y coeficiente cercano a 1 o 100% es mejor modelo.

PseudoR2(modelo_logistico_1, c("McFadden" , "Nagel")) # Mejor modelo
PseudoR2(modelo_logistico_2, c("McFadden" , "Nagel")) 
PseudoR2(modelo_logistico_3, c("McFadden" , "Nagel")) 


### 4. Test de Hosmer Lemeshow
  
  # H0: El modelo predice los datos. Valores observados = valores predichos
  # Ha: El modelo no predice los datos. Valores observados difieren de los predichos

DATOS_OCT <- DATOS %>% filter(PERIODO == "2022-10-30") %>%
hoslem.test(DATOS_OCT$`Non Default` , fitted(modelo_logistico_1))




################################ INTERPRETACION DE MODELOS: ODDS RATIO #############################

  # Permite mostrar en una tabla los odds ratio como intervalos de confianza.
    round(exp(cbind(OR = coef(modelo_logistico_3) , confint(modelo_logistico_3))) , 4)

  # Condiciones para el análisis:
    
    # Determinar factores protectores: Valor OR es menor a 1
    # Intervalo de confianza entre 2.5% y 97.5 no debe incluir el 1
  
  # Interpretación de OR:
  
    # Variables continuas -> (1 - OR):  Probabilidad en % de que suceda Y,  cuando incrementa una unidad de X
    # Variables dicótomas -> El valor 1 de la variable X, tiene la probabilidad de OR de que suceda Y , en relación al valor 0



  round(exp(coefficients(modelo_logistico_3)) , 4)
 
  # Condiciones para el análisis:
  
      # Se interpretan únicamente las variables que resulten significativas
    
  # Interpretación de OR:
  
    # Si es igual a 1, quiere decir que la probabilidad de éxito es igual a la de fracaso
  
     # Mas del 1 quiere decir que la probabilidad de éxito es mayor a la de fracaso: 
       # (OR-1 = % aumento de probabilidad de éxito de Y ante incremento unidad de X)
  
     # Menos del 1 quiere decir que la probabilidad de fracaso es mayor a la de éxito: 
       # (1-OR = % disminución de probabilidad de éxito de Y ante incremento unidad de X)
  
  
  
`Modelo Scoring` =data.frame(round(exp(cbind(OR = coef(modelo_logistico_3) , confint(modelo_logistico_3))) , 4))
`Modelo Scoring` %>%
  mutate(`Peso %` = ifelse(OR>1 , ifelse(OR<=2 , (OR-1) , OR ) , -(1-OR)))
  
  
  

# LEER: https://rpubs.com/Joaquin_AR/229736




#.........................................................................................................................











