

#################################  MODIFICACION SIMULADOR TARJETA CUPO ROTATIVO  #####################################

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
# update.packages()

DATOS <- read_excel("C:/Users/david/OneDrive/Documents/DAVID/FORMATOS/DATOS.xlsx" , sheet = "DATOS")
View(DATOS)

options(scipen = 999) # Para evitar notacion cientifica en resultados de comandos

DATOS <- DATOS %>%
  mutate(PERIODO = as.Date(PERIODO) , 
         Sueldo = Sueldo/1000000 ,
         `Total Aportes` = `Total Aportes`/1000000 ,
         Monto = Monto/1000000 ,
         `Saldo Capital` = `Saldo Capital`/1000000 ,
         `Valor Cuota` = `Valor Cuota`/1000000 ,
         `Endeudamiento mensual` = `Endeudamiento mensual`/1000000 ,
         `Endeudamiento total` = `Endeudamiento total`/1000000) 


ROTATIVO <- DATOS %>%
  filter(`Nombre de la Modalidad` == c("TJ DEBITO CREDI FLASH 7X24" ,
                                       "Credi Flash 7x24 Tarjeta Visa Diamante" , 
                                       "Credi Flash 7x24 Tarjeta Visa Platino" , 
                                       "Credi Flash 7x24 Tarjeta Visa Dorada")) %>%
  mutate(`Nombre de la Modalidad` = case_when(
    `Nombre de la Modalidad` == "Credi Flash 7x24 Tarjeta Visa Platino" ~ "Platino" ,
    `Nombre de la Modalidad` == "Credi Flash 7x24 Tarjeta Visa Dorada" ~ "Dorada" ,
    `Nombre de la Modalidad` == "Credi Flash 7x24 Tarjeta Visa Diamante" ~ "Diamante" ,
    `Nombre de la Modalidad` == "TJ DEBITO CREDI FLASH 7X24" ~ "Platino"))


ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  ggplot() +
  aes(`Calificacion antes de Arrastre (Cierre)`) + # fill = `Calificacion del Asociado`) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)) , stat = 'count' , vjust = -0.5) +
  facet_wrap(~ `Nombre de la Modalidad`)


ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  with(table(as.factor(`Nombre de la Modalidad`)))




############   REGRESION LOGIT    ###################



ROTATIVO <- ROTATIVO %>%
  mutate(`Non Default credito` = ifelse(`Calificacion Credito` == "A", 1 , 0) ,
         `Non Default asociado` = ifelse(`Calificacion del Asociado` == "A", 1 , 0) ,
         Capacidad = Sueldo*0.4) %>%
  mutate(`% Cobertura de la Garantia (Cierre)` = 
           ifelse(`% Cobertura de la Garantia (Cierre)` == 70 , 1 , 0)) %>%  
  mutate(`Codeudor` =
           ifelse(`Codeudor` == "SI" , 1 , 0 )) %>%  
  mutate(`Forma de Pago` = as.factor(`Forma de Pago`)) %>%  
  mutate(`Sexo` = ifelse(`Sexo` == "F" , 1 , 0)) %>%
  mutate(`Non Default` = 
           ifelse(`Calificacion antes de Arrastre (Cierre)`=="A",1,0))



t1 = table(ROTATIVO$Sexo , ROTATIVO$`Non Default credito`)
summary(t1)
prop.table(t1 , 1)

a = roc(ROTATIVO$`Non Default credito` ~ ROTATIVO$`Antiguedad laboral dias` )
a
plot(a)



  

modelo_rotativo_1 = glm(`Non Default credito` ~ 
                          `Antiguedad asociado dias` + 
                          Monto + 
                          Capacidad + 
                          `Total Aportes` +
                          `Endeudamiento mensual` +
                          `Num.Cuotas` +
                          `Dias vencidos` , 
                         family = binomial() , data = ROTATIVO %>% filter(PERIODO == "2022-10-30"))

summary(modelo_rotativo_1)
round(exp(cbind(OR = coef(modelo_rotativo_1) , confint(modelo_rotativo_1))) , 4)



modelo_rotativo_2 = lm(`Dias vencidos` ~ 
                          `Antiguedad asociado dias` + 
                          Monto + 
                         Sueldo +
                         `Total Aportes` +
                        `Endeudamiento mensual` ,
                        data = ROTATIVO %>% filter(PERIODO == "2022-10-30"))

summary(modelo_rotativo_2)


modelo_rotativo_3 = lm(`Dias vencidos` ~ 
                         `Antiguedad asociado dias` + 
                         Monto + 
                         Capacidad + 
                         `Total Aportes` +
                         `Endeudamiento mensual` ,
                       data = ROTATIVO %>% filter(PERIODO == "2022-10-30"))

summary(modelo_rotativo_3)





modelo_rotativo_4 = lm(`Dias vencidos` ~ 
                         `Antiguedad asociado dias` + 
                         Monto + 
                         Capacidad + 
                         `Total Aportes` +
                         `Endeudamiento mensual` +
                         `Num.Cuotas` ,
                       data = ROTATIVO %>% filter(PERIODO == "2022-10-30"))

summary(modelo_rotativo_4)






###### CORRECCION PROBLEMAS DE PESO VBLES  ##########

corr1 = lm(`Dias vencidos` ~ 
                Monto + 
                Capacidad + 
                `Endeudamiento mensual` ,
              data = ROTATIVO %>% filter(PERIODO == "2022-10-30"))
summary(corr1)

################   VALIDACION VALOR MAX Y MIN EN PESO % PARA SCORE   ###########

ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
  aes(`Dias vencidos` , `Antiguedad asociado dias`) +
  geom_jitter() +
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400)) +
  #scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (dias)" , 
       y = "Antiguedad asociado x dias" , 
       title = "Antiguedad asociado para menor riesgo: Categoria A mora hasta 30 dias" ,
       subtitle = "A corte octubre de 2022") +
  stat_regline_equation() +
  theme(legend.position = "none")





ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
  aes(`Dias vencidos` , Monto) +
  geom_jitter() +
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400)) +
  #scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (dias)" , 
       y = "Monto" , 
       title = "Monto maximo solicitado para menor riesgo: Categoria A mora hasta 30 dias" ,
       subtitle = "A corte octubre de 2022") +
  stat_regline_equation() +
  theme(legend.position = "none")




ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
  aes(`Dias vencidos` , Sueldo) +
  geom_jitter() +
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400)) +
  #scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (dias)" , 
       y = "Sueldo" , 
       title = "Sueldo maximo para menor riesgo: Categoria A mora hasta 30 dias" ,
       subtitle = "A corte octubre de 2022") +
  stat_regline_equation() +
  theme(legend.position = "none")



ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
    aes(`Dias vencidos` , `Total Aportes`) +
  geom_jitter() +
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400)) +
  #scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (meses)" , 
       y = "Aportes" , 
       title = "Aportes para menor riesgo: Categoria A mora hasta 30 dias" ,
       subtitle = "A corte octubre de 2022") +
  stat_regline_equation() +
  theme(legend.position = "none")



ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
  aes(`Dias vencidos` , `Endeudamiento mensual`) +
  geom_jitter() +
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400  )) +
  #scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (dias)" , 
       y = "Endeudamiento total mensual" , 
       title = "Endeudamiento mensual para menor riesgo: Categoria A mora hasta 30 dias" ,
       subtitle = "A corte octubre de 2022") +
  scale_x_continuous(limits = c(0,90)) +
  scale_y_continuous(limits = c(0,4)) +
  stat_regline_equation() +
  theme(legend.position = "none")


ROTATIVO %>%
  filter(PERIODO == "2022-10-30") %>%
  mutate(`Meses vencidos` = `Dias vencidos`/30) %>%
  ggplot() + 
  aes(`Dias vencidos` , `Num.Cuotas`) +
  geom_jitter() +
  geom_smooth(method = "lm" , color = "grey50") +
  #scale_x_continuous(limits = c(0,400  )) +
  #scale_y_continuous(limits = c(0,40)) +
  labs(x ="Altura mora (dias)" , 
       y = "Numero de cuotas" , 
       title = "Plazo de credito para menor riesgo: Categoria A mora hasta 30 dias" ,
       subtitle = "A corte octubre de 2022") +
  stat_regline_equation() +
  theme(legend.position = "none")

