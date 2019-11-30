#AUTOR: Fabiana Cebrian
#Data: 23/11/2019
#Descrição: Análise das variações na qualidade do ar em Beijing - 
#Demonstração do comportamento dos poluentes particulados PM2.5 e PM10 

install.packages("ggplot2")
install.packages("dplyr") 
install.packages("plyr")
install.packages("readr")
library(plyr)
library(readr)
library(dplyr)
library(ggplot2)

#exibir a pasta de trabalho atual
getwd()

#alterar a pasta de trabalho
setwd("/Users/fabiana.cebrian/Documents/Air Quality")
getwd()

estacao01<-read.csv("Aotizhongxin.csv", TRUE, ",")
estacao02<-read.csv("Changping.csv", TRUE, ",")
estacao03<-read.csv("Dingling.csv", TRUE, ",")
estacao04<-read.csv("Dongsi.csv", TRUE, ",")
estacao05<-read.csv("Guanyuan.csv", TRUE, ",")
estacao06<-read.csv("Gucheng.csv", TRUE, ",")
estacao07<-read.csv("Huairou.csv", TRUE, ",")
estacao08<-read.csv("Nongzhanguan.csv", TRUE, ",")
estacao09<-read.csv("Shunyi.csv", TRUE, ",")
estacao10<-read.csv("Tiantan.csv", TRUE, ",")
estacao11<-read.csv("Wanliu.csv", TRUE, ",")
estacao12<-read.csv("Wanshouxigong.csv", TRUE, ",")

pasta="/Users/fabiana.cebrian/Documents/Air Quality"
estacoes12 = list.files(path=pasta, pattern="*.csv", full.names=TRUE) 
testacoes12 = ldply(estacoes12, read_csv) 


#####################################################################
#Estação Aotizhongxin
########################################################################
head(estacao01)
summary(estacao01)
length(estacao01$PM2.5)

#No período de 01/03/2013 a 28/02/2017 o PM2.5 mínimo foi 3 ug/m^3 e o máximo 898 ug/m^3
# 925 ocorrências não informadas.

#No período de 01/03/2013 a 28/02/2017 o PM10 mínimo foi 2 ug/m^3 e o máximo 984 ug/m^3
# 718 ocorrências não informadas.

#No período de 01/03/2013 a 28/02/2017 a temperatura mínima foi -16.80 Celsius e o máxima foi 40.50 Celsius
# 20 ocorrências não informadas.

#retornar a lista de valores Únicos, retira os repetidos
pm25v = unique(estacao01$PM2.5)
pm25v

#extrair colunas específicas do data frame
resultano <- data.frame(estacao01$day,estacao01$month,estacao01$year,estacao01$hour,estacao01$PM2.5,estacao01$TEMP)
print(resultano)

hist(estacao01$PM2.5)
#PM2.5 na estação 01 possui mais valores entre 0 e 50
#pela tabela até 55,4 de PM2.5 é considerado não saudável para pessoas sensíveis

hist(estacao01$PM10)
#PM10 na estação 01 possui mais valores entre 0 e 50
#pela tabela até 54 de PM10 é considerado bom

# Portanto nesta estação em grande parte dos anos 2013 até 2017, 
# a qualidade do ar conforme o AQI ficou entre boa e moderada.

hist(testacoes12$PM10)
#o mesmo se observa ao agrupar todas as estações
# PM2.5 possui mais valores entre 0 e 50

hist(testacoes12$PM10)
#PM10 possui mais valores entre 0 e 150
#pela tabela até 154 de PM10 é considerado moderado

poluentes <- data.frame(testacoes12$PM2.5,testacoes12$PM10)

ggplot(poluentes, aes(x=testacoes12$PM2.5), color='blue')+
  geom_histogram(fill='red', binwidth = 1)

ggplot(testacoes12, aes(x=testacoes12$PM10), color='blue')+
  geom_histogram(fill='red', binwidth = 1)

#ver histograma de barras


# separa os dados por ano da estação 01
data.frame_por_ano <- split(estacao01,estacao01$year)
head(data.frame_por_ano)

ano2013est01<-(data.frame_por_ano$`2013`)
ano2014est01<-(data.frame_por_ano$`2014`)
ano2015est01<-(data.frame_por_ano$`2015`)
ano2016est01<-(data.frame_por_ano$`2016`)
ano2017est01<-(data.frame_por_ano$`2017`)

summary(ano2013est01)
# de 01/03/2013 a 31/12/2013
#Em 2013 o PM2.5 mínimo 3 ug/m^3 foi e o máximo foi 665 ug/m^3
#Em 2013 o PM10 mínimo 2 ug/m^3 foi e o máximo foi 844 ug/m^3
#Em 2013 a temperatura mínima foi -9.4 Celsius e o máxima foi 37.9 Celsius

summary(ano2014est01)
# de 01/01/2014 a 31/12/2014
#Em 2014 o PM2.5 mínimo 3 ug/m^3 foi e o máximo foi 584 ug/m^3
#Em 2014 o PM10 mínimo 3 ug/m^3 foi e o máximo foi 948 ug/m^3
#Em 2014 a temperatura mínima foi -12.20 Celsius e o máxima foi 40.50 Celsius

summary(ano2015est01)
# de 01/01/2015 a 31/12/2015
#Em 2015 o PM2.5 mínimo 3 ug/m^3 foi e o máximo foi 657 ug/m^3
#Em 2015 o PM10 mínimo 3 ug/m^3 foi e o máximo foi 984 ug/m^3
#Em 2015 a temperatura mínima foi -10.00 Celsius e o máxima foi 38.80 Celsius

summary(ano2016est01)
# de 01/01/2016 a 31/12/2016
#Em 2016 o PM2.5 mínimo 3 ug/m^3 foi e o máximo foi 898 ug/m^3
#Em 2016 o PM10 mínimo 4 ug/m^3 foi e o máximo foi 884 ug/m^3
#Em 2016 a temperatura mínima foi -16.80 Celsius e o máxima foi 37.30 Celsius
# 2016 ano mais frio, PM2.5 atingiu o mínimo e o máximo.

summary(ano2017est01)
# de 01/01/2017 a 28/02/2017
#Em 2017 o PM2.5 mínimo 3 ug/m^3 foi e o máximo foi 713 ug/m^3
#Em 2017 o PM10 mínimo 3 ug/m^3 foi e o máximo foi 858 ug/m^3
#Em 2017 a temperatura mínima foi -9.70 Celsius e o máxima foi 15.00 Celsius

#################################################
# separar por estações do ano
# influência clima x poluentes na estação 01

#PM2.5 menores nos primeiros meses
# dez jan fevereiro inverno
# março abril maio primavera
# junho julho agosto verão
# setembro a novembro outono

data.frame_por_mes <- split(estacao01,estacao01$month)
head(data.frame_por_mes)

data.frame_por_pm25sg <- split(estacao01,estacao01$PM2.5<=55)
#sensitivegroupe<-data.frame_por_pm25mod[["TRUE"]][["PM2.5"]]
head(sensitivegroupe)
data.frame_por_pm25mod <- split(estacao01,estacao01$PM2.5<=35)
moderado<-data.frame_por_pm25mod[["TRUE"]][["PM2.5"]]
head(moderado)

#12141 registros menores que 35
#16520 registros menores que 55

#inverno todos os anos
mes12est01<-(data.frame_por_mes$`12`)
mes1est01<-(data.frame_por_mes$`1`)
mes2est01<-(data.frame_por_mes$`2`)

boxplot(mes12est01$PM2.5,mes1est01$PM2.5,mes2est01$PM2.5,
        col="lightblue")

#primavera todos os anos
mes3est01<-(data.frame_por_mes$`3`)
mes4est01<-(data.frame_por_mes$`4`) #menores discrepâncias
mes5est01<-(data.frame_por_mes$`5`)
head(mes5est01)

boxplot(mes3est01$PM2.5,mes4est01$PM2.5,mes5est01$PM2.5,
        col="lightblue")

#verão todos os anos
mes6est01<-(data.frame_por_mes$`6`)
mes7est01<-(data.frame_por_mes$`7`) #menores discrepâncias
mes8est01<-(data.frame_por_mes$`8`) #menores discrepâncias
head(mes8est01)

boxplot(mes6est01$PM2.5,mes7est01$PM2.5,mes8est01$PM2.5,
                col="lightblue")

#junho a setembro é chuvoso

#outono todos os anos
mes9est01<-(data.frame_por_mes$`9`) #menores discrepâncias
mes10est01<-(data.frame_por_mes$`10`)
mes11est01<-(data.frame_por_mes$`11`) #grandes discrepâncias
head(mes8est01)

boxplot(mes9est01$PM2.5,mes10est01$PM2.5,mes11est01$PM2.5,
        col="lightblue")

#chuva
data.frame_por_chuva <-split(estacao01,estacao01$RAIN) 

# Transformar vetor para data.frame
dados2 <- data.frame(y = dados)

chuva <- data.frame(y = estacao01$RAIN)
chuva




data.frame_por_temp <-split(estacao01,estacao01$TEMP) 

###################################################################
#Estação Aotizhongxin - 2013 - PM2.5
###################################################################

pm25media2013 = mean(ano2013est01$PM2.5,na.rm = TRUE)
pm25mediana2013 = median(ano2013est01$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2013 = sd(ano2013est01$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2013 = var(ano2013est01$PM2.5, na.rm = TRUE)

pm25media2013
pm25mediana2013
pm25desviopadrao2013
pm25variancia2013

# Coeficiente de Variação (CV) 
CV2013pm25 = 100*pm25desviopadrao2013/pm25media2013
CV2013pm25

# Amplitude total da série A
Amax2013pm25 = max(ano2013est01$PM2.5, na.rm = TRUE)
Amax2013pm25
Amin2013pm25 = min(ano2013est01$PM2.5, na.rm = TRUE)
A2013pm25 = Amax2013pm25 - Amin2013pm25
A2013pm25

# Tabela da análise estatística do PM2.5 em 2013: estação Aotizhongxin
dfpm252013<-data.frame(
  dfpm252013.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252013.val = c(pm25media2013,pm25desviopadrao2013,pm25mediana2013,pm25variancia2013,CV2013pm25,Amax2013pm25,
                     Amin2013pm25,A2013pm25)
)

###################################################################
#Estação Aotizhongxin - 2013 - PM10
###################################################################

pm10media2013 = mean(ano2013est01$PM10,na.rm = TRUE)
pm10mediana2013 = median(ano2013est01$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2013 = sd(ano2013est01$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2013 = var(ano2013est01$PM10, na.rm = TRUE)

pm10media2013
pm10mediana2013
pm10desviopadrao2013
pm10variancia2013

# Coeficiente de Variação (CV) 
CV2013pm10 = 100*pm10desviopadrao2013/pm10media2013
CV2013pm10

# Amplitude total da série A
Amax2013pm10 = max(ano2013est01$PM10, na.rm = TRUE)
Amin2013pm10 = min(ano2013est01$PM10, na.rm = TRUE)
A2013pm10 = Amax2013pm10 - Amin2013pm10
A2013pm10

# Tabela da análise estatística do PM10 em 2013: estação Aotizhongxin
dfpm102013<-data.frame(
  dfpm102013.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102013.val = c(pm10media2013,pm10desviopadrao2013,pm10mediana2013,pm10variancia2013,CV2013pm10,Amax2013pm10,
                     Amin2013pm10,A2013pm10)
)

###################################################################
# #Estação Aotizhongxin - 2013: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z1 = plot(ano2013est01$RAIN,ano2013est01$PM2.5)
z2 = plot(ano2013est01$RAIN,ano2013est01$PM10)
z3 = plot(ano2013est01$TEMP,ano2013est01$PM10)
Z4 = plot(ano2013est01$TEMP,ano2013est01$PM2.5)
Z5 = plot(ano2013est01$TEMP,ano2013est01$RAIN)
dados <- data.frame(ano2013est01$TEMP,ano2013est01$RAIN) #criando um data.frame
regressao<-lm(ano2013est01$RAIN~ano2013est01$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2013est01$TEMP,ano2013est01$RAIN) 

boxplot(ano2013est01$PM2.5,ano2013est01$PM10,
        col="lightblue")

hist(ano2013est01$PM2.5,ano2013est01$PM10,
     col="lightblue")

###################################################################
#Estação Aotizhongxin - 2014 - PM2.5
###################################################################

pm25media2014 = mean(ano2014est01$PM2.5,na.rm = TRUE)
pm25mediana2014 = median(ano2014est01$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2014 = sd(ano2014est01$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2014 = var(ano2014est01$PM2.5, na.rm = TRUE)

pm25media2014
pm25mediana2014
pm25desviopadrao2014
pm25variancia2014

# Coeficiente de Variação (CV) 
CV2014pm25 = 100*pm25desviopadrao2014/pm25media2014
CV2014pm25

# Amplitude total da série A
Amax2014pm25 = max(ano2014est01$PM2.5, na.rm = TRUE)
Amax2014pm25
Amin2014pm25 = min(ano2014est01$PM2.5, na.rm = TRUE)
A2014pm25 = Amax2014pm25 - Amin2014pm25
A2014pm25

# Tabela da análise estatística do PM2.5 em 2014: estação Aotizhongxin
dfpm252014<-data.frame(
  dfpm252014.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252014.val = c(pm25media2014,pm25desviopadrao2014,pm25mediana2014,pm25variancia2014,CV2014pm25,Amax2014pm25,
                     Amin2014pm25,A2014pm25)
)

###################################################################
#Estação Aotizhongxin - 2014 - PM10
###################################################################

pm10media2014 = mean(ano2014est01$PM10,na.rm = TRUE)
pm10mediana2014 = median(ano2014est01$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2014 = sd(ano2014est01$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2014 = var(ano2014est01$PM10, na.rm = TRUE)

pm10media2014
pm10mediana2014
pm10desviopadrao2014
pm10variancia2014

# Coeficiente de Variação (CV) 
CV2014pm10 = 100*pm10desviopadrao2014/pm10media2014
CV2014pm10

# Amplitude total da série A
Amax2014pm10 = max(ano2014est01$PM10, na.rm = TRUE)
Amin2014pm10 = min(ano2014est01$PM10, na.rm = TRUE)
A2014pm10 = Amax2014pm10 - Amin2014pm10
A2014pm10

# Tabela da análise estatística do PM10 em 2014: estação Aotizhongxin
dfpm102014<-data.frame(
  dfpm102014.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102014.val = c(pm10media2014,pm10desviopadrao2014,pm10mediana2014,pm10variancia2014,CV2014pm10,Amax2014pm10,
                     Amin2014pm10,A2014pm10)
)

###################################################################
# #Estação Aotizhongxin - 2014: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z11 = plot(ano2014est01$RAIN,ano2014est01$PM2.5)
z22 = plot(ano2014est01$RAIN,ano2014est01$PM10)
z33 = plot(ano2014est01$TEMP,ano2014est01$PM10)
Z44 = plot(ano2014est01$TEMP,ano2014est01$PM2.5)
Z55 = plot(ano2014est01$TEMP,ano2014est01$RAIN)
dados <- data.frame(ano2014est01$TEMP,ano2014est01$RAIN) #criando um data.frame
regressao<-lm(ano2014est01$RAIN~ano2014est01$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2014est01$TEMP,ano2014est01$RAIN) 

boxplot(ano2014est01$PM2.5,ano2014est01$PM10,
        col="lightblue")

###################################################################
#Estação Aotizhongxin - 2015 - PM2.5
###################################################################

pm25media2015 = mean(ano2015est01$PM2.5,na.rm = TRUE)
pm25mediana2015 = median(ano2015est01$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2015 = sd(ano2015est01$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2015 = var(ano2015est01$PM2.5, na.rm = TRUE)

pm25media2015
pm25mediana2015
pm25desviopadrao2015
pm25variancia2015

# Coeficiente de Variação (CV) 
CV2015pm25 = 100*pm25desviopadrao2015/pm25media2015
CV2015pm25

# Amplitude total da série A
Amax2015pm25 = max(ano2015est01$PM2.5, na.rm = TRUE)
Amax2015pm25
Amin2015pm25 = min(ano2015est01$PM2.5, na.rm = TRUE)
A2015pm25 = Amax2015pm25 - Amin2015pm25
A2015pm25

# Tabela da análise estatística do PM2.5 em 2015: estação Aotizhongxin
dfpm252015<-data.frame(
  dfpm252015.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252015.val = c(pm25media2015,pm25desviopadrao2015,pm25mediana2015,pm25variancia2015,CV2015pm25,Amax2015pm25,
                     Amin2015pm25,A2015pm25)
)

###################################################################
#Estação Aotizhongxin - 2015 - PM10
###################################################################

pm10media2015 = mean(ano2015est01$PM10,na.rm = TRUE)
pm10mediana2015 = median(ano2015est01$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2015 = sd(ano2015est01$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2015 = var(ano2015est01$PM10, na.rm = TRUE)

pm10media2015
pm10mediana2015
pm10desviopadrao2015
pm10variancia2015

# Coeficiente de Variação (CV) 
CV2015pm10 = 100*pm10desviopadrao2015/pm10media2015
CV2015pm10

# Amplitude total da série A
Amax2015pm10 = max(ano2015est01$PM10, na.rm = TRUE)
Amin2015pm10 = min(ano2015est01$PM10, na.rm = TRUE)
A2015pm10 = Amax2015pm10 - Amin2015pm10
A2015pm10

# Tabela da análise estatística do PM10 em 2015: estação Aotizhongxin
dfpm102015<-data.frame(
  dfpm102015.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102015.val = c(pm10media2015,pm10desviopadrao2015,pm10mediana2015,pm10variancia2015,CV2015pm10,Amax2015pm10,
                     Amin2015pm10,A2015pm10)
)

###################################################################
# #Estação Aotizhongxin - 2015: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z115 = plot(ano2015est01$RAIN,ano2015est01$PM2.5)
z225 = plot(ano2015est01$RAIN,ano2015est01$PM10)
z335 = plot(ano2015est01$TEMP,ano2015est01$PM10)
Z445 = plot(ano2015est01$TEMP,ano2015est01$PM2.5)
Z555 = plot(ano2015est01$TEMP,ano2015est01$RAIN)
dados <- data.frame(ano2015est01$TEMP,ano2015est01$RAIN) #criando um data.frame
regressao<-lm(ano2015est01$RAIN~ano2015est01$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2015est01$TEMP,ano2015est01$RAIN) 

boxplot(ano2015est01$PM2.5,ano2015est01$PM10,
        col="lightblue")

###################################################################
#Estação Aotizhongxin - 2016 - PM2.5
###################################################################

pm25media2016 = mean(ano2016est01$PM2.5,na.rm = TRUE)
pm25mediana2016 = median(ano2016est01$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2016 = sd(ano2016est01$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2016 = var(ano2016est01$PM2.5, na.rm = TRUE)

pm25media2016
pm25mediana2016
pm25desviopadrao2016
pm25variancia2016

# Coeficiente de Variação (CV) 
CV2016pm25 = 100*pm25desviopadrao2016/pm25media2016
CV2016pm25

# Amplitude total da série A
Amax2016pm25 = max(ano2016est01$PM2.5, na.rm = TRUE)
Amax2016pm25
Amin2016pm25 = min(ano2016est01$PM2.5, na.rm = TRUE)
A2016pm25 = Amax2016pm25 - Amin2016pm25
A2016pm25

# Tabela da análise estatística do PM2.5 em 2016: estação Aotizhongxin
dfpm252016<-data.frame(
  dfpm252016.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252016.val = c(pm25media2016,pm25desviopadrao2016,pm25mediana2016,pm25variancia2016,CV2016pm25,Amax2016pm25,
                     Amin2016pm25,A2016pm25)
)

###################################################################
#Estação Aotizhongxin - 2016 - PM10
###################################################################

pm10media2016 = mean(ano2016est01$PM10,na.rm = TRUE)
pm10mediana2016 = median(ano2016est01$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2016 = sd(ano2016est01$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2016 = var(ano2016est01$PM10, na.rm = TRUE)

pm10media2016
pm10mediana2016
pm10desviopadrao2016
pm10variancia2016

# Coeficiente de Variação (CV) 
CV2016pm10 = 100*pm10desviopadrao2016/pm10media2016
CV2016pm10

# Amplitude total da série A
Amax2016pm10 = max(ano2016est01$PM10, na.rm = TRUE)
Amin2016pm10 = min(ano2016est01$PM10, na.rm = TRUE)
A2016pm10 = Amax2016pm10 - Amin2016pm10
A2016pm10

# Tabela da análise estatística do PM10 em 2016: estação Aotizhongxin
dfpm102016<-data.frame(
  dfpm102016.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102016.val = c(pm10media2016,pm10desviopadrao2016,pm10mediana2016,pm10variancia2016,CV2016pm10,Amax2016pm10,
                     Amin2016pm10,A2016pm10)
)

###################################################################
# #Estação Aotizhongxin - 2016: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z116 = plot(ano2016est01$RAIN,ano2016est01$PM2.5)
z226 = plot(ano2016est01$RAIN,ano2016est01$PM10)
z336 = plot(ano2016est01$TEMP,ano2016est01$PM10)
Z446 = plot(ano2016est01$TEMP,ano2016est01$PM2.5)
Z556 = plot(ano2016est01$TEMP,ano2016est01$RAIN)
dados <- data.frame(ano2016est01$TEMP,ano2016est01$RAIN) #criando um data.frame
regressao<-lm(ano2016est01$RAIN~ano2016est01$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2016est01$TEMP,ano2016est01$RAIN) 

boxplot(ano2016est01$PM2.5,ano2016est01$PM10,
        col="lightblue")

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est01$PM2.5,ano2014est01$PM2.5,ano2015est01$PM2.5,ano2016est01$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est01$PM10,ano2014est01$PM10,ano2015est01$PM10,ano2016est01$PM10,
        col="lightblue")

###################################################################
#Estação Aotizhongxin - 2017 - PM2.5 - INVERNO
###################################################################

pm25media2017 = mean(ano2017est01$PM2.5,na.rm = TRUE)
pm25mediana2017 = median(ano2017est01$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2017 = sd(ano2017est01$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2017 = var(ano2017est01$PM2.5, na.rm = TRUE)

pm25media2017
pm25mediana2017
pm25desviopadrao2017
pm25variancia2017

# Coeficiente de Variação (CV) 
CV2017pm25 = 100*pm25desviopadrao2017/pm25media2017
CV2017pm25

# Amplitude total da série A
Amax2017pm25 = max(ano2017est01$PM2.5, na.rm = TRUE)
Amax2017pm25
Amin2017pm25 = min(ano2017est01$PM2.5, na.rm = TRUE)
A2017pm25 = Amax2017pm25 - Amin2017pm25
A2017pm25

# Tabela da análise estatística do PM2.5 em 2017: estação Aotizhongxin
dfpm252017<-data.frame(
  dfpm252017.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252017.val = c(pm25media2017,pm25desviopadrao2017,pm25mediana2017,pm25variancia2017,CV2017pm25,Amax2017pm25,
                     Amin2017pm25,A2017pm25)
)

###################################################################
#Estação Aotizhongxin - 2017 - PM10 - INVERNO
###################################################################

pm10media2017 = mean(ano2017est01$PM10,na.rm = TRUE)
pm10mediana2017 = median(ano2017est01$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2017 = sd(ano2017est01$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2017 = var(ano2017est01$PM10, na.rm = TRUE)

pm10media2017
pm10mediana2017
pm10desviopadrao2017
pm10variancia2017

# Coeficiente de Variação (CV) 
CV2017pm10 = 100*pm10desviopadrao2017/pm10media2017
CV2017pm10

# Amplitude total da série A
Amax2017pm10 = max(ano2017est01$PM10, na.rm = TRUE)
Amin2017pm10 = min(ano2017est01$PM10, na.rm = TRUE)
A2017pm10 = Amax2017pm10 - Amin2017pm10
A2017pm10

# Tabela da análise estatística do PM10 em 2017: estação Aotizhongxin
dfpm102017<-data.frame(
  dfpm102017.est= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102017.val = c(pm10media2017,pm10desviopadrao2017,pm10mediana2017,pm10variancia2017,CV2017pm10,Amax2017pm10,
                     Amin2017pm10,A2017pm10)
)

###################################################################
# #Estação Aotizhongxin - 2017: INVERNO - PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z117 = plot(ano2017est01$RAIN,ano2017est01$PM2.5)
z227 = plot(ano2017est01$RAIN,ano2017est01$PM10)
z337 = plot(ano2017est01$TEMP,ano2017est01$PM10)
Z447 = plot(ano2017est01$TEMP,ano2017est01$PM2.5)
Z557 = plot(ano2017est01$TEMP,ano2017est01$RAIN)
dados <- data.frame(ano2017est01$TEMP,ano2017est01$RAIN) #criando um data.frame
regressao<-lm(ano2017est01$RAIN~ano2017est01$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2017est01$TEMP,ano2017est01$RAIN) 

boxplot(ano2017est01$PM2.5,ano2017est01$PM10,
        col="lightblue")

#####################################################################
#Estação Changping
########################################################################
head(estacao02)
summary(estacao02)
length(estacao02$PM2.5)

data.frame_por_ano <- split(estacao02,estacao02$year)
head(data.frame_por_ano)

ano2013est02<-(data.frame_por_ano$`2013`)
ano2014est02<-(data.frame_por_ano$`2014`)
ano2015est02<-(data.frame_por_ano$`2015`)
ano2016est02<-(data.frame_por_ano$`2016`)
ano2017est02<-(data.frame_por_ano$`2017`)
summary(ano2013est02)
summary(ano2014est02)
summary(ano2015est02)
summary(ano2016est02)
summary(ano2017est02)

data.frame_por_mes <- split(estacao02,estacao02$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est02<-(data.frame_por_mes$'1')
mes2est02<-(data.frame_por_mes$'2')

###################################################################
#Estação Changping - 2013 - PM2.5
###################################################################

pm25media2013 = mean(ano2013est02$PM2.5,na.rm = TRUE)
pm25mediana2013 = median(ano2013est02$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2013 = sd(ano2013est02$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2013 = var(ano2013est02$PM2.5, na.rm = TRUE)

pm25media2013
pm25mediana2013
pm25desviopadrao2013
pm25variancia2013

# Coeficiente de Variação (CV) 
CV2013pm25 = 100*pm25desviopadrao2013/pm25media2013
CV2013pm25

# Amplitude total da série A
Amax2013pm25 = max(ano2013est02$PM2.5, na.rm = TRUE)
Amax2013pm25
Amin2013pm25 = min(ano2013est02$PM2.5, na.rm = TRUE)
A2013pm25 = Amax2013pm25 - Amin2013pm25
A2013pm25

# Tabela da análise estatística do PM2.5 em 2013: estação Changping
dfpm252013est02<-data.frame(
  dfpm252013.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252013.val02 = c(pm25media2013,pm25desviopadrao2013,pm25mediana2013,pm25variancia2013,CV2013pm25,Amax2013pm25,
                     Amin2013pm25,A2013pm25)
)

###################################################################
#Estação Changping - 2013 - PM10
###################################################################

pm10media2013 = mean(ano2013est02$PM10,na.rm = TRUE)
pm10mediana2013 = median(ano2013est02$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2013 = sd(ano2013est02$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2013 = var(ano2013est02$PM10, na.rm = TRUE)

pm10media2013
pm10mediana2013
pm10desviopadrao2013
pm10variancia2013

# Coeficiente de Variação (CV) 
CV2013pm10 = 100*pm10desviopadrao2013/pm10media2013
CV2013pm10

# Amplitude total da série A
Amax2013pm10 = max(ano2013est02$PM10, na.rm = TRUE)
Amin2013pm10 = min(ano2013est02$PM10, na.rm = TRUE)
A2013pm10 = Amax2013pm10 - Amin2013pm10
A2013pm10

# Tabela da análise estatística do PM10 em 2013: estação Changping
dfpm102013est02<-data.frame(
  dfpm102013.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102013.val02 = c(pm10media2013,pm10desviopadrao2013,pm10mediana2013,pm10variancia2013,CV2013pm10,Amax2013pm10,
                     Amin2013pm10,A2013pm10)
)

###################################################################
# #Estação Changping - 2013: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z113 = plot(ano2013est02$RAIN,ano2013est02$PM2.5)
z223 = plot(ano2013est02$RAIN,ano2013est02$PM10)
z333 = plot(ano2013est02$TEMP,ano2013est02$PM10)
Z443 = plot(ano2013est02$TEMP,ano2013est02$PM2.5)
Z553 = plot(ano2013est02$TEMP,ano2013est02$RAIN)
dados <- data.frame(ano2013est02$TEMP,ano2013est02$RAIN) #criando um data.frame
regressao<-lm(ano2013est02$RAIN~ano2013est02$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2013est02$TEMP,ano2013est02$RAIN) 

boxplot(ano2013est02$PM2.5,ano2013est02$PM10,
        col="lightblue")

###################################################################
#Estação Changping - 2014 - PM2.5
###################################################################

pm25media2014 = mean(ano2014est02$PM2.5,na.rm = TRUE)
pm25mediana2014 = median(ano2014est02$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2014 = sd(ano2014est02$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2014 = var(ano2014est02$PM2.5, na.rm = TRUE)

pm25media2014
pm25mediana2014
pm25desviopadrao2014
pm25variancia2014

# Coeficiente de Variação (CV) 
CV2014pm25 = 100*pm25desviopadrao2014/pm25media2014
CV2014pm25

# Amplitude total da série A
Amax2014pm25 = max(ano2014est02$PM2.5, na.rm = TRUE)
Amax2014pm25
Amin2014pm25 = min(ano2014est02$PM2.5, na.rm = TRUE)
A2014pm25 = Amax2014pm25 - Amin2014pm25
A2014pm25

# Tabela da análise estatística do PM2.5 em 2014: estação Changping
dfpm252014est02<-data.frame(
  dfpm252014.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252014.val02 = c(pm25media2014,pm25desviopadrao2014,pm25mediana2014,pm25variancia2014,CV2014pm25,Amax2014pm25,
                     Amin2014pm25,A2014pm25)
)

###################################################################
#Estação Changping - 2014 - PM10
###################################################################

pm10media2014 = mean(ano2014est02$PM10,na.rm = TRUE)
pm10mediana2014 = median(ano2014est02$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2014 = sd(ano2014est02$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2014 = var(ano2014est02$PM10, na.rm = TRUE)

pm10media2014
pm10mediana2014
pm10desviopadrao2014
pm10variancia2014

# Coeficiente de Variação (CV) 
CV2014pm10 = 100*pm10desviopadrao2014/pm10media2014
CV2014pm10

# Amplitude total da série A
Amax2014pm10 = max(ano2014est02$PM10, na.rm = TRUE)
Amin2014pm10 = min(ano2014est02$PM10, na.rm = TRUE)
A2014pm10 = Amax2014pm10 - Amin2014pm10
A2014pm10

# Tabela da análise estatística do PM10 em 2014: estação Changping
dfpm102014est02<-data.frame(
  dfpm102014.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102014.val02 = c(pm10media2014,pm10desviopadrao2014,pm10mediana2014,pm10variancia2014,CV2014pm10,Amax2014pm10,
                     Amin2014pm10,A2014pm10)
)

###################################################################
# #Estação Changping - 2014: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z11 = plot(ano2014est02$RAIN,ano2014est02$PM2.5)
z22 = plot(ano2014est02$RAIN,ano2014est02$PM10)
z33 = plot(ano2014est02$TEMP,ano2014est02$PM10)
Z44 = plot(ano2014est02$TEMP,ano2014est02$PM2.5)
Z55 = plot(ano2014est02$TEMP,ano2014est02$RAIN)
dados <- data.frame(ano2014est02$TEMP,ano2014est02$RAIN) #criando um data.frame
regressao<-lm(ano2014est02$RAIN~ano2014est02$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2014est02$TEMP,ano2014est02$RAIN) 

boxplot(ano2014est02$PM2.5,ano2014est02$PM10,
        col="lightblue")

###################################################################
#Estação Changping - 2015 - PM2.5
###################################################################

pm25media2015 = mean(ano2015est02$PM2.5,na.rm = TRUE)
pm25mediana2015 = median(ano2015est02$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2015 = sd(ano2015est02$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2015 = var(ano2015est02$PM2.5, na.rm = TRUE)

pm25media2015
pm25mediana2015
pm25desviopadrao2015
pm25variancia2015

# Coeficiente de Variação (CV) 
CV2015pm25 = 100*pm25desviopadrao2015/pm25media2015
CV2015pm25

# Amplitude total da série A
Amax2015pm25 = max(ano2015est02$PM2.5, na.rm = TRUE)
Amax2015pm25
Amin2015pm25 = min(ano2015est02$PM2.5, na.rm = TRUE)
A2015pm25 = Amax2015pm25 - Amin2015pm25
A2015pm25

# Tabela da análise estatística do PM2.5 em 2015: estação Changping
dfpm252015est02<-data.frame(
  dfpm252015.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252015.val02 = c(pm25media2015,pm25desviopadrao2015,pm25mediana2015,pm25variancia2015,CV2015pm25,Amax2015pm25,
                     Amin2015pm25,A2015pm25)
)

###################################################################
#Estação Changping - 2015 - PM10
###################################################################

pm10media2015 = mean(ano2015est02$PM10,na.rm = TRUE)
pm10mediana2015 = median(ano2015est02$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2015 = sd(ano2015est02$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2015 = var(ano2015est02$PM10, na.rm = TRUE)

pm10media2015
pm10mediana2015
pm10desviopadrao2015
pm10variancia2015

# Coeficiente de Variação (CV) 
CV2015pm10 = 100*pm10desviopadrao2015/pm10media2015
CV2015pm10

# Amplitude total da série A
Amax2015pm10 = max(ano2015est02$PM10, na.rm = TRUE)
Amin2015pm10 = min(ano2015est02$PM10, na.rm = TRUE)
A2015pm10 = Amax2015pm10 - Amin2015pm10
A2015pm10

# Tabela da análise estatística do PM10 em 2015: estação Changping
dfpm102015est02<-data.frame(
  dfpm102015.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102015.val02 = c(pm10media2015,pm10desviopadrao2015,pm10mediana2015,pm10variancia2015,CV2015pm10,Amax2015pm10,
                     Amin2015pm10,A2015pm10)
)

###################################################################
# #Estação Changping - 2015: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z115 = plot(ano2015est02$RAIN,ano2015est02$PM2.5)
z225 = plot(ano2015est02$RAIN,ano2015est02$PM10)
z335 = plot(ano2015est02$TEMP,ano2015est02$PM10)
Z445 = plot(ano2015est02$TEMP,ano2015est02$PM2.5)
Z555 = plot(ano2015est02$TEMP,ano2015est02$RAIN)
dados <- data.frame(ano2015est02$TEMP,ano2015est02$RAIN) #criando um data.frame
regressao<-lm(ano2015est02$RAIN~ano2015est02$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2015est02$TEMP,ano2015est02$RAIN) 

boxplot(ano2015est02$PM2.5,ano2015est02$PM10,
        col="lightblue")
###################################################################
#Estação Changping - 2016 - PM2.5
###################################################################

pm25media2016 = mean(ano2016est02$PM2.5,na.rm = TRUE)
pm25mediana2016 = median(ano2016est02$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2016 = sd(ano2016est02$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2016 = var(ano2016est02$PM2.5, na.rm = TRUE)

pm25media2016
pm25mediana2016
pm25desviopadrao2016
pm25variancia2016

# Coeficiente de Variação (CV) 
CV2016pm25 = 100*pm25desviopadrao2016/pm25media2016
CV2016pm25

# Amplitude total da série A
Amax2016pm25 = max(ano2016est02$PM2.5, na.rm = TRUE)
Amax2016pm25
Amin2016pm25 = min(ano2016est02$PM2.5, na.rm = TRUE)
A2016pm25 = Amax2016pm25 - Amin2016pm25
A2016pm25

# Tabela da análise estatística do PM2.5 em 2016: estação Changping 
dfpm252016est02<-data.frame(
  dfpm252016.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252016.val02 = c(pm25media2016,pm25desviopadrao2016,pm25mediana2016,pm25variancia2016,CV2016pm25,Amax2016pm25,
                     Amin2016pm25,A2016pm25)
)

###################################################################
#Estação Changping  - 2016 - PM10
###################################################################

pm10media2016 = mean(ano2016est02$PM10,na.rm = TRUE)
pm10mediana2016 = median(ano2016est02$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2016 = sd(ano2016est02$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2016 = var(ano2016est02$PM10, na.rm = TRUE)

pm10media2016
pm10mediana2016
pm10desviopadrao2016
pm10variancia2016

# Coeficiente de Variação (CV) 
CV2016pm10 = 100*pm10desviopadrao2016/pm10media2016
CV2016pm10

# Amplitude total da série A
Amax2016pm10 = max(ano2016est02$PM10, na.rm = TRUE)
Amin2016pm10 = min(ano2016est02$PM10, na.rm = TRUE)
A2016pm10 = Amax2016pm10 - Amin2016pm10
A2016pm10

# Tabela da análise estatística do PM10 em 2016: estação Changping 
dfpm102016est02<-data.frame(
  dfpm102016.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102016.val02 = c(pm10media2016,pm10desviopadrao2016,pm10mediana2016,pm10variancia2016,CV2016pm10,Amax2016pm10,
                     Amin2016pm10,A2016pm10)
)

###################################################################
# #Estação Changping - 2016: PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z116 = plot(ano2016est02$RAIN,ano2016est02$PM2.5)
z226 = plot(ano2016est02$RAIN,ano2016est02$PM10)
z336 = plot(ano2016est02$TEMP,ano2016est02$PM10)
Z446 = plot(ano2016est02$TEMP,ano2016est02$PM2.5)
Z556 = plot(ano2016est02$TEMP,ano2016est02$RAIN)
dados <- data.frame(ano2016est02$TEMP,ano2016est02$RAIN) #criando um data.frame
regressao<-lm(ano2016est02$RAIN~ano2016est02$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2016est02$TEMP,ano2016est02$RAIN) 

boxplot(ano2016est02$PM2.5,ano2016est02$PM10,
        col="lightblue")

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est02$PM2.5,ano2014est02$PM2.5,ano2015est02$PM2.5,ano2016est02$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est02$PM10,ano2014est02$PM10,ano2015est02$PM10,ano2016est02$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin e Changping respectivamente
boxplot(ano2014est02$PM10,ano2014est02$PM10,
        col="lightblue")

###################################################################
#Estação Changping - 2017 - PM2.5 - INVERNO
###################################################################

pm25media2017 = mean(ano2017est02$PM2.5,na.rm = TRUE)
pm25mediana2017 = median(ano2017est02$PM2.5, na.rm = TRUE)
# Desvio-padrão (σ) 
pm25desviopadrao2017 = sd(ano2017est02$PM2.5, na.rm = TRUE)  
# Variância (σ2)
pm25variancia2017 = var(ano2017est02$PM2.5, na.rm = TRUE)

pm25media2017
pm25mediana2017
pm25desviopadrao2017
pm25variancia2017

# Coeficiente de Variação (CV) 
CV2017pm25 = 100*pm25desviopadrao2017/pm25media2017
CV2017pm25

# Amplitude total da série A
Amax2017pm25 = max(ano2017est02$PM2.5, na.rm = TRUE)
Amax2017pm25
Amin2017pm25 = min(ano2017est02$PM2.5, na.rm = TRUE)
A2017pm25 = Amax2017pm25 - Amin2017pm25
A2017pm25

# Tabela da análise estatística do PM2.5 em 2017: estação Changping
dfpm252017est02<-data.frame(
  dfpm252017.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm252017.val02 = c(pm25media2017,pm25desviopadrao2017,pm25mediana2017,pm25variancia2017,CV2017pm25,Amax2017pm25,
                     Amin2017pm25,A2017pm25)
)

###################################################################
#Estação Changping - 2017 - PM10 - INVERNO
###################################################################

pm10media2017 = mean(ano2017est02$PM10,na.rm = TRUE)
pm10mediana2017 = median(ano2017est02$PM10, na.rm = TRUE)
# Desvio-padrão (σ) 
pm10desviopadrao2017 = sd(ano2017est02$PM10, na.rm = TRUE)  
# Variância (σ2)
pm10variancia2017 = var(ano2017est02$PM10, na.rm = TRUE)

pm10media2017
pm10mediana2017
pm10desviopadrao2017
pm10variancia2017

# Coeficiente de Variação (CV) 
CV2017pm10 = 100*pm10desviopadrao2017/pm10media2017
CV2017pm10

# Amplitude total da série A
Amax2017pm10 = max(ano2017est02$PM10, na.rm = TRUE)
Amin2017pm10 = min(ano2017est02$PM10, na.rm = TRUE)
A2017pm10 = Amax2017pm10 - Amin2017pm10
A2017pm10

# Tabela da análise estatística do PM10 em 2017: estação Changping
dfpm102017est02<-data.frame(
  dfpm102017.est02= c("Média", "Desvio Padrão", "Mediana", "Variância", "Coeficiente de Variação", 
                    "Amplitude máx", "Amplitude mín", "Amplitude Total"),
  dfpm102017.val02 = c(pm10media2017,pm10desviopadrao2017,pm10mediana2017,pm10variancia2017,CV2017pm10,Amax2017pm10,
                     Amin2017pm10,A2017pm10)
)

###################################################################
# #Estação Changping - 2017: INVERNO - PM2.5 - PM10 - Temperatura - Chuva
###################################################################

z117 = plot(ano2017est02$RAIN,ano2017est02$PM2.5)
z227 = plot(ano2017est02$RAIN,ano2017est02$PM10)
z337 = plot(ano2017est02$TEMP,ano2017est02$PM10)
Z447 = plot(ano2017est02$TEMP,ano2017est02$PM2.5)
Z557 = plot(ano2017est02$TEMP,ano2017est02$RAIN)
dados <- data.frame(ano2017est02$TEMP,ano2017est02$RAIN) #criando um data.frame
regressao<-lm(ano2017est02$RAIN~ano2017est02$TEMP,data=dados) 
regressao
abline(regressao)
cor(ano2017est02$TEMP,ano2017est02$RAIN) 

boxplot(ano2017est02$PM2.5,ano2017est02$PM10,
        col="lightblue")

#####################################################################
#Estação Dingling
########################################################################

head(estacao03)
summary(estacao03)
length(estacao03$PM2.5)

data.frame_por_ano <- split(estacao03,estacao03$year)
head(data.frame_por_ano)

ano2013est03<-(data.frame_por_ano$`2013`)
ano2014est03<-(data.frame_por_ano$`2014`)
ano2015est03<-(data.frame_por_ano$`2015`)
ano2016est03<-(data.frame_por_ano$`2016`)
ano2017est03<-(data.frame_por_ano$`2017`)
summary(ano2013est03)
summary(ano2014est03)
summary(ano2015est03)
summary(ano2016est03)
summary(ano2017est03)

data.frame_por_mes <- split(estacao03,estacao03$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est03<-(data.frame_por_mes$'1')
mes2est03<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est03$PM2.5,ano2014est03$PM2.5,ano2015est03$PM2.5,ano2016est03$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est03$PM10,ano2014est03$PM10,ano2015est03$PM10,ano2016est03$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10,
        col="lightblue")

#####################################################################
#Estação Dongsi
########################################################################

head(estacao04)
summary(estacao04)
length(estacao04$PM2.5)

data.frame_por_ano <- split(estacao04,estacao04$year)
head(data.frame_por_ano)

ano2013est04<-(data.frame_por_ano$`2013`)
ano2014est04<-(data.frame_por_ano$`2014`)
ano2015est04<-(data.frame_por_ano$`2015`)
ano2016est04<-(data.frame_por_ano$`2016`)
ano2017est04<-(data.frame_por_ano$`2017`)
summary(ano2013est04)
summary(ano2014est04)
summary(ano2015est04)
summary(ano2016est04)
summary(ano2017est04)

data.frame_por_mes <- split(estacao04,estacao04$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est04<-(data.frame_por_mes$'1')
mes2est04<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est04$PM2.5,ano2014est04$PM2.5,ano2015est04$PM2.5,ano2016est04$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est04$PM10,ano2014est04$PM10,ano2015est04$PM10,ano2016est04$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10,
        col="lightblue")

#####################################################################
#Estação Guanyuan
########################################################################

head(estacao05)
summary(estacao05)
length(estacao05$PM2.5)

data.frame_por_ano <- split(estacao05,estacao05$year)
head(data.frame_por_ano)

ano2013est05<-(data.frame_por_ano$`2013`)
ano2014est05<-(data.frame_por_ano$`2014`)
ano2015est05<-(data.frame_por_ano$`2015`)
ano2016est05<-(data.frame_por_ano$`2016`)
ano2017est05<-(data.frame_por_ano$`2017`)
summary(ano2013est05)
summary(ano2014est05)
summary(ano2015est05)
summary(ano2016est05)
summary(ano2017est05)

data.frame_por_mes <- split(estacao05,estacao05$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est05<-(data.frame_por_mes$'1')
mes2est05<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est05$PM2.5,ano2014est05$PM2.5,ano2015est04$PM2.5,ano2016est04$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est05$PM10,ano2014est05$PM10,ano2015est05$PM10,ano2016est05$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        col="lightblue")

#####################################################################
#Estação Gucheng
########################################################################

head(estacao06)
summary(estacao06)
length(estacao06$PM2.5)

data.frame_por_ano <- split(estacao06,estacao06$year)
head(data.frame_por_ano)

ano2013est06<-(data.frame_por_ano$`2013`)
ano2014est06<-(data.frame_por_ano$`2014`)
ano2015est06<-(data.frame_por_ano$`2015`)
ano2016est06<-(data.frame_por_ano$`2016`)
ano2017est06<-(data.frame_por_ano$`2017`)
summary(ano2013est06)
summary(ano2014est06)
summary(ano2015est06)
summary(ano2016est06)
summary(ano2017est06)

data.frame_por_mes <- split(estacao06,estacao06$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est06<-(data.frame_por_mes$'1')
mes2est06<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est06$PM2.5,ano2014est06$PM2.5,ano2015est06$PM2.5,ano2016est06$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est06$PM10,ano2014est06$PM10,ano2015est06$PM10,ano2016est06$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        ano2014est06$PM10,
        col="lightblue")

#####################################################################
#Estação Huairou
########################################################################

head(estacao07)
summary(estacao07)
length(estacao07$PM2.5)

data.frame_por_ano <- split(estacao07,estacao07$year)
head(data.frame_por_ano)

ano2013est07<-(data.frame_por_ano$`2013`)
ano2014est07-(data.frame_por_ano$`2014`)
ano2015est07<-(data.frame_por_ano$`2015`)
ano2016est07<-(data.frame_por_ano$`2016`)
ano2017est07<-(data.frame_por_ano$`2017`)
summary(ano2013est07)
summary(ano2014est07)
summary(ano2015est07)
summary(ano2016est07)
summary(ano2017est07)

data.frame_por_mes <- split(estacao07,estacao07$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est07<-(data.frame_por_mes$'1')
mes2est07<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est07$PM2.5,ano2014est07$PM2.5,ano2015est07$PM2.5,ano2016est07$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est07$PM10,ano2014est07$PM10,ano2015est07$PM10,ano2016est07$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        ano2014est06$PM10, ano2014est07$PM10,
        col="lightblue")

#####################################################################
#Estação Nongzhanguan
########################################################################

head(estacao08)
summary(estacao08)
length(estacao08$PM2.5)

data.frame_por_ano <- split(estacao08,estacao08$year)
head(data.frame_por_ano)

ano2013est08<-(data.frame_por_ano$`2013`)
ano2014est08-(data.frame_por_ano$`2014`)
ano2015est08<-(data.frame_por_ano$`2015`)
ano2016est08<-(data.frame_por_ano$`2016`)
ano2017est08<-(data.frame_por_ano$`2017`)
summary(ano2013est08)
summary(ano2014est08)
summary(ano2015est08)
summary(ano2016est08)
summary(ano2017est08)

data.frame_por_mes <- split(estacao08,estacao08$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est08<-(data.frame_por_mes$'1')
mes2est08<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est08$PM2.5,ano2014est08$PM2.5,ano2015est08$PM2.5,ano2016est08$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est08$PM10,ano2014est08$PM10,ano2015est08$PM10,ano2016est08$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        ano2014est06$PM10, ano2014est07$PM10, ano2014est08$PM10,
        col="lightblue")

#####################################################################
#Estação Shunyi
########################################################################

head(estacao09)
summary(estacao09)
length(estacao09$PM2.5)

data.frame_por_ano <- split(estacao09,estacao09$year)
head(data.frame_por_ano)

ano2013est09<-(data.frame_por_ano$`2013`)
ano2014est09-(data.frame_por_ano$`2014`)
ano2015est09<-(data.frame_por_ano$`2015`)
ano2016est09<-(data.frame_por_ano$`2016`)
ano2017est09<-(data.frame_por_ano$`2017`)
summary(ano2013est09)
summary(ano2014est09)
summary(ano2015est09)
summary(ano2016est09)
summary(ano2017est09)

data.frame_por_mes <- split(estacao09,estacao09$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est09<-(data.frame_por_mes$'1')
mes2est09<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est09$PM2.5,ano2014est09$PM2.5,ano2015est09$PM2.5,ano2016est09$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est09$PM10,ano2014est09$PM10,ano2015est09$PM10,ano2016est09$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        ano2014est06$PM10, ano2014est07$PM10, ano2014est08$PM10, ano2014est09$PM10,
        col="lightblue")

## Box plot do PM2.5 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM2.5,ano2014est02$PM2.5, ano2014est03$PM2.5, ano2014est04$PM2.5, ano2014est05$PM2.5,
        ano2014est06$PM2.5, ano2014est07$PM2.5, ano2014est08$PM2.5, ano2014est09$PM2.5,
        col="lightblue")

#####################################################################
#Estação Tiantan
########################################################################

head(estacao10)
summary(estacao10)
length(estacao10$PM2.5)

data.frame_por_ano <- split(estacao10,estacao10$year)
head(data.frame_por_ano)

ano2013est10<-(data.frame_por_ano$`2013`)
ano2014est10-(data.frame_por_ano$`2014`)
ano2015est10<-(data.frame_por_ano$`2015`)
ano2016est10<-(data.frame_por_ano$`2016`)
ano2017est10<-(data.frame_por_ano$`2017`)
summary(ano2013est10)
summary(ano2014est10)
summary(ano2015est10)
summary(ano2016est10)
summary(ano2017est10)

data.frame_por_mes <- split(estacao10,estacao10$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est10<-(data.frame_por_mes$'1')
mes2est10<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est10$PM2.5,ano2014est10$PM2.5,ano2015est10$PM2.5,ano2016est10$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est10$PM10,ano2014est10$PM10,ano2015est10$PM10,ano2016est10$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10,ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        ano2014est06$PM10, ano2014est07$PM10, ano2014est08$PM10, ano2014est09$PM10, ano2014est10$PM10,
        col="lightblue")

## Box plot do PM2.5 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM2.5,ano2014est02$PM2.5, ano2014est03$PM2.5, ano2014est04$PM2.5, ano2014est05$PM2.5,
        ano2014est06$PM2.5, ano2014est07$PM2.5, ano2014est08$PM2.5, ano2014est09$PM2.5, ano2014est10$PM2.5,
        col="lightblue")

#####################################################################
#Estação Wanliu
########################################################################

head(estacao11)
summary(estacao11)
length(estacao11$PM2.5)

data.frame_por_ano <- split(estacao11,estacao11$year)
head(data.frame_por_ano)

ano2013est11<-(data.frame_por_ano$`2013`)
ano2014est11-(data.frame_por_ano$`2014`)
ano2015est11<-(data.frame_por_ano$`2015`)
ano2016est11<-(data.frame_por_ano$`2016`)
ano2017est11<-(data.frame_por_ano$`2017`)
summary(ano2013est11)
summary(ano2014est11)
summary(ano2015est11)
summary(ano2016est11)
summary(ano2017est11)

data.frame_por_mes <- split(estacao10,estacao11$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est11<-(data.frame_por_mes$'1')
mes2est11<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est11$PM2.5,ano2014est11$PM2.5,ano2015est11$PM2.5,ano2016est11$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est11$PM10,ano2014est11$PM10,ano2015est11$PM10,ano2016est11$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10, ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        ano2014est06$PM10, ano2014est07$PM10, ano2014est08$PM10, ano2014est09$PM10, ano2014est10$PM10,
        ano2014est11$PM10,
        col="lightblue")

## Box plot do PM2.5 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM2.5, ano2014est02$PM2.5, ano2014est03$PM2.5, ano2014est04$PM2.5, ano2014est05$PM2.5,
        ano2014est06$PM2.5, ano2014est07$PM2.5, ano2014est08$PM2.5, ano2014est09$PM2.5, ano2014est10$PM2.5,
        ano2014est11$PM2.5,
        col="lightblue")

## Box plot do PM2.5 de 2016 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2016est01$PM2.5, ano2016est02$PM2.5, ano2016est03$PM2.5, ano2016est04$PM2.5, ano2016est05$PM2.5,
        ano2016est06$PM2.5, ano2016est07$PM2.5, ano2016est08$PM2.5, ano2016est09$PM2.5, ano2016est10$PM2.5,
        ano2016est11$PM2.5,
        col="lightblue")

## Box plot do PM10 de 2016 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2016est01$PM10, ano2016est02$PM10, ano2016est03$PM10, ano2016est04$PM10, ano2016est05$PM10,
        ano2016est06$PM10, ano2016est07$PM10, ano2016est08$PM10, ano2016est09$PM10, ano2016est10$PM10,
        ano2016est11$PM10,
        col="lightblue")

#####################################################################
#Estação Wanshouxigong
########################################################################

head(estacao12)
summary(estacao12)
length(estacao12$PM2.5)

data.frame_por_ano <- split(estacao11,estacao12$year)
head(data.frame_por_ano)

ano2013est12<-(data.frame_por_ano$`2013`)
ano2014est12-(data.frame_por_ano$`2014`)
ano2015est12<-(data.frame_por_ano$`2015`)
ano2016est12<-(data.frame_por_ano$`2016`)
ano2017est12<-(data.frame_por_ano$`2017`)
summary(ano2013est12)
summary(ano2014est12)
summary(ano2015est12)
summary(ano2016est12)
summary(ano2017est12)

data.frame_por_mes <- split(estacao10,estacao12$month)
head(data.frame_por_mes)
# apenas 2014, 2015 2016 e 2017 possuem janeiro e fevereiro
mes1est12<-(data.frame_por_mes$'1')
mes2est12<-(data.frame_por_mes$'2')

## Box plot do PM2.5 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est12$PM2.5,ano2014est12$PM2.5,ano2015est12$PM2.5,ano2016est12$PM2.5,
        col="lightblue")

## Box plot do PM10 dos anos de 2013, 2014, 2015, 2016 respectivamente
boxplot(ano2013est12$PM10,ano2014est12$PM10,ano2015est12$PM10,ano2016est12$PM10,
        col="lightblue")

## Box plot do PM10 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM10, ano2014est02$PM10, ano2014est03$PM10, ano2014est04$PM10, ano2014est05$PM10,
        ano2014est06$PM10, ano2014est07$PM10, ano2014est08$PM10, ano2014est09$PM10, ano2014est10$PM10,
        ano2014est11$PM10, ano2014est12$PM10,
        col="lightblue")

## Box plot do PM2.5 de 2014 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2014est01$PM2.5, ano2014est02$PM2.5, ano2014est03$PM2.5, ano2014est04$PM2.5, ano2014est05$PM2.5,
        ano2014est06$PM2.5, ano2014est07$PM2.5, ano2014est08$PM2.5, ano2014est09$PM2.5, ano2014est10$PM2.5,
        ano2014est11$PM2.5, ano2014est12$PM2.5,
        col="lightblue")

## Box plot do PM2.5 de 2016 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2016est01$PM2.5, ano2016est02$PM2.5, ano2016est03$PM2.5, ano2016est04$PM2.5, ano2016est05$PM2.5,
        ano2016est06$PM2.5, ano2016est07$PM2.5, ano2016est08$PM2.5, ano2016est09$PM2.5, ano2016est10$PM2.5,
        ano2016est11$PM2.5, ano2016est12$PM2.5,
        col="lightblue")

## Box plot do PM10 de 2016 das estações Aotizhongxin, Changping e Dingling e demais respectivamente
boxplot(ano2016est01$PM10, ano2016est02$PM10, ano2016est03$PM10, ano2016est04$PM10, ano2016est05$PM10,
        ano2016est06$PM10, ano2016est07$PM10, ano2016est08$PM10, ano2016est09$PM10, ano2016est10$PM10,
        ano2016est11$PM10, ano2016est12$PM10,
        col="lightblue")

