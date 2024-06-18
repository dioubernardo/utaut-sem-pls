library(ggplot2)
library(tidyverse)

amostra <- read.csv("dados.csv")

amostra$Idade <- as.numeric(amostra$Idade)
idades <- data.frame(amostra$Sexo, amostra$Idade)
colnames(idades) <- c("Sexo", "Idade")
idades$Sexo <- factor(idades$Sexo, levels=c("Feminino", "Masculino"))

summary(filter(idades, Sexo == "Feminino"))
summary(filter(idades, Sexo == "Masculino"))
summary(idades)

#--------------------------------

amostra$TempoDocencia <- as.numeric(amostra$TempoDocencia)
tempos <- data.frame(amostra$Sexo, amostra$TempoDocencia)
colnames(tempos) <- c("Sexo", "TempoDocencia")
tempos$Sexo <- factor(tempos$Sexo, levels=c("Feminino", "Masculino"))

summary(filter(tempos, Sexo == "Feminino"))
summary(filter(tempos, Sexo == "Masculino"))
summary(tempos)

# ------------------------------

# teste de Normalidade
library("RVAideMemoire")
byf.shapiro(Idade ~ Sexo, amostra)
byf.shapiro(TempoDocencia ~ Sexo, amostra)

# se p-valor > 0,05 
#   não rejeitar hipotese nula
#   então dados com distribuição normal

# Nota: Nem Idade nem tempo de Docência tem distribuição normal

# ------------------------------

# no caso de dados não normais rodar testes não parametricos, 
# como Teste de Mann-Whitney
# H0 -> Os grupos NÃO são estatisticamente diferentes p-valor >  0,05
# H1 -> Os  grupos são estatisticamente diferentes    p-valor <= 0,05

library(stats)
wilcox.test(Idade ~ Sexo, amostra)
wilcox.test(TempoDocencia ~ Sexo, amostra)

