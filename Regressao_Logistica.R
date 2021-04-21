usethis::create_github_token()
usethis::edit_r_environ()

usethis::use_git()
usethis::use_github()
usethis::use_readme_rmd()

library(openxlsx) #biblioteca para escrever arquivo em excel)
library(haven)
library(ggplot2)
library(tidyverse)
library(rcompanion) # cramersV

##################################
#### dia 4 REGRESSAO LOGISTICA

idade=c(21,20,25,26,22,35,36,40,42,46,59,50,60,72,85,59,29,45,39,45,20,25,36,58,95,52,80,85,62,72)
renda=c(1,1,1,1,0,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0)
saude=c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
dados <- as.data.frame(cbind(saude, idade, renda))

modelo_log <- glm(saude~idade+renda,family=binomial(link = "logit"))
summary(modelo_log)
exp(modelo_log$coefficients)


#########################
## Reg logistica

#importando base titanic
titanic <- read_sas("dados/titanic.sas7bdat",
                    NULL)
View(titanic)

#descritivas
ggplot(titanic, aes(x = as.factor(Survival), fill = Gender)) +
  geom_bar() +
  labs(title = "Titanic", x = "Sobreviventes", y = "Frequencia")

ggplot(titanic, aes(x = Gender)) +
      geom_bar()

ggplot(titanic, aes(x = Class)) +
  geom_bar()

#niveis
table(titanic$Survival)
table(titanic$Gender)
table (titanic$Class)

#histogramas
hist(titanic$Age)
boxplot(titanic$Age)
hist(titanic$Fare)
boxplot(titanic$Fare)

# tem missing?
table(is.na (titanic$Survival))
table(is.na (titanic$Gender))
table(is.na (titanic$Class))
table(is.na (titanic$Age))
table(is.na (titanic$Fare))

# existe relacoes entre as variaveis e a resposta?
#relacao com as vars categorica
gender_qui <- table(titanic$Survival, titanic$Gender)
chisq.test(gender_qui)
cramerV(gender_qui)

class_qui <- table(titanic$Survival, titanic$Class)
chisq.test(class_qui)
cramerV(class_qui)

#relacao com as vars numericas
titanic1 <- titanic %>%
  mutate( class1 = as.character(Class),
          Sobre = as.character(Survival))

ggplot(titanic1, aes(x=Sobre, y=Fare)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2)

ggplot(titanic1, aes(x=Sobre, y=Age)) +
  geom_boxplot(color="red", fill="orange", alpha=0.2)

# tenho que tratar o missing
# tomando a decisao de jogar fora (SO NESSA AULA PORQUE ISSO SERA VISTO DEPOIS!!!)
titanic_semmiss <- na.omit(titanic1)

#criando as dummies
titanic_semmiss_dummie <-  titanic_semmiss %>%
  mutate(
    Sexo_Fem = ifelse(Gender == "Female", 1, 0),
    Class_1 = ifelse(as.character(Class) == "1", 1, 0),
    Class_2 = ifelse(Class == 2, 1, 0)
  ) %>%
  select( -Gender, - Class, -Sobre)

#fazendo o modelo
full <- glm(Survival ~ . -Name -class1, family = "binomial", data=titanic_semmiss_dummie)
summary(full)

model_step <- step(full, direction = "both")
summary(model_step)

#########
## odds

exp(model_step$coefficients)

## ods da numerica
exp(model_step$coefficients[2])
## aumentando "a cada incremento" em uma numerica
exp(10*model_step$coefficients[2])


#########

# exemplificando a escolha da refencia
#criando as dummies agora sexo MASCULINO
titanic_semmiss_dummie_masc <-  titanic_semmiss %>%
  mutate(
    Sexo_Fem = ifelse(Gender == "Female", 0, 1),
    Class_1 = ifelse(as.character(Class) == "1", 1, 0),
    Class_2 = ifelse(Class == 2, 1, 0)
  ) %>%
  select( -Gender, - Class, -Sobre)

#fazendo o modelo
full_masc <- glm(Survival ~ . -Name -class1, family = "binomial", data=titanic_semmiss_dummie_masc)
summary(full_masc)

model_step_masc <- step(full_masc, direction = "both")
summary(model_step_masc)

#########
## odds
exp(model_step_masc$coefficients)



#########
## avaliacao

#criando a resposta
resultado <- predict(model_step, titanic_semmiss_dummie, type="response")
#juntando no dado original
dados_estimados <- cbind(titanic_semmiss_dummie, resultado)
#criando regra p decisao
dados_estimados <- dados_estimados %>%
  mutate(
    Decision = ifelse(resultado >0.5, 1, 0)
  )

#matriz de confusao
conf <- table(dados_estimados$Survival, dados_estimados$Decision)
sum(conf[1,])

#precision
#de todos que estou dizendo que sao 1, quantos acertei
prec <- conf[2,2]/sum(conf[,2])
prec

#recall
#de todos que sao 1, quantos estou pegando
recall <- conf[2,2]/sum(conf[2,])
recall


#criando erro para estimativa
dados_estimados_est <- dados_estimados %>%
  mutate( Erro_model = Survival - resultado,
          Erro_media = Survival - mean(Survival))

#RME
rme_model <- sum(dados_estimados_est$Erro_model^2)
rme_model
#e se fosse a média?
rme_mean <- sum(dados_estimados_est$Erro_media^2)
rme_mean

#Lift
#vamos pegar os primeiros 10%
dados_estimados_est_n <- dados_estimados_est %>%
  mutate( n = seq(1045)) %>%
  filter(n<(1045/10))
#taxa so dos 10%
taxa_10 <- mean(dados_estimados_est_n$Survival)
taxa_10
#taxa da base toda
taxa <- mean(dados_estimados_est$Survival)
taxa
#logo, o lift
lift <- taxa_10/ taxa
lift
