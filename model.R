#working directory
getwd()

rm(list = ls())

#livrarias de pacotes
library(tidyverse)
library(haven)
library(viridis)
library(fastDummies)
library(overdisp)
library(hrbrthemes)
library(pscl)
library(glmmTMB)
library(ggplot2)

#lendo bancod e dados
database <- read_dta('database.dta')

database <- read_dta('DATABASE_CORR.dta') #treated with upa etc, see in the website

database$regiao <- as.factor(database$regiao)
database$estrato <- as.factor(database$estrato)

#selecionando variaveis para fazer analise
database_1 <- database %>% 
  dplyr::select(id,l5,l6,f1,sexo,idade,e7,e9,e22,f3,f5,f7,f9,f11,f15,f16,i1,i16,p6,p7,b36,
             e25_1,e25_2,e25_3,e25_4,e25_5,e25_6,e25_7,ar6,s1,s4,s7,rendadom,rendadompc,estrato,peso_calibrado_n,regiao)

database_1$l5 <- as.numeric(database_1$l5)
database_1$l6 <- as.numeric(database_1$l6)
database_1$b36 <- as.numeric(database_1$b36)

database_2 <- database_1 %>% filter(f1 == 1) #selecionado observacoes da area urbana

#retirando observacoes que nao souberam responder
database_3 <- database_1 %>%
  filter(l5 != 9) %>% 
  filter(l6 != 9999) %>% 
  mutate(l6 = case_when(l6 == 8888 ~ 0,TRUE~l6)) %>% 
  mutate(y = l5*l6) %>%
  filter(e9 != 9) %>% 
  filter(e22  != 99) %>% 
  filter(e25_1 != 9) %>% filter(e25_2 != 9) %>%  filter(e25_3 != 9) %>% filter(e25_4 != 9) %>% filter(e25_5 != 9) %>% 
  filter(e25_6 != 9) %>% filter(e25_7 != 9) %>%
  filter(i1 != 9) %>% 
  filter(i16 != 9) %>% 
  filter(s1 != 9) %>% 
  filter(s4 != 9) %>% 
  filter(s7 != 9) %>% 
  filter(b36!= 99) %>% 
  filter(f3 != 9 & f3 != 8) %>% 
  filter(f5 != 9 & f5 != 8) %>% 
  filter(f9 != 9 & f9 != 8) %>% 
  filter(f16 != 9) %>% 
  filter(f7 != 9 & f7 != 8) %>% 
  filter(f11 != 9) %>% 
  filter(f15 != 9) %>% 
  filter(p6 != 9) %>% 
  filter(p7 != 9) 
    
#escolaridade
database_4 <- database_3 %>% 
  mutate(esc1 = case_when(e22 >= 5 & e22 <=8 ~ 1, TRUE~0)) %>% 
  mutate(esc2 = case_when(e22 > 8~1, TRUE ~ 0))

#raca
database_5 <- database_4 %>% 
  mutate(naobranco = case_when(e9 >= 2~1, TRUE~0)) 

#idade
database_6 <- database_5 %>% 
  mutate(idade1 = case_when(idade >= 65 & idade <= 79 ~1, TRUE~0)) %>%
  mutate(idade2 = case_when(idade >= 80~1, TRUE~0))

#estrutura do domicilio
database_7 <- database_6 %>% 
  mutate(fam2 = case_when(ar6 == 2 ~1, TRUE~0)) %>% 
  mutate(fam3 = case_when(ar6 > 2 ~1, TRUE~0))

#faz curso ou nao
database_8 <- database_7 %>%
  mutate(faz_curso = case_when(e25_1 == 1 | e25_2 == 1 | e25_3 == 1 |
                                e25_4 == 1 | e25_5 == 1  | e25_6 == 1 |
                                e25_7 == 1 ~ 1, TRUE~0))

#status relacionamento e quantidade de carros
database_9 <- database_8 %>% ### criando diretorio
    mutate(casado = case_when(e7 == 2~1,TRUE~0)) %>% 
    mutate(b36 = case_when(b36 == 88~ 0, TRUE~ b36))

database <- database_9

rm(database_1,database_2,database_3,database_4,database_5,database_6,database_7,database_8,database_9)

#criando taxa de auto
database$taxauto <- database$b36/database$ar6

#construindo variaveis dummy  
database <- database %>% 
  dummy_columns(select_columns = c('f5','f11','f15','p6','p7'),remove_first_dummy = T)


#media e variancia das amostras
f <- IQR(x = database$y)

outlier <- quantile(database$y,probs = 0.75) + 1.5*f
outlier <- quantile(database$y,probs = 0.75) + 3*f
outlier

database <- database %>% 
  filter(y < outlier)

database <- database %>% 
  mutate(nest = paste(regiao,estrato))

database$nest[1]

database$ind_cami <- 0
database$ind_cami[database$nest == "1 1"] <- -6.2368
database$ind_cami[database$nest == "1 2"] <- -1.988
database$ind_cami[database$nest == "1 3"] <- 2.43613
database$ind_cami[database$nest == "1 4"] <- 5.7887
database$ind_cami[database$nest == "2 1"] <- -4.6974
database$ind_cami[database$nest == "2 2"] <- -2.4222
database$ind_cami[database$nest == "2 3"] <- 0.0094
database$ind_cami[database$nest == "2 4"] <- 7.1102
database$ind_cami[database$nest == "3 1"] <- -4.87
database$ind_cami[database$nest == "3 2"] <- -0.5021
database$ind_cami[database$nest == "3 3"] <- -0.46302
database$ind_cami[database$nest == "3 4"] <- 5.83509
database$ind_cami[database$nest == "4 1"] <- -5.8344
database$ind_cami[database$nest == "4 2"] <- -1.054
database$ind_cami[database$nest == "4 3"] <- 1.3574
database$ind_cami[database$nest == "4 4"] <- 5.53102
database$ind_cami[database$nest == "5 1"] <- -6.266
database$ind_cami[database$nest == "5 2"] <- -1.924
database$ind_cami[database$nest == "5 3"] <- 2.4646
database$ind_cami[database$nest == "5 4"] <- 5.7255

write_dta(data = database,path = paste0(getwd(),'\\database.dta'),version = 13)

write.csv(database,file = "database.csv")
descript <- as.data.frame(psych::describe(database))
descript
write_csv(descript,file = 'descript.csv')

#overdispersion test
names(database)

teste.super <- overdisp::overdisp(database, dependent.position = 2,
                   predictor.position = c(3,5,10,12,13,16:18,30:32,39:60,62)) #teste de super dispersao
teste.super

teste.super <- overdisp::overdisp(database, dependent.position = 38,
                                  predictor.position = c(3,5,10,12,13,16:18,30:32,39:60,62)) #teste de super dispersao
teste.super

mean(database$l5)
sd(database$l5)
var(database$l5)

prop.table(table(database$l5))*100

#################################################### BINOMIAL NEGATIVO INFLACIONADO DE ZEROS MULTINIVEL ####################################################
setwd("C:\\Users\\calde\\Google Drive\\Projetos\\Idosos\\Elsi\\resultados")

########################################### ESTRATO #############################################################

walk.nb <- glmmTMB(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                     taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                     f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7,
                     data = database,family = 'nbinom2')
summary(walk.nb)

walk.nb <- glmmTMB(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                     taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                     f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7,
                   data = database,family = 'nbinom2')

#ZIBN complete
walk.zinb0 <- glmmTMB(formula = l5 ~ 1,
                      zi=~1,data = database,family = 'nbinom2')

summary(walk.zinb0)

walk.zinb0 <- glmmTMB(formula = y ~ 1,
                     zi=~1,data = database,family = 'nbinom2')

summary(walk.zinb0)
logLik(walk.zinb0)
write_rds(walk.zinb0,file = 'walk.zinb0.rds')

zinb_dias <- pscl::zeroinfl(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                         taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                         f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7|
                         idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                         taxauto+fam2+fam3+i16+p6_2+p6_3+
                         p6_4+p7_2+p7_3+p7_4,data = database,link = 'logit',dist = 'negbin')

zinb <- pscl::zeroinfl(formula = y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                         taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                         f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami|
                         idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                         taxauto+fam2+fam3+i16+p6_2+p6_3+
                         p6_4+p7_2+p7_3+p7_4+ind_cami,data = database,link = 'logit',dist = 'negbin')

nb_dias <- MASS::glm(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                          taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                          f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7,data = database)

poisson <- glm(formula = y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                       taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                       f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami
                     ,data = database,family = "poisson")

summary(poisson)

linear <- glm(formula = y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                 taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                 f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami
               ,data = database,family = "gaussian")

linear_log <- glm(formula = ln_y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami
              ,data = database,family = "gaussian")

nb <- pscl::zeroinfl(formula = y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                         taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                         f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami
                     ,data = database,link = 'probit',dist = 'negbin')

summary(zinb_dias)
summary(nb_dias)

pscl::vuong(m1 = zinb_dias,m2 = nb_dias)

ggplot()+
  geom_density(mapping = aes(`resid(nb, type = "pearson")`),data = resid)

resid$`resid(nb, type = "pearson")`
resid <- as.data.frame(resid(nb,type = "pearson"))

summary(walk.zinb)
coefficients(walk.zinb)

walk.zinb <- glmmTMB(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                       taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                       f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7,
                     zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                       taxauto+fam2+fam3+i16+p6_2+p6_3+
                       p6_4+p7_2+p7_3+p7_4,data = database,family = 'nbinom2')

summary(walk.zinb)

walk.zinb <- glmmTMB(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                            taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                            f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami,
                            zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                            taxauto+fam2+fam3+i16+p6_2+p6_3+
                            p6_4+p7_2+p7_3+p7_4+ind_cami,data = database,family = 'nbinom2')

summary(walk.zinb)
walk.zinb$sdr
3.7917754914/0.3018590039

coef(walk.zinb)
confint(walk.zinb)
logLik(walk.zinb)
AIC(walk.zinb)
2*(-as.numeric(logLik(walk.zinb)))+2*48

write_rds(walk.zinb,'walk.zinb.rds')


#MUZIBN
walk.mzinb_e <- glmmTMB(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                          taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                          f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato),
                        zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                          taxauto+fam2+fam3+i16+p6_2+p6_3+
                          p6_4+p7_2+p7_3+p7_4+ind_cami,data = database,family = 'nbinom2')

summary(walk.mzinb_e)
write_rds(walk.mzinb_e,'walk.mzinb_e.rds')
confint(walk.mzinb_e,level = 0.95)
walk.mzinb_e$sdr

walk.mzinb_e <- glmmTMB(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                            taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                            f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato),
                            zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                            taxauto+fam2+fam3+i16+p6_2+p6_3+
                            p6_4+p7_2+p7_3+p7_4+ind_cami,data = database,family = 'nbinom2')

summary(walk.mzinb_e)
write_rds(walk.mzinb_e,'walk.mzinb_e.rds')
confint(walk.mzinb_e,level = 0.95)
walk.mzinb_e$sdr

walk.mzinb_e1 <- glmmTMB(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato),
                            zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                            taxauto+fam2+fam3+i16+p6_2+p6_3+
                            p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e1)
write_rds(walk.mzinb_e1,'walk.mzinb_e1.rds')
coef(walk.mzinb_e1)
confint(walk.mzinb_e1,level = 0.95)

pchisq(2*(as.numeric(logLik(walk.mzinb_e1))-as.numeric(logLik(walk.mzinb_e))),df = 1,lower.tail = FALSE)

walk.mzinb_e1 <- glmmTMB(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato),
                         zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e1)
write_rds(walk.mzinb_e1,'walk.mzinb_e1.rds')
coef(walk.mzinb_e1)
confint(walk.mzinb_e1,level = 0.95)

pchisq(2*(as.numeric(logLik(walk.mzinb_e1))-as.numeric(logLik(walk.mzinb_e))),df = 1,lower.tail = FALSE)


walk.mzinb_e2 <- glmmTMB(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato),
                         zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e2)

walk.mzinb_e2 <- glmmTMB(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato),
                           zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e2)
write_rds(walk.mzinb_e2,'walk.mzinb_e2.rds')
coef(walk.mzinb_e2)
confint(walk.mzinb_e2,level = 0.95)
logLik(walk.mzinb_e2)
AIC(walk.mzinb_e2)
walk.mzinb_e2$sdr
0.092002396/0.01988664 #t test for theta NB parameter
exp(-0.154256)-1

margins::margins(zinb)
a <- mixedup::extract_random_coefficients(model = walk.zinbm)

#marginal effect cond models
ggidade1 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('idade1'),type = 'random')
100*(diff(ggidade1$predicted)/ggidade1$predicted[1])

ggsexo <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('sexo'),type = 'random')
100*(diff(ggsexo$predicted)/ggsexo$predicted[1])

ggesc2 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('esc2'),type = 'fixed')
100*(diff(ggesc2$predicted)/ggesc2$predicted[1])

ggf3 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f3'),type = 'fixed')
100*(diff(ggf3$predicted)/ggf3$predicted[1])

ggf5_2 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f5_2'),type = 'fixed')
100*(diff(ggf5_2$predicted)/ggf5_2$predicted[1])

ggf5_3 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f5_3'),type = 'fixed')
100*(diff(ggf5_3$predicted)/ggf5_3$predicted[1])

ggf7 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f7'),type = 'fixed')
100*(diff(ggf7$predicted)/ggf7$predicted[1])

ggf9 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f9'),type = 'fixed')
100*(diff(ggf9$predicted)/ggf9$predicted[1])

ggfaz_curso <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('faz_curso'),type = 'fixed')
100*(diff(ggfaz_curso$predicted)/ggfaz_curso$predicted[1])

ggi1 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('i1'),type = 'fixed')
100*(diff(ggi1$predicted)/ggi1$predicted[1])

ggf16 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f16'),type = 'fixed')
100*(diff(ggf16$predicted)/ggf16$predicted[1])

ggf11_1 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f11_1'),type = 'fixed')
100*(diff(ggf11_1$predicted)/ggf11_1$predicted[1])

ggf11_2 <- ggeffects::ggpredict(walk.mzinb_e2,terms = c('f11_2'),type = 'fixed')
100*(diff(ggf11_2$predicted)/ggf11_2$predicted[1])


walk.mzinb_e3 <- glmmTMB(formula = l5~l6+idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_3+f11_2+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato),
                         zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato),
                         data = database,family = 'nbinom2')

summary(walk.mzinb_e3)

walk.mzinb_e3 <- glmmTMB(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami+(1+sexo||regiao:estrato),
                           zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1+sexo||regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e3)
write_rds(walk.mzinb_e3,'walk.mzinb_e3.rds')
coef(walk.mzinb_e1)
confint(walk.mzinb_e1,level = 0.95)

walk.mzinb_e4 <- glmmTMB(formula = l5 ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato)+(0+naobranco|regiao:estrato),
                         zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e4)

walk.mzinb_e4 <- glmmTMB(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami+(1+sexo+naobranco||regiao:estrato),
                           zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e4)
write_rds(walk.mzinb_e4,'walk.mzinb_e4.rds')
coef(walk.mzinb_e1)
confint(walk.mzinb_e1,level = 0.95)

walk.mzinb_e5 <- glmmTMB(formula = l5 ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                           f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+s1+s4+s7+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato)+(0+naobranco|regiao:estrato),
                         zi=~idade1+idade2+sexo+casado+esc1+esc2+naobranco+
                           taxauto+fam2+fam3+i16+p6_2+p6_3+
                           p6_4+p7_2+p7_3+p7_4+ind_cami+(1|regiao:estrato)+(0+sexo|regiao:estrato)+(0+naobranco|regiao:estrato),data = database,family = 'nbinom2')

summary(walk.mzinb_e4)


emm_basis.glmmTMB(object = walk.mzinb_e2,trms =walk.mzinb_e2,component = 'cond',data=database)

#TESTE DE RAZAO DE VEROSSIMILHANÇA PARA OS MDOELOS QUE ESTOU COMPARANDO
anova(walk.zinb0,walk.zinb,walk.mzinb_e,walk.mzinb_e1,walk.mzinb_e2)

anova(walk.zinb0,walk.zinb,walk.mzinb_e,walk.mzinb_e1,walk.mzinb_e2,walk.mzinb_e4,walk.mzinb_e5)
anova(walk.zinb,walk.mzinb_e2)

#packages to explore
ggeffects
mixedup
merTools
emmeans

data <- database %>% 
  group_by(regiao,estrato) %>% 
  summarise(coun <- n())

data

mean(data$`coun <- n()`)
min(data$`cont <- n()`)
max(data$`cont <- n()`)


nstall.packages("merTools")

citation("pscl")
AICcmodavg::aictab(cand.set = list(walk.zinb0,walk.zinb,walk.mzinb_e,walk.mzinb_e1,walk.mzinb_e2,walk.mzinb_e3),
                   modnames = c('Null','walk.zinb','walk.mzinb_e','walk.mzinb_e1','walk.mzinb_e2','walk.mzinb2_e3'))


walk.zinb <- readRDS('walk.zinb.rds')
walk.zinbm <- readRDS('walk.mzinb_e2.rds')


modelsummary::modelsummary(models = list(walk.zinb,walk.mzinb_e,walk.mzinb_e1,walk.mzinb_e2))

summary(walk.zinbm)
summary(walk.zinb)
walk.zinbm$sdr
0.092183935/0.01988213
logLik(walk.zinbm)
2*(-as.numeric(logLik(walk.zinbm)))+2*51

database <- database %>% 
  dplyr::select(estrato,regiao,y,idade1,idade2,sexo,casado,esc1,esc2,naobranco,
                  taxauto,fam2,fam3,i16,faz_curso,i1,f16,
                f3,f5_2,f5_3,f11_1,f11_2,f15_1,f15_2,f7,f9,s1,s4,s7,p6_2,p6_3,
                  p6_4,p7_2,p7_3,p7_4,ind_cami) %>% 
  mutate(fitted_zinbm = predict(object = walk.mzinb_e2, 
                                newdata = database,
                                type = "response"),
         fitted_zinb = predict(object = walk.zinb,
                               newdata = database,
                               type = "response"),
         fitted_nb = predict(object = walk.nb,
                               newdata = database,
                               type = "response")) 

  
ggplot(database) +
  geom_point(aes(x = y, y = y, color = "ZINB")) + 
  geom_smooth(aes(x = y, y = fitted_zinb, color = "ZINB")) + 
  geom_smooth(aes(x = y, y = fitted_zinbm, color = "ZINBM")) + 
  geom_smooth(aes(x = y, y = fitted_nb, color = "NB")) + 
  scale_color_manual("Estimation:", values = c("darkgreen", "orange",'red')) +
  labs(x = "Estimado",
       y = "Minutos de caminhada por semana observados") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


database$resid_zinb <- resid(object = walk.zinb$sdr,type = "pearson")
residuals.(object = walk.zinb,type = "pearson")

plotResiduals(simulationOutput = walk.mzinb_e2,smoothScatter = T)
plotResiduals(simulationOutput = walk.zinb,smoothScatter = T)

plotResiduals(simulationOutput = walk.mzinb_e2)

sim.resid <- simulateResiduals(fittedModel = walk.mzinb_e2,n = 10,refit = T,plot = T)

sim.resid <- simulateResiduals(fittedModel = walk.mzinb_e2,plot = T,n = 1000)
sim.resid <- simulateResiduals(fittedModel = walk.zinb,plot = T,n = 1000)
sim.resid <- simulateResiduals(fittedModel = walk.nb,plot = T,n = 1000)
sim.resid <- simulateResiduals(fittedModel = poisson,plot = T,n = 1000)
sim.resid <- simulateResiduals(fittedModel = linear,plot = T,n = 1000)
sim.resid <- simulateResiduals(fittedModel = linear_log,plot = T,n = 1000)

hist(sim.resid, breaks = seq(-0.02, 1.02, len = 53),
     col = c("red", rep("lightgrey", 50), "red"),
     main = "Hist of DHARMa residuals",
     xlab = "Residuals (outliers are marked red)", cex.main = 1, ...)

getResiduals(object = walk.mzinb_e2)

ggplot(database) +
  geom_density(aes(y, color = "Observado")) + 
  geom_density(aes(fitted_zinb, color = "ZINB")) + 
  geom_density(aes(fitted_zinbm, color = "ZINBM")) + 
  geom_density(aes(fitted_nb, color = "NB")) + 
  scale_color_manual("Estimation:", values = c("darkgreen", "orange",'red','blue')) +
  labs(x = "Estimado",
       y = "Minutos de caminhada por semana observados") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

ggeffects::ggemmeans(model = walk.nb,terms = "sexo")
a <- ggeffects::ggemmeans(model = walk.zinb,terms = "sexo")

ggeffects::ggpredict(model = walk.zinbm,terms = "sexo",type="zero_inflated")
a <- ggeffects::ggpredict(model = walk.zinbm,terms = "sexo")
a$predicted
round(diff(a$predicted),digits = 6)
  
plot(a)
summary(walk.zinbm)

##########################################################################################################################

library(extrafont)
font_import()
loadfonts(device = "win")
library(showtext)

font_add_google('Montserrat')
font.files()

a <- database %>%
  select(y) %>% 
  filter(y == 0) 

a <- ggplot()+
  geom_histogram(mapping = aes(x = y),fill='#0c45a6',data = database)+
  labs(title = "Quantidade de minutos de caminhada semanal", y="Frequência",x='Minutos por semana')+
  theme_bw()+
  theme(text = element_text(size = 12, family = "Times New Roman",colour = '#16214b'),axis.text =  element_text(colour = '#16214b'),
        panel.grid = element_blank())

b <- ggplot()+
  geom_density(mapping = aes(x = y),fill='#0c45a6',data = database)+
  labs(title = "Tempo de caminhada médio semanal", y="Densidade",x='Minutos por semana')+
  theme_bw()+
  theme(text = element_text(size = 12, family = "Montserrat",colour = '#16214b'),axis.text =  element_text(colour = '#16214b'),
        panel.grid = element_blank())

a
b

c <- gridExtra::grid.arrange(a,b)
c

d <- ggplot()+
  geom_histogram(mapping = aes(x = l5),fill='#0c45a6',data = database)+
  labs(title = "Número de dias de caminhada", y="Frequência",x='Quantidade de dias')+
  theme_bw()+
  theme(text = element_text(size = 12, family = "Montserrat",colour = '#16214b'),axis.text =  element_text(colour = '#16214b'),
        panel.grid = element_blank())

e <- ggplot()+
  geom_density(mapping = aes(x = l5),fill='#0c45a6',data = database)+
  labs(title = "Número de dias de caminhada", y="Densidade",x='Quantidade de dias')+
  theme_bw()+
  theme(text = element_text(size = 12, family = "Montserrat",colour = '#16214b'),axis.text =  element_text(colour = '#16214b'),
        panel.grid = element_blank())

f <- gridExtra::grid.arrange(d,e)

g <- ggplot()+
  geom_histogram(mapping = aes(x = l6),fill='#0c45a6',data = database)+
  labs(title = "Quantidade de minutos de caminhada diário (minutos)", y="Frequência",x='Minutos por dia')+
  theme_bw()+
  theme(text = element_text(size = 12, family = "Montserrat",colour = '#16214b'),axis.text =  element_text(colour = '#16214b'),
        panel.grid = element_blank())

h <- ggplot()+
  geom_density(mapping = aes(x = l6),fill='#0c45a6',data = database)+
  labs(title = "Tempo médio de caminhada diário (minutos)", y="Densidade",x='Minutos por dia')+
  theme_bw()+
  theme(text = element_text(size = 12, family = "Montserrat",colour = '#16214b'),axis.text =  element_text(colour = '#16214b'),
        panel.grid = element_blank())

i <- gridExtra::grid.arrange(g,h)

j <- gridExtra::grid.arrange(d,g,a)

ggsave(filename = 'HISTOGRAMA.svg',plot = a,dpi = 2000,width = 15,height = 15,units = "cm")

library(viridis)
library(hrbrthemes)

ggplot()+
  geom_boxplot(mapping = aes(x=as.factor(sexo),y=y),data = database)

plot <- database %>%
  ggplot( aes(x=nest, y=y, fill=as.factor(regiao))) +
  geom_jitter(color="gray", size=0.4, alpha=0.9) +
  geom_boxplot(outlier.colour = NA) +
  stat_summary(fun = mean,mapping = aes(),color="red",geom = 'pointrange',size=0.1)+
  theme_light() +  
  scale_x_discrete(labels=c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4))+
  scale_fill_viridis(discrete = TRUE, alpha=0.8,name="Região",labels=c('Norte','Nordeste','Sudeste','Centro-Oeste','Sul')) +
  theme(text=element_text(family='Times New Roman'),
    legend.position='right',
    plot.title = element_text(size=11),axis.title.x = element_text(hjust = 0.5,size = 12),
    axis.title.y = element_text(hjust = 0.5,size = 12)
  ) +
  xlab('Estratos de Regiões')+
  ylab('Quantidade de minutos de caminhada semanal')

plot
svg()
windowsFonts()

getwd()

ggsave(filename = 'boxplot.svg',plot = plot,dpi = 3500,width = 20,height = 20,units = "cm")
  
ggplot()+
  geom_histogram(mapping = aes(x = y),fill='#0c45a6',data = database)
  

#regiao
database %>%
  ggplot( aes(x=regiao, y=y, fill=as.factor(regiao))) +
  geom_boxplot(outlier.colour = 'gray') +
  theme_ipsum() +  
  theme(
    legend.position='bottom',
    plot.title = element_text(size=11),axis.title.x = element_text(hjust = 0.5,size = 12),
    axis.title.y = element_text(hjust = 0.5,size = 12)
  ) +
  ggtitle("Tempo de Caminhada Semanal") +
  xlab('Estratos de Regiões')+
  ylab('Tempo médio de caminhada semanal')

#estrato
database %>%
  ggplot( aes(x=estrato, y=y, fill=as.factor(estrato))) +
  geom_boxplot(outlier.colour = 'gray') +
  theme_ipsum() +  
  theme(
    legend.position='bottom',
    plot.title = element_text(size=11),axis.title.x = element_text(hjust = 0.5,size = 12),
    axis.title.y = element_text(hjust = 0.5,size = 12)
  ) +
  ggtitle("Tempo de Caminhada Semanal") +
  xlab('Estratos de Regiões')+
  ylab('Tempo médio de caminhada semanal')

ggplot()+ 
  geom_density(database,mapping = aes(y))+
  labs(title = "Quantidade de dias por semana", y="Frequ?ncia",x='dias')+
  theme_minimal()

quantile(database$y,probs = 0.75)

p3 <- ggplot()+
  geom_histogram(database,mapping = aes(idade),fill='red')+
  labs(title = "Idade", y="Frequ?ncia",x='anos')+
  theme_minimal()

p4 <- ggplot()+
  geom_histogram(database,mapping = aes(e9),fill='yellow')+
  labs(title = "Ra?a", y="Frequ?ncia",x='ra?a')+
  theme_minimal()

gridExtra::grid.arrange(p2,p1,p3,ncol=3,nrow=1)

database %>%
  ggplot(aes(x=as.factor(l5), y=l6, fill=as.factor(l5))) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

?stat_summary

#boxplot
ggplot(database, aes(x=as.factor(nest), y=y)) +
  geom_boxplot(alpha=0.7,outlier.colour = NA) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="blue", fill="blue") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot de qtd de minutos por dia x qtd vezes na semana") +
  xlab("")

ggplot(database, aes(x=as.factor(sexo), y=y, fill=as.factor(sexo))) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot de qtd de minutos por dia x qtd vezes na semana") +
  xlab("")

ggplot(database240, aes(x=as.factor(sexo), y=l6, fill=as.factor(sexo))) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Boxplot de qtd de minutos por dia x qtd vezes na semana") +
  xlab("")

###training
smp_size <- floor(0.75*nrow(database))

set.seed(1234)
train_ind <- sample(seq_len(nrow(database)),size = smp_size)

data_calib <- database[train_ind,]
data_test <- database[-train_ind,]


citation()



qqnorm(database$y, pch = 1, frame = FALSE)
qqline(database$y, col = "steelblue", lwd = 2)

library("car")
qqPlot(database$y)

database$ln_y <- log(database$y+1)

qqPlot(database$ln_y)

################################## nao vai sar s

setwd("C:\\Users\\calde\\Google Drive\\Projetos\\Idosos\\Elsi\\resultados")
modelsummary::modelsummary(list(walk.zinb_at,walk.zinb_viz,walk.zinb_sn,walk.zinb_com),stars = TRUE)
modelsummary::modelsummary(list(walk.mzinb_at,walk.mzinb_viz,walk.mzinb_sn,walk.mzinb_com),stars = TRUE)


brazil <- geobr::read_country()
cities <- geobr::read_municipality(code_muni = 'all',year = 2010)
regions <- geobr::read_region()

ggplot()+
  geom_sf(mapping = aes(),color="White",fill="lightblue",data = regions)+
  theme_void()

###################### PAPER ANPET ###########################
 
###################### Binomial negativo ##############################3
setwd("C:\\Users\\calde\\Google Drive\\Projetos\\Idosos\\Elsi\\resultados")
  
  #vizinhan?a
  walk.nb_viz <- zeroinfl(formula = y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+log(rendadompc)+
                            taxauto+fam2+fam3+i16+f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+f16 | 0,data = database)
  
  write_rds(walk.nb_viz,'walk.nb_viz.rds')
  
  a <- read_rds('walk.nb_viz.rds')
  summary(a)
  #social networks
  walk.nb_sn <- MASS::glm.nb(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                               taxauto+fam2+fam3+i16+s1+s4+s7|
                               idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                               taxauto+fam2+fam3+i16+p6_2+p6_3+
                               p6_4+p7_2+p7_3+p7_4,data = database)
  
  #activity
  walk.nb_at <- MASS::glm.nb(formula = y ~idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                               taxauto+fam2+fam3+i16+faz_curso+i1 |
                               idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                               taxauto+fam2+fam3+i16+p6_2+p6_3+
                               p6_4+p7_2+p7_3+p7_4,data = database)
  
  #complete
  walk.nb_com <- MASS::glm.nb(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                                taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                                f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+ s1+s4+s7|
                                idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                                taxauto+fam2+fam3+i16+p6_2+p6_3+
                                p6_4+p7_2+p7_3+p7_4,data = database)
  
  ##################### Binomial Negativo inflacionado de zero ###################3
  
  #vizinhan?a
  walk.zinb_viz1 <- zeroinfl(formula = y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+log(rendadompc)+
                               taxauto+fam2+fam3+i16+f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+f16 |
                               idade1+idade2+sexo+casado+esc1+esc2+naobranco+log(rendadompc)+
                               taxauto+fam2+fam3+i16+p6_2+p6_3+
                               p6_4+p7_2+p7_3+p7_4,data = database,dist = 'negbin',link = 'logit',model = T)
  summary(walk.zinb_viz)
  walk.zinb_viz$coefficients
  walk.zinb_viz$loglik
  
  write_rds(walk.zinb_viz)
  #comparando modelo nulo
  m2 <- update(walk.zinb_viz, . ~ 1)
  pchisq(2 * (logLik(walk.zinb_viz) - logLik(m2)), df = 43, lower.tail=FALSE)
  
  pscl::pR2(walk.zinb_viz)
  
  ### GLMTMB
  walk.zinb_viz_glmmtmb <- glmmTMB(formula = y~idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                                     taxauto+fam2+fam3+i16+f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+f16,ziformula = ~
                                     idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                                     taxauto+fam2+fam3+i16+p6_2+p6_3+
                                     p6_4+p7_2+p7_3+p7_4,data = database,family = 'nbinom2')
  
  summary(walk.zinb_viz_glmmtmb)
  
  #social networks
  walk.zinb_sn <- zeroinfl(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                             taxauto+fam2+fam3+i16+s1+s4+s7|
                             idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                             taxauto+fam2+fam3+i16+p6_2+p6_3+
                             p6_4+p7_2+p7_3+p7_4,data = database,dist = 'negbin',link = 'probit')
  summary(walk.zinb_sn)
  walk.zinb_sn$loglik
  
  #comparando modelo nulo
  m3 <- update(walk.zinb_sn, . ~ 1)
  pchisq(2 * (logLik(walk.zinb_sn) - logLik(m3)), df = 36, lower.tail=FALSE)
  
  pscl::pR2(walk.zinb_sn)
  
  #activity
  walk.zinb_at <- zeroinfl(formula = y ~idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                             taxauto+fam2+fam3+i16+faz_curso+i1 |
                             idade1+idade2+sexo+casado+esc1+esc2+naobranco+rendadompc+
                             taxauto+fam2+fam3+i16+p6_2+p6_3+
                             p6_4+p7_2+p7_3+p7_4,data = database,dist = 'negbin',link = 'probit')
  summary(walk.zinb_at)
  walk.zinb_at$loglik
  
  #comparando modelo nulo
  m4 <- update(walk.zinb_at, . ~ 1)
  pchisq(2 * (logLik(walk.zinb_at) - logLik(m4)), df = 35, lower.tail=FALSE)
  
  pscl::pR2(walk.zinb_sn)
  
  #complete
  walk.zinb_com <- zeroinfl(formula = y ~ idade1+idade2+sexo+casado+esc1+esc2+naobranco+log(rendadompc)+
                              taxauto+fam2+fam3+i16+faz_curso+i1+f16+
                              f3+f5_2+f5_3+f11_1+f11_2+f15_1+f15_2+f7+f9+ s1+s4+s7|
                              idade1+idade2+sexo+casado+esc1+esc2+naobranco+log(rendadompc)+
                              taxauto+fam2+fam3+i16+p6_2+p6_3+
                              p6_4+p7_2+p7_3+p7_4,data = database,dist = 'negbin',link = 'probit')
  
  summary(walk.zinb_com)
  walk.zinb_com$coefficients
  walk.zinb_com$loglik
  
  #comparando modelo nulo
  m5 <- update(walk.zinb_com, . ~ 1)
  pchisq(2 * (logLik(walk.zinb_com) - logLik(m5)), df = 48, lower.tail=FALSE)
  
  pscl::pR2(walk.zinb_com)
  
  database$rendapc <- log(database$rendadompc)
  
  ################# TESTE DE VUONG #################3
  pscl::vuong(walk.zinb_viz,walk.nb_viz)
  pscl::vuong(walk.zinb_sn,walk.nb_sn)
  pscl::vuong(walk.zinb_at,walk.nb_at)
  pscl::vuong(walk.zinb_com,walk.nb_com)

  
  TMB:::as.list(sdreport(walk.mzinb_e9$obj))
  TMB::summary.sdreport(object = walk.mzinb_e9$sdr,select = 'fixed',p.value = TRUE)
  
  TMB::summary.sdreport(walk.mzinb_e9$sdr)
  TMB::print.sdreport(x = walk.mzinb_e$sdr)
  
  