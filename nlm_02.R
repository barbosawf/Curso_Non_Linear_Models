##### PACOTES #####

library(tidyverse)
library(nlme)
library(Metrics)
library(nlstools)

##### Prática 1: Visualização de dados #####

#### 1.1. Carregando base de dados ####

head(Orange)
View(Orange)
?Orange

#### 1.2. Análise da primeira árvore ####

#### 1.2.1. Selecionando apenas a primeira árvore ####

df_p1 = Orange %>% filter(Tree == 1)
df_p1

#### 1.2.2. Gráfico para a primeira árvore ####

df_p1 %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 5) +
  labs(x = 'Age', y = 'Circumference')

#### 1.3. Análise da segunda árvore ####

#### 1.3.1. Selecionando a segunda árvore  ####

df_p2 = Orange %>% filter(Tree == 2)
df_p2

#### 1.3.2. Gráfico para a segunda árvore ####

df_p2 %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 5) +
  labs(x = 'Age', y = 'Circumference')

#### 1.4. Gráfico para todas as árvores simultaneamente ####

head(Orange)
unique(Orange$Tree)
length(unique(Orange$Tree)) # 5 árvores no total

Orange %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 3) + facet_wrap(~Tree) +
  labs(x = 'Age', y = 'Circumference')

#### 1.5. Outra opção ####

plot(Orange) # classe 'groupedData'
class(Orange)

#### 1.6. groupedData x data.frame ####

Orange_df = as.data.frame(Orange)
class(Orange_df)
plot(Orange_df)

#### 1.7. Transformando data.frame em groupedData ####

head(Orange_df)
head(Orange)

# obj_agrupado = groupedData(y ~ x | ID, data = data.frame)
Orange_gd = groupedData(circumference ~ age | Tree, data = Orange_df)
class(Orange_gd)
plot(Orange_df)
plot(Orange_gd)

##### Prática 2: Ajustando Modelos não lineares no R #####

#### 2.1. Modelo Logístico (Self-Starting)  ####

df_p1

m1_p1 = nls(circumference ~ SSlogis(age, Asym, xmid, scal),
            data = df_p1, trace = T)
summary(m1_p1)
# Dica: verificar ?selfStart
?selfStart

#### 2.2. Modelo Logístico (função própria) ####

logistic = function(age, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (age - xmid)))
}

m2_p1 = nls(circumference ~ logistic(age, Asym, xmid, scal),
            start = list(Asym = 150, xmid = 600, scal = .005),
            data = df_p1, trace = T)
m2_p1 
m2_p1 |> broom::tidy() 
m2_p1 |> broom::glance()
summary(m2_p1)
#### 2.3. Como identificar bons chutes iniciais (1) ####

df_p1

df_p1 %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 5) + geom_line() +
  labs(x = 'Age', y = 'Circumference') +
  theme_minimal()

#### 2.4. Acessando os valores ajustados ####

#### 2.4.1. Acessando valores ajustados ####

aj = fitted(m1_p1)

#### 2.4.2. Adicionando ao data frame ####

df_p1 = df_p1 %>%
  mutate(aj = round(aj, 2))
head(df_p1)

#### 2.4.3. Plotando valores ajustados ####

df_p1 %>%
  ggplot() +
  geom_point(aes(x = age, y = circumference), size = 4) +
  geom_line(aes(x = age, y = aj), linewidth = 1, color = 'blue') +
  theme_minimal() +
  labs(x = 'Age', y = 'Circumference')

df_p1 %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 4) +
  geom_smooth(
    method = "nls",
    formula = y ~ Asym / (1 + exp(-scal * (x - xmid))),
    se = F,
    method.args = list(start = list(Asym = 150, xmid = 600, scal = .005)))


#### 2.4.4. Predizendo novos valores ####

dados = data.frame(age = 1200)
predict(m1_p1, dados)

dados2 = data.frame(age = c(400, 587, 1471))
predict(m1_p1, dados2)

##### Prática 3: Chutes iniciais (2) #####

#### 3.1. Ganho de peso de aves ####

#### 3.1.1 Base de dados ####

Amount = c(rep(0, 10), rep(c(0.04, .1, .16, .28, .44), each = 5))
Gain = c(644, 631, 661, 624, 633, 610, 615, 605, 608, 599,
         698, 667, 657, 685, 635, 730, 715, 717, 709, 707,
         735, 712, 726, 760, 727, 809, 796, 769, 791, 811,
         767, 771, 799, 799, 791)

turkey = data.frame(Amount, Gain)
head(turkey)

#### 3.1.2. Gráfico ####

turkey %>%
  ggplot(aes(x = Amount, y = Gain)) + 
  geom_point(size = 5, alpha = .5) +
  labs(x = 'Ganho de peso (g)', y = 'Percentual da dieta')

#### 3.1.3. Criando função ####

func = function(Amount, theta1, theta2, theta3){
  theta1 + theta2 * (1 - exp(- theta3 * Amount))
}

#### 3.1.4. Ajustando modelos com chutes específicos ####

mod3 = nls(
  Gain ~ func(Amount, theta1, theta2, theta3),
  data = turkey,
  start = c(
    theta1 = 620,
    theta2 = 180,
    theta3 = 6.38
  ),
  trace = TRUE
)

mod3
mod3 |> broom::glance()
mod3 |> broom::tidy() |> as.data.frame()
summary(mod3)



#### 3.1.5. Gráfico de valores ajustados ####

aj = fitted(mod3)
turkey %>% mutate(aj = aj) %>%
  ggplot() +
  geom_point(aes(x = Amount, y = Gain), size = 4) +
  geom_line(aes(x = Amount, y = aj),
            linewidth = 2,
            color = 'blue')


turkey %>%
  ggplot(aes(x = Amount, y = Gain)) +
  geom_point(size = 4, alpha = .5) +
  stat_function(
    fun = \(x) 622.905123 + 178.813237 * (1 - exp(-7.114521 * x)),
    color = 'blue',
    linewidth = 1
  ) +
  stat_function(
    fun = \(x) 620.905123 + 176.813237 * (1 - exp(-7.114521 * x)),
    color = 'red',
    linewidth = 1
  ) +
  theme_minimal()

# https://rpubs.com/kaz_yos/ggplot2-stat-function
# https://www.tutorialspoint.com/how-to-plot-a-function-with-ggplot2-in-r
# https://r-charts.com/evolution/draw-functions-ggplot2/
# https://r-charts.com/evolution/curve/
# https://r-charts.com/evolution/newggslopegraph/
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://r-charts.com/ggplot2/
turkey %>%
  ggplot(aes(x = Amount, y = Gain)) +
  geom_point(size = 4, alpha = .5) +
  geom_smooth(
    method = "nls",
    formula = y ~ t1 + t2 * (1 - exp(-t3 * x)),
    se = F,
    method.args = list(start =
                         list(
                           t1 = 620, t2 = 180, t3 = 6.38
                         )
    )
  ) +
  theme_minimal()

##### Prática 4: Ajustando novos modelos #####

##### 4.1. Gompertz #####

gompertz = function(age, theta1, theta2, theta3) {
  theta1 * exp(-exp(-theta3 * (age - theta2)))
}

# 150 600 0.001
m1_p1 = nls(circumference ~ gompertz(age, theta1, theta2, theta3),
            start = list(theta1 = 154, theta2 = 627, theta3 = .002),
            data = df_p1)

m1_p1
m1_p1 |> broom::glance()
m1_p1 |> broom::tidy() 
summary(m1_p1)

##### 4.2. von Bertalanffy #####

vonb = function(age, theta1, theta2, theta3){
  theta1 * ((1 - theta2 * exp((- theta3) * age)) ^ 3)
}

m3_p1 = nls(circumference ~ vonb(age, theta1, theta2, theta3),
            start = list(theta1 = 150, theta2 = 800, theta3 = .001),
            data = df_p1)
m3_p1
summary(m3_p1)

#### Prática 5: Comparando os três modelos ####

#### 5.1. AIC ####
AIC(m1_p1) # Gompertz
AIC(m2_p1) # Logístico
AIC(m3_p1) # von Bertalanffy

#### 5.2. BIC ####
BIC(m1_p1) # Gompertz
BIC(m2_p1) # Logístico
BIC(m3_p1) # von Bertalanffy

#### 5.3. EQM (MSE) ####
mse(df_p1$circumference, fitted(m1_p1)) # Gompertz
mse(df_p1$circumference, fitted(m2_p1)) # Logístico
mse(df_p1$circumference, fitted(m3_p1)) # von Bertalanffy

#### 5.4. EAM (MAE) ####
mae(df_p1$circumference, fitted(m1_p1)) # Gompertz
mae(df_p1$circumference, fitted(m2_p1)) # Logístico
mae(df_p1$circumference, fitted(m3_p1)) # von Bertalanffy

# Pacote Metrics: https://cran.r-project.org/web/packages/Metrics/Metrics.pdf

##### PRÁTICA 6: Intervalos de confiança (IC) #####
library(nlstools)
#### 6.1. Método de Wald ####

confint.default(m2_p1)

#### 6.2. Verossimilhança ####

confint2(m2_p1)

###### PRÁTICA 7: Ajustando vários elementos simultaneamente #####

#### 7.1. Base de dados Orange ####

logistic = function(age, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (age - xmid)))
}

#### 7.1.1. Ajuste simultâneo de todos os pontos ####

orange_log1 = nls(circumference ~ logistic(age, Asym, xmid, scal),
                     start = list(Asym = 170, xmid = 600, scal = .004),
                     data = Orange)
orange_log1        # detalhes do modelo
AIC(orange_log1)   # AIC
BIC(orange_log1)   # BIC
mse(Orange$circumference, fitted(orange_log1)) # EQM
mae(Orange$circumference, fitted(orange_log1)) # EAM

aj = fitted(orange_log1)
orange_aj = Orange %>% mutate(aj = aj)

orange_aj %>%
  ggplot() +
  geom_point(aes(x = age, y = circumference, color = Tree), size = 5) +
  geom_line(aes(x = age, y = aj), linewidth = 2) +
  geom_line(aes(x = age, y = circumference, group = Tree, color = Tree), linewidth = 1) +
  labs(x = 'Age', y = 'Circumference') + theme_classic()

#### 7.1.2. Um ajuste por elemento amostral ####

orange_log2 = nlsList(circumference ~ logistic(age, Asym, xmid, scal),
                 start = list(Asym = 170, xmid = 600, scal = .004),
                 data = Orange)
# Resultados do modelo
orange_log2
class(orange_log2)
orange_log2[1]     # detalhes do primeiro modelo
AIC(orange_log2)   # tentativa de extrair o AIC

#### 7.1.3. Extra: AIC e BIC individuais ####

analise_nlsList = function(nlslista){
  ID = names(nlslista)
  aic_vetor = c()
  bic_vetor = c()
  
  for (i in 1:length(nlslista)){
    aic_vetor = c(aic_vetor, AIC(nlslista[[i]]))
    bic_vetor = c(bic_vetor, BIC(nlslista[[i]]))
  }
  
  resumo = data.frame(ID = ID, AIC = aic_vetor, BIC = bic_vetor)
  return(resumo)
}

analise_nlsList(orange_log2)

#### 7.1.4. Gráfico ####

aj = fitted(orange_log2)
orange_aj2 = Orange %>% mutate(aj = aj)

orange_aj2 %>%
ggplot() +
  geom_point(aes(x = age, y = circumference), size = 3) +
  geom_line(aes(x = age, y = aj), linewidth = 1, color = 'blue') +
  facet_wrap(~Tree) +
  labs(x = 'Age', y = 'Circumference') + theme_classic()

#### 7.2. Base de dados Soybean ####

plot(Soybean)

# theta1 = 25, theta2 = 40

#### 7.2.1. Gráfico ####

plot(Soybean)

#### 7.2.2. Modelo para todos os indivíduos ####

soy_nls = nls(weight ~ logistic(Time, Asym, xmid, scal),
                  start = list(Asym = 25, xmid = 60, scal = .103),
                  data = Soybean)

#### 7.2.3. Modelo individual

soylist = nlsList(weight ~ logistic(Time, Asym, xmid, scal),
                  start = list(Asym = 25, xmid = 60, scal = .103),
                  data = Soybean)

Soybean %>%
  group_by(Time) %>% summarise(mean(weight))