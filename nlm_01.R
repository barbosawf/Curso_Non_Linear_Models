##### PACOTES #####

library(tidyverse)
library(nlme)
library(Metrics)

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
unique(Orange$Tree) # 5 árvores no total

Orange %>%
  as_tibble() |>
  mutate(Tree = factor(Tree, levels = c(1:5))) %>% 
  arrange(Tree) %>% 
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 3) + facet_wrap(~Tree) +
  labs(x = 'Age', y = 'Circumference')

#### 1.5. Outra opção ####

plot(Orange) # classe 'groupedData'

#### 1.6. groupedData x data.frame ####

Orange_df = as.data.frame(Orange)
plot(Orange_df)

#### 1.7. Transformando data.frame em groupedData ####

head(Orange_df)
head(Orange)

# obj_agrupado = groupedData(y ~ x, data = data.frame)
Orange_gd = groupedData(circumference ~ age | Tree, data = Orange_df)

plot(Orange_df)
plot(Orange_gd)

##### Prática 2: Ajustando Modelos não lineares no R #####

#### 2.1. Modelo Logístico (Self-Starting)  ####

df_p1

m1_p1 = nls(circumference ~ SSlogis(age, Asym, xmid, scal),
            data = df_p1)
summary(m1_p1)
# Dica: verificar ?selfStart
?selfStart

#### 2.2. Modelo Logístico (função própria) ####

logistic = function(age, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (age - xmid)))
}

m2_p1 = nls(circumference ~ logistic(age, Asym, xmid, scal),
            start = list(Asym = 150, xmid = 600, scal = .001),
            data = df_p1)
m2_p1

#### 2.3. Como identificar bons chutes iniciais (1) ####

df_p1 %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 5) + geom_line() +
  labs(x = 'Age', y = 'Circumference') +
  theme_classic()

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
  geom_point(aes(x = age, y = circumference), size = 3) +
  geom_line(aes(x = age, y = aj), linewidth = 1, color = 'blue')

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
  ggplot(aes(x = Amount, y = Gain)) + geom_point(size = 5, alpha = .5) +
  labs(x = 'Ganho de peso (g)', y = 'Percentual da dieta')

#### 3.1.3. Criando função ####
func = function(Amount, beta1, beta2, beta3){
  beta1 + beta2 * (1 - exp(- beta3 * Amount))
}

#### 3.1.4. Ajustando modelos com chutes específicos ####

mod3 = nls(Gain ~ func(Amount, beta1, beta2, beta3), data = turkey,
           start = c(beta1 = 620, beta2 = 180, beta3 = 6.38),
           trace = TRUE)
mod3

#### 3.1.5. Gráfico de valores ajustados ####

aj = fitted(mod3)
turkey %>% mutate(aj = aj) %>%
  ggplot() +
  geom_point(aes(x = Amount, y = Gain), size = 4) +
  geom_line(aes(x = Amount, y = aj), linewidth = 2, color = 'blue')

##### Prática 4: Ajustando novos modelos #####

##### 4.1. Gompertz #####

gompertz = function(dap, phi1, phi2, phi3){
  phi1 * exp(- exp(- phi3 * (dap - phi2)))
}

# 150 600 0.001
m1_p1 = nls(circumference ~ gompertz(age, phi1, phi2, phi3),
            start = list(phi1 = 154, phi2 = 627, phi3 = .002),
            data = df_p1)
m1_p1
summary(m1_p1)

##### 4.2. von Bertalanffy #####

vonb = function(dap, phi1, phi2, phi3){
  phi1 * ((1 - phi2 * exp((- phi3) * dap)) ^ 3)
}

m3_p1 = nls(circumference ~ vonb(age, phi1, phi2, phi3),
            start = list(phi1 = 150, phi2 = 800, phi3 = .001),
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