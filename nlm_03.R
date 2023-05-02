##### PACOTES #####

library(tidyverse)
library(nlme)
library(Metrics)
library(nlstools)


# Links úteis -------------------------------------------------------------

# https://rpubs.com/kaz_yos/ggplot2-stat-function
# https://www.tutorialspoint.com/how-to-plot-a-function-with-ggplot2-in-r
# https://r-charts.com/evolution/draw-functions-ggplot2/
# https://r-charts.com/evolution/curve/
# https://r-charts.com/evolution/newggslopegraph/
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://r-charts.com/ggplot2/
# https://tibble.tidyverse.org/articles/numbers.html

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
            data = df_p1)
summary(m1_p1)
# Dica: verificar ?selfStart
?selfStart

#### 2.2. Modelo Logístico (função própria) ####

logistic = function(age, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (age - xmid)))
}

m2_p1 = nls(circumference ~ logistic(age, Asym, xmid, scal),
            start = list(Asym = 150, xmid = 600, scal = .005),
            data = df_p1)
m2_p1

#### 2.3. Como identificar bons chutes iniciais (1) ####

df_p1

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
  geom_point(aes(x = age, y = circumference), size = 4) +
  geom_line(aes(x = age, y = aj), linewidth = 1, color = 'blue') +
  theme_classic() +
  labs(x = 'Age', y = 'Circumference')


df_p1 %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 4) +
  geom_smooth(
    method = "nls",
    formula = y ~ Asym / (1 + exp(-scal * (x - xmid))),
    se = F,
    method.args = list(start = list(Asym = 150, xmid = 600, scal = .005))) +
  labs(x = 'Age', y = 'Circumference')


df_p1 %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(size = 4) +
  stat_function(
    fun = \(x) 154.1631 / (1 + exp(- 0.002758071 * (x - 627.1938))),
    color = 'blue',
    linewidth = 1
  ) +
  labs(x = 'Age', y = 'Circumference') +
  theme_minimal()


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

func = function(x, t1, t2, t3){
  t1 + t2 * (1 - exp(- t3 * x))
}

#### 3.1.4. Ajustando modelos com chutes específicos ####

mod3 = nls(Gain ~ func(Amount, t1, t2, t3), data = turkey,
           start = c(t1 = 620, t2 = 180, t3 = 6.38),
           trace = TRUE)
mod3

#### 3.1.5. Gráfico de valores ajustados ####

aj = fitted(mod3)
turkey %>% mutate(aj = aj) %>%
  ggplot() +
  geom_point(aes(x = Amount, y = Gain), size = 4) +
  geom_line(aes(x = Amount, y = aj), linewidth = 2, color = 'blue')


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

gompertz = function(x, t1, t2, t3){
  t1 * exp(- exp(- t3 * (x - t2)))
}

# 150 600 0.001
m1_p1 = nls(circumference ~ gompertz(age, t1, t2, t3),
            start = list(t1 = 154, t2 = 627, t3 = .002),
            data = df_p1)

m1_p1
m1_p1 |> broom::glance()
m1_p1 |> broom::tidy() 
summary(m1_p1)

##### 4.2. von Bertalanffy #####

vonb = function(x, t1, t2, t3){
  t1 * ((1 - t2 * exp((- t3) * x)) ^ 3)
}

m3_p1 = nls(circumference ~ vonb(age, t1, t2, t3),
            start = list(t1 = 150, t2 = 800, t3 = .001),
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

logistic = function(x, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (x - xmid)))
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
  geom_line(aes(x = age, y = aj), linewidth = 1) +
  geom_line(aes(
    x = age,
    y = circumference,
    group = Tree,
    color = Tree
  ),
  linewidth = 1) +
  labs(x = 'Age', y = 'Circumference') + theme_minimal()

#### 7.1.2. Um ajuste por elemento amostral ####

orange_log2 = nlsList(
  circumference ~ logistic(age, Asym, xmid, scal),
  start = list(Asym = 170, xmid = 600, scal = .004),
  data = Orange
)

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

orange_log2 |>
  map_dfr(\(x) tibble(AIC = AIC(x), BIC = BIC(x)), .id = 'ID') 

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

logistic = function(x, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (x - xmid)))
}

#### 7.2.1. Gráfico ####

head(Soybean)
Soybean %>%
  ggplot(aes(x = Time, y = weight)) +
  geom_point() + geom_line() + facet_wrap(~ Plot)

plot(Soybean)

#### 7.2.2. Modelo para todos os indivíduos ####

soy_nls = nls(weight ~ logistic(Time, Asym, xmid, scal),
                  start = list(Asym = 25, xmid = 60, scal = .103),
                  data = Soybean)
soy_nls

#### 7.2.3. Modelo individual

# Asym = 25, xmid = 60, scal = .103
soylist = nlsList(weight ~ logistic(Time, Asym, xmid, scal),
                  start = list(Asym = 18.4182, xmid = 53.9562, scal = 0.1226),
                  data = Soybean)

soylist$`1988F4` |> AIC()

#### 7.2.4. Observando elementos onde não houve convergência ####

Soybean2 = Soybean %>% filter(Plot %in% c('1989P5', '1989P8'))

plot(Soybean2)

soylist |>
  map_dfr(\(x) if (!is.null(x))
    tibble(AIC = AIC(x),
           BIC = BIC(x)), .id = 'ID') 

soylist |>
  map_dfr(\(x) if (!is.null(x))
    tibble(
      AIC = num(AIC(x), digits = 4),
      BIC = num(BIC(x), digits = 4)
    ), .id = 'ID')

#### 7.2.4. Valores ajustados ####

library(tidyverse)
conv_fit = na.omit(fitted(soylist))
soybean_conv = Soybean %>% filter(! Plot %in% c('1989P5', '1989P8'))

soybean_conv = soybean_conv %>%
  mutate(conv_fit = conv_fit)

soybean_conv %>%
  ggplot() +
  geom_point(aes(x = Time, y = weight)) +
  geom_line(aes(x = Time, y = conv_fit), color = 'blue') +
  facet_wrap(~ Plot)

##### PRÁTICA 8: Matéria seca de alho ####

acesso = c(rep('ID01', 4), rep('ID02', 4), rep('ID03', 4), rep('ID04', 4))
dap = rep(c(60, 90, 120, 150), 4)
mstp = c(1.7, 6.5, 28.7, 36.5, 1.3, 4.3, 19.5, 23.7,
         .8, 4.6, 14.2, 18.8, 1.1, 3.4, 15.1, 17.2)

dados = data.frame(acesso, dap, mstp)
dados_ag = groupedData(mstp ~ dap | acesso)

dados_ag %>%
  ggplot(aes(x = dap, y = mstp)) + geom_point() + geom_line() +
  facet_wrap(~ acesso)

logistic = function(age, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (age - xmid)))
}

head(dados_ag)

#### 8.1. Modelo geral ####

#mod8 = nls(formula,
#           data = dados_ag,
#           start = )
#mod8

#### 8.2. Modelo individual ####

#mod8_lista = nlsList(formula,
#                     data = dados_ag,
#                     start = )

##### PRÁTICA 9: Curvas de Lactação #####

##### 9.1. Leitura da base de dados #####

ID = c(rep('ID001', 10), rep('ID002', 7), rep('ID003', 9))

dia = c(33, 59, 94, 120, 156, 180, 207, 240, 269, 304,
        15, 56, 99, 144, 189, 234, 268,
        9, 42, 72, 104, 135, 163, 195, 224, 254)

mkg = c(40.4, 43.4, 31, 34.4, 32.7, 29.4, 30.4, 23.4, 23, 16.6,
        18, 22.5, 19, 16.2, 13.5, 11.5, 9.5,
        12.3, 11.4, 12.5, 10.1, 11.1, 9.2, 7.6, 6.4, 6.1)

lact2 = data.frame(ID = ID, dia = dia, mkg = mkg)
head(lact2)
lact2 = groupedData(mkg ~ dia | ID, data = lact2)
head(lact2)

##### 9.2. Análise exploratória #####

plot(lact2)
lact2 |> ggplot(aes(x = dia, y = mkg, color = ID)) + geom_point(size = 4) +
  geom_line() + facet_wrap(~ ID)

##### 9.3. Ajuste do modelo de Wood #####

Wood = function(dia, a, b, c){ 
  a * dia ^ b * exp(- c * dia)
}

head(lact2)

mnl_wood = nlsList(mkg ~ Wood(dia, a, b, c), data = lact2,
                   start = list(a = 22, b = .15, c = .001)) 
summary(mnl_wood)
plot(intervals(mnl_wood)) # Intervalos de confiança

##### 9.4. Ajuste do modelo de Nelder #####

Nelder = function(dia, a, b, c){ 
  dia / (a + b * dia + c * dia ^ 2)
}

mnl_nelder = nlsList(mkg ~ Nelder(dia, a, b, c), data = lact2,
                     start = list(a = 22, b = .15, c = .001))
summary(mnl_nelder)
plot(intervals(mnl_nelder))

#### 9.5. Comparando modelos ####

#### AIC e BIC ####

# Individualmente
AIC(mnl_wood[[1]])
# ...
BIC(mnl_wood[[1]])

# Usando função criada

analise_nlsList(mnl_wood)
analise_nlsList(mnl_nelder)

#### EQM (MSE) ####
mse(lact2$mkg, fitted(mnl_wood))
mse(lact2$mkg, fitted(mnl_nelder))

#### (MAE) ####
mae(lact2$mkg, fitted(mnl_wood))
mae(lact2$mkg, fitted(mnl_nelder))

#### 9.6. Curvas para os modelos ####

Wood = fitted(mnl_wood)
Nelder = fitted(mnl_nelder)

lact2 = lact2 %>%
  mutate(Wood = Wood, Nelder = Nelder)

View(lact2)

lact2 %>%
  ggplot() +
  geom_point(aes(x = dia, y = mkg), size = 3) +
  geom_line(aes(x = dia, y = Wood), color = 'blue', linewidth = 1) +
  geom_line(aes(x = dia, y = Nelder), color = 'green', linewidth = 1) +
  facet_wrap(~ ID)

##### PRÁTICA 10: ChickWeight #####

plot(ChickWeight)
head(ChickWeight)

logistic = function(age, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (age - xmid)))
}

# Modelo geral para tentar aproximar os chutes
mod11 = nls(weight ~ logistic(Time, Asym, xmid, scal),
    start = list(Asym = 200, xmid = 10, scal = .103),
    data = ChickWeight)
# 337.6049  16.0688   0.1249

mod11_lista = nlsList(weight ~ logistic(Time, Asym, xmid, scal),
            start = list(Asym = 337, xmid = 16, scal = .12),
            data = ChickWeight)
coef(mod11_lista)

##### PRÁTICA 11: Modelos Não Lineares Mistos #####

logistic = function(input, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (input - xmid)))
}

#### 12.1. Orange ####

#### Gráfico ####

plot(Orange)

# 170, 600, .15
# 1.927e+02 7.288e+02 2.829e-03 
nlme1 = nlme(circumference ~ logistic(age, Asym, xmid, scal), data = Orange,
             fixed = Asym + xmid + scal ~ 1,
             random = pdDiag(Asym + xmid + scal ~ 1),
             start = c(192, 728, .002))
nlme1
summary(nlme1)

#### 12.2. Soybean ####

nlme2 = nlme(weight ~ logistic(Time, Asym, xmid, scal),
             data = Soybean,
             fixed = Asym + xmid + scal ~ 1,
             random = pdDiag(Asym + xmid + scal ~ 1),
             start = c(18.4182, 53.9562,  0.1226))
nlme2
summary(nlme2)