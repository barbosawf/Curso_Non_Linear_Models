library(nlme)
library(ggpubr)
library(rlang)
library(tidyverse)

df <- datasets::Orange
logistic <- function(x, Asym, xmid, scal){Asym / (1 + exp(- scal * (x - xmid)))}
nlsList.obj <- nlsList(circumference ~ logistic(age, Asym, xmid, scal),
start = list(Asym = 170, xmid = 600, scal = .004),
data = df)
x.axis <- "age"
y.axis <- "circumference"
grouping.var <- "Tree"
fun <- function(x, .args) {.args$Asym / (1 + exp(-.args$scal * (x - .args$xmid)))}
x.lab <- "Age (days since 1968/12/31)"
y.lab <- "Trunk circumference (mm)"


nlsList_plot(df = df,
            x.axis = x.axis,
            y.axis = y.axis,
            grouping.var = grouping.var,
            fun = fun,
            nlsList.obj = nlsList.obj,
            color = 'blue',
            linewidth = 0.5,
            x.lab = x.lab,
            y.lab = y.lab) -> res

res$panel_p

