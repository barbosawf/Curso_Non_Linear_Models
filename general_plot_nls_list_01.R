
soy_nls = nls(weight ~ logistic(Time, Asym, xmid, scal),
              start = list(Asym = 25, xmid = 60, scal = .103),
              data = Soybean)
soy_nls


logistic = function(x, Asym, xmid, scal){
  Asym / (1 + exp(- scal * (x - xmid)))
}

soylist = nlsList(weight ~ logistic(Time, Asym, xmid, scal),
                  start = list(Asym = 18.4182, xmid = 53.9562, scal = 0.1226),
                  data = Soybean)


logistic_func <- function(x, .args) {
  .args$Asym / (1 + exp(-.args$scal * (x - .args$xmid)))
}

fun <- logistic_func
df <- Soybean
nlsList.obj <- soylist
x.axis <- 'Time'
y.axis <- 'weight'
grouping.var <- 'Plot'
color = 'blue'
linewidth = 0.5

nlsList_plot <-
  function(df,
           x.axis,
           y.axis,
           fun,
           nlsList.obj,
           grouping.var,
           color = 'blue',
           linewidth = 1,
           x.lab = NULL,
           y.lab = NULL
  ) {
    
    
    # grouping.var is turned into symbol to be used in nest_by function
    g.var <- sym(grouping.var)
    
    ordem <- sort(levels(df[[grouping.var]]))
    
    df |>
      mutate_at(vars({{grouping.var}}), 
                ~ factor(., levels = ordem)) |>
      arrange_at(grouping.var) -> df
    
    
    coef(nlsList.obj) |>
      rownames_to_column(var = grouping.var) |>
      mutate_at(vars({{grouping.var}}), 
                ~ factor(., levels = ordem)) |>
      arrange_at(grouping.var) -> arr.nlsList.obj
    
    
    arr.nlsList.obj |>
      na.omit() |>
      nest_by({{g.var}}, .keep = T) -> nested_args
    
    
    args_list <- nested_args$data
    
    
    args_list |>
      setNames(nested_args[[grouping.var]]) -> args_list
    
    
    arr.nlsList.obj |>
      apply(1, anyNA) -> id_with_nas
    
    arr.nlsList.obj |>
      dplyr::select({{grouping.var}}) |> pull() -> gv
    
    id_with_nas <- as.vector(droplevels(gv[id_with_nas]))
    
    
    x_min_max <- c(min = min(df[[x.axis]]), max = max(df[[x.axis]]))
    
    fit_values <- as.vector(na.omit(fitted(nlsList.obj)))
    y_min_max <- c(min = min(c(fit_values, df[[y.axis]])),
                   max = max(c(fit_values, df[[y.axis]])))
    
    
    filtered_df <-
      df |> filter_at(vars(all_of(grouping.var)), ~ !. %in% id_with_nas)
    
    
    nested_df <- filtered_df |>
      nest_by({{g.var}}, .keep = T)
    
    df_list <- nested_df$data |>
      setNames(nested_df[[1]])
    
    df_list |>
      map(~.x |>
            summarise_at(vars({{x.axis}}), 
                         list(min, max))) -> lms
    
    # x.axis and y.axis are turned into symbol to be used in geom_point function
    x.axis <- sym(x.axis)
    y.axis <- sym(y.axis)
    
    
    
    f.x <- function(x.lab) {
      if (is.null(x.lab)) {
        # A waiver is a "flag" object to indicate the calling function 
        # should just use the default value.
        xlab(waiver())
      } else {
        xlab(x.lab)
      }
    }
    
    f.y <- function(y.lab) {
      if (is.null(y.lab)) {
        ylab(waiver())
      } else {
        ylab(y.lab)
      }
    }
    
    
    # the pmap function is needed to map objects into three lists simultaneously
    # df_list contain a list of data separated by the grouping.var argument 
    # therefore df_list is used in the geom_point function
    # args_list contains the list of parameters estimated by the nlsList function
    # therefore args_list is used in the stat_function function
    # lms is a list with the limits of the x-axis where the non-linear function is applied

    
    pmap(list(df_list, args_list, lms), \(.df, .args, .lms) {
      ggplot() +
        geom_point(aes(x = {{x.axis}}, y = {{y.axis}}), data = .df) +
        stat_function(
          fun = \(x) fun(x, .args),
          color = color,
          xlim = c(.lms[[1]], .lms[[2]]),
          linewidth = linewidth
        ) +
        scale_x_continuous(limits = x_min_max) +
        scale_y_continuous(limits = y_min_max) +
        labs(title = .args[[grouping.var]]) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            size = 10,
            hjust = 0.5,
            vjust = -0.5
          )
        )  +
        f.x(x.lab) +
        f.y(y.lab)
        
    }) -> list_p
    
    
    ggarrange(
      plotlist = list_p |>
        map( ~ . + rremove('xlab') +
               rremove('ylab'))
    ) |>
      annotate_figure(
        left = textGrob(
          ifelse(is.null(y.lab), y.axis, y.lab),
          rot = 90,
          vjust = 0.5,
          gp = gpar(cex = 1.2)
        ),
        bottom = textGrob(ifelse(is.null(x.lab), x.axis, x.lab), 
                          gp = gpar(cex = 1.2))
      ) -> panel_p
    
    return(list(list_p = list_p, panel_p = panel_p))
    
    
  }


nlsList_plot(df,
             x.axis,
             y.axis,
             fun,
             nlsList.obj,
             grouping.var,
             color = 'blue',
             linewidth = 0.5,
             x.lab = 'Time (days after planting)',
             y.lab = 'Weight (g)'
) -> pps


pps$list_p[[1]]
pps$panel_p

# Usefull links -----------------------------------------------------------
# https://stackoverflow.com/questions/1376967/using-stat-function-and-facet-wrap-together-in-ggplot2-in-r
# https://stackoverflow.com/questions/64757410/shared-x-and-y-axis-labels-ggplot2-with-ggarrange


# Garbage -----------------------------------------------------------------

soylist |>
  map_dfr(\(x) if (!is.null(x))
    broom::tidy(x)[, 1:2], .id = 'ID') |>
  group_split(ID, .keep = T)

x = length(p)

cols = round(sqrt(x),0)
rows = ceiling(x/cols)

ggarrange(plotlist = p |>
            map(~ . + rremove('xlab') +
                  rremove('ylab')), ncol=cols, nrow = rows)




p
cowplot::plot_grid(plotlist = p |>
                     map(\(x) x + rremove('xlab') + rremove('ylab')),
                   nrow = 5) |>
  annotate_figure(
    left = textGrob(
      "Common y-axis",
      rot = 90,
      vjust = 0.5,
      gp = gpar(cex = 1.3)
    ),
    bottom = textGrob("Common x-axis", gp = gpar(cex = 1.3))
  )



require(ggpubr)
ggarrange(plotlist = p |>
            map(\(x) x + rremove('xlab') + rremove('ylab'))) |>
  annotate_figure(
    left = textGrob(
      "Common y-axis",
      rot = 90,
      vjust = 1,
      gp = gpar(cex = 1.3)
    ),
    bottom = textGrob("Common x-axis", gp = gpar(cex = 1.3))
  )



