
soy_nls = nls(weight ~ logistic(Time, Asym, xmid, scal),
              start = list(Asym = 25, xmid = 60, scal = .103),
              data = Soybean)
soy_nls

library(rlang)
library(tidyverse)
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
           grouping.var,
           fun,
           nlsList.obj,
           color = 'blue',
           linewidth = 0.5,
           x.lab = NULL,
           y.lab = NULL
  ) {
    
    
    # grouping.var is turned into symbol to be used in nest_by function
    g.var <- rlang::sym(grouping.var)
    
    ordem <- sort(levels(df[[grouping.var]]))
    
    df |>
      as.data.frame() |>
      dplyr::mutate_at(dplyr::vars({{grouping.var}}),
                       ~ factor(., levels = ordem)) |>
      dplyr::arrange_at(grouping.var) -> df
    
    
    stats::coef(nlsList.obj) |>
      as.data.frame() |>
      tibble::rownames_to_column(var = grouping.var) |>
      dplyr::mutate_at(dplyr::vars({{grouping.var}}),
                       ~ factor(., levels = ordem)) |>
      dplyr::arrange_at(grouping.var) -> arr.nlsList.obj
    
    
    
    arr.nlsList.obj |>
      stats::na.omit() |>
      droplevels() |>
      split(arr.nlsList.obj[[grouping.var]]) |>
      purrr::map(as_tibble) -> args_list
    
    
    arr.nlsList.obj |>
      base::apply(1, anyNA) -> id_with_nas
    
    arr.nlsList.obj |>
      dplyr::select({{grouping.var}}) |> dplyr::pull() -> gv
    
    id_with_nas <- base::as.vector(base::droplevels(gv[id_with_nas]))
    
    
    x_min_max <- c(min = min(df[[x.axis]]), max = max(df[[x.axis]]))
    
    fit_values <- as.vector(na.omit(fitted(nlsList.obj)))
    y_min_max <- c(min = min(c(fit_values, df[[y.axis]])),
                   max = max(c(fit_values, df[[y.axis]])))
    
    
    filtered_df <-
      df |> dplyr::filter_at(dplyr::vars(dplyr::all_of(grouping.var)), ~ !. %in% id_with_nas)
    
    
    nested_df <- filtered_df |>
      dplyr::nest_by({{g.var}}, .keep = T)
    
    df_list <- nested_df$data |>
      stats::setNames(nested_df[[1]])
    
    df_list |>
      purrr::map(~.x |>
                   dplyr::summarise_at(dplyr::vars(dplyr::all_of(x.axis)),
                                       list(min, max))) -> lms
    
    # x.axis and y.axis are turned into symbol to be used in geom_point function
    x.axis <- rlang::sym(x.axis)
    y.axis <- rlang::sym(y.axis)
    
    
    
    f.x <- function(x.lab) {
      if (is.null(x.lab)) {
        # A waiver is a "flag" object to indicate the calling function
        # should just use the default value.
        ggplot2::xlab(ggplot2::waiver())
      } else {
        ggplot2::xlab(x.lab)
      }
    }
    
    f.y <- function(y.lab) {
      if (is.null(y.lab)) {
        ggplot2::ylab(ggplot2::waiver())
      } else {
        ggplot2::ylab(y.lab)
      }
    }
    
    
    # the pmap function is needed to map objects into three lists simultaneously
    # df_list contain a list of data separated by the grouping.var argument
    # therefore df_list is used in the geom_point function
    # args_list contains the list of parameters estimated by the nlsList function
    # therefore args_list is used in the stat_function function
    # lms is a list with the limits of the x-axis where the non-linear function is applied
    
    
    purrr::pmap(list(df_list, args_list, lms), \(.df, .args, .lms) {
      ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = {{x.axis}}, y = {{y.axis}}), data = .df) +
        ggplot2::stat_function(
          fun = \(x) {fun(x, .args)},
          color = color,
          xlim = c(.lms[[1]], .lms[[2]]),
          linewidth = linewidth
        ) +
        ggplot2::scale_x_continuous(limits = x_min_max) +
        ggplot2::scale_y_continuous(limits = y_min_max) +
        ggplot2::labs(title = .args[[grouping.var]]) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 10,
            hjust = 0.5,
            vjust = -0.5
          )
        )  +
        f.x(x.lab) +
        f.y(y.lab)
      
    }) -> list_p
    
    
    ggpubr::ggarrange(
      plotlist = list_p |>
        purrr::map( ~ . + ggpubr::rremove('xlab') +
                      ggpubr::rremove('ylab'))
    ) |>
      ggpubr::annotate_figure(
        left = ggpubr::text_grob(
          ifelse(is.null(y.lab), y.axis, y.lab),
          rot = 90,
          vjust = 0.5,
          size = 14
        ),
        bottom = ggpubr::text_grob(ifelse(is.null(x.lab), x.axis, x.lab),
                                   size = 14)
      ) -> panel_p
    
    return(list(list_p = list_p, panel_p = panel_p))
    
    
  }
