dd <- data.frame(
  predicted = rnorm(72, mean = 2, sd = 2),
  state = rep(c("A", "B", "C"), each = 24)
) 

# Split the data by state, apply a function on each member that converts it into a 
# plot object, and return the result as a vector.
plots <- lapply(
  split(dd, dd$state),
  FUN = function(state_slice) {
    # The code here is the plot code generation. You can do anything you would
    # normally do for a single plot, such as calling stat_function, and you do this
    # one slice at a time.
    ggplot(state_slice, aes(predicted)) +
      geom_density() +
      stat_function(
        fun = dnorm,
        args = list(
          mean = mean(state_slice$predicted),
          sd = sd(state_slice$predicted)
        ),
        color = "red"
      )
  }
)






# Finally, present the plots on 3 columns.
multiplot(plotlist = plots, cols=3)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


soylist |>
  map_dfr(\(x) if (!is.null(x))
    broom::tidy(x)[, 1:2], .id = 'ID') |>
  group_split(ID, .keep = T)


tapply(warpbreaks$breaks, warpbreaks[,-1], sum)


v1 <- 'Plot'
v2 <- sym(v1)
coef(soylist) |>
  na.omit() |> 
  rownames_to_column(var = v1) |>
  nest_by({{v2}}) -> nested_args


args_list <- nested_args$data

args_list |>
  setNames(coef(soylist) |>
             na.omit() |>
             rownames()) -> args_list

coef(soylist) |> 
  rownames_to_column(var = v1) |>
  filter(is.na(Asym)) |>
  dplyr::select({{v2}}) |> pull() -> id_with_nas


filtered_df <-
  Soybean |> filter_at(vars(all_of(v1)), ~ !. %in% id_with_nas)


nested_df <- filtered_df |>
  nest_by({{v2}})

df_list <- nested_df$data |>
  setNames(nested_df[[1]])

x <- sym('Time')
y <- sym('weight')

arguments <- c('Asym',  'xmid',  'scal')


map2(df_list, args_list, \(.df, .args) {
  ggplot() +
    geom_point(aes(x = {{x}}, y = {{y}}), data = .df) +
    geom_function(
      fun = \(x) .args[[arguments[1]]] / (1 + exp(- .args[[arguments[3]]] * (x - .args[[arguments[2]]]))),
      color = 'blue',
      linewidth = 1
    ) +
    labs(x = x, y = y) +
    theme_minimal()
})


