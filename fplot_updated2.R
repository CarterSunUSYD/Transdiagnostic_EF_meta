#' Create stacked effect size plot
#' 
#' Note that this is a custom function and will not work in a "universal" way
#' without additional coding.
#'
#' @param x a data.frame object
#' @param group string. From your data frame, specify the variable to be used to group the plots.
#' @param x_axis logical. Defaults to FALSE. Set to TRUE to show the x-axis and labelss.
#' @param var_names logical. Defaults to TRUE. Set to FALSE to activate variable titles in table column.
#'
#' @return an object of class ggplot.
#' @export
#'
fplot <- function(x, group, fontsize = 7, mid_title = FALSE, x_axis = FALSE, var_names = TRUE,
                  x_axis_centred = FALSE) {
  
  # detect limits
  lims <- c(floor(min(x$lower_ci)*10)/10, 
            ceiling(max(x$upper_ci)*10)/10)
  print(lims)
  # some options (since this is a custom function)
  x_interval <- 0.5 # x-axis tick intervals
  linetype = 3 # also use "solid" or numbers
  
  # filter data by `comparison` and then plot the effect sizes
  mydf <- filter(x, comparison == group)
  p <- ggplot(mydf, mapping = aes(x = estimate, y = ef_domain),fontsize=7) + 
    geom_linerange(mapping = aes(xmin = lower_ci, xmax = upper_ci)) +
    geom_point(aes(size = k)) +
    scale_size(limits = c(0, max(mydf$k)), range = c(0, 3)) + #Also if you notice that the circles are to large, then in fplot.R line 29 you can reduce the range from (0, 7) to something smaller like (0, 5). It might happen when you start to export the plot into a vector format file. 
    geom_vline(xintercept = 0, linetype = linetype) +
    theme_half_open(font_size = fontsize) +
    theme(plot.title.position = "plot") +
    theme(panel.grid.major.x = element_line(color = "azure2",
                                            size = 0.5,
                                            linetype = 1)) +
    ggeasy::easy_remove_y_axis(what = c("line")) +
    ggeasy::easy_remove_legend() +
    scale_x_continuous(limits = lims,
                       breaks = seq(lims[1], lims[2], x_interval)) + 
    ylab("") +
    xlab(expression("Hedges' " ~italic(g))) +
    ggtitle(group)
  if (x_axis == FALSE) p <- p + ggeasy::easy_remove_x_axis()
  if (x_axis_centred == TRUE) {
    p <- p + xlim(-1.3, 4.1)
  }
  
  # generate theme for table graphics
  tab_base <- 
    filter(mydf, comparison == group) %>%
    ggplot(aes(y = ef_domain),fontsize=7) +
    ylab(NULL) +
    xlab("  ") +
    theme_map(7) +  # not sure if this is needed
    theme(plot.title = element_text(hjust = 0.5, size = 7)) +
    update_geom_defaults("text", list(size = fontsize/3.14)) # change table font size
  
  # plot individual table column 
  t0 <- 
    tab_base + 
    geom_text(aes(x = 1, label = k)) +
    if (var_names) ggtitle(expression(~bolditalic(k)))
  t1 <-
    tab_base +
    geom_text(aes(x = 1, label = N)) +
    if (var_names) ggtitle(expression(~bolditalic(N)))
  tbrackets <-
    tab_base +
    geom_text(aes(x = 1, label = brackets)) +
    if (var_names) ggtitle("Hedges' g (95% CI)")
  # t1 <- 
  #   tab_base + 
  #   geom_text(aes(x = 1, label = estimate)) +
  #   if (var_names) ggtitle("Hedges' g")
  # t2 <- 
  #   tab_base + 
  #   geom_text(aes(x = 1, label = lower_ci)) +
  #   if (var_names) ggtitle("L_CI 95")
  # t3 <- 
  #   tab_base + 
  #   geom_text(aes(x = 1, label = upper_ci)) +
  #   if (var_names) ggtitle("U_CI 95")
  t4 <- 
    tab_base + 
    geom_text(aes(x = 1, label = pval)) +
    if (var_names) ggtitle(expression(~bolditalic(p)))
  # combine everything:
  out <- 
    # p + t0 + t1 + t2 + t3 + t4 +
    p + t0 + t1+  tbrackets + t4 +
    plot_layout(nrow = 1, widths = c(10,1,1,3,1))
  cat("Plot of", group, "complete")
  return(out)
}
