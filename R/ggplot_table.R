#' table with histogram Function
#'
#' @description The function takes a data frame with group and value columns, then creates a summary statsistics table with embedded histograms
#'
#' @param data the data frame being used in the Shiny app
#' @references
#' https://rfortherestofus.com/2023/10/ggplots-in-gt-tables
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr gt ggplot2 stringr
#' @export

#Test data
#data=data.frame(group = rep(c("WT","TX"),each=35),value = c(rnorm(35,0,1),rnorm(35,4,4)))

ggplot_table <- function(data){
  Exp_data = data |>
    summarise(
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      n = length(value),
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value,na.rm = TRUE),
      mad = mad(value,na.rm = TRUE),
      GiniMD = Hmisc::GiniMd(value,na.rm=T),
      .by = 'group'
    )

    plot_table = Exp_data |>
      gt() |>
      data_color(
        columns = group,
        method = "auto",
        palette = c("darkorchid4","springgreen3","darkblue","orange")
      ) |>
      tab_spanner(
        label = 'Summary Statistics',
        columns = -group
      ) |>
      cols_label_with(fn = str_to_title) |>
      fmt_number(decimals = 2) |>
      cols_align('left', columns = group)

    return(plot_table)

}
