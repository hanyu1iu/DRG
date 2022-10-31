#' Statistics for DRG
#'
#'This function produces a table of either mean, median, or standard deviations
#'of all the 3 digit DRG codes for average Medical payments.
#'
#' @param option a string name for statistics mean, median, or standard deviation
#'
#' @return A statistical calculation for mean or median or standard deviation
#' @export
#'
#' @import tidyverse
#'
#' @examples
#' drg_statistics("mean")
drg_statistics <- function(option){ #create a function to calculate mean, median and sd
  drg_stat <- DRG %>%
    group_by(DRG.Definition) %>%  #rearrange data by using DRG.Definition
    summarise(mean = mean(Average.Medicare.Payments, na.rm = TRUE), #calculate the mean
              median = median(Average.Medicare.Payments, na.rm = TRUE),  #calculate the median
              "standard deviation" = sd(Average.Medicare.Payments, na.rm = TRUE))  #calculate the sd
  drg_stat %>%
    select(DRG.Definition, all_of(option))
}
