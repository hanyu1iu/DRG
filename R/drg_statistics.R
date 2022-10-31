#' Statistics for DRG
#'
#'This function calculates statistics over all of the \code{DRG code} for \code{average medicare payments}
#'
#' @param option a statistics among mean, median, or standard deviation
#'
#' @return A statistical calculation for mean or median or standard deviation
#' @export
#' 
#' @importFrom tidyverse
#'
#' @examples
drg_statistics <- function(option){
  drg_stat <- DRG %>%
    group_by(DRG.Definition) %>%
    summarise(mean = mean(Average.Medicare.Payments, na.rm = TRUE),
              median = median(Average.Medicare.Payments, na.rm = TRUE),
              "standard deviation" = sd(Average.Medicare.Payments, na.rm = TRUE))
  drg_stat %>%
    select(DRG.Definition, all_of(option))
}
