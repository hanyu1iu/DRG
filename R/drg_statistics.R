#' Statistics for DRG
#'
#'This function makes a boxplot of \code{payment} versus \code{DRG code}
#'
#' @param option a string name for variable in the DRG dataframe
#'
#' @return A plot of \code{payment} versus \code{DRG code}
#' @export
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
