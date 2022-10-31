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
drg_to_payment <- function(option){
  DRG <- DRG %>%
    mutate(DRG.Definition = substr(DRG.Definition, 0, 3))
  if(option == "Medicare"){
    DRG %>%
      ggplot(aes(x = DRG.Definition, y = Average.Medicare.Payments)) +
      geom_boxplot() +
      labs(x = "DRG code",
           y = "Average Medicare Payments",
           title = "Boxplot of average medicare payments by DRG code") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))
  }
  else if(option == "Total"){
    DRG %>%
      ggplot(aes(x = DRG.Definition, y = Average.Total.Payments)) +
      geom_boxplot() +
      labs(x = "DRG code",
           y = "Average Total Payments",
           title = "Boxplot of average total payments by DRG code") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))
  }
  else if (option == "Covered"){
    DRG %>%
      ggplot(aes(x = DRG.Definition, y = Average.Covered.Charges)) +
      geom_boxplot() +
      labs(x = "DRG code",
           y = "Average Covered Charges",
           title = "Boxplot of average covered charges by DRG code") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))
  }
}
