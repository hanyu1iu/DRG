#' Boxplot for DRG
#'
#'This function produces a boxplot of payments by DRG code.
#'
#' @param option a string name for different payments: Medicare, Total, or Covered.
#'
#' @return A boxplot for the average Medicare payments, the average total payment, or the average covered charges
#' @export
#'
#' @import tidyverse
#'
#' @examples
#' drg_statistics("mean")
drg_to_payment <- function(option){  #create a function to produce boxplot
  DRG <- DRG %>%
    mutate(DRG.Definition = substr(DRG.Definition, 0, 3))  #create a new variable to shorten the name of DRG.Definition
  if(option == "Medicare"){
    DRG %>%
      ggplot(aes(x = DRG.Definition, y = Average.Medicare.Payments)) + #make a ggplot
      geom_boxplot() + #make a boxplot
      labs(x = "DRG code",  #relabel x axis
           y = "Average Medicare Payments",  #relabel y axis
           title = "Boxplot of average medicare payments by DRG code") + #add a title to the boxplot
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))  #adjust formats
  }
  else if(option == "Total"){
    DRG %>%
      ggplot(aes(x = DRG.Definition, y = Average.Total.Payments)) +   #make a ggplot
      geom_boxplot() +   #make a boxplot
      labs(x = "DRG code",   #relabel x axis
           y = "Average Total Payments",   #relabel y axis
           title = "Boxplot of average total payments by DRG code") +    #add a title to the boxplot
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))  #adjust formats
  }
  else if (option == "Covered"){
    DRG %>%
      ggplot(aes(x = DRG.Definition, y = Average.Covered.Charges)) +   #make a ggplot
      geom_boxplot() +   #make a boxplot
      labs(x = "DRG code",   #relabel x axis
           y = "Average Covered Charges",  #relabel y axis
           title = "Boxplot of average covered charges by DRG code") +  #add a title to the boxplot
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust = 1))  #adjust formats
  }
}
