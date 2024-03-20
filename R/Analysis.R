#' Summary statistics of formulation data
#' 
#' This function calculates summary statistics of the provided formulation data.
#' 
#' @param formulation_data A data frame containing formulation data.
#' @return Summary statistics of the formulation data.
#' @export
summary_statistics <- function(formulation_data) {
  summary(formulation_data)
}

#' Confidence intervals of drug release
#' 
#' This function computes confidence intervals for drug release based on the provided formulation data.
#' 
#' @param formulation_data A data frame containing formulation data.
#' @return Confidence intervals for drug release.
#' @export
confidence_intervals <- function(formulation_data) {
  t.test(formulation_data$Drug_Release)$conf.int
}


#' Compare means across groups
#' 
#' This function compares the means of a response variable across groups specified by group_var.
#' 
#' @param formulation_data A data frame containing formulation data.
#' @param group_var The variable defining the groups for comparison.
#' @param response_var The response variable to compare across groups.
#' @return Results of the t-test comparing means across groups.
#' @export
compare_means <- function(formulation_data, group_var, response_var) {
  t.test(formulation_data[[response_var]] ~ formulation_data[[group_var]])
}

#' Compare distributions across groups
#' 
#' This function compares the distributions of a response variable across groups specified by group_var.
#' 
#' @param formulation_data A data frame containing formulation data.
#' @param group_var The variable defining the groups for comparison.
#' @param response_var The response variable to compare across groups.
#' @return A boxplot comparing the distributions across groups.
#' @export
compare_distributions <- function(formulation_data, group_var, response_var) {
  ggplot(formulation_data, aes_string(x = group_var, y = response_var)) +
    geom_boxplot(fill = "lightblue", color = "black") +
    labs(x = group_var, y = response_var, title = "Boxplot Comparison")
}

#' Control chart for quality control
#' 
#' This function generates a control chart for monitoring the quality control parameter over time.
#' 
#' @param formulation_data A data frame containing formulation data.
#' @param parameter The quality control parameter to monitor.
#' @return A control chart for the specified quality control parameter.
#' @export
control_chart <- function(formulation_data, parameter) {
  ggplot(formulation_data, aes_string(x = "Time", y = parameter)) +
    geom_line() +
    labs(x = "Time", y = parameter, title = "Control Chart")
}

#' Assess batch-to-batch variability
#' 
#' This function calculates the batch-to-batch variability of a specified parameter.
#' 
#' @param formulation_data A data frame containing formulation data.
#' @param parameter The parameter for which batch-to-batch variability is calculated.
#' @return The batch-to-batch variability of the specified parameter.
#' @export
batch_variability <- function(formulation_data, parameter) {
  variability <- sd(formulation_data[[parameter]])
  paste("Batch-to-batch variability in", parameter, ":", variability)
}
