#' Vector Comparison Function
#'
#' This function allows you to compare vectors.
#' @param probabilities Vector of probabilities to be analyzed. Required parameter.
#' @param outcomes Vector of outcomes used to grade the vector of probabilities. Required parameter
#' @param cutoff Threshold used to determine point at which probabilities are successes. Defaults to 0.5.
#' @keywords accuracy
#' @export
#' @examples
#' accuracy()

##Accuracy Function to record accuracy of models
accuracy <- function(probabilities, outcomes, cutoff = 0.5) {
  predAccuracy <- numeric(nrow(probabilities))
  for (i in 1:length(outcomes)) {
    if ((((probabilities[i] >= cutoff) &
          (outcomes[i] == 1))) ||
        (((probabilities[i] < cutoff) & (outcomes[i] == 0)))) {
      predAccuracy[i] = 1
    } else {
      predAccuracy[i] = 0
    }

  }

  differences <- numeric(probabilities)
  for (i in 1:length(outcomes)) {
    differences[i] = abs(outcomes[i] - probabilities[i])
  }
  avgDistfromOutcome <- sum(differences) / length(differences)
  feedback <- sum(predAccuracy) / length(predAccuracy)
  cat(
    "Correct Predictions: ",
    sum(predAccuracy),
    "\nObservations: ",
    length(predAccuracy),
    "\nAverage Error: ",
    avgDistfromOutcome,
    "\n"
  )

  return(feedback)
}
