#include <vector>
#include <Rcpp.h>


// [[Rcpp::export]]
Rcpp::NumericMatrix mvReturns (const Rcpp::NumericMatrix levels) {
  // Can we compute the returns:
  if (levels.nrow() < 2 || levels.ncol() == 0) {
    // Nope, raise error:
    Rcpp::stop("Cannot compute returns: At least one series with two observations needed");
  }

  // Get the number of rows and columns for the return value:
  const int rows = levels.nrow() - 1;
  const int cols = levels.ncol();

  // Initialize the return value:
  Rcpp::NumericMatrix returns(rows, cols);

  // Iterate over the levels and compute returns:
  for (int c = 0; c < cols; c++) {
    for (int r = 0; r < rows; r++) {
      returns(r, c) = levels(r + 1, c) / levels(r, c) - 1;
    }
  }

  // Done, return computed returns:
  return returns;
}


// [[Rcpp::export]]
Rcpp::NumericVector uvReturns (const Rcpp::NumericVector levels) {
  // Can we compute the returns:
  if (levels.size() < 2) {
    // Nope, raise error:
    return Rcpp::NumericVector(0);
  }

  // Get the length of the vector:
  const int length = levels.size() - 1;

  // First, create the returns vector:
  Rcpp::NumericVector returns(length);

  // Iterate over the levels and compute the returns:
  for (int i = 0; i < length; i++) {
    returns[i] = levels[i + 1] / levels[i] - 1;
  }

  // Done, return:
  return returns;
}
