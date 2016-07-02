#include <vector>
#include <Rcpp.h>


inline int randWrapper (const int n) {
  return floor(unif_rand() * n);
}


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

  // Lastly, apply the column:
  Rcpp::colnames(returns) = Rcpp::colnames(levels);

  // Apply row names:
  if (!Rf_isNull(Rcpp::rownames(levels))) {
    Rcpp::CharacterVector rownames = Rcpp::rownames(levels);
    rownames.erase(0);
    Rcpp::rownames(returns) = rownames;
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

// [[Rcpp::export]]
Rcpp::IntegerVector indexSequence (const int length) {
  // Initialize the return value:
  Rcpp::IntegerVector retval(length);

  // Iterate over indices and populate the return value:
  for (int i = 0; i < length; i++) {
    retval[i] = i;
  }

  // Done, return the index vector:
  return retval;
}

// [[Rcpp::export]]
Rcpp::IntegerVector shuffleIndicesDestructive (Rcpp::IntegerVector idx) {
  // Shuffle in place:
  std::random_shuffle(idx.begin(), idx.end(), randWrapper);

  // Done return:
  return idx;
}

// [[Rcpp::export]]
Rcpp::IntegerVector shuffleIndicesSafe (Rcpp::IntegerVector idx) {
  // Copy the stuff:
  Rcpp::IntegerVector cloned = Rcpp::clone(idx);

  // Send to destructive shuffle:
  return shuffleIndicesDestructive(cloned);
}

// [[Rcpp::export]]
Rcpp::IntegerVector shuffleIndicesByLength (const int length) {
  return shuffleIndicesDestructive(indexSequence(length));
}

std::vector<int> shuffleIndices (const int length) {
  // First, declare the return value:
  std::vector<int> idx (length);

  // Assign indices:
  for (int i = 0; i < length; i++) {
    idx[i] = i;
  }

  // Now shuffle:
  std::random_shuffle(idx.begin(), idx.end(), randWrapper);

  // Done, return:
  return idx;
}

// [[Rcpp::export]]
Rcpp::NumericVector zmbdSafe (const double target, const Rcpp::NumericVector lower, const Rcpp::NumericVector upper) {
  // Get the dimension:
  const int dimension = lower.size();

  // Declare the return value:
  Rcpp::NumericVector values(dimension);

  // If we have only one element, short circuit:
  if (dimension == 1) {
    // Set the value:
    values[0] = target;

    // Done, return:
    return values;
  }

  // Get the sum of lowers and uppers:
  const double lowerSum = std::accumulate(lower.begin(), lower.end(), 0.0);
  const double upperSum = std::accumulate(upper.begin(), upper.end(), 0.0);

  // Initialize the contingencies to cumulative sums:
  Rcpp::NumericVector contingencyLower = Rcpp::cumsum(lower);
  Rcpp::NumericVector contingencyUpper = Rcpp::cumsum(upper);

  // Iterate over contingencies and update:
  for (int i = 0; i < dimension; i++) {
    contingencyLower[i] = lowerSum - contingencyLower[i];
    contingencyUpper[i] = upperSum - contingencyUpper[i];
  }

  // Define the mean value to be updated:
  double mean = 0.0, minValue, maxValue;

  // Iterate over the dimension in a random order and update permissible draws and mean value:
  for (int index = 0; index < dimension; index++) {
    // Define the min/max value:
    minValue = lower[index];
    maxValue = upper[index];

    // Simulate or get antithetical for the element:
    // TODO: Check if this is a viable equality check.
    if (mean == target) {
      // Get correct contingency U/L bounds by sign:
      const double contingencyU = contingencyUpper[index] > 0 ? contingencyUpper[index] : -contingencyUpper[index];
      const double contingencyL = contingencyLower[index] < 0 ? contingencyLower[index] : -contingencyLower[index];

      // Update the effective range:
      minValue = minValue < 0 ? std::max(minValue, -contingencyU) : std::min(minValue, -contingencyL);
      maxValue = maxValue > 0 ? std::min(maxValue, -contingencyL) : std::max(maxValue, -contingencyU);

      // Get the value:
      if (minValue == maxValue) {
        values[index] = minValue;

        if (values[index] < lower[index]) {
          values[index] = lower[index];
        }
        else if(values[index] > upper[index]){
          values[index] = upper[index];
        }
      }
      else {
        values[index] = Rf_runif(std::min(minValue, maxValue), std::max(minValue, maxValue));
      }
    }
    else if (mean > target) {
      values[index] = maxValue > -mean ? std::max(-mean, minValue) : std::min(-mean, maxValue);
    }
    else {
      values[index] = minValue < -mean ? std::min(-mean, maxValue) : std::max(-mean, minValue);
    }

    // Update the mean:
    mean += values[index]; //Rcpp::mean(values) - target;
  }

  // Done, return:
  return values;
}

// [[Rcpp::export]]
Rcpp::NumericVector ttbdInner (const double target, const Rcpp::NumericVector lower, const Rcpp::NumericVector upper) {
  // Get the dimension:
  const int dimension = lower.size();

  // Declare the return value:
  Rcpp::NumericVector values(dimension);

  // If we have only one element, short circuit:
  if (dimension == 1) {
    // Set the value:
    values[0] = target;

    // Done, return:
    return values;
  }

  // Fill in:
  double total = 0.0;
  for (int i = 0; i < dimension; i++) {
    const double lowerValue = lower[i];
    const double upperValue = upper[i];
    const double value = lowerValue == upperValue ? lowerValue : Rf_runif(lowerValue, upperValue);
    values[i] = value;
    total += value;
  }

  // Compute the gap of simulated total and target total:
  double gap = target - total;

  // Iterate over the return values and adjust as per gap:
  for (int i = 0; i < dimension && gap != 0.0; i++) {
    // Calculate the distances to limits:
    const double distanceToLower = lower[i] - values[i];
    const double distanceToUpper = upper[i] - values[i];

    // Compute the permissible shift:
    const double shift = gap > 0 ? std::min(distanceToUpper, gap) : std::max(distanceToLower, gap);

    // Apply the shift:
    values[i] += shift;

    // Update gap:
    gap -= shift;
  }

  // Done, return:
  return values;
}


// [[Rcpp::export]]
Rcpp::NumericVector ttbd(const double target, const Rcpp::NumericVector lower, const Rcpp::NumericVector upper) {
  // We must have both dimensions greater than 0.
  if (lower.size() == 0 || upper.size() == 0) {
    Rcpp::warning("[ttbd] Lower/upper dimensions are 0.");
    return Rcpp::NumericVector(0);
  }

  // Both dimensions must be equal:
  if (lower.size() != upper.size()) {
    Rcpp::stop("Lower/upper dimensions don't match each other.");
  }

  // Get the dimension:
  const int dimension = lower.size();

  // Shuffle the indices:
  Rcpp::IntegerVector indices = Rcpp::wrap(shuffleIndices(dimension));

  // Get the ttbded:
  Rcpp::NumericVector retval = ttbdInner(target, lower[indices], upper[indices]);

  // Done, return:
  return retval[Rcpp::match(indexSequence(dimension), indices) - 1];
}
