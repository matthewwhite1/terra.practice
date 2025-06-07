#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
double sap_day_rast_helper(NumericVector cell_vect) {
  // Initialize count variable and vector length
  int count = 0;
  int valid = 0;
  int num_days = cell_vect.length() / 2;

  // Loop through each cell value
  for (int i = 0; i < num_days; i++) {
    // Skip if NA
    if (NumericVector::is_na(cell_vect[i]) || NumericVector::is_na(cell_vect[i + num_days])) {
      continue;
    }
    // Increment valid days count
    valid++;
    // If tmax > 2.2 and tmin < -1.1 at same coordinates, increment count
    if (cell_vect[i] > 2.2 && cell_vect[i + num_days] < -1.1) {
      count += 1;
    }
  }

  // Return proportion
  if (valid == 0) {
    return NA_REAL;
  } else {
    return (double)count / valid;
  }
}
