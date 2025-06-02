#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector sap_day_rast_helper(NumericMatrix tmax_vals,
                                  NumericMatrix tmin_vals) {
  // Get dimensions
  int num_cells = tmax_vals.nrow();
  int num_days = tmax_vals.ncol();

  // Define inequality results vector
  NumericVector result(num_cells);

  // Perform inequality for each value
  for (int cell = 0; cell < num_cells; cell++) {
    int count = 0;
    for (int day = 0; day < num_days; day++) {
      if (NumericMatrix::is_na(tmax_vals(cell, day)) || NumericMatrix::is_na(tmin_vals(cell, day))) {
        continue;
      }
      if (tmax_vals(cell, day) > 2.2 && tmin_vals(cell, day) < -1.1) {
        count += 1;
      }
    }
    result[cell] = count / num_days;
  }

  // Return proportion vector
  return result;
}
