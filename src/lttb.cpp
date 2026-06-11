#include <Rcpp.h>
#include <cmath>
#include <limits>

using namespace Rcpp;

namespace {
bool finite_pair(double x, double y) {
  return std::isfinite(x) && std::isfinite(y);
}
}

// [[Rcpp::export]]
IntegerVector lttb_indices_cpp(NumericVector x, NumericVector y, int n_out) {
  const int n = x.size();
  if (y.size() != n) {
    stop("x and y must have the same length.");
  }
  if (n <= 0 || n_out <= 0) {
    return IntegerVector(0);
  }
  if (n <= 2 || n_out >= n) {
    IntegerVector all(n);
    for (int i = 0; i < n; ++i) {
      all[i] = i + 1;
    }
    return all;
  }
  if (n_out <= 2) {
    IntegerVector endpoints(2);
    endpoints[0] = 1;
    endpoints[1] = n;
    return endpoints;
  }

  IntegerVector sampled(n_out);
  sampled[0] = 1;
  sampled[n_out - 1] = n;

  const double every = static_cast<double>(n - 2) / static_cast<double>(n_out - 2);
  int a = 0;

  for (int i = 0; i < n_out - 2; ++i) {
    const int range_start = static_cast<int>(std::floor(i * every)) + 1;
    const int range_end = std::min(static_cast<int>(std::floor((i + 1) * every)) + 1, n - 1);
    const int next_start = static_cast<int>(std::floor((i + 1) * every)) + 1;
    const int next_end = std::min(static_cast<int>(std::floor((i + 2) * every)) + 1, n);

    double avg_x = 0.0;
    double avg_y = 0.0;
    int avg_count = 0;
    for (int j = next_start; j < next_end; ++j) {
      if (finite_pair(x[j], y[j])) {
        avg_x += x[j];
        avg_y += y[j];
        ++avg_count;
      }
    }
    if (avg_count > 0) {
      avg_x /= avg_count;
      avg_y /= avg_count;
    } else {
      avg_x = x[n - 1];
      avg_y = y[n - 1];
    }

    double max_area = -1.0;
    int max_area_index = range_start;
    for (int j = range_start; j < range_end; ++j) {
      if (!finite_pair(x[a], y[a]) || !finite_pair(x[j], y[j]) || !finite_pair(avg_x, avg_y)) {
        continue;
      }
      const double area = std::abs(
        (x[a] - avg_x) * (y[j] - y[a]) -
          (x[a] - x[j]) * (avg_y - y[a])
      ) * 0.5;
      if (area > max_area) {
        max_area = area;
        max_area_index = j;
      }
    }

    sampled[i + 1] = max_area_index + 1;
    a = max_area_index;
  }

  return sampled;
}
