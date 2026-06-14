#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

using namespace Rcpp;

namespace {
bool finite_pair(double x, double y) {
  return std::isfinite(x) && std::isfinite(y);
}

double ols_slope_cpp(const std::vector<double>& x, const std::vector<double>& y) {
  const int n = static_cast<int>(x.size());
  if (n < 2 || y.size() != x.size()) {
    return NA_REAL;
  }

  double mean_x = 0.0;
  double mean_y = 0.0;
  int keep = 0;
  for (int i = 0; i < n; ++i) {
    if (std::isfinite(x[i]) && std::isfinite(y[i])) {
      mean_x += x[i];
      mean_y += y[i];
      ++keep;
    }
  }
  if (keep < 2) {
    return NA_REAL;
  }
  mean_x /= keep;
  mean_y /= keep;

  double numerator = 0.0;
  double denominator = 0.0;
  for (int i = 0; i < n; ++i) {
    if (std::isfinite(x[i]) && std::isfinite(y[i])) {
      const double centered = x[i] - mean_x;
      numerator += centered * (y[i] - mean_y);
      denominator += centered * centered;
    }
  }
  if (!std::isfinite(denominator) || denominator <= 0.0) {
    return NA_REAL;
  }
  return numerator / denominator;
}

double rms_detrended_linear_cpp(const std::vector<double>& y, int start, int window) {
  if (window < 2) {
    return NA_REAL;
  }

  std::vector<double> x(window);
  std::vector<double> segment(window);
  for (int i = 0; i < window; ++i) {
    x[i] = static_cast<double>(i + 1);
    segment[i] = y[start + i];
  }

  const double slope = ols_slope_cpp(x, segment);
  if (!std::isfinite(slope)) {
    return NA_REAL;
  }

  double mean_x = 0.0;
  double mean_y = 0.0;
  for (int i = 0; i < window; ++i) {
    mean_x += x[i];
    mean_y += segment[i];
  }
  mean_x /= window;
  mean_y /= window;
  const double intercept = mean_y - slope * mean_x;

  double rss = 0.0;
  for (int i = 0; i < window; ++i) {
    const double residual = segment[i] - (intercept + slope * x[i]);
    rss += residual * residual;
  }
  return std::sqrt(rss / window);
}

bool has_variation(const std::vector<double>& x) {
  if (x.empty()) {
    return false;
  }
  double min_value = x[0];
  double max_value = x[0];
  for (size_t i = 1; i < x.size(); ++i) {
    min_value = std::min(min_value, x[i]);
    max_value = std::max(max_value, x[i]);
  }
  return max_value > min_value;
}

std::vector<double> finite_values(NumericVector x) {
  std::vector<double> out;
  out.reserve(x.size());
  for (int i = 0; i < x.size(); ++i) {
    const double value = x[i];
    if (std::isfinite(value)) {
      out.push_back(value);
    }
  }
  return out;
}

std::vector<double> finite_positive_values(NumericVector x) {
  std::vector<double> out;
  out.reserve(x.size());
  for (int i = 0; i < x.size(); ++i) {
    const double value = x[i];
    if (std::isfinite(value) && value > 0.0) {
      out.push_back(value);
    }
  }
  return out;
}

double mean_cpp(const std::vector<double>& x) {
  if (x.empty()) {
    return NA_REAL;
  }
  double total = 0.0;
  for (size_t i = 0; i < x.size(); ++i) {
    total += x[i];
  }
  return total / static_cast<double>(x.size());
}

double sd_cpp(const std::vector<double>& x) {
  if (x.size() < 2) {
    return NA_REAL;
  }
  const double avg = mean_cpp(x);
  double total = 0.0;
  for (size_t i = 0; i < x.size(); ++i) {
    const double centered = x[i] - avg;
    total += centered * centered;
  }
  return std::sqrt(total / static_cast<double>(x.size() - 1));
}

double lag_sd_cpp(NumericVector x, int lag) {
  if (lag < 1 || x.size() <= lag) {
    return NA_REAL;
  }
  std::vector<double> diffs;
  diffs.reserve(x.size() - lag);
  for (int i = lag; i < x.size(); ++i) {
    const double current = x[i];
    const double previous = x[i - lag];
    if (std::isfinite(current) && std::isfinite(previous)) {
      diffs.push_back(current - previous);
    }
  }
  return sd_cpp(diffs);
}

double lag_mean_abs_diff_cpp(NumericVector x, int lag) {
  if (lag < 1 || x.size() <= lag) {
    return NA_REAL;
  }
  double total = 0.0;
  int count = 0;
  for (int i = lag; i < x.size(); ++i) {
    const double current = x[i];
    const double previous = x[i - lag];
    if (std::isfinite(current) && std::isfinite(previous)) {
      total += std::abs(current - previous);
      ++count;
    }
  }
  if (count == 0) {
    return NA_REAL;
  }
  return total / static_cast<double>(count);
}

struct TimedGlucose {
  double timestamp;
  double glucose;
};

std::vector<TimedGlucose> finite_timed_glucose(NumericVector timestamp, NumericVector glucose) {
  const int n = std::min(timestamp.size(), glucose.size());
  std::vector<TimedGlucose> out;
  out.reserve(n);
  for (int i = 0; i < n; ++i) {
    const double t = timestamp[i];
    const double g = glucose[i];
    if (std::isfinite(t) && std::isfinite(g)) {
      out.push_back(TimedGlucose{t, g});
    }
  }
  std::sort(out.begin(), out.end(), [](const TimedGlucose& a, const TimedGlucose& b) {
    if (a.timestamp == b.timestamp) {
      return a.glucose < b.glucose;
    }
    return a.timestamp < b.timestamp;
  });
  return out;
}

double median_positive_interval_cpp(const std::vector<TimedGlucose>& x) {
  if (x.size() < 2) {
    return NA_REAL;
  }
  std::vector<double> diffs;
  diffs.reserve(x.size() - 1);
  for (size_t i = 1; i < x.size(); ++i) {
    const double diff = x[i].timestamp - x[i - 1].timestamp;
    if (std::isfinite(diff) && diff > 0.0) {
      diffs.push_back(diff);
    }
  }
  if (diffs.empty()) {
    return NA_REAL;
  }
  const size_t mid = diffs.size() / 2;
  std::nth_element(diffs.begin(), diffs.begin() + mid, diffs.end());
  double median = diffs[mid];
  if (diffs.size() % 2 == 0) {
    std::nth_element(diffs.begin(), diffs.begin() + mid - 1, diffs.end());
    median = (median + diffs[mid - 1]) / 2.0;
  }
  return median;
}

std::vector<double> lag_differences_by_time_cpp(
  const std::vector<TimedGlucose>& x,
  double lag_seconds,
  double tolerance_seconds,
  bool absolute_values
) {
  std::vector<double> diffs;
  if (x.size() < 2 || !std::isfinite(lag_seconds) || lag_seconds <= 0.0 ||
      !std::isfinite(tolerance_seconds) || tolerance_seconds < 0.0) {
    return diffs;
  }

  std::vector<double> times;
  times.reserve(x.size());
  for (size_t i = 0; i < x.size(); ++i) {
    times.push_back(x[i].timestamp);
  }

  for (size_t i = 0; i < x.size(); ++i) {
    const double target = x[i].timestamp + lag_seconds;
    const double lower = target - tolerance_seconds;
    const double upper = target + tolerance_seconds;
    std::vector<double>::const_iterator it = std::lower_bound(times.begin(), times.end(), lower);
    int best_index = -1;
    double best_distance = std::numeric_limits<double>::infinity();
    for (; it != times.end() && *it <= upper; ++it) {
      const int candidate = static_cast<int>(it - times.begin());
      if (candidate == static_cast<int>(i)) {
        continue;
      }
      const double distance = std::abs(*it - target);
      if (distance < best_distance) {
        best_distance = distance;
        best_index = candidate;
      }
    }
    if (best_index >= 0) {
      double diff = x[best_index].glucose - x[i].glucose;
      if (absolute_values) {
        diff = std::abs(diff);
      }
      if (std::isfinite(diff)) {
        diffs.push_back(diff);
      }
    }
  }
  return diffs;
}

double mean_abs_diff_by_time_cpp(
  const std::vector<TimedGlucose>& x,
  double lag_seconds,
  double tolerance_seconds
) {
  std::vector<double> diffs = lag_differences_by_time_cpp(x, lag_seconds, tolerance_seconds, true);
  return mean_cpp(diffs);
}

double mage_cpp(const std::vector<double>& x) {
  if (x.size() < 4) {
    return NA_REAL;
  }
  const double threshold = sd_cpp(x);
  if (!std::isfinite(threshold) || threshold <= 0.0) {
    return NA_REAL;
  }

  std::vector<double> extrema;
  extrema.reserve(x.size());
  for (size_t i = 1; i + 1 < x.size(); ++i) {
    const bool peak = x[i] >= x[i - 1] && x[i] > x[i + 1];
    const bool trough = x[i] <= x[i - 1] && x[i] < x[i + 1];
    if (peak || trough) {
      extrema.push_back(x[i]);
    }
  }
  if (extrema.size() < 2) {
    return NA_REAL;
  }

  double total = 0.0;
  int count = 0;
  for (size_t i = 1; i < extrema.size(); ++i) {
    const double excursion = std::abs(extrema[i] - extrema[i - 1]);
    if (std::isfinite(excursion) && excursion >= threshold) {
      total += excursion;
      ++count;
    }
  }
  if (count == 0) {
    return NA_REAL;
  }
  return total / static_cast<double>(count);
}

std::vector<double> thin_evenly(const std::vector<double>& x, int max_points) {
  if (max_points < 1 || static_cast<int>(x.size()) <= max_points) {
    return x;
  }
  std::vector<double> out;
  out.reserve(max_points);
  const double step = static_cast<double>(x.size() - 1) / static_cast<double>(max_points - 1);
  int previous = -1;
  for (int i = 0; i < max_points; ++i) {
    int index = static_cast<int>(std::round(i * step));
    index = std::max(0, std::min(index, static_cast<int>(x.size()) - 1));
    if (index != previous) {
      out.push_back(x[index]);
      previous = index;
    }
  }
  return out;
}

std::vector<double> coarse_grain(const std::vector<double>& x, int scale) {
  std::vector<double> out;
  if (scale < 1) {
    return out;
  }
  const int n = static_cast<int>(x.size()) / scale;
  out.reserve(n);
  for (int i = 0; i < n; ++i) {
    double total = 0.0;
    for (int j = 0; j < scale; ++j) {
      total += x[i * scale + j];
    }
    out.push_back(total / static_cast<double>(scale));
  }
  return out;
}

double sample_entropy_cpp(const std::vector<double>& x, int m, double tolerance) {
  const int n = static_cast<int>(x.size());
  if (m < 1 || n <= m + 1 || !std::isfinite(tolerance) || tolerance <= 0.0) {
    return NA_REAL;
  }

  double matches_m = 0.0;
  double matches_m1 = 0.0;
  for (int i = 0; i <= n - m - 1; ++i) {
    for (int j = i + 1; j <= n - m - 1; ++j) {
      double max_diff_m = 0.0;
      for (int k = 0; k < m; ++k) {
        max_diff_m = std::max(max_diff_m, std::abs(x[i + k] - x[j + k]));
        if (max_diff_m > tolerance) {
          break;
        }
      }
      if (max_diff_m <= tolerance) {
        matches_m += 1.0;
        if (std::abs(x[i + m] - x[j + m]) <= tolerance) {
          matches_m1 += 1.0;
        }
      }
    }
  }

  if (matches_m <= 0.0 || matches_m1 <= 0.0) {
    return NA_REAL;
  }
  return -std::log(matches_m1 / matches_m);
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

// [[Rcpp::export]]
List dfa_details_cpp(NumericVector x) {
  std::vector<double> values = finite_values(x);
  const int n = static_cast<int>(values.size());
  if (n < 32 || !has_variation(values)) {
    return List::create(
      Named("value") = NA_REAL,
      Named("scale_value") = NumericVector(0),
      Named("metric_value") = NumericVector(0)
    );
  }

  double mean_value = 0.0;
  for (int i = 0; i < n; ++i) {
    mean_value += values[i];
  }
  mean_value /= n;

  std::vector<double> y(n);
  double running = 0.0;
  for (int i = 0; i < n; ++i) {
    running += values[i] - mean_value;
    y[i] = running;
  }

  const double start_log = std::log(8.0);
  const double end_log = std::log(std::floor(n / 4.0));
  std::vector<int> windows;
  int previous = -1;
  for (int i = 0; i < 8; ++i) {
    const double fraction = i / 7.0;
    const int window = static_cast<int>(std::floor(std::exp(start_log + (end_log - start_log) * fraction)));
    if (window >= 8 && window < n / 2.0 && window != previous) {
      windows.push_back(window);
      previous = window;
    }
  }

  std::vector<double> scale_values;
  std::vector<double> fluctuation_values;
  for (size_t w = 0; w < windows.size(); ++w) {
    const int window = windows[w];
    const int segments = n / window;
    if (segments < 2) {
      continue;
    }

    double rms_sum = 0.0;
    int rms_count = 0;
    for (int segment = 0; segment < segments; ++segment) {
      const double rms = rms_detrended_linear_cpp(y, segment * window, window);
      if (std::isfinite(rms)) {
        rms_sum += rms * rms;
        ++rms_count;
      }
    }
    if (rms_count > 0) {
      const double fluctuation = std::sqrt(rms_sum / rms_count);
      if (std::isfinite(fluctuation) && fluctuation > 0.0) {
        scale_values.push_back(static_cast<double>(window));
        fluctuation_values.push_back(fluctuation);
      }
    }
  }

  double value = NA_REAL;
  if (scale_values.size() >= 2) {
    std::vector<double> log_scale(scale_values.size());
    std::vector<double> log_fluctuation(fluctuation_values.size());
    for (size_t i = 0; i < scale_values.size(); ++i) {
      log_scale[i] = std::log(scale_values[i]);
      log_fluctuation[i] = std::log(fluctuation_values[i]);
    }
    value = ols_slope_cpp(log_scale, log_fluctuation);
  }

  return List::create(
    Named("value") = value,
    Named("scale_value") = wrap(scale_values),
    Named("metric_value") = wrap(fluctuation_values)
  );
}

// [[Rcpp::export]]
List higuchi_details_cpp(NumericVector x, int kmax = 8) {
  std::vector<double> values = finite_values(x);
  const int n = static_cast<int>(values.size());
  if (kmax < 2) {
    kmax = 8;
  }
  if (n < kmax * 2 || !has_variation(values)) {
    return List::create(
      Named("value") = NA_REAL,
      Named("scale_value") = NumericVector(0),
      Named("metric_value") = NumericVector(0)
    );
  }

  std::vector<double> k_values;
  std::vector<double> lengths;
  for (int k = 1; k <= kmax; ++k) {
    double length_sum = 0.0;
    int length_count = 0;
    for (int m = 1; m <= k; ++m) {
      int point_count = 0;
      double curve_length = 0.0;
      int previous_index = -1;
      for (int index = m - 1; index < n; index += k) {
        if (previous_index >= 0) {
          curve_length += std::abs(values[index] - values[previous_index]);
        }
        previous_index = index;
        ++point_count;
      }
      if (point_count >= 2) {
        const double scaled = curve_length * (n - 1.0) / ((point_count - 1.0) * k);
        if (std::isfinite(scaled)) {
          length_sum += scaled;
          ++length_count;
        }
      }
    }
    if (length_count > 0) {
      const double mean_length = length_sum / length_count;
      if (std::isfinite(mean_length) && mean_length > 0.0) {
        k_values.push_back(static_cast<double>(k));
        lengths.push_back(mean_length);
      }
    }
  }

  double value = NA_REAL;
  if (k_values.size() >= 2) {
    std::vector<double> log_inverse_k(k_values.size());
    std::vector<double> log_lengths(lengths.size());
    for (size_t i = 0; i < k_values.size(); ++i) {
      log_inverse_k[i] = std::log(1.0 / k_values[i]);
      log_lengths[i] = std::log(lengths[i]);
    }
    value = ols_slope_cpp(log_inverse_k, log_lengths);
  }

  return List::create(
    Named("value") = value,
    Named("scale_value") = wrap(k_values),
    Named("metric_value") = wrap(lengths)
  );
}

// [[Rcpp::export]]
List optional_lag_metrics_by_time_cpp(NumericVector timestamp, NumericVector glucose, double tolerance_seconds = NA_REAL) {
  std::vector<TimedGlucose> values = finite_timed_glucose(timestamp, glucose);
  double tolerance = tolerance_seconds;
  const double median_interval = median_positive_interval_cpp(values);
  if (!std::isfinite(tolerance) || tolerance < 0.0) {
    tolerance = std::isfinite(median_interval) ? std::max(60.0, median_interval * 0.51) : NA_REAL;
  }

  const double conga_12h = sd_cpp(lag_differences_by_time_cpp(values, 12.0 * 60.0 * 60.0, tolerance, false));
  const double conga_24h = sd_cpp(lag_differences_by_time_cpp(values, 24.0 * 60.0 * 60.0, tolerance, false));
  const double modd = mean_abs_diff_by_time_cpp(values, 24.0 * 60.0 * 60.0, tolerance);

  return List::create(
    Named("conga_12h") = conga_12h,
    Named("conga_24h") = conga_24h,
    Named("modd") = modd,
    Named("tolerance_seconds") = tolerance,
    Named("median_interval_seconds") = median_interval
  );
}

// [[Rcpp::export]]
List optional_metrics_cpp(NumericVector glucose, double interval_minutes = NA_REAL) {
  std::vector<double> positive = finite_positive_values(glucose);
  const double avg = mean_cpp(positive);
  const double sd = sd_cpp(positive);

  double lbgi_total = 0.0;
  double hbgi_total = 0.0;
  for (size_t i = 0; i < positive.size(); ++i) {
    const double f = 1.509 * (std::pow(std::log(positive[i]), 1.084) - 5.381);
    const double risk = 10.0 * f * f;
    if (f < 0.0) {
      lbgi_total += risk;
    } else if (f > 0.0) {
      hbgi_total += risk;
    }
  }

  const double count = static_cast<double>(positive.size());
  const double lbgi = count > 0.0 ? lbgi_total / count : NA_REAL;
  const double hbgi = count > 0.0 ? hbgi_total / count : NA_REAL;
  const double j_index = std::isfinite(avg) && std::isfinite(sd) ? 0.001 * std::pow(avg + sd, 2.0) : NA_REAL;
  const double mage = mage_cpp(positive);

  double conga_12h = NA_REAL;
  double conga_24h = NA_REAL;
  double modd = NA_REAL;
  if (std::isfinite(interval_minutes) && interval_minutes > 0.0) {
    const int lag_12h = static_cast<int>(std::round(12.0 * 60.0 / interval_minutes));
    const int lag_24h = static_cast<int>(std::round(24.0 * 60.0 / interval_minutes));
    conga_12h = lag_sd_cpp(glucose, lag_12h);
    conga_24h = lag_sd_cpp(glucose, lag_24h);
    modd = lag_mean_abs_diff_cpp(glucose, lag_24h);
  }

  return List::create(
    Named("conga_12h") = conga_12h,
    Named("conga_24h") = conga_24h,
    Named("modd") = modd,
    Named("lbgi") = lbgi,
    Named("hbgi") = hbgi,
    Named("j_index") = j_index,
    Named("mage") = mage
  );
}

// [[Rcpp::export]]
List mse_scales_cpp(
  NumericVector x,
  int scale_max = 5,
  int embedding_dimension = 2,
  double tolerance = 0.15,
  int max_points = 1000
) {
  std::vector<double> values = thin_evenly(finite_values(x), max_points);
  if (scale_max < 1) {
    scale_max = 5;
  }
  if (embedding_dimension < 1) {
    embedding_dimension = 2;
  }

  std::vector<double> scales;
  std::vector<double> entropies;
  for (int scale = 1; scale <= scale_max; ++scale) {
    std::vector<double> coarse = coarse_grain(values, scale);
    if (static_cast<int>(coarse.size()) <= embedding_dimension + 1 || !has_variation(coarse)) {
      continue;
    }
    const double coarse_sd = sd_cpp(coarse);
    const double r = tolerance * coarse_sd;
    const double entropy = sample_entropy_cpp(coarse, embedding_dimension, r);
    if (std::isfinite(entropy)) {
      scales.push_back(static_cast<double>(scale));
      entropies.push_back(entropy);
    }
  }

  return List::create(
    Named("Scale") = wrap(scales),
    Named("SampleEntropy") = wrap(entropies)
  );
}
