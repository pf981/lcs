#include <Rcpp.h>
using namespace Rcpp;

String longest_common_substring_single(String str1, String str2) {
  return String();
}

// [[Rcpp::export]]
StringVector longest_common_substring(StringVector str1, StringVector str2) {
  if (str1.length() != str2.length()) {
    stop("str1 and str2 must be the same length.");
  }
  
  return mapply(str1, str2, longest_common_substring_single);
}
