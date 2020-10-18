#include <Rcpp.h>
using namespace Rcpp;

String longest_common_substring_single(String str1, String str2) {
  std::string a = str1;
  std::string b = str2;
  int r = a.length();
  int n = b.length();
  std::vector<std::vector<int> > table(r , std::vector<int>(n));
  int z = 0;
  std::string result;
  
  for (int i = 0; i < r; ++i) {
    for (int j = 0; j < n; ++j) {
      if (a[i] == b[j]) {
        if (i == 0 || j == 0)
          table[i][j] = 1;
        else
          table[i][j] = table[i - 1][j - 1] + 1;
        
        if (table[i][j] > z) {
          z = table[i][j];
          result = a.substr(i - z + 1, z);
        }
        else if (table[i][j] == z) {
          result = result + a.substr(i - z + 1, z);
        }
      }
      else {
        table[i][j] = 0;
      }
    }
  }
  
  return result;
}

//' Longest common substring
//' 
//' Find the longest common substring of two strings.
//' 
//' Implemented in C++. The algorithm runs in \code{O(n*r)} time where \code{n} 
//' and \code{r} are the lengths of the two strings.
//'
//' @param str1 A character vector
//' @param str2 A character vector
//' @return A character vector of the same length as \code{str1} and 
//'   \code{str2}.
//' @export
// [[Rcpp::export]]
StringVector longest_common_substring(StringVector str1, StringVector str2) {
  if (str1.length() != str2.length())
    stop("str1 and str2 must be the same length.");
  
  return mapply(str1, str2, longest_common_substring_single);
}
