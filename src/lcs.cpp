#include <Rcpp.h>
using namespace Rcpp;

String longest_common_substring_single(String str1, String str2) {
  std::string a = str1;
  std::string b = str2;
  int r = a.length();
  int n = b.length();
  std::vector<std::vector<int> > table(r, std::vector<int>(n));
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
//' @details
//' Implemented in C++. The algorithm runs in \code{O(n*r)} time where \code{n} 
//' and \code{r} are the lengths of the two strings.
//' 
//' Substrings are required to be a contiguous sequence of characters within 
//' the two strings. For subsequences, which allow discontiguity, use 
//' \code{\link[lcs]{longest_common_subsequence}}.
//'
//' @param str1 A character vector
//' @param str2 A character vector
//' @return A character vector of the same length as \code{str1} and 
//'   \code{str2}.
//' @examples
//' longest_common_substring('abcdefgh', 'axxxbcdexxx')
//' @export
// [[Rcpp::export]]
StringVector longest_common_substring(StringVector str1, StringVector str2) {
  if (str1.length() != str2.length())
    stop("str1 and str2 must be the same length.");
  
  return mapply(str1, str2, longest_common_substring_single);
}

String longest_common_subsequence_single(String str1, String str2) {
  std::string a = str1;
  std::string b = str2;
  int r = a.length();
  int n = b.length();
  std::vector<std::vector<int> > table(r + 1, std::vector<int>(n + 1));
  
  for (int i = 0; i < r; i++)
    table[i][0] = 0;

  for (int j = 0; j < n; j++)
    table[0][j] = 0;

  for (int i = 1; i <= r; i++) {
    for (int j = 1; j <= n; j++)
    {
      if (a[i - 1] == b[j - 1])
        table[i][j] = table[i - 1][j - 1] + 1;
      else
        table[i][j] = std::max(table[i][j - 1], table[i - 1][j]);
    }
  }
  
  int index = table[r][n];
  std::string result(index, ' ');
  int i = r;
  int j = n;
  while (i > 0 && j > 0) { 
    if (a[i - 1] == b[j - 1]) { 
      result[index - 1] = a[i - 1];
      i--;
      j--;
      index--;
    }
    else if (table[i - 1][j] > table[i][j - 1]) {
      i--; 
    }
    else {
      j--;
    }
  }

  return result;
}

//' Longest common subsequence
//' 
//' Find the longest common subsequence of two strings.
//' 
//' @details
//' Implemented in C++. The algorithm runs in \code{O(n*r)} time where \code{n} 
//' and \code{r} are the lengths of the two strings.
//' 
//' Subsequences, unlike substrings, are not required to be a contiguous 
//' sequence of characters within the two strings. For substrings, use 
//' \code{\link[lcs]{longest_common_substring}}.
//'
//' @param str1 A character vector
//' @param str2 A character vector
//' @return A character vector of the same length as \code{str1} and 
//'   \code{str2}.
//' @examples
//' longest_common_subsequence('abcdefgh', 'axxxbcdexxx')
//' @export
// [[Rcpp::export]]
StringVector longest_common_subsequence(StringVector str1, StringVector str2) {
  if (str1.length() != str2.length())
    stop("str1 and str2 must be the same length.");
  
  return mapply(str1, str2, longest_common_subsequence_single);
}
