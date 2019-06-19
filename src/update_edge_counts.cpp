#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void update_edge_counts(IntegerVector ids,
                        CharacterVector votes,
                        IntegerMatrix adj_mat_agree,
                        IntegerMatrix adj_mat_total){
  int n = ids.size();
  for(int i = 0; i < n - 1; ++i){
    for(int j = i + 1; j < n; ++j){
      int ix1 = ids[i] - 1;
      int ix2 = ids[j] - 1;
      std::string d1 = as<std::string>(votes[i]);
      std::string d2 = as<std::string>(votes[j]);
      bool b1 = (d1 == "POOLT" or d1 == "VASTU");
      bool b2 = (d2 == "POOLT" or d2 == "VASTU");
      if(b1 and b2){
        if(d1 == d2) adj_mat_agree(ix1, ix2) += 1;
        adj_mat_total(ix1, ix2) += 1;
      }
    }
  }
}
