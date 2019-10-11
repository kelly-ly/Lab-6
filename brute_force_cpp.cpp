#include <Rcpp.h>
#include <vector>
#include <math.h>
using namespace Rcpp;
// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List brute_force(IntegerVector w, NumericVector v ,int W) {
  int n=sizeof(w);
  NumericVector result(0);
  int allCase =  static_cast<int>(pow(2, n));
  int maxValue = 0;
  for (int i = 0; i < allCase; i++) {
    NumericVector temp(0);
    int currentCase = i, currentWeight = 0, currentValue = 0;
    for(int j=0;j<n;j++){
      if(currentCase&1){
        currentWeight+=w[j];
        currentValue+=v[j];
        temp.push_back(j+1);
      }
      if(currentWeight>W){
        break;
      }
      currentCase=currentCase>>1;
      if(currentWeight<=W&&currentValue>maxValue){
        maxValue=currentValue;
        result=temp;
      }
    }
  }
  List L=List::create(Named("value")=maxValue,Named("element")=result);
  
  return L;
}
  
  /*
  int allCase =  static_cast<int>(pow(2, 5));
  int maxValue=0;
  for(int i=0;i<=allCase;i++){
    int currentCase=i;
    int currentWeight=0;
    int currentValue=0;
    for(int j=0;j<=sizeof(w);j++){
      if(currentCase&1){
        currentWeight+=w[j];
        currentValue+=v[j];
      }
      if(currentWeight>W){
        break;
      }
      currentCase=currentCase>>1;
    }
    if(currentWeight<=W&&currentValue>maxValue){
      maxValue=currentValue;
    }
  }
  return maxValue;
   */



