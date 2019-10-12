#include <Rcpp.h> 
#include <vector>
#include <math.h>
using namespace Rcpp;
using namespace std;


//[[Rcpp::export]]
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