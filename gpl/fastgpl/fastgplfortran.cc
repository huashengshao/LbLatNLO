
#include <iostream>
#include "FastGPL.h"
#include <vector>

using namespace std;
using namespace FastGPL;


extern "C" {

  // It is important that the function or subroutine name should be in lower cases
  // for Fortran
  complex<double> fastgpl_g_(const int & n, const complex<double> * a, const int * s, const double & x, const bool & is_clean) {
    // call GPL function G(a,s,x) available in FastGPL (2112.04122)
    // Inputs:
    // n: is an integer for the size of a and s
    // a: is a vector of complex indices
    // s: is a vector with elements being +-1, which denote the signs
    //    of the imaginary parts of the corresponding indices
    //    s(i) is relevant only when Im(a(i))=0
    // x: a non-negative real number
    
    vector<complex<double>> aindices(n);
    vector<int> svalues(n);
    for (unsigned i = 0; i < n; i++) {
      aindices[i] = *(a++);
      svalues[i] = *(s++);
    };

    return G(aindices,svalues,x,is_clean);
  }

  // special logs from FastGPL
  complex<double> fastgpl_log_(const complex<double> & x, const int & s) {

    return Log(x,s);
  }

  // special poly logs from FastGPL
  complex<double> fastgpl_polylog_(const int & n, const complex<double> & x, const int & s){
    return PolyLog(n,x,s);
  }

  // S_{2,2} from FastGPL
  complex<double> fastgpl_s22_(const complex<double> & x, const int & s){
    return S22(x,s);
  }

  // elliptic function from FastGPL
  complex<double> fastgpl_elliptick_(const complex<double> & z) {
    return EllipticK(z);
  }

  // elliptic function from FastGPL
  complex<double> fastgpl_elliptice_(const complex<double> & z) {
    return EllipticE(z);
  }
}
