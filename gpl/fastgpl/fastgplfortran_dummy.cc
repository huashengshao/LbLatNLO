#include <complex.h>
#include <iostream>
#include <vector>

using namespace std;


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

    return 0.0+0.0i;
    
  }

}
