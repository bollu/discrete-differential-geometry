#include <stdlib.h>
#include <string.h>
#include <vector>
#include <iostream>
using namespace std;


using Mat = int*;
using Vec = int*;

struct Manifold {
    int dim;
    // `dim` sizes
    vector<int> sizes;
    // `dim-1` boundary operators
    vector<Mat> boundaries;
};

// A chain on a manifold
struct Chain {
    Manifold &m;
    int dim; // dimension of the chain
    Vec coeffs; // coefficients of cells
};

struct Form {
    Manifold &m;
    // dimension of form, ie, what dimension of chain
    int dim; 
    double *v; // valuations of the manifold, at index i
    int nintegrate; // number of times this form has been integrated.

    Form (const Form &other) : m(other.m), dim(other.dim), nintegrate(other.nintegrate) {
        const int nv = m.sizes[dim];
        v = new double[nv];
        memcpy(v, other.v, nv * sizeof(double));
    }

    Form integrate() {
        Form fi(*this);
        fi.nintegrate++;
        return fi;
    }
};

double evaluateForm(Chain &c, Form &f) {
}

int main() {
}
