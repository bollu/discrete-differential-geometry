#include <stdlib.h>
#include <string.h>
#include <vector>
#include <iostream>
using namespace std;


using Mat = int*;
using Vec = int*;

int matvec(Mat m, Vec v) {};
int vecvec(Vec v, Vec w) {};

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


    Chain (Manifold &m) : m (m), dim(-42), coeffs(nullptr) { };

    Chain boundary() {
        Chain c(m);
        assert(dim >= 1);
        c.dim = dim - 1;
        matvec(boundaries[c.dim], coeffs, c.coeffs);
        c.verify();
        return c;
    }

    void verify() {
        assert(dim >= 0);
        assert(coeffs);
    }
};

struct Form {
    Manifold &m;
    // dimension of form, ie, what dimension of chain
    int dim; 
    double *v; // valuations of the manifold, at index i
    int nintegrate; // number of times this form has been integrated.

    Form(const Form &other):m(other.m), 
        dim(other.dim),
        nintegrate(other.nintegrate) {
        const int nv = m.sizes[dim];
        v = new double[nv];
        memcpy(v, other.v, nv * sizeof(double));
    }

    Form integrate() {
        Form fi(*this);
        fi.nintegrate++;
        fi.dim++;
        return fi;
    }
};

// evaluate the form on the chain...
// eval c df = eval (boundary c) f
double evaluateForm(Chain c, Form f) {
    for(int i = 0; i < f.nintegrate; ++i) {
        c = c.boundary();
        f.dim -= 1;
    }

    assert(c.dim == f.dim);
    // f has valuations
    // c hs coefficients
    return vecvec(c.v, f.v);
}

int main() {
}
