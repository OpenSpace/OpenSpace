/*
Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files(the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and / or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions :

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT.IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
 */

#include <stdio.h>
#include <math.h>
#include <modules/touch/ext/levmarq.h>

#define TOL 1e-30 // smallest value allowed in cholesky_decomp()


// set parameters required by levmarq() to default values 
void levmarq_init(LMstat *lmstat) {
	lmstat->verbose = 0;
	lmstat->max_it = 5000;
	lmstat->init_lambda = 1e-6;
	lmstat->up_factor = 10;
	lmstat->down_factor = 10;
	lmstat->target_derr = 1e-12;
}


/* 
perform least-squares minimization using the Levenberg-Marquardt algorithm.  
The arguments are as follows:
   npar    number of parameters
   par     array of parameters to be varied
   ny      number of measurements to be fit
   y       array of measurements
   dysq    array of error in measurements, squared
           (set dysq=NULL for unweighted least-squares)
   func    function to be fit
   grad    gradient of "func" with respect to the input parameters
   fdata   pointer to any additional data required by the function
   lmstat  pointer to the "status" structure, where minimization parameters
           are set and the final status is returned.

Before calling levmarq, several of the parameters in lmstat must be set.
For default values, call levmarq_init(lmstat).
*/
bool levmarq(int npar, double *par, int ny, double *dysq,
	    double (*func)(double *, int, void *),
	    void (*grad)(double *, double *, int, void *),
	    void *fdata, LMstat *lmstat) {

	int x, i, j, it, nit, ill, verbose;
	double lambda, up, down, mult, weight, err, newerr, derr, target_derr;

	// allocate the arrays
	double** h = new double*[npar];
	double** ch = new double*[npar];
	for (int i = 0; i < npar; i++) {
		h[i] = new double[npar];
		ch[i] = new double[npar];
	}
	double* g = new double[npar];
	double* d = new double[npar];
	double* delta = new double[npar];
	double* newpar = new double[npar];

	verbose = lmstat->verbose;
	nit = lmstat->max_it;
	lambda = lmstat->init_lambda;
	up = lmstat->up_factor;
	down = 1/lmstat->down_factor;
	target_derr = lmstat->target_derr;
	weight = 1;
	derr = newerr = 0; // to avoid compiler warnings

	// calculate the initial error ("chi-squared")
	err = error_func(par, ny, dysq, func, fdata);

	// main iteration
	for (it = 0; it < nit; it++) {
		// calculate the approximation to the Hessian and the "derivative" d
		for (i = 0; i < npar; i++) {
			d[i] = 0;
			for (j = 0; j <= i; j++)
				h[i][j] = 0;
		}
		for (x = 0; x < ny; x++) {
			if (dysq) 
				weight = 1/dysq[x]; // for weighted least-squares
			grad(g, par, x, fdata);
			for (i = 0; i < npar; i++) {
				d[i] += (0.0 - func(par, x, fdata)) * g[i] * weight; //(y[x] - func(par, x, fdata)) * g[i] * weight;
				for (j = 0; j <= i; j++)
					h[i][j] += g[i] * g[j] * weight;
			}
		}
		// make a step "delta."  If the step is rejected, increase lambda and try again
		mult = 1 + lambda;
		ill = 1; // ill-conditioned?
		while (ill && (it < nit)) {
			for (i = 0; i < npar; i++)
				h[i][i] = h[i][i]*mult;
			ill = cholesky_decomp(npar, ch, h);
			if (!ill) {
				solve_axb_cholesky(npar, ch, delta, d);
				for (i = 0; i < npar; i++)
					newpar[i] = par[i] + delta[i];
				newerr = error_func(newpar, ny, dysq, func, fdata);
				derr = newerr - err;
				ill = (derr > 0);
			} 
			if (verbose) {
				printf("it = %4d,   lambda = %10g,   err = %10g,   derr = %10g (%d)\n", it, lambda, err, derr, !(newerr > err));
			}
			if (ill) {
				mult = (1 + lambda * up) / (1 + lambda);
				lambda *= up;
				it++;
			}
		}
		for (i = 0; i < npar; i++)
			par[i] = newpar[i];
		err = newerr;
		lambda *= down;

		if ((!ill) && (-derr < target_derr)) 
			break;
	}

	lmstat->final_it = it;
	lmstat->final_err = err;
	lmstat->final_derr = derr;

	// deallocate the arrays
	for (int i = 0; i < npar; i++) {
		delete[] h[i];
		delete[] ch[i];
	}
	delete[] h;
	delete[] ch;
	delete[] g;
	delete[] d;
	delete[] delta;
	delete[] newpar;

	return (it != lmstat->max_it);
}


// calculate the error function (chi-squared)
double error_func(double *par, int ny, double *dysq,
		double (*func)(double *, int, void *), void *fdata) {
	int x;
	double res, e = 0;

	for (x = 0; x < ny; x++) {
		res = func(par, x, fdata);
		if (dysq) // weighted least-squares 
			e += res*res/dysq[x];
		else
			e += res*res;
	}
	return e;
}


// solve Ax=b for a symmetric positive-definite matrix A using the Cholesky decomposition A=LL^T, L is passed in "l", elements above the diagonal are ignored.
void solve_axb_cholesky(int n, double** l, double* x, double* b) { // n = npar, l = ch, x = delta (solution), b = d (func(par, x, fdata) * g[i]);
	int i,j;
	double sum;
	// solve L*y = b for y (where x[] is used to store y)
	for (i = 0; i < n; i++) {
		sum = 0;
		for (j = 0; j < i; j++)
			sum += l[i][j] * x[j];
		x[i] = (b[i] - sum) / l[i][i]; 
	}
	// solve L^T*x = y for x (where x[] is used to store both y and x)
	for (i = n-1; i >= 0; i--) {
		sum = 0;
		for (j = i+1; j < n; j++)
			sum += l[j][i] * x[j];
		x[i] = (x[i] - sum) / l[i][i];
	}
}



// symmetric, positive-definite matrix "a" and returns its (lower-triangular) Cholesky factor in "l", if l=a the decomposition is performed in place, elements above the diagonal are ignored.
int cholesky_decomp(int n, double** l, double** a) {
	int i,j,k;
	double sum;
	for (i = 0; i < n; i++) {
		for (j = 0; j < i; j++) {
			sum = 0;
			for (k = 0; k < j; k++)
				sum += l[i][k] * l[j][k];
			l[i][j] = (a[i][j] - sum) / l[j][j];
		}
		sum = 0;
		for (k = 0; k < i; k++)
			sum += l[i][k] * l[i][k];
		sum = a[i][i] - sum;
		if (sum < TOL) 
			return 1; // not positive-definite
		l[i][i] = sqrt(sum);
	}
	return 0;
}
