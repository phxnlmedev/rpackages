#include "mutil_unused.h"

#include <math.h>

/******************************************************************************/
/*
Purpose:

POLY_VALUE evaluates a double precision polynomial.

Discussion:

For sanity's sake, the value of N indicates the NUMBER of
coefficients, or more precisely, the ORDER of the polynomial,
rather than the DEGREE of the polynomial.  The two quantities
differ by 1, but cause a great deal of confusion.

Given N and A, the form of the polynomial is:

p(x) = a[0] + a[1] * x + ... + a[n-2] * x^(n-2) + a[n-1] * x^(n-1)

Licensing:

This code is distributed under the GNU LGPL license.

Modified:

19 March 2010

Author:

John Burkardt

Parameters:

Input, int N, the order of the polynomial.

Input, double A[N], the coefficients of the polynomial.
A[0] is the constant term.

Input, double X, the point at which the polynomial is to be evaluated.

Output, double POLY_VALUE, the value of the polynomial at X.
*/
static double poly_value(int n, double a[], double x)
{
    int i;
    double value = 0;

    for (i = n - 1; 0 <= i; i--)
    {
        value = value * x + a[i];
    }

    return value;
}

/******************************************************************************/
/*
Purpose:

probit or original asa241 name R8_NORMAL_01_CDF_INVERSE inverts the standard normal CDF.

Discussion:

The result is accurate to about 1 part in 10^16.

Licensing:

This code is distributed under the GNU LGPL license.

Modified:

19 March 2010

Author:

Original FORTRAN77 version by Michael Wichura.
C version by John Burkardt.

Reference:

Michael Wichura,
The Percentage Points of the Normal Distribution,
Algorithm AS 241,
Applied Statistics,
Volume 37, Number 3, pages 477-484, 1988.

Parameters:

Input, double P, the value of the cumulative probability
densitity function.  0 < P < 1.  If P is outside this range, an "infinite"
value is returned.

Output, double R8_NORMAL_01_CDF_INVERSE, the normal deviate value
with the property that the probability of a standard normal deviate being
less than or equal to this value is P.
*/
double probit(double p)
{
    static double a[8] =
    {
        3.3871328727963666080,     1.3314166789178437745e+2,
        1.9715909503065514427e+3,  1.3731693765509461125e+4,
        4.5921953931549871457e+4,  6.7265770927008700853e+4,
        3.3430575583588128105e+4,  2.5090809287301226727e+3
    };
    static double b[8] =
    {
        1.0,                       4.2313330701600911252e+1,
        6.8718700749205790830e+2,  5.3941960214247511077e+3,
        2.1213794301586595867e+4,  3.9307895800092710610e+4,
        2.8729085735721942674e+4,  5.2264952788528545610e+3
    };
    static double c[8] =
    {
        1.42343711074968357734,     4.63033784615654529590,
        5.76949722146069140550,     3.64784832476320460504,
        1.27045825245236838258,     2.41780725177450611770e-1,
        2.27238449892691845833e-2,  7.74545014278341407640e-4
    };
    static double const1 = 0.180625;
    static double const2 = 1.6;
    static double d[8] =
    {
        1.0,                        2.05319162663775882187,
        1.67638483018380384940,     6.89767334985100004550e-1,
        1.48103976427480074590e-1,  1.51986665636164571966e-2,
        5.47593808499534494600e-4,  1.05075007164441684324e-9
    };
    static double e[8] =
    {
        6.65790464350110377720,     5.46378491116411436990,
        1.78482653991729133580,     2.96560571828504891230e-1,
        2.65321895265761230930e-2,  1.24266094738807843860e-3,
        2.71155556874348757815e-5,  2.01033439929228813265e-7
    };
    static double f[8] =
    {
        1.0,                        5.99832206555887937690e-1,
        1.36929880922735805310e-1,  1.48753612908506148525e-2,
        7.86869131145613259100e-4,  1.84631831751005468180e-5,
        1.42151175831644588870e-7,  2.04426310338993978564e-15
    };
    double q;
    double r;
    static double split1 = 0.425;
    static double split2 = 5.0;
    double value;

    if (p <= 0)
    {
        return -1e30;
    }

    if (p >= 1)
    {
        return 1e30;
    }

    q = p - 0.5;

    if (q >= -split1 && q <= split1)
    {
        r = const1 - q * q;
        value = q * poly_value(8, a, r) / poly_value(8, b, r);
    }
    else
    {
        r = (q < 0 ? p : 1 - p);

        // this should not be possible, and even if it is, exit() is a no-no
        //if (r <= 0){
        //	value = - 1;
        //	exit(1);
        //}

        r = sqrt(-log(r));

        if (r <= split2)
        {
            r = r - const2;
            value = poly_value(8, c, r) / poly_value(8, d, r);
        }
        else
        {
            r = r - split2;
            value = poly_value(8, e, r) / poly_value(8, f, r);
        }

        if (q < 0)
        {
            value = -value;
        }
    }

    return value;
}


extern int bModelAnagrad;
void SetModelAnagrad(int b);

int bModelAnagrad = 0;

void SetModelAnagrad(int b)
{
    bModelAnagrad = b;
}

#if 0
/****
TODO: When we get around to it:
How to convert from matrix exponent form to closed form.
Here's the general form of the Jacobian (not counting the infusion column)
General 3-compartment, 1st-order, with effect cpt:

A1					A2		A3		Aa		Ce
-------------------------------------------------------
A1'		|	-K10 -K13 -K12	  |	+K21  |	+K31  |	F*Ka  |		  |
-------------------------------------------------------
A2'		|	+K12			  |	-K21  |		  |		  |		  |
-------------------------------------------------------
A3'		|	+K13			  |		  |	-K31  |		  |		  |
-------------------------------------------------------
Aa'		|	-Ka				  |		  |		  |		  |		  |
-------------------------------------------------------
Ce'		|	+KCe/V1			  |		  |		  |		  |	-KCe  |
-------------------------------------------------------

1) identify peripheral compartments, in this case columns 2 & 3. (get K21 & K31) Suggesting 1 is central.
2) identify absorption compartment, int this case column 4 and row 4, suggesting central compartment, rowcol 1.
(get Ka and F)
3) identify effect compartment, in this case rowcol 4, suggesting 1 as central.
(get KCe and V1)  Confirm V1 is correct.
4) even without absorption and central cpt suggestions, the remaining rowcol is 1. (Get K10)
5) confirm all Ks are >0, F>0, V1>0, V1 = computed volume of central cpt.
6) make sure none of K10, K12, K21, K13, K31 are equal.

Simple 1-compartment, 1st-order:

A1		Aa
-------------------
A1'		|	-K10  |	F*Ka  |
-------------------
Aa'		|	-Ka	  |		  |
-------------------

In this case, rules 1 & 3 fail.

Simple 1-compartment, with effect cpt:

A1			Ce
-----------------------
A1'		|	-K10	  |		  |
-----------------------
Ce'		|	+KCe/V1	  |	-KCe  |
-----------------------

In this case, rules 1 & 2 fail.

Only do this at start of subject, and it is not valid if jacobian changes during subject.
If there is a Ce, then no other compartment may be referred to.
If there is not a Ce, then only A1 may be referred to.
Otherwise cannot use closed form.
If there is an absorption compartment, may be necessary to have 2 superimposed closed form machines, one for
vascular doses, and one for non-vascular.

****/

void MatrixExponentTaylorSeries(double inmat[], double result[], double tolerance)
{
#define N (NINTEG_ALL + 1)
    double k;
    double term[N * N];
    double temp[N * N];
    // clear the result matrix
    _MatZero(N, N, result);
    // first term is identity matrix
    _MatIdentity(N, term);

    // for k = 1, ...
    for (k = 1; k < 100; k++)
    {
        // add term to the result
        _MatAddTo(N, N, term, result);

        // see if we need more terms
        if (MatInTolerance(N, N, term, tolerance))
        {
            break;
        }

        // multiply the term by the matrix, and divide by k
        _SqMatMulTo(N, inmat, term, temp);
        _MatScaleTo(N, N, temp, term, 1.0 / k);
    }

#undef N
}

// see if this speeds things up
int _bOldMatInitialized;
#define N (NINTEG_ALL + 1)
double _oldmat[N * N];
double _oldexpmat[N * N];
#undef N

// set result = exp(jacobian * dt)
void MatrixExponent(double dt, double * result, int * pError)
{
#define N (NINTEG_ALL + 1)
    int nHalve = 0;
    double tolerance = 1e-10;
    int i;
    //	double k;
    double inmat[N * N];
    //	double term[N*N];
    double temp[N * N];
    double temp1[N * N];
    double dLargestElement = 0;
    double dScaleDown = 1;
    //	if (!_finite(_oldexpmat[0])){
    //		CallDebug(500, _oldexpmat[0]);
    //	}
    *pError = 0;
    // scale the jacobian by delta-time
    //	if (!_finite(_jacobian[0])) CallDebug(300, _jacobian[0]);
    //	if (!_finite(dt)) CallDebug(301, dt);
    _MatScaleTo(N, N, _jacobian, inmat, dt);

    //	if (!_finite(inmat[0])) CallDebug(302, inmat[0]);
    // if we have a prior matrix and result
    if (_bOldMatInitialized)  // normal code
    {
        //	if (_bOldMatInitialized && _finite(_oldexpmat[0])){ // workaround nan bug
        // if current matrix is equal to the old matrix, use the old result
        if (MatEqual(N, N, inmat, _oldmat))
        {
            _MatCopyTo(N, N, _oldexpmat, result);
            return;
        }

        // form the matrix temp = inmat - _oldmat (the difference matrix)
        //		if (!_finite(_oldmat[0])) CallDebug(303, _oldmat[0]);
        _MatCopyTo(N, N, inmat, temp);
        _MatSubFrom(N, N, _oldmat, temp);
        // see how big it is
        dLargestElement = MatLargest(N, N, temp);

        // if the difference is not too great
        if (dLargestElement < 0.1)
        {
            // form the matrix temp1 = exp(temp)
            MatrixExponentTaylorSeries(temp, temp1, tolerance);
            // form result = exp(oldmat) * exp(inmat - oldmat)
            _SqMatMulTo(N, _oldexpmat, temp1, result);

            // check for errors
            if (!_MatFinite(N, N, temp))
            {
                //				CallDebug(4981, temp[0]);
                //				CallDebug(4982, temp1[0]);
                //				CallDebug(4983, inmat[0]);
                //				CallDebug(4984, _oldmat[0]);
                //				CallDebug(4985, dLargestElement);
                *pError = 1;
                return;
            }

            // record the result
            _MatCopyTo(N, N, inmat, _oldmat);
            _MatCopyTo(N, N, result, _oldexpmat);
            _bOldMatInitialized = 1;
            return;
        }
    }

    // if not, record the mat and expmat
    _MatCopyTo(N, N, inmat, _oldmat);
    _MatZero(N, N, _oldexpmat);

    // halve the matrix until its largest element is <= 1
    dLargestElement = MatLargest(N, N, inmat);

    if (!_finite(dLargestElement))
    {
        //		CallDebug(499, dLargestElement);
        *pError = 1;
        return;
    }

    while (dLargestElement > 1)
    {
        dLargestElement *= 0.5;
        dScaleDown *= 0.5;
        nHalve++;
    }

    if (nHalve > 0)
    {
        _MatScaleTo(N, N, inmat, inmat, dScaleDown);
    }

    // run the basic taylor series
    MatrixExponentTaylorSeries(inmat, result, tolerance);

    // square the matrix the number of times that it was halved
    for (i = 0; i < nHalve; i++)
    {
        _SqMatMulTo(N, result, result, temp);
        _MatCopyTo(N, N, temp, result);
    }

    // record the result
    _MatCopyTo(N, N, result, _oldexpmat);
    _bOldMatInitialized = 1;

    //	if (!_finite(_oldexpmat[0])){
    //		CallDebug(501, _oldexpmat[0]);
    //	}
    // check for errors
    if (!_MatFinite(N, N, temp))
    {
        //		CallDebug(502, _oldexpmat[0]);
        *pError = 1;
        return;
    }

#undef  N
}

void AdvanceByMatrixExponent(double dt)
{
    int Error = 0;
    // size of matrix is number of integrators plus 1
#define N (NINTEG_ALL + 1)
    double exmat[N * N];
    double tempstate[N];
    //	if (!_MatFinite(N, N, _jacobian)){
    //		SetErrorMsg("Error: non-finite terms in differential equations");
    //		return;
    //	}
    MatrixExponent(dt, exmat, &Error);

    if (Error)
    {
        SetErrorMsg("Error: non-finite terms in differential equations");
        return;
    }

    // make sure extra element of state is 1, so that infusions get done
    zzY[NINTEG_ALL] = 1;
    // state = exmat * state
    _MatMulTo(N, N, exmat, 1, zzY, tempstate);
    memcpy(zzY, tempstate, sizeof(double)*N);
#undef  N
}

#endif