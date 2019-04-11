#pragma once

// levels of ode solver from fastest to simplest
#define ODELEVEL_MATEXP		6	// best: matrix exponent with constant jacobian
#define ODELEVEL_SODAJAC	5	// LSODA with user-supplied jacobian
#define ODELEVEL_SODA		4	// LSODA with finite-difference jacobian
#define ODELEVEL_NONSTIFF	3	// non-stiff solver without jacobian
#define ODELEVEL_STIFFJAC	2	// stiff solver with user-supplied jacobian
#define ODELEVEL_STIFF		1	// stiff solver with finite-difference jacobian

#define ODESTATE_START		1	// value of ISTATE when subject starts
#define ODESTATE_UNDERWAY	2	// returned value of ISTATE after a successful step
#define ODESTATE_CHANGE		3	// set ISTATE to this after dose or covariate change (but only in LSODA)

typedef void   (fODESolve)(int iODELevel
    , int * piState
    , int neq
    , double * ptOuter
    , double dt
    , double y[]
    , void(*calcderiv)(long *, double *, double *, double *)
    , void(*addIncrements)(double *, double, double)
    , int bTimeAdvances
    , void(*jacobian)(int *, double *, double *, int *, int *, double *, int *)
    );

typedef int   (fIsEnabled)(int _iBitNumber);

struct ODECallbackTable
{
    fODESolve		* _pfODESolve;
    fIsEnabled    *   _pfIsEnabled;
};

void SetODECallbacks(ODECallbackTable * _p);

bool _ISENABLED(int i);

void ODESolve(int bGenGradients, int iODELevel, double *pTimeOuter, double _dt, int bTimeAdvances, double zzY[]);