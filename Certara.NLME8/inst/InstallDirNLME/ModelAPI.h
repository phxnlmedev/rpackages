// ModelAPI.h

#ifndef MODEL_API_H
#define MODEL_API_H

extern "C"
{
#include "ModelAPI/OpCode.h"
#include "ModelAPI/ModelCallbackTable.h"
#include "ModelAPI/plot.h"
#include "ModelAPI/delay.h"
#include "ModelAPI/fixef.h"
#include "ModelAPI/ranef.h"
#include "ModelAPI/packing.h"
#include "ModelAPI/covariate.h"
#include "ModelAPI/CV.h"
#include "ModelAPI/SAEM.h"

double lnorm(double x, double stdev);

// Methods defined in Model.cpp
void GetSecondaryName(int * _pi, char * _nm);
void GetSecondary(int * n, double * sec);
void InitEnable();	// Initialize Enablement
void SetUsingGaussianAlgorithm(int * _pBool);
int GetNThetaOld();	// get number of theta minus possibly 1 sigma to estimate
int GetNTheta();	// get number of theta to estimate
int GetNSigma();	// get number of sigma to estimate, 0 or 1
void GetErrorName(int * _pi, char * _nm);
void SetArgsOK(double ** __args, int * __pbOK, int * __piState);
bool IsOK();
void ClearUnits();
void SetVarUnits(const char * _nm, const char * _units);
int GetStripDoseName(char * _nm);
int GetNumStructural();
int GetStructuralNumber(const char * _nm);
void GetStructuralName(int * _pi, char * _nm);
void GetVarUnits(const char * _nm, char * _units);
void GetThetaLimits(double * _fixefLow, double * _fixefHigh);
void SetMainSigma(double * _sig);
double GetObsSigma(int * _piWhichObs);
void GetJacobianInfo(int * _bUseAnagrad, int * _piJacobianInfo);
void GetMaxODELevel(int * _bUseAnagrad, int * _piMaxODELevel);
int GetNumCompartments();
int GetCompartmentNumber(const char * _nm);
void GetCompartmentName(int * _pi, char * _nm);
int GetVarValue(const char * _nm, double * _pv, double zzY[]);
void SetVarValue(const char * _nm, double * _pv, double zzY[]);
void DoEndObsActions(double zzY[]);
int GetNIntegGradientDeriv();
void GetLLGradientEta(
    int * _pnObs
    , const unsigned char * *ppb
    , const SubjectId & subjId
    , double _dLL[]
    , double _dGrad[]
);
void NewPredOneSubject(
    int * _pnObs
    , const unsigned char * *ppb
    , const SubjectId & subjId
    , int * _pbOK
    , int * _nPrediction
    , double * __daXPrediction
    , double * __taLastDose
    , int  * __iaWhichDose
    , int  * __iaWhichReset
    , double * __daYPrediction
    , double * __daYObservation
    , double * __daVarianceFunction
    , double * _dYdK // comes out as _daYdK(iEta, iObs)
    , double * _dSdK // comes out as _daSdK(iEta, iObs)
    , int * _iaWhichObs
);
int GetNumObservations();
int GetObservationNumber(const char * _nm);
const char * GetObservationName(int * _pi);
int GetObservationType(int * _pi);
void GetObservationBqlLimit(int * _pi, double * _bqllimit);
const char * GetObsSigmaName(int* _piWhichObs);
int GetPredObsError(int _iObs);
double GetPredVFCorrectionFactor(int _iError);
void GetPredName(int * _iWhichObs, char * _nm);
void GetObsName(int * _iWhichObs, char * _nm);
void GetIVarName(int * _iWhichObs, char * _nm);
void GetObsVF(int * _iWhichObs, double * _vf, double zzY[]);
void GetNumGaussianObs(int * _nGaussianObs);
void GetNumCategories(const char * _nm, int * pNCat);

void SimulateForSimTbl(double _t, const char * _nm, double * _pObs, double * _pPred, double zzY[]);

void InitDelays(double _t);
void InitCF(double _t);
void ReInitCF(double _t);
void SetTimeOfStateCF(double _t);
void SetTimeOfGetValCF(double _t);

void ResetSubject();
void ResetTime();
void ResetCF();

void ScheduleBolus1(int _mark, double _t, int _iCpt, double _amt, int _addl);
void ScheduleBolus2(int _mark, double _t, int _iCpt, double _amt, int _addl);
void ScheduleIV1(int _mark, double _t, int _iCpt, double _amt, double _rate, int _addl);
void ScheduleIV2(int _mark, double _t, int _iCpt, double _amt, double _rate, int _addl);

void PerformBolus1(double * _pt, double zzY[], double zzR[], int _iCpt, double _dv, double _rate);
void PerformBolus2(double * _pt, double zzY[], double zzR[], int _iCpt, double _dv, double _rate);

void PerformInfStrt1(double * _pt, double zzY[], double zzR[], double zzIRate[], int _iCpt, double _dv);
void PerformInfStrt2(double * _pt, double zzY[], double zzR[], double zzIRate[], int _iCpt, double _dv);

void PerformInfEnd1(double * _pt, double zzY[], double zzR[], double zzIRate[], int _iCpt, double _dv);
void PerformInfEnd2(double * _pt, double zzY[], double zzR[], double zzIRate[], int _iCpt, double _dv);

// clear all integrators (at start of subject)
void InitSubject();

// restart subject sequence state machines
void RestartSubjectSequences(double* zzY);
void RestartSubjectEventSims(double* zzY);

// do miscellaneous actions at end of subject
void EndSubject(int _bSimulating);

// recompute all elements of the jacobian matrix whenever covariates change
void SetJacobian();

// set value of named covariate
void SetCovariate(double _t, const char* _nm, double _v);
// given current parameters and state, compute various variables and check for in bounds
void EvalGroup(double* zzY);
void EvalSParms(double* zzY);
void EvalAssign(double* zzY);

// set model time variable, if it is a time-based model
void SetModelTime(double _t);

// given fixed effects, compute secondary parameters
void CalcSecondary();

// advance the state of the accumulators for given interval of time
void Advance(int bGenGradients, int _iODELevel, double *_pTimeOuter, double _dt, int _bTimeAdvances, double zzY[]);

// retrieve/set values of CF machines
void GetAllCFState(double *_y);
void SetAllCFState(double *_y);

void GetLL(int bGenGradients, double _dLL[], double _dGrad[], double zzY[], double zzIRate[], int *_pnObs, double _t, const char* _nm, double _amt, int _bBql);
void NewGetLL(int bGenGradients, double _dLL[], double _dGrad[], double zzY[], double zzIRate[], int *_pnObs, double _t, const char* _nm, double _amt, int _bBql);
void ClearSSIrrelevantState(double* _y);
void Deriv(long* _pN, double* _pt, double zzY[], double zzR[], double zzIRate[]);
void GradientDeriv(long* _pN, double* _pt, double zzY[], double zzR[], double zzIRate[]);
void Jacobian(int* pN, double* pt, double zzY[], double zzIRate[], int* pML, int* pMU, double* _jacobian, int* pNRJ);
void GradientJacobian(int* pN, double* pt, double zzY[], double zzIRate[], int* pML, int* pMU, double* _gjacobian, int* pNRJ);
void AddIncrements(double zzY[], double ta, double tb);
void GetPred(int *_pnObs, double zzY[], double zzIRate[], double _t, const char* _nm, double _amt);
void Simulate(double _t, const char* _nm, double _v1, int _bBql, int bSampleEpsilon, int bForVPC, double zzY[], double zzIRate[]);
void GetNumCategories(const char* _nm, int* pNCat);
void NewGetPred(int bGenGradients, int* pnObs, double zzY[], double zzIRate[], double _t, const char* _nm, double _v1);

double unif();
double norm();

int  GetNFixef();
int  GetNSecondary();
int  GetNError();
int  GetNRanef1();
int  GetNRanef2();
int  GetNRanef3();
int  GetNRanef4();
int  GetNRanef5();
int  GetInSS();
int  MultiLevelRanef();
int  AllowGaussianFit();
int  AllowLogTransform();
int  TrulyTimeBased();
int  ModelUniqueId();
int  GetNderiv();
int  GetNurine();
int  GetNtransit2();
int  GetNtransit();
int  GetNevent();
int  GetNintegAll();
int  GetNcfmacro();

extern ModelCallbackTable * _pcb;
}

#endif	// MODEL_API_H
