// MUtil.h

#include "mutil/mutil_bool.h"
#include "mutil/closed_form.h"
#include "mutil/action.h"
#include "mutil/ODE.h"
#include "mutil/Subject.h"

enum InterpMode
{
    INTERP_MODE_LL      = 0,
    INTERP_MODE_PRED,
    INTERP_MODE_SIM,
    INTERP_MODE_INIT1
};

#define _UNK 3.3e-151
#define _NA 3.3e-151

extern double curTime, curTimeLastDose;
extern int iCurWhichDose;
extern int iCurWhichReset;
extern double _dMaxTime;
extern int _bOnTrajectory;
extern int _bDroppingOut;

// table stuff
void _tableGetXPos(int _n, double _x, int * _ix0, int * _ix1, double * _xFrac, ...);
void _tableSetYVal(int _n, double * _y, int _ix0, int _ix1, double _xFrac, ...);

// delay stuff
#define COUNTOF(a) (sizeof(a)/sizeof((a)[0]))
#define CDF_TABLE_SIZE 128
#define GROW_DELAYTABLE 1024
#define MAX_DELAYTABLE (GROW_DELAYTABLE * 1024)
typedef struct _delayTableTag
{
    double delt, c, lgammc, gammc;
    int iDoseMode;
    // table of past values of expression
    int nt;
    int mxt;
    double * t;
    double * y;
    // historical y value prior to table
    double _yHist;
    // past dose functions
    int ntDose;
    int mxtDose;
    double * tDose; // time
    double * yDose; // amount
    double * rDose; // rate (or _NA)
    // gamma distribution function
    double cq;	// c for current distribution function table
    int nq;
    double q[CDF_TABLE_SIZE + 1];
    double p[CDF_TABLE_SIZE + 1];
} _delayTable_t;

void _initDelayTable(_delayTable_t * pt);
double _delayFunc(double _tt, double _y, double _delt, double _shape, double _threshold, _delayTable_t * _pt);
void _delayAddIncrements(double * py, double _ta, double _tb, double _delt, double _shape, double _threshold, _delayTable_t * _pt);
void _insertDose(double _tt, double _y, double _r, _delayTable_t * _pt);

// peak stuff
typedef struct _peakTableTag
{
    double v[10];
} _peakTable_t;

void _initPeakTable(_peakTable_t * pt);
double _peakFunc(double _tt, double _y, double _bMin, double * _cmax, _peakTable_t * _pt);

double _TransitCorrectionFactor(double ntr);
double _interpolate(double x, double x0, double y0, double x1, double y1);
double factorial(double _x);
double dLNormdEta(double _x, double _s, double _dXdEta, double _dSdEta);
double gammCDF(double x, double c, double b, double lb, double lgammc, double gammc);
double gammPDF(double x, double c, double b, double lb, double lgammc);
double lnegbin_rp(double r, double p, double y);
double lnegbin(double mean, double beta, double power, double y);
double pnegbin(double mean, double beta, double power, double y);
int rnegbin_rp(double r, double p);
int rnegbin(double mean, double beta, double power);
int test_rnegbin();
double lpois(double _mean, int _x);
double ppois(double _mean, int _x);
int rpois(double lambda);
double lphi(double _x, double _v);
double phi(double _x);
double lambertw(const double _z);
double ilogit(double _x);	// inverse logit link function
double ilogit(double _x, int l, int r);	// inverse logit link function
double iloglog(double _x);	// inverse log-log link function
double icloglog(double _x);	// inverse complementary log-log link function
double lgamm(double _x);	// log gamma function
double erfunc(double _x);	// error function
double iprobit(double _x);	// inverse probit function (gaussian distribution function)
int unifToPoisson(double _mean, double _r);	// convert uniform distribution to poisson
double CalcTMax(double A, double a, double B, double b, double C, double c);
// there's always somebody who asks why we don't have "ln", so here it is
double ln(double _x);
double logfl(double _x);
double _exp_cached(double _x, double * _tempArg, double * _tempResult);
double _log_cached(double _x, double * _tempArg, double * _tempResult);
double _ln_cached(double _x, double * _tempArg, double * _tempResult);
double _pow_cached(double _x, double _y, double * _tempArg1, double * _tempArg2, double * _tempResult);
char * __NumberString(double _d);
char * _units_Mul(const char * _a, const char * _b);
char * _units_Div(const char * _a, const char * _b);
char * _units_Exp(const char * _a);
char * _units_Log(const char * _a);
double vfwt(double _f, double _p);

void _VecAddTo(int _n, double * _a, double * _b);
void _MatMulTo(int _na, int _ma, double * _a, int _mb, double * _b, double * _c);

extern int iODELevel;

extern void * _dbg;	// pointer to debug callback routine

extern int * _piState;	// state of ODE solver

extern int bEnableLogTransform;	// turn on log transform if it's allowed
extern double _logFloor;

// routine for debugging that calls back into NLME7
void CallDebug(int i, double d);

void StartDroppingOut();

void RecordObservation(double _dXPred, double _tLastDose, int _iWhichDose, int _iWhichReset, double _dYPred, double _dYObs, double _dVFunc, int _iWhichObs);
void RecordObservationGradient(double _dYdK, double _dSdK, int _iWhichK);

void SetErrorMsg(const char*);
void ClearErrorMsg();
void SetErrorMessageWithParams(const char*);


void SetGradArrays(int __nEta, double * __daYdK, double * __daSdK);

// pointer to routine to that can be called for diagnostic purposes
typedef void   (fDebug)(int i, double d);

void _setOnTrajectory(int b);
void  SetDbgCallback(fDebug * _p);
void  SetODELevel(int * _piODELevel);
void  ModelBreakPoint();
const char*  GetErrorMsg();
bool IsErrorMessageSet();
void  SetEnableLogTransform(int * _pbEnableLogTransform);
void  SetBreakTime(double * pdTime);
void  SetLogFloor(double * _lf);
void  SetPredArrays(
    int * __nPrediction
    , double * __daXPrediction
    , double * __taLastDose
    , int  * __iaWhichDose
    , int  * __iaWhichReset
    , double * __daYPrediction
    , double * __daYObservation
    , double * __daVarianceFunction
    , int  * __iaWhichObs
);
void  InitHist();
void  EnableHistBreak(int * pb);
void  RecordHist(double d);
void  SetMaxTime(double d);
double  GetMaxTime();

void LLOneSubject1(
    int bGenGradients
    , int nEta
    , int * pnObs
    , const unsigned char * *pb
    , const SubjectId & subjId
    , double _dLL[]
    , double _dGrad[]
    , int * pbOK
);
void NewPredOneSubject1(
    int _bGenGradients
    , int _nEta
    , int * _pnObs
    , const unsigned char * *_ppb
    , const SubjectId & subjId
    , int * _pbOK
    , int * __nPrediction
    , double * __daXPrediction, double * __taLastDose, int * __iaWhichDose, int * __iaWhichReset, double * __daYPrediction, double * __daYObservation, double * __daVarianceFunction
    , double * __daYdK
    , double * __daSdK
    , int * __iaWhichObs
);

void  LLOneSubject(
    int bGenGradients
    , int * pnObs
    , const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
    , double dLL[]
    , double dGrad[]
);

void  PredOneSubject(
    int * pnObs
    , const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
    , int * __nPrediction
    , double * __daXPrediction, double * __taLastDose, int * __iaWhichDose, int * __iaWhichReset, double * __daYPrediction, double * __daYObservation, double * __daVarianceFunction
    , int * __iaWhichObs
);
void  SimOneSubject(
    const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
    , int bSampleEpsilon
    , int bForSimTbl
    , int bForVPC
);
void  Init1OneSubject(
    const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
);
void  ScanOneSubject(
    const unsigned char * *pb
);
void ScanOneSubjectCountObs(
    const unsigned char * *pb
    , int * pnObs
    , double * pdMaxTime
);
void ScanOneSubjectGetCovariateMean(
    const unsigned char * *pb
    , const char * sCovName
    , double * pVal
);

void ScanOneSubjectGetCovariateMedian(
    const unsigned char * *pb
    , const char * sCovName
    , double * pVal
);

//////////////////////////////////////////////////////////////////////////
// TODO: following stuff uses global arrays, e.g. Y, R, and IRate
void DerivWrapped(long* _pN, double* _pt, double zzY[], double zzR[]);
void GradientDerivWrapped(long* _pN, double* _pt, double zzY[], double zzR[]);
void JacobianWrapped(int* pN, double* pt, double zzY[], int* pML, int* pMU, double* _jacobian, int* pNRJ);
void GradientJacobianWrapped(int* pN, double* pt, double zzY[], int* pML, int* pMU, double* _gjacobian, int* pNRJ);

int GetVarValue(const char * _nm, double * _pv);
void SetVarValue(const char * _nm, double * _pv);

// reset drug compartments and infusion state to zero (at start of subject)
void ResetInteg();

// reset urine compartments to zero (at start of urine collection interval) 
void ResetUrine();

// reset event hazard accumulators to zero (at start of subject) 
void TDL4ResetEvent();

// reset gradient accumulators to zero (at start of subject) 
void ResetGradient();

//////////////////////////////////////////////////////////////////////////

