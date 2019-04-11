#pragma once

struct SubjectId;

typedef double (fGetUniform)(void);
typedef double (fGetNormal)(void);
typedef void   (fOnObserve)(double _t, double _tLastDose, const char * nm, int iCat, double dPred, double dPredObs, double dRawObs, double dSig, double dSqrVF, int bBql);
typedef void   (fOnId)(const SubjectId & id);
typedef void   (fAfterLastEvent)(void);
typedef void   (fOnEndId)(void);
typedef void   (fOnCovariate)(double _t, double _tLastDose, const char * nm, double dVal);
typedef void   (fOnInit1)(void);
typedef void   (fOnEndRow)(void);
typedef void   (fOnStripDose)(double _t, double _tLastDose, const char * nm, double dVal);
typedef void   (fOnBolus)(double _t, double _tLastDose, int iPath, const char * nm, double dAmt);
typedef void   (fOnInfusion)(double _t, double _tLastDose, int iPath, const char * nm, double dAmt, double dRate);
typedef void   (fOnSS)(void);
typedef void   (fOnSSi)(int iInSS);
typedef void   (fOnEvent)(double _t, double _tLastDose, const char * nm, double censor);
typedef void   (fOnReset)(void);
typedef void   (fOnTime)(double _t, double _tLastDose);

struct ModelCallbackTable
{
    fGetUniform		* _pfGetUniform;
    fGetNormal		* _pfGetNormal;
    fOnObserve		* _pfOnObserve;
    fOnId		*	 _pfOnId;
    fAfterLastEvent	* _pfAfterLastEvent;
    fOnEndId		* _pfOnEndId;
    fOnCovariate	* _pfOnCovariate;
    fOnStripDose	* _pfOnStripDose;
    fOnInit1		* _pfOnInit1;
    fOnEndRow		* _pfOnEndRow;
    fOnBolus		* _pfOnBolus;
    fOnInfusion		* _pfOnInfusion;
    fOnSS		*	 _pfOnSS;
    fOnSSi		*	 _pfOnSSi;
    fOnEvent		* _pfOnEvent;
    fOnReset		* _pfOnReset;
    fOnTime		*	 _pfOnTime;
};

void SetCallbacks(ModelCallbackTable * _p);
