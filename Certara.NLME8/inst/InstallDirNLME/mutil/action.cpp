#include "action.h"

#include <memory.h>
#include <math.h>

#include "ModelAPI.h"
#include "MUtil.h"

int zzNAction = 0;

double zzDT[ZZMAX_ACTION];          // time remaining to future action
int    zzPriority[ZZMAX_ACTION];    // sort order of actions at same time
int    zzActionCode[ZZMAX_ACTION];  // what to do at that time
int    zzActionIArg[ZZMAX_ACTION];  // action argument integer
double zzActionDArg1[ZZMAX_ACTION]; // action argument double
double zzActionDArg2[ZZMAX_ACTION]; // action argument double
void * zzActionPArg[ZZMAX_ACTION];  // action argument pointer

// backup copy of the action queue
int zzNAction_Bak = 0;

double zzDT_Bak[ZZMAX_ACTION];
int    zzPriority_Bak[ZZMAX_ACTION];
int    zzActionCode_Bak[ZZMAX_ACTION];
int    zzActionIArg_Bak[ZZMAX_ACTION];
double zzActionDArg1_Bak[ZZMAX_ACTION];
double zzActionDArg2_Bak[ZZMAX_ACTION];
void * zzActionPArg_Bak[ZZMAX_ACTION];

extern double * globalY;
extern double * globalIRate;
extern double * globalR;

struct _statemachine
{
    int _iState;
    void(*_func)(void * zzp, double t, double zzY[]);
};

void SaveEventQueue()
{
    zzNAction_Bak = zzNAction;
    memcpy(zzDT_Bak, zzDT, zzNAction * sizeof(zzDT[0]));
    memcpy(zzPriority_Bak, zzPriority, zzNAction * sizeof(zzPriority[0]));
    memcpy(zzActionCode_Bak, zzActionCode, zzNAction * sizeof(zzActionCode[0]));
    memcpy(zzActionIArg_Bak, zzActionIArg, zzNAction * sizeof(zzActionIArg[0]));
    memcpy(zzActionDArg1_Bak, zzActionDArg1, zzNAction * sizeof(zzActionDArg1[0]));
    memcpy(zzActionDArg2_Bak, zzActionDArg2, zzNAction * sizeof(zzActionDArg2[0]));
    memcpy(zzActionPArg_Bak, zzActionPArg, zzNAction * sizeof(zzActionPArg[0]));
    zzNAction = 0;
}

void RestoreEventQueue()
{
    zzNAction = zzNAction_Bak;
    memcpy(zzDT, zzDT_Bak, zzNAction * sizeof(zzDT[0]));
    memcpy(zzPriority, zzPriority_Bak, zzNAction * sizeof(zzPriority[0]));
    memcpy(zzActionCode, zzActionCode_Bak, zzNAction * sizeof(zzActionCode[0]));
    memcpy(zzActionIArg, zzActionIArg_Bak, zzNAction * sizeof(zzActionIArg[0]));
    memcpy(zzActionDArg1, zzActionDArg1_Bak, zzNAction * sizeof(zzActionDArg1[0]));
    memcpy(zzActionDArg2, zzActionDArg2_Bak, zzNAction * sizeof(zzActionDArg2[0]));
    memcpy(zzActionPArg, zzActionPArg_Bak, zzNAction * sizeof(zzActionPArg[0]));
    zzNAction_Bak = 0;
}

void ClearActions()
{
    zzNAction = 0;
}

void InsertAction(double dt, int priority, int code, int iarg, double darg1, double darg2, void * parg)
{
    int i, j;

    if (zzNAction >= ZZMAX_ACTION)
    {
        return;	// TODO: handle error
    }

    // find where to insert this action
    for (i = 0; i < zzNAction; i++)
    {
        if (zzDT[i] < dt)
        {
            continue;
        }

        if (zzDT[i] == dt && zzPriority[i] <= priority)
        {
            continue;
        }

        break;
    }

    // i is the index to hold the new action
    // make space for it
    for (j = zzNAction; --j >= i;)
    {
        zzDT[j + 1] = zzDT[j];
        zzPriority[j + 1] = zzPriority[j];
        zzActionCode[j + 1] = zzActionCode[j];
        zzActionIArg[j + 1] = zzActionIArg[j];
        zzActionDArg1[j + 1] = zzActionDArg1[j];
        zzActionDArg2[j + 1] = zzActionDArg2[j];
        zzActionPArg[j + 1] = zzActionPArg[j];
    }

    // insert it
    zzDT[i] = dt;
    zzPriority[i] = priority;
    zzActionCode[i] = code;
    zzActionIArg[i] = iarg;
    zzActionDArg1[i] = darg1;
    zzActionDArg2[i] = darg2;
    zzActionPArg[i] = parg;
    zzNAction++;
}

void RemoveActionZero()
{
    int i;

    for (i = 0; i < zzNAction - 1; i++)
    {
        zzDT[i] = zzDT[i + 1];
        zzPriority[i] = zzPriority[i + 1];
        zzActionCode[i] = zzActionCode[i + 1];
        zzActionIArg[i] = zzActionIArg[i + 1];
        zzActionDArg1[i] = zzActionDArg1[i + 1];
        zzActionDArg2[i] = zzActionDArg2[i + 1];
        zzActionPArg[i] = zzActionPArg[i + 1];
    }

    zzNAction--;
}

extern int _bDroppingOut;

void AdvanceWithActions(int bGenGradients, double * pTimeOuter, double dt, int bTimeAdvances, int bFinishUpSubject)
{
    int i, iCpt;
    int bDoEndRow = 0;
    int bDidSomething = 1;
    char szCompartmentName[512];
    // while there is more time to expire
    SetModelTime(*pTimeOuter);

    while (isfinite(dt) && (dt > 0 || bDidSomething) && !_bDroppingOut)
    {
        double drmin = dt;
        SetModelTime(*pTimeOuter);
        bDidSomething = 0;

        // get time to next action
        if (zzNAction > 0 && drmin > zzDT[0])
        {
            drmin = zzDT[0];
        }

        // advance for the minimum delta time
        ModelBreakPoint();

        if (drmin > 0)
        {
            double tim = *pTimeOuter;
            Advance(bGenGradients, iODELevel, &tim, drmin, bTimeAdvances, globalY);

            if (bTimeAdvances)
            {
                *pTimeOuter += drmin;
            }

            SetModelTime(*pTimeOuter);
        }

        ModelBreakPoint();

        // subtract it from the remaining drip and dose times
        if (drmin != 0)
        {
            for (i = 0; i < zzNAction; i++)
            {
                zzDT[i] -= drmin;

                if (zzDT[i] < 0)
                {
                    zzDT[i] = 0;    // try to correct for roundoff
                }
            }
        }

        if (drmin < 0)
            drmin = 0;

        // for each action to be done at this time
        // (stop short of actions at the end with priority > 1)
        // This is so that TIME actions don't get done too soon.
        bDoEndRow = 0;

        while (zzNAction > 0
            && zzDT[0] == 0
            && (dt > 0 || zzPriority[0] <= 1 || bFinishUpSubject)
            )
        {
            switch (zzActionCode[0])
            {
            case ZZACTION_DOSETIME:
                curTimeLastDose = curTime;
                iCurWhichDose++;
                break;

            // if bolus, do so
            case ZZACTION_BOLUS1:
                iCpt = zzActionIArg[0];
                PerformBolus1(&curTime, globalY, globalR, iCpt, zzActionDArg1[0], zzActionDArg2[0]);	// routine defined in model
                break;

            case   ZZMARK_BOLUS1:
                iCpt = zzActionIArg[0];
                GetCompartmentName(&iCpt, szCompartmentName);

                if (_pcb)
                {
                    (*_pcb->_pfOnBolus)(curTime, curTimeLastDose, 1, szCompartmentName, zzActionDArg1[0]);
                }
                break;

            case ZZACTION_BOLUS2:
                iCpt = zzActionIArg[0];
                PerformBolus2(&curTime, globalY, globalR, iCpt, zzActionDArg1[0], zzActionDArg2[0]);	// routine defined in model
                break;

            case   ZZMARK_BOLUS2:
                iCpt = zzActionIArg[0];
                GetCompartmentName(&iCpt, szCompartmentName);

                if (_pcb)
                {
                    (*_pcb->_pfOnBolus)(curTime, curTimeLastDose, 2, szCompartmentName, zzActionDArg1[0]);
                }
                break;

            // if change in infusion rate, do so
            case ZZACTION_INFSTRT1:
                iCpt = zzActionIArg[0];
                PerformInfStrt1(&curTime, globalY, globalR, globalIRate, iCpt, zzActionDArg1[0]);	// routine defined in model
                break;

            case   ZZMARK_INFSTRT1:
                iCpt = zzActionIArg[0];
                GetCompartmentName(&iCpt, szCompartmentName);

                if (_pcb)
                {
                    (*_pcb->_pfOnInfusion)(curTime, curTimeLastDose, 1, szCompartmentName, zzActionDArg1[0], zzActionDArg2[0]);
                }
                break;

            case ZZACTION_INFSTRT2:
                iCpt = zzActionIArg[0];
                PerformInfStrt2(&curTime, globalY, globalR, globalIRate, iCpt, zzActionDArg1[0]);	// routine defined in model
                break;

            case   ZZMARK_INFSTRT2:
                iCpt = zzActionIArg[0];
                GetCompartmentName(&iCpt, szCompartmentName);

                if (_pcb)
                {
                    (*_pcb->_pfOnInfusion)(curTime, curTimeLastDose, 2, szCompartmentName, zzActionDArg1[0], zzActionDArg2[0]);
                }
                break;

            // if change in infusion rate, do so
            case ZZACTION_INFEND1:
                iCpt = zzActionIArg[0];
                PerformInfEnd1(&curTime, globalY, globalR, globalIRate, iCpt, zzActionDArg1[0]);	// routine defined in model
                break;

                break;

            case ZZACTION_INFEND2:
                iCpt = zzActionIArg[0];
                PerformInfEnd2(&curTime, globalY, globalR, globalIRate, iCpt, zzActionDArg1[0]);	// routine defined in model
                break;

            case   ZZMARK_INFEND1:
            case   ZZMARK_INFEND2:
                break;

            case   ZZMARK_ENDROW:
                bDoEndRow = 1;
                break;

            // if wakeup from sleep, do so
            case ZZACTION_SLEEP:
            {
                _statemachine * p = (_statemachine *)zzActionPArg[0];

                if (p->_iState >= 0)
                {
                    p->_func(p, curTime, globalY);
                }
            }
            break;

            // if time callback, do so
            case ZZACTION_TIME:
                if (_pcb && _pcb->_pfOnTime)
                {
                    _pcb->_pfOnTime(curTime, curTimeLastDose);
                }
            break;

            case ZZACTION_COVARIATE:
            {
                const char * name = reinterpret_cast<const char *>(zzActionPArg[0]);
                SetCovariate(*pTimeOuter, name, zzActionDArg1[0]);
            }
            break;
            } // end switch

            // remove this action
            RemoveActionZero();
            bDidSomething = 1;
        } // end for each action at this time

        if (bDoEndRow)
        {
            bDoEndRow = 0;

            if (_pcb && _pcb->_pfOnEndRow)
            {
                (*_pcb->_pfOnEndRow)();
            }
        }

        // subtract it from the total time
        dt -= drmin;

        if (dt < 0)
        {
            dt = 0;    // try to correct for roundoff
        }
    } // end while there is more time to expire

      // if dropping out, delete all actions
    if (_bDroppingOut)
    {
        zzNAction = 0;
        memset(zzDT, 0, sizeof(zzDT));
        memset(zzPriority, 0, sizeof(zzPriority));
        memset(zzActionCode, 0, sizeof(zzActionCode));
        memset(zzActionIArg, 0, sizeof(zzActionIArg));
        memset(zzActionDArg1, 0, sizeof(zzActionDArg1));
        memset(zzActionDArg2, 0, sizeof(zzActionDArg2));
        memset(zzActionPArg, 0, sizeof(zzActionPArg));
    }

    ModelBreakPoint();
}

void CleanOutActionList(int bGenGradients)
{
    // clean out the action list
    // this allows the model to run past the last event, such as when simulating
    while (zzNAction > 0
        && isfinite(zzDT[0])
        && (_dMaxTime < 0 || curTime <= _dMaxTime)
        )
    {
        // delay by v1
        EvalSParms(globalY);
        // last argument allows us to finish up subject
        AdvanceWithActions(bGenGradients, &curTime, zzDT[0], TRUE, TRUE);
    }
}
