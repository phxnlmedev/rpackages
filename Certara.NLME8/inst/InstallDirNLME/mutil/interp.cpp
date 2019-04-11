#include "StdAfx.h"

#include "MUtil.h"

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "ModelAPI/OpCode.h"
#include "ModelAPI/ModelCallbackTable.h"
#include "ModelAPI.h"
#include "ODE.h"
#include "nlme_extern.h"

#define MAXSTACK 100

const size_t nIntegAll = GetNintegAll();
const size_t nCFMacro = GetNcfmacro();
const size_t nUrine = GetNurine();
const size_t nEvent = GetNevent();

int ndStack;
double dStack[MAXSTACK];
int npcStack;
const unsigned char * pcStack[MAXSTACK];

double * globalY = new double[nIntegAll + 1 + 100];
double * globalIRate = new double[nIntegAll + 1];
double * globalR = new double[nIntegAll + 1 + 100];

#define FLAG_POW 16
#define FLAG_DIV 8
#define FLAG_MUL 4
#define FLAG_SUB 2
#define FLAG_ADD 1
int flags;

static void SetFlag(int flg)
{
    if (flags == 0 && flg == 0)
    {
        return;
    }

    if ((flags >= flg) && (flags & FLAG_POW))
    {
        dStack[ndStack - 2] = pow(dStack[ndStack - 2], dStack[ndStack - 1]);
        ndStack--;
        flags &= ~FLAG_POW;
    }

    if ((flags >= flg) && (flags & FLAG_DIV))
    {
        dStack[ndStack - 2] = dStack[ndStack - 2] / dStack[ndStack - 1];
        ndStack--;
        flags &= ~FLAG_DIV;
    }

    if ((flags >= flg) && (flags & FLAG_MUL))
    {
        dStack[ndStack - 2] = dStack[ndStack - 2] * dStack[ndStack - 1];
        ndStack--;
        flags &= ~FLAG_MUL;
    }

    if ((flags >= flg) && (flags & FLAG_SUB))
    {
        dStack[ndStack - 2] = dStack[ndStack - 2] - dStack[ndStack - 1];
        ndStack--;
        flags &= ~FLAG_SUB;
    }

    if ((flags >= flg) && (flags & FLAG_ADD))
    {
        dStack[ndStack - 2] = dStack[ndStack - 2] + dStack[ndStack - 1];
        ndStack--;
        flags &= ~FLAG_ADD;
    }

    flags |= flg;
}

static void MyAssert(BOOL b)
{
    if (!b)
    {
        int breakhere = 1;
    }
}

static void ScanOverRP(const unsigned char * &pc)
{
    unsigned char c;
    int iLevel = 1;

    while (iLevel && (c = *pc))
    {
        pc++;

        switch (c)
        {
        case 255:
            break;

        case OP_LP:
            iLevel++;
            break;

        case OP_RP:
            iLevel--;
            break;

        case OP_NUM:
            pc += sizeof(double);
            break;

        case OP_BOLUS1:
        case OP_BOLUS2:
        case OP_INF1:
        case OP_INF2:
        case OP_OBS:
        case OP_OBS_BQL:
        case OP_COVR:
        case OP_COVR_SRML8:
        case OP_DCOVR:
        case OP_FCOVR:
        case OP_DEF:
        case OP_SET:
        case OP_REF:
            while (*pc++);
            break;

        case OP_CLR:
        case OP_DT:
        case OP_RESET:
        case OP_RPT:
        case OP_SS:
        case OP_SS1:
        case OP_SS2:
        case OP_SSEND:
        case OP_SSNM:
        case OP_ADDL:
        case OP_ADDLNM:
        case OP_DUP:
        case OP_COMMA:
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_POW:
        case OP_QUEST:
            break;

        default:
            MyAssert(0);
        }
    }
}

int _bDroppingOut;
int _bInSS;
int  GetInSS()
{
    return _bInSS;
}

static void InterpSS(int bGenGradients, const unsigned char * pc, int * pbOK, int bTimeAdvances, int bFirstTimeThrough)
{
    int iCpt = -1;
    double v1, v2, dNum;
    char idbuf[4096];
    char * q;
    const unsigned char * pc1;
    const double * pdTemp = NULL;

    while (*pc)
    {
        ModelBreakPoint();

        switch (*pc++)
        {
        case 255:
            break;

        case OP_SS1:
        {
            if (_pcb && bFirstTimeThrough)
            {
                (*_pcb->_pfOnSSi)(1);
            }
        }
        break;

        case OP_SS2:
        {
            if (_pcb && bFirstTimeThrough)
            {
                (*_pcb->_pfOnSSi)(2);
            }
        }
        break;

        case OP_SSEND:
        {
            if (_pcb && bFirstTimeThrough)
            {
                (*_pcb->_pfOnSSi)(0);
            }
        }
        break;

        case OP_RP:
        {
            SetFlag(0);
            return;
        }
        break;

        case OP_QUEST:
        {
            dStack[ndStack++] = _UNK;	// unknown value
        }
        break;

        case OP_LP:
        {
            pc1 = pc;
            ScanOverRP(pc);
            // save pc1
            pcStack[npcStack++] = pc1;
        }
        break;

        case OP_DT:
        {
            SetFlag(0);
            v1 = dStack[--ndStack];

            // delay by v1
            if (isfinite(v1))
            {
                AdvanceWithActions(bGenGradients, &curTime, v1, bTimeAdvances, 0);
            }
        }
        break;

        case OP_BOLUS1:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];
            // bolus to variable
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleBolus1(bFirstTimeThrough, 0, iCpt, v1, 0);
            AdvanceWithActions(bGenGradients, &curTime, 0, bTimeAdvances, 0);
        }
        break;

        case OP_BOLUS2:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];
            // bolus to variable
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleBolus2(bFirstTimeThrough, 0, iCpt, v1, 0);
            AdvanceWithActions(bGenGradients, &curTime, 0, bTimeAdvances, 0);
        }
        break;

        case OP_INF1:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// rate
            v1 = dStack[--ndStack];	// amt
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleIV1(bFirstTimeThrough, 0, iCpt, v1, v2, 0);
            AdvanceWithActions(bGenGradients, &curTime, 0, bTimeAdvances, 0);
        }
        break;

        case OP_INF2:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// rate
            v1 = dStack[--ndStack];	// amt
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleIV2(bFirstTimeThrough, 0, iCpt, v1, v2, 0);
            AdvanceWithActions(bGenGradients, &curTime, 0, bTimeAdvances, 0);
        }
        break;

        case OP_RPT:
        {
            SetFlag(0);

            if (dStack[ndStack - 1] <= 0)
            {
                ndStack--;
                npcStack--;
            }
            else
            {
                pc -= 1;	// will redo the op
                dStack[ndStack - 1] -= 1;
                InterpSS(bGenGradients, pcStack[npcStack - 1], pbOK, bTimeAdvances, bFirstTimeThrough);
            }
        }
        break;

        case OP_NUM:
        {
            pdTemp = (const double *)pc;
            dNum = *pdTemp++;
            pc = (const unsigned char *)pdTemp;
            dStack[ndStack++] = dNum;
        }
        break;

        case OP_DUP:
        {
            dNum = dStack[ndStack - 1];
            dStack[ndStack++] = dNum;
        }
        break;

        case OP_COMMA:
            SetFlag(0);
            break;

        case OP_ADD:
            SetFlag(FLAG_ADD);
            break;

        case OP_SUB:
            SetFlag(FLAG_SUB);
            break;

        case OP_MUL:
            SetFlag(FLAG_MUL);
            break;

        case OP_DIV:
            SetFlag(FLAG_DIV);
            break;

        case OP_POW:
            SetFlag(FLAG_POW);
            break;

        default:
        {
            int breakhere = 1;
        }
        }
    }
}

static double vdis(int n, double a[], double b[])
{
    double sum = 0, del;
    int i;

    for (i = 0; i < n; i++)
    {
        del = a[i] - b[i];
        sum += del * del;
    }

    return sqrt(sum);
}

static void GetSS(int bGenGradients, const unsigned char * pc, int * pbOK)
{
    static double * zvec = new double[nIntegAll + nCFMacro * 6 + 1];
    static double * y0 = new double[nIntegAll + nCFMacro * 6 + 1];
    static double * y1 = new double[nIntegAll + nCFMacro * 6 + 1];
    static double * y2 = new double[nIntegAll + nCFMacro * 6 + 1];
    static double * y3 = new double[nIntegAll + nCFMacro * 6 + 1];
    static double * vec = new double[nIntegAll + nCFMacro * 6 + 1];

    double dy01, dy02, dy1z;
    double _a, _absa;
    int i, j, bTrySimpleWay = 0, bSSFailed = 0;
    double _tol = 1e-5;
    memset(zvec, 0, sizeof(zvec));

    SaveEventQueue();

    // first run two cycles to get in the ballpark
    if (pbOK && *pbOK == 0)
    {
        return;
    }

    InterpSS(bGenGradients, pc, pbOK, FALSE, TRUE);

    if (pbOK && *pbOK == 0)
    {
        return;
    }

    InterpSS(bGenGradients, pc, pbOK, FALSE, FALSE);

    if (pbOK && *pbOK == 0)
    {
        return;
    }

    for (j = 0; j < 20 && !bTrySimpleWay; j++)
    {
        for (i = 0; i < nIntegAll; i++)
        {
            y0[i] = globalY[i];
        }

        GetAllCFState(y0 + nIntegAll);
        ClearSSIrrelevantState(y0);

        InterpSS(bGenGradients, pc, pbOK, FALSE, FALSE);

        if (pbOK && *pbOK == 0)
        {
            return;
        }

        for (i = 0; i < nIntegAll; i++)
        {
            y1[i] = globalY[i];
        }

        GetAllCFState(y1 + nIntegAll);
        ClearSSIrrelevantState(y1);

        dy01 = vdis(nIntegAll + nCFMacro * 6, y0, y1);
        dy1z = vdis(nIntegAll + nCFMacro * 6, y1, zvec);

        // check for convergence
        if (dy01 < _tol * dy1z)
        {
            break;
        }

        InterpSS(bGenGradients, pc, pbOK, FALSE, FALSE);

        if (pbOK && *pbOK == 0)
        {
            return;
        }

        for (i = 0; i < nIntegAll; i++)
        {
            y2[i] = globalY[i];
        }

        GetAllCFState(y2 + nIntegAll);
        ClearSSIrrelevantState(y2);

        dy02 = vdis(nIntegAll + nCFMacro * 6, y0, y2);

        // guard against zero-divide
        if (2 * dy01 - dy02 == 0)
        {
            bTrySimpleWay = 1;
            break;
        }

        // get vector y2 - y0
        for (i = 0; i < nIntegAll + nCFMacro * 6; i++)
        {
            vec[i] = (y2[i] - y0[i]);
        }

        // extrapolate
        _a = (dy01 * dy01) / (2 * dy01 - dy02);
        // if it's suspiciously large, give up extrapolation
        _absa = fabs(_a);

        if (_absa > 5 * dy02)
        {
            bTrySimpleWay = 1;
            break;
        }

        // do the extrapolation
        for (i = 0; i < nIntegAll + nCFMacro * 6; i++)
        {
            y3[i] = y0[i] + vec[i] / dy02 * _a;
        }

        // if any closed-form component is negative, give up extrapolation
        for (i = nIntegAll; i < nIntegAll + nCFMacro * 6; i++)
        {
            if (y3[i] < 0)
            {
                bTrySimpleWay = 1;
                break;
            }
        }

        for (i = 0; i < nIntegAll; i++)
        {
            globalY[i] = y3[i];
        }

        SetAllCFState(y3 + nIntegAll);
    }

    if (j >= 20)
    {
        bTrySimpleWay = 1;
    }

    if (bTrySimpleWay)
    {
        bSSFailed = 0;

        for (i = 0; i < nIntegAll; i++)
        {
            y0[i] = globalY[i];
        }

        GetAllCFState(y0 + nIntegAll);
        ClearSSIrrelevantState(y0);

        // allow it to cycle up to this number of times before giving up
        // in case it's a really bad but still possible case
        for (j = 1000; --j >= 0;)
        {
            if (pbOK && *pbOK == 0)
            {
                return;
            }

            InterpSS(bGenGradients, pc, pbOK, FALSE, FALSE);

            if (pbOK && *pbOK == 0)
            {
                return;
            }

            for (i = 0; i < nIntegAll; i++)
            {
                y1[i] = globalY[i];
            }

            GetAllCFState(y1 + nIntegAll);
            ClearSSIrrelevantState(y1);

            dy01 = vdis(nIntegAll + nCFMacro * 6, y0, y1);
            dy1z = vdis(nIntegAll + nCFMacro * 6, y1, zvec);

            // check for convergence
            if (dy01 < _tol * dy1z)
            {
                break;
            }

            for (i = 0; i < nIntegAll + nCFMacro * 6; i++)
            {
                y0[i] = y1[i];
            }
        }

        if (j < 0)
        {
            bSSFailed = 1;
        }
    }

    // it's a choice between failing and stopping with a result
    // that is pretty good but doesn't meet the tolerance
    // here I'm choosing to accept the pretty-good result
    if (bSSFailed) {
		SetErrorMessageWithParams("Error: unable to achieve steady-state.");

        if (pbOK != 0L)
        {
            (*pbOK) = 0;
        }
    }

    RestoreEventQueue();
}

// this is used to schedule additional doses into the future
static void InterpADDL(int bGenGradients, const unsigned char * pc, int * pbOK, double * pdTime)
{
    int iCpt = -1;
    double v1, v2, dNum;
    int bOK = 1;
    char idbuf[4096];
    char * q;
    const unsigned char * pc1;
    const double * pdTemp = NULL;

    while (*pc)
    {
        ModelBreakPoint();

        switch (*pc++)
        {
        case 255:
            break;

        case OP_RP:
        {
            SetFlag(0);
            return;
        }
        break;

        case OP_QUEST:
        {
            dStack[ndStack++] = _UNK;	// unknown value
        }
        break;

        case OP_LP:
        {
            pc1 = pc;
            ScanOverRP(pc);
            // save pc1
            pcStack[npcStack++] = pc1;
        }
        break;

        case OP_DT:
        {
            SetFlag(0);
            v1 = dStack[--ndStack];
            // delay by v1
            *pdTime += v1;
        }
        break;

        case OP_BOLUS1:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];
            // bolus to variable
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleBolus1(1, *pdTime - curTime, iCpt, v1, 1);
        }
        break;

        case OP_INF1:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// rate
            v1 = dStack[--ndStack];	// amt
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleIV1(1, *pdTime - curTime, iCpt, v1, v2, 1);
        }
        break;

        case OP_BOLUS2:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];
            // bolus to variable
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleBolus2(1, *pdTime - curTime, iCpt, v1, 1);
        }
        break;

        case OP_INF2:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// rate
            v1 = dStack[--ndStack];	// amt
            iCpt = GetCompartmentNumber(idbuf);
            ScheduleIV2(1, *pdTime - curTime, iCpt, v1, v2, 1);
        }
        break;

        case OP_RPT:
        {
            SetFlag(0);

            if (dStack[ndStack - 1] <= 0)
            {
                ndStack--;
                npcStack--;
            }
            else
            {
                pc -= 1;	// will redo the op
                dStack[ndStack - 1] -= 1;
                InterpADDL(bGenGradients, pcStack[npcStack - 1], pbOK, pdTime);
            }
        }
        break;

        case OP_NUM:
        {
            pdTemp = (const double *)pc;
            dNum = *pdTemp++;
            pc = (const unsigned char *)pdTemp;
            dStack[ndStack++] = dNum;
        }
        break;

        case OP_DUP:
        {
            dNum = dStack[ndStack - 1];
            dStack[ndStack++] = dNum;
        }
        break;

        case OP_COMMA:
            SetFlag(0);
            break;

        case OP_ADD:
            SetFlag(FLAG_ADD);
            break;

        case OP_SUB:
            SetFlag(FLAG_SUB);
            break;

        case OP_MUL:
            SetFlag(FLAG_MUL);
            break;

        case OP_DIV:
            SetFlag(FLAG_DIV);
            break;

        case OP_POW:
            SetFlag(FLAG_POW);
            break;

        default:
        {
            int breakhere = 1;
        }
        }
    }
}

static void GetADDL(int bGenGradients, const unsigned char * pc, int * pbOK, double dOffset, int nAddl)
{
    int i;
    // if there's an offset, apply that
    double dLocalTime = curTime + dOffset;

    for (i = 0; i < nAddl; i++)
    {
        InterpADDL(bGenGradients, pc, pbOK, &dLocalTime);
    }
}

static char idbuf[256];
static const unsigned char * pc1;

static int nRunStack;
static const unsigned char * pcRunStack[MAXSTACK];

static const unsigned char * pc;
static const unsigned char * pcLast;

void StartDroppingOut()
{
    _bDroppingOut = 1;
}

static void Interp(
    int bGenGradients
    , double dLL[]
    , double dGrad[]
    , const SubjectId & subjId
    , int * pnObs
    , int * pbOK
    , int iInterpMode
    , int bSampleEpsilon
    , int bForSimTbl
    , int bForVPC
)
{
    int iCpt = -1;
    char * q;
    char c;
    BOOL bDone = FALSE;
    const double * pdTemp = NULL;
    _bDroppingOut = FALSE;
    _bInSS = FALSE;

    double dNum;
    double v1, v2, v3;

    if (_pcb)
    {
        (*_pcb->_pfOnId)(subjId);
    }

    while (!bDone && (pbOK == 0L || *pbOK) && IsOK() && (pcLast = pc, *pc))
    {
        SetTimeOfStateCF(curTime);
        SetTimeOfGetValCF(curTime);
        c = *pc++;

        switch (c)
        {
        case 127:
            break;

        case OP_NUL:
        case OP_ID1:
        case OP_ID2:
        case OP_ID3:
        case OP_ID4:
        case OP_ID5:
            pc--;
            bDone = TRUE;
            break;

        case OP_CLR:
        {
            if (!_bDroppingOut)
            {
                ndStack = nRunStack = npcStack = flags = 0;
            }
        }
        break;

        case OP_CLR_RESET:
        {
            if (!_bDroppingOut)
            {
                iCurWhichReset = 0;
            }
        }
        break;

        case OP_INIT1:
        {
            if (iInterpMode == INTERP_MODE_INIT1)
            {
                bDone = TRUE;

                if (_pcb)
                {
                    (*_pcb->_pfOnInit1)();
                }
            }
            else if (!_bDroppingOut)
            {
                EvalSParms(globalY);
                InitDelays(curTime);
                InitCF(curTime);
                RestartSubjectSequences(globalY);

                if (iInterpMode == INTERP_MODE_SIM)
                {
                    RestartSubjectEventSims(globalY);
                }

                if (_pcb)
                {
                    (*_pcb->_pfOnInit1)();
                }
            }
        }
        break;

        case OP_ENDROW:
        {
            if (_pcb)
            {
                (*_pcb->_pfOnEndRow)();
            }
        }
        break;

        case OP_RP:
        {
            if (!_bDroppingOut)
            {
                SetFlag(0);

                if (nRunStack <= 0)
                {
                    return;
                }

                pc = pcRunStack[--nRunStack];
            }
        }
        break;

        case OP_QUEST:
        {
            if (!_bDroppingOut)
            {
                dStack[ndStack++] = _UNK;	// unknown value
            }
        }
        break;

        case OP_LP:
        {
            pc1 = pc;
            ScanOverRP(pc);

            if (!_bDroppingOut)
            {
                // save pc1
                pcStack[npcStack++] = pc1;
            }
        }
        break;

        case OP_DT:
        {
            if (!_bDroppingOut)
            {
                SetFlag(0);
                v1 = dStack[--ndStack];
                // delay by v1
                EvalSParms(globalY);
                AdvanceWithActions(bGenGradients, &curTime, v1, TRUE, 0);
            }
        }
        break;

        case OP_BOLUS1:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v1 = dStack[--ndStack];
                // bolus to variable
                iCpt = GetCompartmentNumber(idbuf);
                EvalSParms(globalY);
                ScheduleBolus1(1, 0, iCpt, v1, 0);
                AdvanceWithActions(bGenGradients, &curTime, 0, TRUE, 0);
            }
        }
        break;

        case OP_INF1:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v2 = dStack[--ndStack];	// rate
                v1 = dStack[--ndStack];	// amt
                                        // start infusion to variable
                iCpt = GetCompartmentNumber(idbuf);
                EvalSParms(globalY);
                ScheduleIV1(1, 0, iCpt, v1, v2, 0);
                AdvanceWithActions(bGenGradients, &curTime, 0, TRUE, 0);
            }
        }
        break;

        case OP_BOLUS2:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v1 = dStack[--ndStack];
                // bolus to variable
                iCpt = GetCompartmentNumber(idbuf);
                EvalSParms(globalY);
                ScheduleBolus2(1, 0, iCpt, v1, 0);
                AdvanceWithActions(bGenGradients, &curTime, 0, TRUE, 0);
            }
        }
        break;

        case OP_INF2:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v2 = dStack[--ndStack];	// rate
                v1 = dStack[--ndStack];	// amt
                                        // start infusion to variable
                iCpt = GetCompartmentNumber(idbuf);
                EvalSParms(globalY);
                ScheduleIV2(1, 0, iCpt, v1, v2, 0);
                AdvanceWithActions(bGenGradients, &curTime, 0, TRUE, 0);
            }
        }
        break;

        case OP_RESET:
        {
            if (!_bDroppingOut)
            {
                CleanOutActionList(bGenGradients);
                ResetSubject();
                iCurWhichReset++;

                if (_pcb)
                {
                    (*_pcb->_pfOnReset)();
                }
            }
        }
        break;

        case OP_EVENT:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v1 = dStack[--ndStack];
                // observed value is v1
                EvalSParms(globalY);

                if (iInterpMode == INTERP_MODE_LL)
                {
                    if (bGenGradients)
                    {
                        NewGetLL(bGenGradients, dLL, dGrad, globalY, globalIRate, pnObs, curTime, idbuf, v1, 0);
                    }
                    else
                    {
                        GetLL(bGenGradients, dLL, dGrad, globalY, globalIRate, pnObs, curTime, idbuf, v1, 0);
                    }
                }
                else if (iInterpMode == INTERP_MODE_SIM)
                {
                }
            }
        }
        break;

        case OP_STRTOBS:
        {
        }
        break;

        case OP_OBS:
        case OP_OBS_BQL:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v1 = dStack[--ndStack];
                // observed value is v1
                EvalSParms(globalY);

                if (iInterpMode == INTERP_MODE_PRED)
                {
                    if (c == OP_OBS)
                    {
                        NewGetPred(bGenGradients, pnObs, globalY, globalIRate, curTime, idbuf, v1);
                    }
                }
                else if (iInterpMode == INTERP_MODE_LL)
                {
                    if (bGenGradients)
                    {
                        NewGetLL(bGenGradients, dLL, dGrad, globalY, globalIRate, pnObs, curTime, idbuf, v1, c == OP_OBS_BQL);
                    }
                    else
                    {
                        GetLL(bGenGradients, dLL, dGrad, globalY, globalIRate, pnObs, curTime, idbuf, v1, c == OP_OBS_BQL);
                    }
                }
                else if (iInterpMode == INTERP_MODE_SIM)
                {
                    if (!bForSimTbl)
                    {
                        Simulate(curTime, idbuf, v1, c == OP_OBS_BQL, bSampleEpsilon, bForVPC, globalY, globalIRate);
                    }
                }
            }
        }
        break;

        case OP_ENDOBS:
        {
            if (!_bDroppingOut)
            {
                DoEndObsActions(globalY);
            }
        }
        break;

        case OP_COVR:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                v1 = dStack[--ndStack];	// val0
                                        // set covariate
                SetCovariate(curTime, idbuf, v1);

                if (*_piState == ODESTATE_UNDERWAY)
                {
                    *_piState = ODESTATE_CHANGE;
                }

                if (GetStripDoseName(idbuf))
                {
                    if (v1 == 0)
                    {
                        SetErrorMsg("Error: stripping dose is zero.");

                        if (pbOK != 0L)
                        {
                            (*pbOK) = 0;
                        }
                    }

                    if (_pcb)
                    {
                        (*_pcb->_pfOnStripDose)(curTime, curTimeLastDose, idbuf, v1);
                    }
                }
                else
                {
                    if (_pcb)
                    {
                        (*_pcb->_pfOnCovariate)(curTime, curTimeLastDose, idbuf, v1);
                    }
                }
            }
        }
        break;

        case OP_COVR_SRML8:
        {
            const unsigned char * covName = pc;

            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                double time = dStack[--ndStack];
                double val = dStack[--ndStack];
                InsertAction(time, 0, ZZACTION_COVARIATE, 0, val, 0, (void*)covName);
            }
        }
        break;

        // val1 dt val2 dcovr(covname) // backward version
        case OP_DCOVR:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v2 = dStack[--ndStack];	// val2
                v3 = dStack[--ndStack];	// dt
                v1 = dStack[--ndStack];	// val1
                                        // set covariate interpolation params
                dNum = 0;
                GetVarValue(idbuf, &dNum);
                SetCovariateInterp(curTime, idbuf, v1, v3, v2, FALSE);

                if (*_piState == ODESTATE_UNDERWAY)
                {
                    *_piState = ODESTATE_CHANGE;
                }

                if (v2 != dNum)
                {
                    Advance(bGenGradients, iODELevel, &curTime, 0, TRUE, globalY);
                    EvalSParms(globalY);
                    ReInitCF(curTime);
                }

                if (_pcb)
                {
                    EvalSParms(globalY);
                    (*_pcb->_pfOnCovariate)(curTime, curTimeLastDose, idbuf, v1);
                }
            }
        }
        break;

        // val1 dt val2 fcovr(covname) // forward version
        case OP_FCOVR:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (!_bDroppingOut)
            {
                SetFlag(0);
                v2 = dStack[--ndStack];	// val2
                v3 = dStack[--ndStack];	// dt
                v1 = dStack[--ndStack];	// val1
                                        // set covariate interpolation params
                dNum = 0;
                GetVarValue(idbuf, &dNum);
                SetCovariateInterp(curTime, idbuf, v1, v3, v2, TRUE);

                if (*_piState == ODESTATE_UNDERWAY)
                {
                    *_piState = ODESTATE_CHANGE;
                }

                if (v1 != dNum)
                {
                    AdvanceWithActions(bGenGradients, &curTime, 0, TRUE, 0);
                    EvalSParms(globalY);
                    ReInitCF(curTime);
                }

                if (_pcb)
                {
                    EvalSParms(globalY);
                    (*_pcb->_pfOnCovariate)(curTime, curTimeLastDose, idbuf, v1);
                }
            }
        }
        break;

        case OP_RPT:
        {
            if (!_bDroppingOut)
            {
                SetFlag(0);

                if (dStack[ndStack - 1] <= 0)
                {
                    ndStack--;
                    npcStack--;
                }
                else
                {
                    pc -= 1;	// will redo the op
                    dStack[ndStack - 1] -= 1;
                    pcRunStack[nRunStack++] = pc;
                    pc = pcStack[npcStack - 1];
                }
            }
        }
        break;

        case OP_SS:
        {
            if (!_bDroppingOut)
            {
                if (_pcb)
                {
                    (*_pcb->_pfOnSS)();
                }

                SetFlag(0);
                _bInSS = TRUE;
                GetSS(bGenGradients, pcStack[npcStack - 1], pbOK);

                if (pbOK && *pbOK == 0 || !IsOK())
                {
                    break;
                }

                _bInSS = FALSE;
                npcStack--;
            }
        }
        break;

        case OP_SS1:
        {
        }
        break;

        case OP_SS2:
        {
        }
        break;

        case OP_SSEND:
        {
        }
        break;

        case OP_SSNM:
        {
            if (!_bDroppingOut)
            {
                if (_pcb)
                {
                    (*_pcb->_pfOnSS)();
                }

                SetFlag(0);
                _bInSS = TRUE;
                GetSS(bGenGradients, pcStack[npcStack - 1], pbOK);

                if (pbOK && *pbOK == 0 || !IsOK())
                {
                    break;
                }

                _bInSS = FALSE;

                // schedule an additional cycle starting at this time
                if (*pbOK && !_bDroppingOut)
                {
                    SetFlag(0);
                    GetADDL(bGenGradients, pcStack[npcStack - 1], pbOK, 0.0, 1);
                }

                npcStack--;
            }
        }
        break;

        case OP_ADDL:
        {
            if (!_bDroppingOut)
            {
                int nAddl = 0;
                v1 = dStack[--ndStack];	// val0
                nAddl = (int)floor(v1 + 0.5);
                SetFlag(0);
                GetADDL(bGenGradients, pcStack[npcStack - 1], pbOK, 0.0, nAddl);
                npcStack--;
            }
        }
        break;

        case OP_ADDLNM:
        {
            if (!_bDroppingOut)
            {
                int nAddl = 0;
                double dAddlOffset = 0;
                v2 = dStack[--ndStack]; // offset
                dAddlOffset = v2;
                v1 = dStack[--ndStack];	// repetitions
                nAddl = (int)floor(v1 + 0.5);
                SetFlag(0);
                GetADDL(bGenGradients, pcStack[npcStack - 1], pbOK, dAddlOffset, nAddl);
                npcStack--;
            }
        }
        break;

        case OP_NUM:
        {
            pdTemp = (const double *)pc;
            dNum = *pdTemp++;
            pc = (const unsigned char *)pdTemp;

            if (!_bDroppingOut)
            {
                dStack[ndStack++] = dNum;
            }
        }
        break;

        case OP_DUP:
        {
            if (!_bDroppingOut)
            {
                dNum = dStack[ndStack - 1];
                dStack[ndStack++] = dNum;
            }
        }
        break;

        case OP_COMMA:
            SetFlag(0);
            break;

        case OP_ADD:
            SetFlag(FLAG_ADD);
            break;

        case OP_SUB:
            SetFlag(FLAG_SUB);
            break;

        case OP_MUL:
            SetFlag(FLAG_MUL);
            break;

        case OP_DIV:
            SetFlag(FLAG_DIV);
            break;

        case OP_POW:
            SetFlag(FLAG_POW);
            break;

        default:
        {
            int breakhere = 1;
        }
        }

        ModelBreakPoint();
    }

    if (pbOK && *pbOK == 0 || !IsOK())
    {
		if (pbOK)
			*pbOK = *pbOK && IsOK();
    }

    if (_pcb)
    {
        (*_pcb->_pfAfterLastEvent)();
    }

    CleanOutActionList(bGenGradients);

    if (_pcb)
    {
        (*_pcb->_pfOnEndId)();
    }
}

static void InterpScanSubject(int * pnObs, double * pdMaxTime);	// forward

static void InterpScanSubjectGetADDL(const unsigned char * pc1, int nAddl, double dOffset, double * pdMaxTime)
{
    int i;
    const unsigned char * pcSave = pc;
    int nObs = 0;

    for (i = 0; i < nAddl; i++)
    {
        double dLocalMaxTime = 0;
        pc = pc1;
        InterpScanSubject(&nObs, &dLocalMaxTime);
        *pdMaxTime += dLocalMaxTime;
    }

    // if there's an offset, apply that
    *pdMaxTime += dOffset;
    pc = pcSave;
}

static void InterpScanSubject(int * pnObs, double * pdMaxTime)
{
    int iCpt = -1;
    char * q;
    char c;
    BOOL bDone = FALSE;
    double dLocalTime = 0;
    const double * pdTemp = NULL;

    double dNum;
    double v1, v2, v3;

    while (!bDone && (pcLast = pc, *pc))
    {
        c = *pc++;

        switch (c)
        {
        case 127:
            break;

        case OP_NUL:
        case OP_ID1:
        case OP_ID2:
        case OP_ID3:
        case OP_ID4:
        case OP_ID5:
            pc--;
            bDone = TRUE;
            break;

        case OP_CLR:
        {
            ndStack = nRunStack = npcStack = flags = 0;
        }
        break;

        case OP_CLR_RESET:
        {}
        break;

        case OP_INIT1:
        {}
        break;

        case OP_ENDROW:
        {}
        break;

        case OP_RP:
        {
            SetFlag(0);

            if (nRunStack <= 0)
            {
                return;
            }

            pc = pcRunStack[--nRunStack];
        }
        break;

        case OP_QUEST:
        {
            dStack[ndStack++] = _UNK;	// unknown value
        }
        break;

        case OP_LP:
        {
            pc1 = pc;
            ScanOverRP(pc);
            // save pc1
            pcStack[npcStack++] = pc1;
        }
        break;

        case OP_DT:
        {
            SetFlag(0);
            v1 = dStack[--ndStack];
            dLocalTime += v1;

            if (*pdMaxTime < dLocalTime)
            {
                *pdMaxTime = dLocalTime;
            }
        }
        break;

        case OP_BOLUS1:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];
        }
        break;

        case OP_INF1:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// rate
            v1 = dStack[--ndStack];	// amt

            if (v2 != 0 && *pdMaxTime < dLocalTime + v1 / v2)
            {
                *pdMaxTime = dLocalTime + v1 / v2;
            }
        }
        break;

        case OP_BOLUS2:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];
        }
        break;

        case OP_INF2:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// rate
            v1 = dStack[--ndStack];	// amt

            if (v2 != 0 && *pdMaxTime < dLocalTime + v1 / v2)
            {
                *pdMaxTime = dLocalTime + v1 / v2;
            }
        }
        break;

        case OP_RESET:
        break;

        case OP_EVENT:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];
        }
        break;

        case OP_STRTOBS:
        break;

        case OP_OBS:
        case OP_OBS_BQL:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v1 = dStack[--ndStack];

            if (c == OP_OBS)
            {
                (*pnObs)++;
            }
        }
        break;

        case OP_ENDOBS:
        break;

        case OP_COVR:
        {
            for (q = idbuf; *q++ = *pc++;);

            v1 = dStack[--ndStack];	// val0
        }
        break;

        case OP_COVR_SRML8:
        {
            for (q = idbuf; *q++ = *pc++;);

            double time = dStack[--ndStack];
            double val = dStack[--ndStack];
        }
        break;

        // val1 dt val2 dcovr(covname) // backward version
        case OP_DCOVR:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// val2
            v3 = dStack[--ndStack];	// dt
            v1 = dStack[--ndStack];	// val1
        }
        break;

        // val1 dt val2 fcovr(covname) // forward version
        case OP_FCOVR:
        {
            for (q = idbuf; *q++ = *pc++;);

            SetFlag(0);
            v2 = dStack[--ndStack];	// val2
            v3 = dStack[--ndStack];	// dt
            v1 = dStack[--ndStack];	// val1
        }
        break;

        case OP_RPT:
        {
            SetFlag(0);

            if (dStack[ndStack - 1] <= 0)
            {
                ndStack--;
                npcStack--;
            }
            else
            {
                pc -= 1;	// will redo the op
                dStack[ndStack - 1] -= 1;
                pcRunStack[nRunStack++] = pc;
                pc = pcStack[npcStack - 1];
            }
        }
        break;

        case OP_SS:
        {
            SetFlag(0);
            npcStack--;
        }
        break;

        case OP_SS1:
        case OP_SS2:
        case OP_SSEND:
        break;

        case OP_SSNM:
        {
            SetFlag(0);
            npcStack--;
        }
        break;

        case OP_ADDL:
        {
            int nAddl = 0;
            v1 = dStack[--ndStack];	// n repetitions
            nAddl = (int)floor(v1 + 0.5);
            SetFlag(0);
            InterpScanSubjectGetADDL(pcStack[npcStack - 1], nAddl, 0.0, pdMaxTime);
            npcStack--;
        }
        break;

        case OP_ADDLNM:
        {
            int nAddl = 0;
            double dAddlOffset = 0;
            v2 = dStack[--ndStack];	// offset
            dAddlOffset = v2;
            v1 = dStack[--ndStack];	// n repetitions
            nAddl = (int)floor(v1 + 0.5);
            SetFlag(0);
            InterpScanSubjectGetADDL(pcStack[npcStack - 1], nAddl, dAddlOffset, pdMaxTime);
            npcStack--;
        }
        break;

        case OP_NUM:
        {
            pdTemp = (const double *)pc;
            dNum = *pdTemp++;
            pc = (const unsigned char *)pdTemp;
            dStack[ndStack++] = dNum;
        }
        break;

        case OP_DUP:
        {
            dNum = dStack[ndStack - 1];
            dStack[ndStack++] = dNum;
        }
        break;

        case OP_COMMA:
            SetFlag(0);
            break;

        case OP_ADD:
            SetFlag(FLAG_ADD);
            break;

        case OP_SUB:
            SetFlag(FLAG_SUB);
            break;

        case OP_MUL:
            SetFlag(FLAG_MUL);
            break;

        case OP_DIV:
            SetFlag(FLAG_DIV);
            break;

        case OP_POW:
            SetFlag(FLAG_POW);
            break;

        default:
        {
            int breakhere = 1;
        }
        }
    }
}

static void ScanSubject(int * pnObs)
{
    BOOL bDone = FALSE;

    while (!bDone && (pcLast = pc, *pc))
    {
        int c = *pc++;

        switch (c)
        {
        case 255:
            break;

        case OP_NUL:
        case OP_ID1:
        case OP_ID2:
        case OP_ID3:
        case OP_ID4:
        case OP_ID5:
        {
            pc--;
            bDone = TRUE;
        }
        break;

        case OP_RP:
        {
            MyAssert(0);	// should not get here
        }
        break;

        case OP_INIT1:
        case OP_ENDROW:
        case OP_STRTOBS:
        case OP_ENDOBS:
        case OP_CLR:
        case OP_CLR_RESET:
        case OP_DT:
        case OP_RESET:
        case OP_RPT:
        case OP_SS:
        case OP_SS1:
        case OP_SS2:
        case OP_SSEND:
        case OP_SSNM:
        case OP_ADDL:
        case OP_ADDLNM:
        case OP_DUP:
        case OP_COMMA:
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_POW:
        case OP_QUEST:
            break;

        case OP_LP:
        {
            ScanOverRP(pc);
        }
        break;

        case OP_BOLUS1:
        case OP_INF1:
        case OP_BOLUS2:
        case OP_INF2:
        case OP_COVR:
        case OP_COVR_SRML8:
        case OP_DCOVR:
        case OP_FCOVR:
        case OP_EVENT:
        {
            while (*pc++);
        }
        break;

        case OP_OBS:
        case OP_OBS_BQL:
        {
            if (c == OP_OBS)
            {
                (*pnObs)++;
            }

            while (*pc++);
        }
        break;

        case OP_NUM:
        {
            pc += sizeof(double);
        }
        break;

        default:
        {
            MyAssert(0);	// should not get here
        }
        }
    }
}

static void ScanSubjectGetCovariate(const char * sCovName, double * pVal)
{
    int nCV = 0;
    char * q;
    double dCVSum = 0;
    const double * pdTemp = NULL;
    double dNum0 = 0, dNum1 = 0, dNum2 = 0;
    BOOL bDone = FALSE;

    while (!bDone && (pcLast = pc, *pc))
    {
        int c = *pc++;

        switch (c)
        {
        case 255:
            break;

        case OP_NUL:
        case OP_ID1:
        case OP_ID2:
        case OP_ID3:
        case OP_ID4:
        case OP_ID5:
        {
            pc--;
            bDone = TRUE;
        }
        break;

        case OP_RP:
        {
            MyAssert(0);	// should not get here
        }
        break;

        case OP_INIT1:
        case OP_ENDROW:
        case OP_STRTOBS:
        case OP_ENDOBS:
        case OP_CLR:
        case OP_CLR_RESET:
        case OP_DT:
        case OP_RESET:
        case OP_RPT:
        case OP_SS:
        case OP_SS1:
        case OP_SS2:
        case OP_SSEND:
        case OP_SSNM:
        case OP_ADDL:
        case OP_ADDLNM:
        case OP_DUP:
        case OP_COMMA:
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_POW:
        case OP_QUEST:
            break;

        case OP_LP:
        {
            ScanOverRP(pc);
        }
        break;

        case OP_BOLUS1:
        case OP_INF1:
        case OP_BOLUS2:
        case OP_INF2:
        case OP_COVR:
        case OP_COVR_SRML8:
        case OP_DCOVR:
        case OP_FCOVR:
        case OP_EVENT:
        {
            for (q = idbuf; *q++ = *pc++;);

            if (strcmp(idbuf, sCovName) == 0)
            {
                if (c == OP_COVR || c == OP_COVR_SRML8)
                {
                    double d = dNum0;
                    dCVSum += d;
                    nCV++;
                }
                else if (c == OP_DCOVR || c == OP_FCOVR)
                {
                    double d = (dNum0 + dNum2) / 2;
                    dCVSum += d;
                    nCV++;
                }
            }
        }
        break;

        case OP_OBS:
        case OP_OBS_BQL:
        {
            while (*pc++);
        }
        break;

        case OP_NUM:
        {
            dNum2 = dNum1;
            dNum1 = dNum0;
            pdTemp = (const double *)pc;
            dNum0 = *pdTemp++;
            pc = (const unsigned char *)pdTemp;
        }
        break;

        default:
        {
            MyAssert(0);	// should not get here
        }
        }
    }

    if (nCV > 0)
    {
        *pVal = dCVSum / nCV;
    }
}

void LLOneSubject1(
    int bGenGradients
    , int nEta
    , int * pnObs
    , const unsigned char * *pb
    , const SubjectId & subjId
    , double _dLL[]
    , double _dGrad[]
    , int * pbOK
)
{
    int iState = 1;
    pc = *pb;
    InitSubject();
    Interp(bGenGradients, _dLL, _dGrad, subjId, pnObs, pbOK, INTERP_MODE_LL, TRUE, FALSE, FALSE);
    EndSubject(0);
    *pb = pc;
}

void LLOneSubject(
    int bGenGradients
    , int * pnObs
    , const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
    , double dLL[]
    , double dGrad[]
)
{
    int iState = 1;
    pc = *pb;

    if (bGenGradients)
    {
        GetLLGradientEta(pnObs, pb, subjId, dLL, dGrad);
        *pb = pc;
    }
    else
    {
        InitSubject();
        Interp(FALSE, dLL, dGrad, subjId, pnObs, pbOK, INTERP_MODE_LL, TRUE, FALSE, FALSE);
        EndSubject(0);
        *pb = pc;
    }
}

void PredOneSubject(
    int * pnObs
    , const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
    , int * __nPrediction
    , double * __daXPrediction, double * __taLastDose, int * __iaWhichDose, int * __iaWhichReset, double * __daYPrediction, double * __daYObservation, double * __daVarianceFunction
    , int * __iaWhichObs
)
{
    int iState = 1;
    double dLL[1], dGrad[1];
    dLL[0] = 0;
    pc = *pb;
    SetPredArrays(__nPrediction
        , __daXPrediction
        , __taLastDose
        , __iaWhichDose
        , __iaWhichReset
        , __daYPrediction
        , __daYObservation
        , __daVarianceFunction
        , __iaWhichObs
    );
    InitSubject();
    Interp(FALSE, dLL, dGrad
        , subjId
        , pnObs
        , pbOK
        , INTERP_MODE_PRED
        , TRUE
        , FALSE
        , FALSE
    );
    EndSubject(0);
    *pb = pc;
}

void NewPredOneSubject1(
    int bGenGradients
    , int nEta
    , int * pnObs
    , const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
    , int * __nPrediction
    , double * __daXPrediction, double * __taLastDose, int * __iaWhichDose, int * __iaWhichReset, double * __daYPrediction, double * __daYObservation, double * __daVarianceFunction
    , double * __daYdK
    , double * __daSdK
    , int * __iaWhichObs
)
{
    int iState = 1;
    pc = *pb;
    SetPredArrays(__nPrediction
        , __daXPrediction
        , __taLastDose
        , __iaWhichDose
        , __iaWhichReset
        , __daYPrediction
        , __daYObservation
        , __daVarianceFunction
        , __iaWhichObs
    );

    if (bGenGradients)
    {
        SetGradArrays(nEta, __daYdK, __daSdK);
    }

    InitSubject();
    Interp(bGenGradients, NULL, NULL
        , subjId
        , pnObs
        , pbOK
        , INTERP_MODE_PRED
        , TRUE
        , FALSE
        , FALSE
    );
    EndSubject(0);
    *pb = pc;
}

void SimOneSubject(
    const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
    , int bSampleEpsilon
    , int bForSimTbl
    , int bForVPC
)
{
    int iState = 1;
    pc = *pb;
    InitSubject();
    Interp(FALSE, NULL, NULL
        , subjId
        , NULL
        , pbOK
        , INTERP_MODE_SIM
        , bSampleEpsilon
        , bForSimTbl
        , bForVPC
    );
    EndSubject(1);
    *pb = pc;
}

void Init1OneSubject(
    const unsigned char * *pb
    , const SubjectId & subjId
    , int * pbOK
)
{
    int iState = 1;
    pc = *pb;
    InitSubject();
    Interp(FALSE, NULL, NULL
        , subjId
        , NULL
        , pbOK
        , INTERP_MODE_INIT1
        , TRUE
        , FALSE
        , FALSE
    );
    EndSubject(0);
    *pb = pc;
}

void ScanOneSubject(
    const unsigned char * *pb
)
{
    int n = 0;
    pc = *pb;
    ScanSubject(&n);
    *pb = pc;
}

void ScanOneSubjectCountObs(
    const unsigned char * *pb
    , int * pnObs
    , double * pdMaxTime
)
{
    pc = *pb;
    InterpScanSubject(pnObs, pdMaxTime);
    *pb = pc;
}

void ScanOneSubjectGetCovariate(
    const unsigned char * *pb
    , const char * sCovName
    , double * pVal
)
{
    pc = *pb;
    ScanSubjectGetCovariate(sCovName, pVal);
    *pb = pc;
}

void ResetInteg() {
    int i;
    memset(globalY, 0, (nIntegAll + 1 + 100) * sizeof(globalY[0]));
    memset(globalIRate, 0, (nIntegAll + 1) * sizeof(globalIRate[0]));
    globalY[nIntegAll] = 1; // init dummy integrator for infusions
}

void ResetUrine() {
    int i;
    for (i = nIntegAll - nUrine - nEvent; i < nIntegAll - nEvent; i++) globalY[i] = 0;
}
void TDL4ResetEvent() {
    int i;
    for (i = nIntegAll - nEvent; i < nIntegAll; i++) globalY[i] = 0;
}
void ResetGradient() {
}

void DerivWrapped(long* _pN, double* _pt, double zzY[], double zzR[])
{
    Deriv(_pN, _pt, zzY, zzR, globalIRate);
}

void GradientDerivWrapped(long* _pN, double* _pt, double zzY[], double zzR[])
{
    GradientDeriv(_pN, _pt, zzY, zzR, globalIRate);
}

void JacobianWrapped(int* pN, double* pt, double zzY[], int* pML, int* pMU, double* _jacobian, int* pNRJ)
{
    Jacobian(pN, pt, zzY, globalIRate, pML, pMU, _jacobian, pNRJ);
}

void GradientJacobianWrapped(int* pN, double* pt, double zzY[], int* pML, int* pMU, double* _gjacobian, int* pNRJ)
{
    GradientJacobian(pN, pt, zzY, globalIRate, pML, pMU, _gjacobian, pNRJ);
}

int GetVarValue(const char * _nm, double * _pv)
{
    return GetVarValue(_nm, _pv, globalY);
}

void SetVarValue(const char * _nm, double * _pv)
{
    SetVarValue(_nm, _pv, globalY);
}
