#include "ODE.h"

#include "ModelAPI.h"
#include "MUtil.h"

ODECallbackTable * _pocb = 0;

void SetODECallbacks(ODECallbackTable* _p)
{
    _pocb = _p;
}

bool _ISENABLED(int i)
{
    return !_pocb || _pocb->_pfIsEnabled(i);
}

void ODESolve(int bGenGradients, int iODELevel, double *pTimeOuter, double _dt, int bTimeAdvances, double zzY[])
{
    if (_pocb && _pocb->_pfODESolve)
    {
        if (bGenGradients)
        {
            int nIntegGradientDeriv = GetNIntegGradientDeriv();
            (*_pocb->_pfODESolve)(iODELevel, _piState, nIntegGradientDeriv, pTimeOuter, _dt, zzY, GradientDerivWrapped, AddIncrements, bTimeAdvances, GradientJacobianWrapped);
        }
        else
        {
            (*_pocb->_pfODESolve)(iODELevel, _piState, GetNintegAll() + 1, pTimeOuter, _dt, zzY, DerivWrapped, AddIncrements, bTimeAdvances, JacobianWrapped);
        }
    }

}
