#include "closed_form.h"

#include "mutil_bool.h"

#include <math.h>
#include <memory.h>

static void MyAssert(BOOL b)
{
    if (!b)
    {
        int breakhere = 1;
    }
}

////////////////// Closed Form stuff

void CF2Clear(_cf2machine * _p)
{
    memset(_p, 0, sizeof(*_p));
}

void CF2InitMacro1(_cf2machine * _p, double _al0)
{
    CF2Reset(_p);
    _p->na = 1;
    _p->a11[0] = 1;
    _p->x[0] = -_al0;
}

void CF2InitMacro2(_cf2machine * _p, double _al0, double _a1, double _al1)
{
    CF2Reset(_p);
    _p->na = 2;
    _p->x[0] = -_al0;
    _p->x[1] = -_al1;
    _p->a11[0] = 1 - _a1;
    _p->a11[1] = _a1;
}

void CF2InitMacro3(_cf2machine * _p, double _al0, double _a1, double _al1, double _a2, double _al2)
{
    CF2Reset(_p);
    _p->na = 3;
    _p->x[0] = -_al0;
    _p->x[1] = -_al1;
    _p->x[2] = -_al2;
    _p->a11[0] = 1 - _a1 - _a2;
    _p->a11[1] = _a1;
    _p->a11[2] = _a2;
}

void CF2InitMacro1New(_cf2machine * _p, double _a0, double _al0)
{
    CF2Reset(_p);
    _p->na = 1;
    _p->a11[0] = _a0;
    _p->x[0] = -_al0;
}

void CF2InitMacro2New(_cf2machine * _p, double _a0, double _al0, double _a1, double _al1)
{
    CF2Reset(_p);
    _p->na = 2;
    _p->x[0] = -_al0;
    _p->x[1] = -_al1;
    _p->a11[0] = _a0;
    _p->a11[1] = _a1;
}

void CF2InitMacro3New(_cf2machine * _p, double _a0, double _al0, double _a1, double _al1, double _a2, double _al2)
{
    CF2Reset(_p);
    _p->na = 3;
    _p->x[0] = -_al0;
    _p->x[1] = -_al1;
    _p->x[2] = -_al2;
    _p->a11[0] = _a0;
    _p->a11[1] = _a1;
    _p->a11[2] = _a2;
}

void CF2AppendKaNew(_cf2machine * _p, double Ka)
{
    int i, n = _p->na;
    double sum = 0;

    for (i = 0; i < n; i++)
    {
        sum += _p->a11[i];
    }

    // add absorption rate term
    _p->a11[n] = -sum;
    _p->x[n] = -Ka;
    _p->na = n + 1;
}

void CF2InitMicro1(_cf2machine * _p, double _k10)
{
    CF2Reset(_p);
    _p->na = 1;
    _p->a11[0] = 1;
    _p->x[0] = -_k10;
}

void CF2InitMicro2(_cf2machine * _p, double _k10, double _k12, double _k21)
{
    double ktot, temp, root;
    ktot = _k21 + _k12 + _k10;
    temp = ktot * ktot - 4 * _k10 * _k21;
    MyAssert(temp >= 0);
    root = sqrt(temp);
    CF2Reset(_p);
    _p->na = 2;
    _p->x[0] = (-ktot - root) / 2;
    _p->x[1] = (-ktot + root) / 2;
    MyAssert(_p->x[0] < 0);
    MyAssert(_p->x[1] < 0);
    MyAssert(_p->x[1] != _p->x[0]);
    _p->a11[0] = (_k21 + _p->x[0]) / (_p->x[0] - _p->x[1]);	// cpt 1 from y1(0)
    _p->a11[1] = (_k21 + _p->x[1]) / (_p->x[1] - _p->x[0]);

    _p->a12[0] = (_k21) / (_p->x[0] - _p->x[1]);	// cpt 1 from y2(0)
    _p->a12[1] = (_k21) / (_p->x[1] - _p->x[0]);

    _p->a21[0] = (_k12) / (_p->x[0] - _p->x[1]);	// cpt 2 from y1(0)
    _p->a21[1] = (_k12) / (_p->x[1] - _p->x[0]);

    _p->a22[0] = (_k10 + _k12 + _p->x[0]) / (_p->x[0] - _p->x[1]);	// cpt 2 from y2(0)
    _p->a22[1] = (_k10 + _k12 + _p->x[1]) / (_p->x[1] - _p->x[0]);
}

void CF2InitMicro3(_cf2machine * _p, double _k10, double _k12, double _k21, double _k13, double _k31)
{
    const double pi = 2 * asin(1.0);
    double u0, u1, u2, p, q, r1, r2, r3, r4;
    CF2Reset(_p);
    _p->na = 3;
    u0 = _k10 * _k21 * _k31;
    u1 = _k10 * _k31 + _k21 * _k31 + _k21 * _k13 + _k10 * _k21 + _k31 * _k12;
    u2 = _k10 + _k12 + _k13 + _k21 + _k31;
    p = u1 - (u2 * u2 / 3);
    q = 2 * u2 * u2 * u2 / 27 - u1 * u2 / 3 + u0;
    MyAssert(p < 0);
    r1 = sqrt(-p * p * p / 27);
    r2 = -q / 2 / r1;
    MyAssert(r2 >= -1 && r2 <= 1);
    r3 = acos(r2) / 3;
    r4 = 2 * exp(log(r1) / 3);
    _p->x[0] = (cos(r3 + 0 * pi / 3) * r4 - u2 / 3);
    MyAssert(_p->x[0] < 0);
    _p->x[1] = (cos(r3 + 2 * pi / 3) * r4 - u2 / 3);
    MyAssert(_p->x[1] < 0);
    _p->x[2] = (cos(r3 + 4 * pi / 3) * r4 - u2 / 3);
    MyAssert(_p->x[2] < 0);
    MyAssert(_p->x[0] != _p->x[1]);
    MyAssert(_p->x[1] != _p->x[2]);
    MyAssert(_p->x[0] != _p->x[2]);

    //---------------------------------------------------------------------------------
    _p->a11[0] = (_k21 + _p->x[0]) * (_k31 + _p->x[0]) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);	// cpt 1 from y1(0)
    _p->a11[1] = (_k21 + _p->x[1]) * (_k31 + _p->x[1]) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a11[2] = (_k21 + _p->x[2]) * (_k31 + _p->x[2]) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    _p->a12[0] = (_k21) * (_k31 + _p->x[0]) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);	// cpt 1 from y2(0)
    _p->a12[1] = (_k21) * (_k31 + _p->x[1]) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a12[2] = (_k21) * (_k31 + _p->x[2]) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    _p->a13[0] = (_k31) * (_k21 + _p->x[0]) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);	// cpt 1 from y3(0)
    _p->a13[1] = (_k31) * (_k21 + _p->x[1]) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a13[2] = (_k31) * (_k21 + _p->x[2]) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    //---------------------------------------------------------------------------------
    _p->a21[0] = (_k12) * (_k31 + _p->x[0]) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);					// cpt 2 from y1(0)
    _p->a21[1] = (_k12) * (_k31 + _p->x[1]) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a21[2] = (_k12) * (_k31 + _p->x[2]) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    _p->a22[0] = ((_k31 + _p->x[0]) * (_k12 + _k10 + _p->x[0]) + _k13 * _p->x[0]) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);	// cpt 2 from y2(0)
    _p->a22[1] = ((_k31 + _p->x[1]) * (_k12 + _k10 + _p->x[1]) + _k13 * _p->x[1]) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a22[2] = ((_k31 + _p->x[2]) * (_k12 + _k10 + _p->x[2]) + _k13 * _p->x[2]) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    _p->a23[0] = (_k12 * _k31) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);									// cpt 2 from y3(0)
    _p->a23[1] = (_k12 * _k31) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a23[2] = (_k12 * _k31) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    //---------------------------------------------------------------------------------
    _p->a31[0] = (_k13) * (_k21 + _p->x[0]) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);					// cpt 3 from y1(0)
    _p->a31[1] = (_k13) * (_k21 + _p->x[1]) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a31[2] = (_k13) * (_k21 + _p->x[2]) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    _p->a32[0] = (_k13 * _k21) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);									// cpt 3 from y3(0)
    _p->a32[1] = (_k13 * _k21) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a32[2] = (_k13 * _k21) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);

    _p->a33[0] = ((_k21 + _p->x[0]) * (_k13 + _k10 + _p->x[0]) + _k12 * _p->x[0]) / (_p->x[0] - _p->x[1]) / (_p->x[0] - _p->x[2]);	// cpt 3 from y2(0)
    _p->a33[1] = ((_k21 + _p->x[1]) * (_k13 + _k10 + _p->x[1]) + _k12 * _p->x[1]) / (_p->x[1] - _p->x[0]) / (_p->x[1] - _p->x[2]);
    _p->a33[2] = ((_k21 + _p->x[2]) * (_k13 + _k10 + _p->x[2]) + _k12 * _p->x[2]) / (_p->x[2] - _p->x[0]) / (_p->x[2] - _p->x[1]);
}

void CF2Convolve(
    int _na, double _a[], double _al[]
    , int _nb, double _b[], double _bl[]
    , double _u[], double _v[]
)
{
    int i, j;
    double sum;

    for (i = 0; i < _na; i++)
    {
        MyAssert(_al[i] < 0);
        _v[i] = _al[i];
    }

    for (j = 0; j < _nb; j++)
    {
        MyAssert(_bl[j] < 0);
        _v[_na + j] = _bl[j];
    }

    for (i = 0; i < _na; i++)
    {
        sum = 0;

        for (j = 0; j < _nb; j++)
        {
            // this convolution has a singularity if any alphas match,
            MyAssert(_bl[j] != _al[i]);
            sum += _b[j] / (_al[i] - _bl[j]);
        }

        _u[i] = sum * _a[i];
    }

    for (j = 0; j < _nb; j++)
    {
        sum = 0;

        for (i = 0; i < _na; i++)
        {
            MyAssert(_bl[j] != _al[i]);
            sum += _a[i] / (_bl[j] - _al[i]);
        }

        _u[_na + j] = sum * _b[j];
    }
}

void CF2AdjustRoundoff(int _na, double _a[])
{
    // example: _na = 3, _a = -1.6534994858145, 1.5464462632091, 0.10705322260540
    int _i;
    double _sum = 0, _dif = 0;

    if (_na < 2)
    {
        return;
    }

    // only consider case where first is negative and rest are positive
    if (_a[0] >= 0)
    {
        return;
    }

    for (_i = 1; _i < _na; _i++)
        if (_a[_i] <= 0)
        {
            return;
        }

    for (_i = 1; _i < _na; _i++)
    {
        _sum += _a[_i];    // sum = 1.6534994858145
    }

    _dif = _sum + _a[0]; // dif = -2.0816681711722e-016

    if (_dif < 0)
    {
        _dif = -_dif;    // dif = 2.0816681711722e-016
    }

    if (_dif > _sum * 1e-14)
    {
        return;    // compare against 1.6534994858145e-014
    }

    _a[0] = -_sum; // if dif < roundoff error, make it zero
}

void CF2Add1stOrd(_cf2machine * _p, double _scale, double _ka)
{
    BOOL bChanged;
    int i, j;
    double a[5], al[5];
    double b[2], bl[2];
    double u[10], v[10];
    int nb = 1, nu = _p->na + 1;

    for (i = 0; i < _p->na; i++)
    {
        al[i] = _p->x[i];
    }

    bChanged = TRUE;

    for (j = 0; j < 10 && bChanged; j++)
    {
        bChanged = FALSE;

        for (i = 0; i < _p->na; i++)
        {
            if (fabs(_ka + _p->x[i]) < 1e-7)
            {
                _ka += 1e-6;
                bChanged = TRUE;
            }
        }
    }

    bl[0] = -_ka;
    b[0] = -bl[0];

    // get the exponents here
    v[0] = bl[0];

    for (i = 0; i < _p->na; i++)
    {
        v[i + 1] = _p->x[i];
    }

    // residues for compartment A as a function of A
    _p->aaa[0] = 1;

    // residues for compartment 1 as a function of A
    for (i = 0; i < _p->na; i++)
    {
        a[i] = _p->a11[i];
    }

    CF2Convolve(nb, b, bl, _p->na, a, al, u, v);
    // make sure amount in plasma cpt when empty is exactly zero
    CF2AdjustRoundoff(nu, u);

    for (i = 0; i < nu; i++)
    {
        _p->a1a[i] = u[i];
    }

    // residues for compartment 2 as a function of A
    for (i = 0; i < _p->na; i++)
    {
        a[i] = _p->a21[i];
    }

    CF2Convolve(nb, b, bl, _p->na, a, al, u, v);

    for (i = 0; i < nu; i++)
    {
        _p->a2a[i] = u[i];
    }

    // residues for compartment 3 as a function of A
    for (i = 0; i < _p->na; i++)
    {
        a[i] = _p->a31[i];
    }

    CF2Convolve(nb, b, bl, _p->na, a, al, u, v);

    for (i = 0; i < nu; i++)
    {
        _p->a3a[i] = u[i];
    }

#define SHIFT_A(a) for (i = nu; --i >= 1;) _p->a[i] = _p->a[i-1];	_p->a[0] = 0
    // residues for compartment 1 as a function of 1
    SHIFT_A(a11);
    SHIFT_A(a21);
    SHIFT_A(a31);

    SHIFT_A(a12);
    SHIFT_A(a22);
    SHIFT_A(a32);

    SHIFT_A(a13);
    SHIFT_A(a23);
    SHIFT_A(a33);
#undef  SHIFT_A

    for (i = 0; i < nu; i++)
    {
        _p->x[i] = v[i];
    }

    _p->na = nu;
}

void CF2Reset(_cf2machine * _p)
{
    CF2Clear(_p);
}

void CF2SetTimeOfState(_cf2machine * _p, double _t)
{
    _p->dTimeOfState = _t;
}

void CF2SetTimeOfGetVal(_cf2machine * _p, double _t)
{
    _p->dTimeOfGetVal = _t;
}

void CF2Advance(_cf2machine * _p, double _dt)
{
    int i;
    double ta;

    double trateaa;
    double trate1a;
    double trate2a;
    double trate3a;

    double trate11;
    double trate21;
    double trate31;

    for (i = 0; i < _p->na; i++)
    {
        MyAssert(_p->x[i] < 0);
        ta = exp(_p->x[i] * _dt);

        trateaa = -_p->dRateA * _p->aaa[i] * (1 - ta) / _p->x[i];
        trate1a = -_p->dRateA * _p->a1a[i] * (1 - ta) / _p->x[i];
        trate2a = -_p->dRateA * _p->a2a[i] * (1 - ta) / _p->x[i];
        trate3a = -_p->dRateA * _p->a3a[i] * (1 - ta) / _p->x[i];

        trate11 = -_p->dRate1 * _p->a11[i] * (1 - ta) / _p->x[i];
        trate21 = -_p->dRate1 * _p->a21[i] * (1 - ta) / _p->x[i];
        trate31 = -_p->dRate1 * _p->a31[i] * (1 - ta) / _p->x[i];

        _p->staa[i] = _p->staa[i] * ta + trateaa;

        _p->st1a[i] = _p->st1a[i] * ta + trate1a;
        _p->st11[i] = _p->st11[i] * ta + trate11;
        _p->st12[i] = _p->st12[i] * ta;
        _p->st13[i] = _p->st13[i] * ta;

        _p->st2a[i] = _p->st2a[i] * ta + trate2a;
        _p->st21[i] = _p->st21[i] * ta + trate21;
        _p->st22[i] = _p->st22[i] * ta;
        _p->st23[i] = _p->st23[i] * ta;

        _p->st3a[i] = _p->st3a[i] * ta + trate3a;
        _p->st31[i] = _p->st31[i] * ta + trate31;
        _p->st32[i] = _p->st32[i] * ta;
        _p->st33[i] = _p->st33[i] * ta;
    }

    _p->dTimeOfState += _dt;
    _p->dTimeOfGetVal = _p->dTimeOfState;
}

double CF2GetValA(_cf2machine * _p)
{
    double val = 0;
    double _staa0 = _p->staa[0];
    double _dt = (_p->dTimeOfGetVal - _p->dTimeOfState);

    if (_dt != 0)
    {
        double ta = exp(_p->x[0] * _dt);
        double trateaa = -_p->dRateA * _p->aaa[0] * (1 - ta) / _p->x[0];
        _staa0 = _staa0 * ta + trateaa;
    }

    val += _staa0;
    return val;
}

double CF2GetVal1(_cf2machine * _p)
{
    int i;
    double val = 0;
    double _st1a[5], _st11[5], _st12[5], _st13[5];
    double _dt = (_p->dTimeOfGetVal - _p->dTimeOfState);

    for (i = 0; i < _p->na; i++)
    {
        _st1a[i] = _p->st1a[i];
        _st11[i] = _p->st11[i];
        _st12[i] = _p->st12[i];
        _st13[i] = _p->st13[i];
    }

    if (_dt != 0)
    {
        for (i = 0; i < _p->na; i++)
        {
            double ta = exp(_p->x[i] * _dt);
            double trate1a = -_p->dRateA * _p->a1a[i] * (1 - ta) / _p->x[i];
            double trate11 = -_p->dRate1 * _p->a11[i] * (1 - ta) / _p->x[i];
            _st1a[i] = _p->st1a[i] * ta + trate1a;
            _st11[i] = _p->st11[i] * ta + trate11;
            _st12[i] = _p->st12[i] * ta;
            _st13[i] = _p->st13[i] * ta;
        }
    }

    for (i = 0; i < _p->na; i++)
    {
        val += _st1a[i];
        val += _st11[i];
        val += _st12[i];
        val += _st13[i];
    }

    return val;
}

double CF2GetVal2(_cf2machine * _p)
{
    int i;
    double val = 0;
    double _st2a[5], _st21[5], _st22[5], _st23[5];
    double _dt = (_p->dTimeOfGetVal - _p->dTimeOfState);

    for (i = 0; i < _p->na; i++)
    {
        _st2a[i] = _p->st2a[i];
        _st21[i] = _p->st21[i];
        _st22[i] = _p->st22[i];
        _st23[i] = _p->st23[i];
    }

    if (_dt != 0)
    {
        for (i = 0; i < _p->na; i++)
        {
            double ta = exp(_p->x[i] * _dt);
            double trate2a = -_p->dRateA * _p->a2a[i] * (1 - ta) / _p->x[i];
            double trate21 = -_p->dRate1 * _p->a21[i] * (1 - ta) / _p->x[i];
            _st2a[i] = _p->st2a[i] * ta + trate2a;
            _st21[i] = _p->st21[i] * ta + trate21;
            _st22[i] = _p->st22[i] * ta;
            _st23[i] = _p->st23[i] * ta;
        }
    }

    for (i = 0; i < _p->na; i++)
    {
        val += _st2a[i];
        val += _st21[i];
        val += _st22[i];
        val += _st23[i];
    }

    return val;
}

double CF2GetVal3(_cf2machine * _p)
{
    int i;
    double val = 0;
    double _st3a[5], _st31[5], _st32[5], _st33[5];
    double _dt = (_p->dTimeOfGetVal - _p->dTimeOfState);

    for (i = 0; i < _p->na; i++)
    {
        _st3a[i] = _p->st3a[i];
        _st31[i] = _p->st31[i];
        _st32[i] = _p->st32[i];
        _st33[i] = _p->st33[i];
    }

    if (_dt != 0)
    {
        for (i = 0; i < _p->na; i++)
        {
            double ta = exp(_p->x[i] * _dt);
            double trate3a = -_p->dRateA * _p->a3a[i] * (1 - ta) / _p->x[i];
            double trate31 = -_p->dRate1 * _p->a31[i] * (1 - ta) / _p->x[i];
            _st3a[i] = _p->st3a[i] * ta + trate3a;
            _st31[i] = _p->st31[i] * ta + trate31;
            _st32[i] = _p->st32[i] * ta;
            _st33[i] = _p->st33[i] * ta;
        }
    }

    for (i = 0; i < _p->na; i++)
    {
        val += _st3a[i];
        val += _st31[i];
        val += _st32[i];
        val += _st33[i];
    }

    return val;
}

double CF2GetRateA(_cf2machine * _p)
{
    return _p->dRateA;
}

double CF2GetRate1(_cf2machine * _p)
{
    return _p->dRate1;
}

void CF2SetValA(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->staa[i] = _dv * _p->aaa[i];    // set state for how A depends on A
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st1a[i] = _dv * _p->a1a[i];    // set state for how 1 depends on A
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st2a[i] = _dv * _p->a2a[i];    // set state for how 2 depends on A
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st3a[i] = _dv * _p->a3a[i];    // set state for how 3 depends on A
    }
}

void CF2SetVal1(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->st11[i] = _dv * _p->a11[i];    // set state for how 1 depends on 1
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st21[i] = _dv * _p->a21[i];    // set state for how 2 depends on 1
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st31[i] = _dv * _p->a31[i];    // set state for how 3 depends on 1
    }
}

void CF2SetVal2(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->st12[i] = _dv * _p->a12[i];    // set state for how 1 depends on 2
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st22[i] = _dv * _p->a22[i];    // set state for how 2 depends on 2
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st32[i] = _dv * _p->a32[i];    // set state for how 3 depends on 2
    }
}

void CF2SetVal3(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->st13[i] = _dv * _p->a13[i];    // set state for how 1 depends on 3
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st23[i] = _dv * _p->a23[i];    // set state for how 2 depends on 3
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st33[i] = _dv * _p->a33[i];    // set state for how 3 depends on 3
    }
}

void CF2SetRateA(_cf2machine * _p, double _dr)
{
    _p->dRateA = _dr;
}

void CF2SetRate1(_cf2machine * _p, double _dr)
{
    _p->dRate1 = _dr;
}

void CF2AddValA(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->staa[i] += _dv * _p->aaa[i];    // set state for how A depends on A
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st1a[i] += _dv * _p->a1a[i];    // set state for how 1 depends on A
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st2a[i] += _dv * _p->a2a[i];    // set state for how 2 depends on A
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st3a[i] += _dv * _p->a3a[i];    // set state for how 3 depends on A
    }
}

void CF2AddVal1(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->st11[i] += _dv * _p->a11[i];    // set state for how 1 depends on 1
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st21[i] += _dv * _p->a21[i];    // set state for how 2 depends on 1
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st31[i] += _dv * _p->a31[i];    // set state for how 3 depends on 1
    }
}

void CF2AddVal2(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->st12[i] += _dv * _p->a12[i];    // set state for how 1 depends on 2
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st22[i] += _dv * _p->a22[i];    // set state for how 2 depends on 2
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st32[i] += _dv * _p->a32[i];    // set state for how 3 depends on 2
    }
}

void CF2AddVal3(_cf2machine * _p, double _dv)
{
    int i;

    for (i = 0; i < _p->na; i++)
    {
        _p->st13[i] += _dv * _p->a13[i];    // set state for how 1 depends on 3
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st23[i] += _dv * _p->a23[i];    // set state for how 2 depends on 3
    }

    for (i = 0; i < _p->na; i++)
    {
        _p->st33[i] += _dv * _p->a33[i];    // set state for how 3 depends on 3
    }
}

void CF2AddRateA(_cf2machine * _p, double _dr)
{
    _p->dRateA += _dr;
}

void CF2AddRate1(_cf2machine * _p, double _dr)
{
    _p->dRate1 += _dr;
}
