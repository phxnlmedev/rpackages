#pragma once

// declare generic closed-form state machine structure
typedef struct _cf2machineTag
{
    double dTimeOfState;
    double dTimeOfGetVal;
    double dRateA;	// infusion rate into absorption cpt
    double dRate1;	// infusion rate into cpt 1
    int na;			// number of exponentials, max 5
    int err;

    double aaa[5];							// residues for compartment A from a(0)
    double a1a[5], a11[5], a12[5], a13[5];	// residues for compartment 1 from a(0), y1(0), y2(0), y3(0)
    double a2a[5], a21[5], a22[5], a23[5];	// residues for compartment 2 from a(0), y1(0), y2(0), y3(0)
    double a3a[5], a31[5], a32[5], a33[5];	// residues for compartment 3 from a(0), y1(0), y2(0), y3(0)

    double x[5];	// X's (negative rate constants)

    double staa[5];	// state of each exponential at time t (compartment a from a)

    double st1a[5];	// state of each exponential at time t (compartment 1 from a)
    double st11[5];	// state of each exponential at time t (compartment 1 from 1)
    double st12[5];	// state of each exponential at time t (compartment 1 from 2)
    double st13[5];	// state of each exponential at time t (compartment 1 from 3)

    double st2a[5];	// state of each exponential at time t (compartment 2 from a)
    double st21[5];	// state of each exponential at time t (compartment 2 from 1)
    double st22[5];	// state of each exponential at time t (compartment 2 from 2)
    double st23[5];	// state of each exponential at time t (compartment 2 from 3)

    double st3a[5];	// state of each exponential at time t (compartment 3 from a)
    double st31[5];	// state of each exponential at time t (compartment 3 from 1)
    double st32[5];	// state of each exponential at time t (compartment 3 from 2)
    double st33[5];	// state of each exponential at time t (compartment 3 from 3)

} _cf2machine;

void CF2Clear(_cf2machine * _p);
void CF2InitMacro1(_cf2machine * _p, double _al0);
void CF2InitMacro2(_cf2machine * _p, double _al0, double _a1, double _al1);
void CF2InitMacro3(_cf2machine * _p, double _al0, double _a1, double _al1, double _a2, double _al2);

void CF2InitMacro1New(_cf2machine * _p, double _a0, double _al0);
void CF2InitMacro2New(_cf2machine * _p, double _a0, double _al0, double _a1, double _al1);
void CF2InitMacro3New(_cf2machine * _p, double _a0, double _al0, double _a1, double _al1, double _a2, double _al2);
void CF2AppendKaNew(_cf2machine * _p, double Ka);

void CF2InitMicro1(_cf2machine * _p, double _k10);
void CF2InitMicro2(_cf2machine * _p, double _k10, double _k12, double _k21);
void CF2InitMicro3(_cf2machine * _p, double _k10, double _k12, double _k21, double _k13, double _k31);
void CF2Convolve(
    int _na, double _a[], double _al[]
    , int _nb, double _b[], double _bl[]
    , double _u[], double _v[]
);
void CF2Add1stOrd(_cf2machine * _p, double _scale, double _ka);

void CF2Reset(_cf2machine * _p);

void CF2Advance(_cf2machine * _p, double _dt);

void CF2SetTimeOfState(_cf2machine * _p, double _t);
void CF2SetTimeOfGetVal(_cf2machine * _p, double _t);

double CF2GetValA(_cf2machine * _p);
double CF2GetVal1(_cf2machine * _p);
double CF2GetVal2(_cf2machine * _p);
double CF2GetVal3(_cf2machine * _p);
double CF2GetRateA(_cf2machine * _p);
double CF2GetRate1(_cf2machine * _p);

void CF2SetValA(_cf2machine * _p, double _dv);
void CF2SetVal1(_cf2machine * _p, double _dv);
void CF2SetVal2(_cf2machine * _p, double _dv);
void CF2SetVal3(_cf2machine * _p, double _dv);
void CF2SetRateA(_cf2machine * _p, double _dr);
void CF2SetRate1(_cf2machine * _p, double _dr);

void CF2AddValA(_cf2machine * _p, double _dv);
void CF2AddVal1(_cf2machine * _p, double _dv);
void CF2AddVal2(_cf2machine * _p, double _dv);
void CF2AddVal3(_cf2machine * _p, double _dv);
void CF2AddRateA(_cf2machine * _p, double _dr);
void CF2AddRate1(_cf2machine * _p, double _dr);

