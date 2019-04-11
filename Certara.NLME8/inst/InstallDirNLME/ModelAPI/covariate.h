#pragma once

int GetNumCovariates();
int GetCovariateNumber(const char * _nm);
void GetCovariateName(int * _pi, char * _nm);
void GetCovariateCategorical(int * _pi, int * _cat);
void GetCovariateDirection(int * _pi, int * _dir);
void SetCovariateInterp(double _t, const char * _nm, double _v1, double _dt, double _v2, int _bForward);
