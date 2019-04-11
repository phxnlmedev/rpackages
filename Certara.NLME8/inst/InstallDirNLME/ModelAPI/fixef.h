#pragma once

void GetFixefName(int * _pi, char * _nm);
void GetFixefUnits(int * _pi, char * _nm);
void RestoreFixefLowInitHighFromModel();
void SetFixefLowInitHigh(const char * _nm, double * _fixefLow, double * _fixefInit, double * _fixefHigh);
void GetFixefLowInitHigh(const char * _nm, double * _fixefLow, double * _fixefInit, double * _fixefHigh);
void InitFixefErrorEstimate();
void GetFixefErrorInitial(double * _fixef);
void GetFixefErrorLimits(double * _low, double * _high);
void GetFixefValue(int * _iFixef, double * _dValue);
