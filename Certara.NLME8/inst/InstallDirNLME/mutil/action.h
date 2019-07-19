#pragma once

// action table
#define ZZMAX_ACTION 10000

#define ZZACTION_BOLUS1		1
#define ZZACTION_INFSTRT1	2
#define ZZACTION_INFEND1	3
#define ZZACTION_BOLUS2		4
#define ZZACTION_INFSTRT2	5
#define ZZACTION_INFEND2	6

#define ZZACTION_SLEEP		7
#define ZZACTION_TIME		8
#define ZZACTION_DOSETIME	9

#define ZZMARK_BOLUS1		21
#define ZZMARK_INFSTRT1		22
#define ZZMARK_INFEND1		23
#define ZZMARK_BOLUS2		24
#define ZZMARK_INFSTRT2		25
#define ZZMARK_INFEND2		26
#define ZZMARK_ENDROW		27

void InsertAction(double _dt, int _priority, int _code, int _iarg, double _darg1, double _darg2, void * _parg);
void AdvanceWithActions(int bGenGradients, double * pTimeOuter, double dt, int bTimeAdvances, int bFinishUpSubject);

void SaveEventQueue();
void RestoreEventQueue();

void CleanOutActionList(int bGenGradients);

void ClearActions();
