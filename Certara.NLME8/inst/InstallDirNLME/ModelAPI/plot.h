#pragma once

// plot variable flags
#define _SWEEP_AVAIL (1<<0)
#define _POINT_AVAIL (1<<1)

#define _CAN_LOG     (1<<2)
#define _CAN_TRELLIS (1<<3)
#define _CAN_OVERLAY (1<<4)

#define _TYP_CONTIN  (1<<8)
#define _TYP_MULTI   (2<<8)
#define _TYP_EVENT   (3<<8)
#define _TYP_COUNT   (4<<8)

void GetPlotVarInfo(int * _pi, int * _pflags, char * _nmObs, char * _nmPred);
int GetNPlotVar();
