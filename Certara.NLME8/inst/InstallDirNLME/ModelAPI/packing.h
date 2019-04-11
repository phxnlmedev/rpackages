#pragma once

void GetInitialStdev(double * _svec);
void GetInitialOmega5(double * _omat);
void TruncateOmega5(double * _omat);
void UnpackParameters(double * _thvec, double * _omat, double * _pack, int * _ldOmat, int * _nPack);
void UnpackParameters1(double * _thvec, double * _omat, double * _svec, double * _pack, int * _ldOmat, int * _nPack);
void PackParameters(double * _thvec, double * _omat, double * _pack, int * _ldOmat, int * _nPack);
void PackParameters1(double * _thvec, double * _omat, double * _svec, double * _pack, int * _ldOmat, int * _nPack);
void TypXParameters(double * _thvec, double * _omat, double * _pack, int * _ldOmat, int * _nPack);
void TypXParameters1(double * _thvec, double * _omat, double * _svec, double * _pack, int * _ldOmat, int * _nPack);
void GetFixefErrToThetaMap(int * _iaTheta, int * _nth);
void GetRanefToEtaMap(int * _iaEta, int * _nth);
void UnpackOmega(double * _omat, double * _pack, int * _ldOmat, int * _nPack);
void PackOmega(double * _omat, double * _pack, int * _ldOmat, int * _nPack);
void TypXOmega(double * _omat, double * _pack, int * _ldOmat, int * _nPack);
void UnpackTheta(double * _fixef, double * _thvec);
void PackTheta(double * _fixef, double * _thvec, int * _nth);
void TypXTheta(double * _fixef, double * _thvec, int * _nth);
void GetNPack(int * _nPack);
void GetNPackOmega(int * _nPack);
void GetNSeqOmega(int * _nSeq);
void GetOmegaSequenceInfo(int * _iSeq1, int * _nEta, int * _nSame);
void GetNPackRanCov(int * _nPack);
void GetPackedIndexRanCov(int * _iRan, int * _jRan, int * _index);
