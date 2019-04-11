#pragma once

void GetSaemCMatrixSize(int * nBaseEta, int * nExtraEta, int * nTheta);
void GetSaemCMatrix(double * daCMat);
int GetCovariateIsInCMatrix(const char * name);
void GetSaemThetaInfo(int * iaCColToITheta, int * baCColIsLog);
void GetSaemNaivePoolThetas(int * nNPTheta, int * iaTheta);
int CanUseSAEM();
