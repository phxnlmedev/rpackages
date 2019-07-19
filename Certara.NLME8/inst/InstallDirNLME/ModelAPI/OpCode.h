#pragma once

enum OpCode
{
    OP_NUL = 0,
    OP_ID1,
    OP_ID2,
    OP_ID3,
    OP_ID4,
    OP_ID5,
    OP_NUM,
    OP_DT,
    OP_COVR,
    OP_DCOVR,	// backward delta covr
    OP_FCOVR,	// forward delta covr
    OP_BOLUS1,
    OP_BOLUS2,
    OP_INF1,
    OP_INF2,
    OP_RESET,
    OP_STRTOBS,
    OP_OBS,
    OP_OBS_BQL,
    OP_ENDOBS,
    OP_RP,
    OP_QUEST,
    OP_LP,
    OP_CLR,		// clear the stack
    OP_CLR_RESET,
    OP_INIT1,	// initialize subject after covariates initialized
    OP_ENDROW,	// mark end of input row. needed for tables
    OP_EVENT,
    OP_RPT,
    OP_SS,
    OP_SS1,
    OP_SS2,
    OP_SSEND,
    OP_SSNM,
    OP_ADDL,
    OP_ADDLNM,
    OP_DEF,
    OP_COMMA,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_POW,
    OP_DUP,
    OP_SET,
    OP_REF,
};
