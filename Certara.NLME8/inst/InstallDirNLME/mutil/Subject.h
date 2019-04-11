#pragma once

#include <vector>

struct SubjectId
{
    typedef double id_type;
    id_type data[5];
};

struct TimeInterval
{
	double in;
	double out;
};

struct Subject
{
    SubjectId id;
    const unsigned char * pb;
    double * eta;
    int index;
	std::vector<TimeInterval> zeroIntervals;
};
