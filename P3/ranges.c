#include "ranges.h"

static void init_ranges(range_t* ranges, size_t threads, int beg, int end) {
	size_t size = end - beg;

	ranges[0].beg = beg;

	for (size_t i = 0; i < threads - 1; i++)
		ranges[i].end = ranges[i + 1].beg = ranges[i].beg + size / threads;

	ranges[threads - 1].end = end;
}


range_t* make_ranges(size_t threads, int beg, int end) {
	range_t* ranges = malloc(sizeof(*ranges) * threads);
	init_ranges(ranges, threads, beg, end);
	return ranges;
}

int beg(range_t* ranges, int i) {
	return ranges[i].beg;
}

int end(range_t* ranges, int i) {
	return ranges[i].end;
}

int size(range_t* ranges, int i) {
	return end(ranges, i) - beg(ranges, i);
}

void free_ranges(range_t* ranges) {
	free(ranges);
}
