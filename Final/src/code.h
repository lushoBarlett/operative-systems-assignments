#pragma once

typedef enum {
	Put = 11,
	Del = 12,
	Get = 13,
	Take = 14,
	Stats = 21,
	Ok = 101,
	Einval = 111,
	Enotfound = 112,
	Ebinary = 113,
	Ebig = 114,
	Eunk = 115,
} Code;
