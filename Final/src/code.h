#pragma once

/*
 * Códigos para representar comandos a la base de datos,
 * como también los diferentes tipos de errores.
 */
typedef enum {
	Nothing,
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
