/*
 * Constantes usadas por el protocolo
 * de comunicación del servidor de memcached
 */
enum Protocol {
	PUT = 11,
	DEL = 12,
	GET = 13,
	TAKE = 14,
	STATS = 21,
	OK = 101,
	EINVAL = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
};
