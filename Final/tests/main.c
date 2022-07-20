#include "blob_tests.h"
#include "bucket_tests.h"
#include "counter64_tests.h"
#include "lru_queue_tests.h"
#include "cell_tests.h"
#include "rw_lock_tests.h"
#include "database_tests.h"

int main() {
	/*
	 * Corridos primero para probar los límites de memoria
	 */
	database_tests();

	blob_tests();
	bucket_tests();
	counter64_tests();
	lru_queue_tests();
	cell_tests();
	rw_lock_tests();
}
