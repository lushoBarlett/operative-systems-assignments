#include "blob_tests.h"
#include "counter64_tests.h"
#include "lru_queue_tests.h"

int main() {
	blob_tests();
	counter64_tests();
	lru_queue_tests();
}
