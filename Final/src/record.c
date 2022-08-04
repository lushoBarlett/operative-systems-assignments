#include "record.h"

void record_init(concurrent_record_t* concurrent_record) {
	counter_init(&concurrent_record->puts, 0);
	counter_init(&concurrent_record->dels, 0);
	counter_init(&concurrent_record->gets, 0);
	counter_init(&concurrent_record->takes, 0);
}

record_t report(concurrent_record_t* concurrent_record, uint64_t keys) {
	return (record_t){
		.puts  = counter_get(&concurrent_record->puts),
		.dels  = counter_get(&concurrent_record->dels),
		.gets  = counter_get(&concurrent_record->gets),
		.takes  = counter_get(&concurrent_record->takes),
		.keys  = keys,
	};
}
