#pragma once

#include "database.h"

bucket_t* bucket_create(blob_t* key, blob_t* value);

void bucket_free(bucket_t* bucket);
