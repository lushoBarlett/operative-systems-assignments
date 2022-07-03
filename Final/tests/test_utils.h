#pragma once

#include "../src/blob.h"
#include "../src/database.h"

blob_t blob_from_string(const char* string);

bucket_t* new_bucket();
