CC := gcc
SRC   := src
TESTS := tests

BIN   := bin
BUILD := build

FLAGS := -Wall -Wextra -Werror -std=c17 -g



CORE := \
	blob \
	counter64 \
	record \
	lru_queue \
	bucket \
	cell \
	database \
	binary_parser \
	text_parser

TEST := \
	blob \
	counter64 \
	lru_queue \
	bucket \
	cell \
	database

OBJECTS := $(CORE:%=$(BUILD)/%.o)

TEST_OBJECTS := $(TEST:%=$(BUILD)/%_tests.o)



# GENERAL TARGETS

.PHONY: all
all: tests memcache

.PHONY: dirs
dirs:
	@mkdir -p $(BIN)
	@mkdir -p $(BUILD)

.PHONY: clean
clean:
	@rm -r $(BIN)
	@rm -r $(BUILD)



# MAIN EXECUTABLE

.PHONY: memcache
memcache: dirs $(BIN)/memcache

$(BIN)/memcache: $(BUILD)/main.o $(OBJECTS)
	$(CC) $(FLAGS) -o $@ $^

$(BUILD)/main.o: $(SRC)/main.c
	$(CC) -c $(FLAGS) -o $@ $<



# TEST EXECUTABLE

.PHONY: tests
tests: dirs $(BIN)/tests

$(BIN)/tests: $(BUILD)/tests.o $(TEST_OBJECTS) $(OBJECTS) $(BUILD)/test_utils.o
	$(CC) $(FLAGS) -fopenmp -o $@ $^

$(BUILD)/tests.o: $(TESTS)/main.c
	$(CC) -c $(FLAGS) -o $@ $<




$(BUILD)/test_utils.o: $(TESTS)/test_utils.c $(TESTS)/test_utils.h
	$(CC) -c $(FLAGS) -o $@ $<



# FALLBACK TEST RULE

$(BUILD)/%_tests.o: $(TESTS)/%_tests.c $(TESTS)/%_tests.h $(SRC)/%.h
	$(CC) -c $(FLAGS) -fopenmp -o $@ $<



# FALLBACK CORE RULE

$(BUILD)/%.o: $(SRC)/%.c $(SRC)/%.h
	$(CC) -c $(FLAGS) -o $@ $<