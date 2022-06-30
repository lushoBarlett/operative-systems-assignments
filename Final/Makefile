SRC   := src
TESTS := tests

BIN   := bin
BUILD := build

FLAGS := -Wall -Wextra -Werror -std=c17



CORE := \
	blob \
	counter64

TEST := \
	blob \
	counter64

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

$(BIN)/memcache: $(BUILD)/main.o
	$(CC) $(FLAGS) -o $@ $^

$(BUILD)/main.o: $(SRC)/main.c $(OBJECTS)
	$(CC) -c $(FLAGS) -o $@ $<



# TEST EXECUTABLE

.PHONY: tests
tests: dirs $(BIN)/tests

$(BIN)/tests: $(BUILD)/tests.o $(TEST_OBJECTS) $(OBJECTS)
	$(CC) $(FLAGS) -o $@ $^

$(BUILD)/tests.o: $(TESTS)/main.c
	$(CC) -c $(FLAGS) -o $@ $<



# FALLBACK TEST RULE

$(BUILD)/%_tests.o: $(TESTS)/%_tests.c $(TESTS)/%_tests.h $(SRC)/%.h
	$(CC) -c $(FLAGS) -o $@ $<



# FALLBACK CORE RULE

$(BUILD)/%.o: $(SRC)/%.c $(SRC)/%.h
	$(CC) -c $(FLAGS) -o $@ $<