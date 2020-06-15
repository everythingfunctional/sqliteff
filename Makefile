INCLUDE_FLAGS = $(addprefix -I,$(INCLUDE_DIRS))
ifeq ($(notdir $(basename $(FC))),gfortran)
CC = $(subst gfortran,gcc,$(FC))
CFLAGS = $(subst -fbacktrace,,$(subst -fcheck-array-temporaries,,$(subst -Wimplicit-interface,,$(FFLAGS))))
endif

$(BUILD_DIR)/libsqliteff.a: $(BUILD_DIR)/sqliteff.o $(BUILD_DIR)/csqlite3.o $(BUILD_DIR)/sqlite3.o
	ar rs $(@) $(^)

$(BUILD_DIR)/sqliteff.mod: src/sqliteff.f90

$(BUILD_DIR)/sqliteff.o: src/sqliteff.f90
	$(FC) -c -J$(BUILD_DIR) $(INCLUDE_FLAGS) $(FFLAGS) -o $(@) $(<)

$(BUILD_DIR)/csqlite3.o: src/csqlite3.c src/sqlite3.h
	$(CC) -c $(CFLAGS) -o $(@) $(<)

$(BUILD_DIR)/sqlite3.o: src/sqlite3.c
	$(CC) -c $(CFLAGS) -DSQLITE_OMIT_LOAD_EXTENSION -DSQLITE_THREADSAFE=0 -o $(@) $(<)
