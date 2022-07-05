BUILD_TYPE ?= DEBUG

BUILD_DIR ?= build
BIN_DIR ?= ${BUILD_DIR}/bin
OBJ_DIR ?= ${BUILD_DIR}/src
SRC_DIR ?= src

DBG_ARGS ?= .

CC := clang++

ifeq (${BUILD_TYPE},DEBUG)
C_FLAGS := -g
else
C_FLAGS := -O3 -DNDEBUG
endif

C_FLAGS += -std=c++17 -Wall -Wpedantic -Wextra -Werror
C_FLAGS += -Wsign-conversion -Wstrict-prototypes -Wold-style-definition
C_FLAGS += -I./src

EXEC := ${BIN_DIR}/lcc.exe
SRC_FILES := $(shell ls ${SRC_DIR}/*.cpp)
OBJS := $(patsubst ${SRC_DIR}/%.cpp,${OBJ_DIR}/%.o,${SRC_FILES})
DEPS := $(patsubst ${SRC_DIR}/%.cpp,${OBJ_DIR}/%.d,${SRC_FILES})
-include ${DEPS}

.PHONY: build test clean

force:

build: ${EXEC}

${EXEC}: ${OBJS}
				@mkdir -p ${dir $@}
				${CC} ${C_FLAGS} $^ -o $@

${OBJ_DIR}/%.o: ${SRC_DIR}/%.cpp
				@mkdir -p ${dir $@}
				${CC} ${C_FLAGS} -c $< -MMD -MF $(@:.o=.d) -o $@

gdb: build
				gdb -ex 'b abort' -ex run --args ${EXEC} ${DBG_ARGS}

clean:
				rm -f ${BIN_DIR}/* ${OBJ_DIR}/*
