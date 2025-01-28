cmake_minimum_required(VERSION 3.31)
project(lustre-verilog)

set(TEST_DIR ${CMAKE_SOURCE_DIR}/test)


# Find verilator program
find_package(verilator HINTS $ENV{VERILATOR_ROOT} ${VERILATOR_ROOT})
if (NOT verilator_FOUND)
  message(FATAL_ERROR "Verilator was not found. Either install it, or set the VERILATOR_ROOT environment variable")
endif()

# Find gtkwave program
find_program(GTKWAVE gtkwave REQUIRED)

# Default Verilator Args
set(VERILATOR_ARGS --x-assign unique --relative-includes --x-initial unique --timing)

# Retrieve Include Directory
execute_process(
    COMMAND cabal run LustreVerilog -- --stdlib-dir
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE VERILATOR_INCLUDE
    OUTPUT_STRIP_TRAILING_WHITESPACE
    COMMAND_ERROR_IS_FATAL ANY
)

function(lustre_test TEST_NAME)
    set(oneValueArgs LUSTRE TOP)
    cmake_parse_arguments(PARSE_ARGV 0 arg "" "${oneValueArgs}" "")

    if(NOT arg_LUSTRE)
        set(arg_LUSTRE "main.lus")
    endif()

    if(NOT arg_TOP)
        set(arg_TOP "top.cpp")
    endif()

    set(TEST_SOURCE_PATH ${TEST_DIR}/${TEST_NAME})
    set(TEST_BUILD_PATH ${CMAKE_CURRENT_BINARY_DIR}/test_${TEST_NAME})

    file(MAKE_DIRECTORY ${TEST_BUILD_PATH})

    set(WAVEFORM_TARGET ${TEST_NAME}-waveform)
    set(GTKWAVE_TARGET ${TEST_NAME}-gtkwave)

    set(IN_TOP ${TEST_SOURCE_PATH}/${arg_TOP})
    set(IN_LUSTRE ${TEST_SOURCE_PATH}/${arg_LUSTRE})
    set(OUT_VERILOG ${TEST_BUILD_PATH}/${TEST_NAME}.v)
    set(OUT_WAVEFORM ${TEST_BUILD_PATH}/waveform.vcd)

    execute_process(
        COMMAND cabal run LustreVerilog -- --output ${OUT_VERILOG} --main-node main --int-size 32 --reset-high ${IN_LUSTRE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMAND_ERROR_IS_FATAL ANY
    )

    add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:-Wno-narrowing>")

    add_executable(${TEST_NAME} ${IN_TOP})
    verilate(${TEST_NAME}
        SOURCES ${OUT_VERILOG}
        DIRECTORY ${TEST_BUILD_PATH}
        TOP_MODULE main
        INCLUDE_DIRS ${VERILATOR_INCLUDE}
        VERILATOR_ARGS ${VERILATOR_ARGS}
        TRACE
    )

    target_compile_definitions(${TEST_NAME} PUBLIC WAVEFORM_FILE="${OUT_WAVEFORM}")

    add_custom_target(${WAVEFORM_TARGET}
        COMMAND ${TEST_NAME}
        DEPENDS ${TEST_NAME}
    )

    add_custom_target(${GTKWAVE_TARGET}
        COMMAND ${GTKWAVE} ${OUT_WAVEFORM}
        DEPENDS ${WAVEFORM_TARGET}
    )
endfunction()


function(verilog_test TEST_NAME)
    set(oneValueArgs VERILOG TOP)
    cmake_parse_arguments(PARSE_ARGV 0 arg "" "${oneValueArgs}" "")

    if(NOT arg_VERILOG)
        set(arg_VERILOG "main.v")
    endif()

    if(NOT arg_TOP)
        set(arg_TOP "top.cpp")
    endif()

    set(TEST_SOURCE_PATH ${TEST_DIR}/${TEST_NAME})
    set(TEST_BUILD_PATH ${CMAKE_CURRENT_BINARY_DIR}/test_${TEST_NAME})

    file(MAKE_DIRECTORY ${TEST_BUILD_PATH})

    set(WAVEFORM_TARGET ${TEST_NAME}-waveform)
    set(GTKWAVE_TARGET ${TEST_NAME}-gtkwave)

    set(IN_TOP ${TEST_SOURCE_PATH}/${arg_TOP})
    set(IN_VERILOG ${TEST_SOURCE_PATH}/${arg_VERILOG})
    set(OUT_WAVEFORM ${TEST_BUILD_PATH}/waveform.vcd)

    add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:-Wno-narrowing>")

    add_executable(${TEST_NAME} ${IN_TOP})
    verilate(${TEST_NAME}
        SOURCES ${IN_VERILOG}
        DIRECTORY ${TEST_BUILD_PATH}
        TOP_MODULE main
        INCLUDE_DIRS ${VERILATOR_INCLUDE}
        VERILATOR_ARGS ${VERILATOR_ARGS}
        TRACE
    )

    target_compile_definitions(${TEST_NAME} PUBLIC WAVEFORM_FILE="${OUT_WAVEFORM}")

    add_custom_target(${WAVEFORM_TARGET}
        COMMAND ${TEST_NAME}
        DEPENDS ${TEST_NAME}
    )

    add_custom_target(${GTKWAVE_TARGET}
        COMMAND ${GTKWAVE} ${OUT_WAVEFORM}
        DEPENDS ${WAVEFORM_TARGET}
    )
endfunction()

lustre_test(add)
lustre_test(and)
lustre_test(eq)
lustre_test(fby)
lustre_test(if)
lustre_test(neg)
lustre_test(not)
lustre_test(or)
lustre_test(sub)

verilog_test(verilog-playground)
