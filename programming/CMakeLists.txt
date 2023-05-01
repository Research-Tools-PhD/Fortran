# CMake build for P2.1 code
cmake_minimum_required(VERSION 3.10)
project(project VERSION 1.0 LANGUAGES Fortran C)
set(CMAKE_BUILD_TYPE RelWithDebInfo)

# Adding the source files to the project
add_executable(00_hello src/00_hello.f90)
add_executable(01_read_int src/01_read_int.f90)
add_executable(02_readfloat src/02_readfloat.f90)
add_executable(03_sorted src/03_sorted.f90)
add_executable(04_realsorted src/04_realsorted.f90)
add_executable(05_optfunction src/05_optfunction.f90)
add_executable(06_functionoverload src/06_functionoverload.f90)
add_executable(07_derived src/07_derived.f90)
add_executable(08_module src/08_module.f90)
add_executable(09_simple_sort src/09_simple_sort.f90)
add_executable(10_bubble_sort src/10_bubble_sort.f90)
add_executable(11_insertion_sort src/11_insertion_sort.f90)
add_executable(12_quick_sort src/12_quick_sort.f90)
add_executable(13_merge_sort src/13_merge_sort.f90)
add_executable(14_hybrid_sort src/14_hybrid_sort.f90)
add_executable(16_pow2 src/16_pow2.f90)
add_executable(17_linked_list src/17_linked_list.f90)
add_executable(18_hash src/18_hash.f90)
add_executable(19_stack src/19_stack.f90)
add_executable(20_stack_list src/20_stack_list.f90)
add_executable(21_binary src/21_binary.f90)
add_executable(22_count src/22_count.f90)
add_executable(23_tree_balance src/23_tree_balance.f90)


#Adding libraries to the project and linking them to the executables
add_library(list_tools src/list_tools.f90)
add_library(list_types src/list_types.f90)
target_link_libraries(list_tools PRIVATE list_types)
add_library(sorting src/sorting.f90)
add_library(inthash src/inthash.c)

# target_include_directories(04_realsorted PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(04_realsorted PRIVATE list_tools) # 04_realsorted needs list_tools to run properly and private means that it is only used in this executable
target_include_directories(05_optfunction PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(05_optfunction PRIVATE list_tools)
target_include_directories(06_functionoverload PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(06_functionoverload PRIVATE list_tools)
target_include_directories(08_module PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(08_module PRIVATE list_tools)
target_include_directories(09_simple_sort PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(09_simple_sort PRIVATE list_tools sorting)
target_include_directories(10_bubble_sort PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(10_bubble_sort PRIVATE list_tools sorting)
target_include_directories(11_insertion_sort PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(11_insertion_sort PRIVATE list_tools sorting)
target_include_directories(12_quick_sort PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(12_quick_sort PRIVATE list_tools sorting)
target_include_directories(13_merge_sort PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(13_merge_sort PRIVATE list_tools sorting)
target_include_directories(14_hybrid_sort PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(14_hybrid_sort PRIVATE list_tools sorting)
target_include_directories(17_linked_list PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(17_linked_list PRIVATE list_types)
target_include_directories(18_hash PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(18_hash PRIVATE list_types inthash)
target_include_directories(20_stack_list PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(20_stack_list PRIVATE list_types)
target_include_directories(21_binary PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(21_binary PRIVATE list_types)
target_include_directories(22_count PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(22_count PRIVATE list_types)
target_include_directories(23_tree_balance PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(23_tree_balance PRIVATE list_types)

# Adding the tests
enable_testing()

add_test(NAME Hello
    COMMAND sh -c "exec $<TARGET_FILE:00_hello> ${PROJECT_SOURCE_DIR}/data/d1_2.dat")
set_tests_properties(Hello 
    PROPERTIES PASS_REGULAR_EXPRESSION "Hello, World!")

################################################################################################

add_test(NAME ReadInt
    COMMAND sh -c "exec $<TARGET_FILE:01_read_int> ${PROJECT_SOURCE_DIR}/data/d1_1.dat")
set_tests_properties(ReadInt
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME ReadInt2
    COMMAND sh -c "exec $<TARGET_FILE:01_read_int> ${PROJECT_SOURCE_DIR}/data/d1_2.dat")
set_tests_properties(ReadInt2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME ReadInt3
    COMMAND sh -c "exec $<TARGET_FILE:01_read_int> ${PROJECT_SOURCE_DIR}/data/d1_3.dat")
set_tests_properties(ReadInt3
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME ReadInt4
    COMMAND sh -c "exec $<TARGET_FILE:01_read_int> ${PROJECT_SOURCE_DIR}/data/d1_4.dat")
set_tests_properties(ReadInt4
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME ReadInt5
    COMMAND sh -c "exec $<TARGET_FILE:01_read_int> ${PROJECT_SOURCE_DIR}/data/d1_5.dat")
set_tests_properties(ReadInt5
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME ReadInt6
    COMMAND sh -c "exec $<TARGET_FILE:01_read_int> ${PROJECT_SOURCE_DIR}/data/d1_6.dat")
set_tests_properties(ReadInt6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

################################################################################################

add_test(NAME Readfloat
    COMMAND sh -c "exec $<TARGET_FILE:02_readfloat> ${PROJECT_SOURCE_DIR}/data/d2_1.dat")
set_tests_properties(Readfloat
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME Readfloat2
    COMMAND sh -c "exec $<TARGET_FILE:02_readfloat> ${PROJECT_SOURCE_DIR}/data/d2_2.dat")
set_tests_properties(Readfloat2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME Readfloat3
    COMMAND sh -c "exec $<TARGET_FILE:02_readfloat> ${PROJECT_SOURCE_DIR}/data/d2_3.dat")
set_tests_properties(Readfloat3
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME Readfloat4
    COMMAND sh -c "exec $<TARGET_FILE:02_readfloat> ${PROJECT_SOURCE_DIR}/data/d2_4.dat")
set_tests_properties(Readfloat4
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME Readfloat5
    COMMAND sh -c "exec $<TARGET_FILE:02_readfloat> ${PROJECT_SOURCE_DIR}/data/d2_5.dat")
set_tests_properties(Readfloat5
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

add_test(NAME Readfloat6
    COMMAND sh -c "exec $<TARGET_FILE:02_readfloat> ${PROJECT_SOURCE_DIR}/data/d2_6.dat")
set_tests_properties(Readfloat6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum matches!")

################################################################################################

add_test(NAME Sorted
    COMMAND sh -c "exec $<TARGET_FILE:03_sorted> ${PROJECT_SOURCE_DIR}/data/d1_1.dat")
set_tests_properties(Sorted
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted")

add_test(NAME Sorted2
    COMMAND sh -c "exec $<TARGET_FILE:03_sorted> ${PROJECT_SOURCE_DIR}/data/d1_2.dat")
set_tests_properties(Sorted2
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted")

add_test(NAME Sorted3
    COMMAND sh -c "exec $<TARGET_FILE:03_sorted> ${PROJECT_SOURCE_DIR}/data/d1_3.dat")
set_tests_properties(Sorted3
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted")

add_test(NAME Sorted4
    COMMAND sh -c "exec $<TARGET_FILE:03_sorted> ${PROJECT_SOURCE_DIR}/data/d1_4.dat")
set_tests_properties(Sorted4
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted")

add_test(NAME Sorted5
    COMMAND sh -c "exec $<TARGET_FILE:03_sorted> ${PROJECT_SOURCE_DIR}/data/d1_5.dat")
set_tests_properties(Sorted5
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted")

add_test(NAME Sorted6
    COMMAND sh -c "exec $<TARGET_FILE:03_sorted> ${PROJECT_SOURCE_DIR}/data/d1_6.dat")
set_tests_properties(Sorted6
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted")

################################################################################################

add_test(NAME RealSorted
    COMMAND sh -c "exec $<TARGET_FILE:04_realsorted> ${PROJECT_SOURCE_DIR}/data/d2_1.dat")
set_tests_properties(RealSorted
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted")

add_test(NAME RealSorted2
    COMMAND sh -c "exec $<TARGET_FILE:04_realsorted> ${PROJECT_SOURCE_DIR}/data/d2_2.dat")
set_tests_properties(RealSorted2
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted")

add_test(NAME RealSorted3
    COMMAND sh -c "exec $<TARGET_FILE:04_realsorted> ${PROJECT_SOURCE_DIR}/data/d2_3.dat")
set_tests_properties(RealSorted3
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted")

add_test(NAME RealSorted4
    COMMAND sh -c "exec $<TARGET_FILE:04_realsorted> ${PROJECT_SOURCE_DIR}/data/d2_4.dat")
set_tests_properties(RealSorted4
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted")

add_test(NAME RealSorted5
    COMMAND sh -c "exec $<TARGET_FILE:04_realsorted> ${PROJECT_SOURCE_DIR}/data/d2_5.dat")
set_tests_properties(RealSorted5
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted")

add_test(NAME RealSorted6
    COMMAND sh -c "exec $<TARGET_FILE:04_realsorted> ${PROJECT_SOURCE_DIR}/data/d2_6.dat")
set_tests_properties(RealSorted6
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted")

################################################################################################

add_test(NAME OptFunction
    COMMAND sh -c "exec $<TARGET_FILE:05_optfunction> ${PROJECT_SOURCE_DIR}/data/d2_1.dat")
set_tests_properties(OptFunction
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted in ascending order")

add_test(NAME OptFunction2
    COMMAND sh -c "exec $<TARGET_FILE:05_optfunction> ${PROJECT_SOURCE_DIR}/data/d2_2.dat")
set_tests_properties(OptFunction2
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted in ascending order")

add_test(NAME OptFunction3
    COMMAND sh -c "exec $<TARGET_FILE:05_optfunction> ${PROJECT_SOURCE_DIR}/data/d2_3.dat")
set_tests_properties(OptFunction3
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted in ascending order")

add_test(NAME OptFunction4
    COMMAND sh -c "exec $<TARGET_FILE:05_optfunction> ${PROJECT_SOURCE_DIR}/data/d2_4.dat")
set_tests_properties(OptFunction4
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted in ascending order")

add_test(NAME OptFunction5
    COMMAND sh -c "exec $<TARGET_FILE:05_optfunction> ${PROJECT_SOURCE_DIR}/data/d2_5.dat")
set_tests_properties(OptFunction5
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted in ascending order")

add_test(NAME OptFunction6
    COMMAND sh -c "exec $<TARGET_FILE:05_optfunction> ${PROJECT_SOURCE_DIR}/data/d2_6.dat")
set_tests_properties(OptFunction6
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted in ascending order")

################################################################################################

add_test(NAME FunctionOverload
    COMMAND sh -c "exec $<TARGET_FILE:06_functionoverload> ${PROJECT_SOURCE_DIR}/data/d1_1.dat")
set_tests_properties(FunctionOverload
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted in ascending order")

add_test(NAME FunctionOverload2
    COMMAND sh -c "exec $<TARGET_FILE:06_functionoverload> ${PROJECT_SOURCE_DIR}/data/d1_2.dat")
set_tests_properties(FunctionOverload2
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted in ascending order")

add_test(NAME FunctionOverload3
    COMMAND sh -c "exec $<TARGET_FILE:06_functionoverload> ${PROJECT_SOURCE_DIR}/data/d1_3.dat")
set_tests_properties(FunctionOverload3
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted in ascending order")

add_test(NAME FunctionOverload4
    COMMAND sh -c "exec $<TARGET_FILE:06_functionoverload> ${PROJECT_SOURCE_DIR}/data/d1_4.dat")
set_tests_properties(FunctionOverload4
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is sorted in ascending order")

add_test(NAME FunctionOverload5
    COMMAND sh -c "exec $<TARGET_FILE:06_functionoverload> ${PROJECT_SOURCE_DIR}/data/d1_5.dat")
set_tests_properties(FunctionOverload5
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted in ascending order")

add_test(NAME FunctionOverload6
    COMMAND sh -c "exec $<TARGET_FILE:06_functionoverload> ${PROJECT_SOURCE_DIR}/data/d1_6.dat")
set_tests_properties(FunctionOverload6
    PROPERTIES PASS_REGULAR_EXPRESSION "Array is not sorted in ascending order")

################################################################################################

add_test(NAME Derived
    COMMAND sh -c "exec $<TARGET_FILE:07_derived> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(Derived
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Derived2
    COMMAND sh -c "exec $<TARGET_FILE:07_derived> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(Derived2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Derived3
    COMMAND sh -c "exec $<TARGET_FILE:07_derived> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(Derived3   
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Derived4
    COMMAND sh -c "exec $<TARGET_FILE:07_derived> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(Derived4   
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Derived5
    COMMAND sh -c "exec $<TARGET_FILE:07_derived> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(Derived5   
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Derived6
    COMMAND sh -c "exec $<TARGET_FILE:07_derived> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(Derived6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

################################################################################################

add_test(NAME Module
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(Module
    PROPERTIES PASS_REGULAR_EXPRESSION "Values are sorted in ascending order")

add_test(NAME Module2
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(Module2
    PROPERTIES PASS_REGULAR_EXPRESSION "Values are sorted in ascending order")

add_test(NAME Module3
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(Module3
    PROPERTIES PASS_REGULAR_EXPRESSION " Values are not sorted in ascending order")

add_test(NAME Module4
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(Module4
    PROPERTIES PASS_REGULAR_EXPRESSION "Values are sorted in ascending order")

add_test(NAME Module5
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(Module5
    PROPERTIES PASS_REGULAR_EXPRESSION "Values are not sorted in ascending order")

add_test(NAME Module6
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(Module6
    PROPERTIES PASS_REGULAR_EXPRESSION "Values are not sorted in ascending order")

################################################################################################
##################For keys####################################################################

add_test(NAME Modulek 
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(Modulek
    PROPERTIES PASS_REGULAR_EXPRESSION "Keys are not sorted in descending order")

add_test(NAME Module2k
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(Module2k
    PROPERTIES PASS_REGULAR_EXPRESSION "Keys are not sorted in descending order")

add_test(NAME Module3k
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(Module3k
    PROPERTIES PASS_REGULAR_EXPRESSION "Keys are not sorted in descending order")

add_test(NAME Module4k
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(Module4k
    PROPERTIES PASS_REGULAR_EXPRESSION "Keys are sorted in descending order")

add_test(NAME Module5k
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(Module5k
    PROPERTIES PASS_REGULAR_EXPRESSION "Keys are not sorted in descending order")

add_test(NAME Module6k
    COMMAND sh -c "exec $<TARGET_FILE:08_module> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(Module6k
    PROPERTIES PASS_REGULAR_EXPRESSION "Keys are not sorted in descending order")

################################################################################################
add_test(NAME Simple
    COMMAND sh -c "exec $<TARGET_FILE:09_simple_sort> test")
set_tests_properties(Simple
        PROPERTIES FAIL_REGULAR_EXPRESSION "Array was not sorted by simple sort")

################################################################################################

add_test(NAME Bubble 
    COMMAND sh -c "exec $<TARGET_FILE:10_bubble_sort> test")
set_tests_properties(Bubble
        PROPERTIES FAIL_REGULAR_EXPRESSION "Array was not sorted by bubble sort")

################################################################################################

add_test(NAME Insertion
    COMMAND sh -c "exec $<TARGET_FILE:11_insertion_sort> test")
set_tests_properties(Insertion
        PROPERTIES FAIL_REGULAR_EXPRESSION "Array was not sorted by insertion sort")

################################################################################################

add_test(NAME Quick
    COMMAND sh -c "exec $<TARGET_FILE:12_quick_sort> test")
set_tests_properties(Quick
        PROPERTIES FAIL_REGULAR_EXPRESSION "Array was not sorted by quick sort")

################################################################################################

add_test(NAME Merge
    COMMAND sh -c "exec $<TARGET_FILE:13_merge_sort> test")
set_tests_properties(Merge
        PROPERTIES FAIL_REGULAR_EXPRESSION "Array was not sorted by merge sort")

################################################################################################

add_test(NAME Hybrid
    COMMAND sh -c "exec $<TARGET_FILE:14_hybrid_sort> test")
set_tests_properties(Hybrid
        PROPERTIES FAIL_REGULAR_EXPRESSION "Array was not sorted by hybrid sort")

################################################################################################

add_test(NAME Pow
    COMMAND sh -c "exec && $<TARGET_FILE:16_pow2>")
set_tests_properties(Pow
        PROPERTIES PASS_REGULAR_EXPRESSION ".*1.*2.*4.*8.*")

################################################################################################

add_test(NAME 17_linked_list
    COMMAND sh -c "exec $<TARGET_FILE:17_linked_list> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(17_linked_list
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 17_linked_list2
    COMMAND sh -c "exec $<TARGET_FILE:17_linked_list> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(17_linked_list2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 17_linked_list3
    COMMAND sh -c "exec $<TARGET_FILE:17_linked_list> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(17_linked_list3
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 17_linked_list4
    COMMAND sh -c "exec $<TARGET_FILE:17_linked_list> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(17_linked_list4
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 17_linked_list5
    COMMAND sh -c "exec $<TARGET_FILE:17_linked_list> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(17_linked_list5
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 17_linked_list6
    COMMAND sh -c "exec $<TARGET_FILE:17_linked_list> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(17_linked_list6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

################################################################################################

add_test(NAME 18_hash
    COMMAND sh -c "exec $<TARGET_FILE:18_hash> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(18_hash
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 18_hash2
    COMMAND sh -c "exec $<TARGET_FILE:18_hash> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(18_hash2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 18_hash3
    COMMAND sh -c "exec $<TARGET_FILE:18_hash> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(18_hash3
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 18_hash4
    COMMAND sh -c "exec $<TARGET_FILE:18_hash> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(18_hash4
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 18_hash5
    COMMAND sh -c "exec $<TARGET_FILE:18_hash> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(18_hash5
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME 18_hash6
    COMMAND sh -c "exec $<TARGET_FILE:18_hash> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(18_hash6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

################################################################################################

add_test(NAME StackI
    COMMAND sh -c "exec && $<TARGET_FILE:19_stack>")
set_tests_properties(StackI
        PROPERTIES PASS_REGULAR_EXPRESSION ".*Done.*")

################################################################################################

add_test(NAME Stacklist
    COMMAND sh -c "exec && $<TARGET_FILE:20_stack_list>")
set_tests_properties(Stacklist
        PROPERTIES PASS_REGULAR_EXPRESSION ".*Done*")

################################################################################################

add_test(NAME Binary 
    COMMAND sh -c "exec $<TARGET_FILE:21_binary> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(Binary
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Binary2
    COMMAND sh -c "exec $<TARGET_FILE:21_binary> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(Binary2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Binary3
    COMMAND sh -c "exec $<TARGET_FILE:21_binary> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(Binary3
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Binary4
    COMMAND sh -c "exec $<TARGET_FILE:21_binary> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(Binary4
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Binary5
    COMMAND sh -c "exec $<TARGET_FILE:21_binary> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(Binary5
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Binary6
    COMMAND sh -c "exec $<TARGET_FILE:21_binary> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(Binary6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME ValgrindBinary
    COMMAND sh -c "exec valgrind --leak-check=full --show-leak-kinds=all $<TARGET_FILE:21_binary> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(ValgrindBinary
    PROPERTIES PASS_REGULAR_EXPRESSION "ERROR SUMMARY: 0 errors from 0 contexts")
################################################################################################

add_test(NAME count
    COMMAND sh -c "exec $<TARGET_FILE:22_count> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(count
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME count2
    COMMAND sh -c "exec $<TARGET_FILE:22_count> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(count2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME count3
    COMMAND sh -c "exec $<TARGET_FILE:22_count> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(count3
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME count4
    COMMAND sh -c "exec $<TARGET_FILE:22_count> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(count4
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME count5
    COMMAND sh -c "exec $<TARGET_FILE:22_count> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(count5
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME count6
    COMMAND sh -c "exec $<TARGET_FILE:22_count> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(count6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Valgrindcount
    COMMAND sh -c "exec valgrind --leak-check=full --show-leak-kinds=all $<TARGET_FILE:22_count> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(Valgrindcount
    PROPERTIES PASS_REGULAR_EXPRESSION "ERROR SUMMARY: 0 errors from 0 contexts")

################################################################################################

add_test(NAME Treebalance
    COMMAND sh -c "exec $<TARGET_FILE:23_tree_balance> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(Treebalance
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Treebalance2
    COMMAND sh -c "exec $<TARGET_FILE:23_tree_balance> ${PROJECT_SOURCE_DIR}/data/d3_2.dat")
set_tests_properties(Treebalance2
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Treebalance3
    COMMAND sh -c "exec $<TARGET_FILE:23_tree_balance> ${PROJECT_SOURCE_DIR}/data/d3_3.dat")
set_tests_properties(Treebalance3
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Treebalance4
    COMMAND sh -c "exec $<TARGET_FILE:23_tree_balance> ${PROJECT_SOURCE_DIR}/data/d3_4.dat")
set_tests_properties(Treebalance4
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Treebalance5
    COMMAND sh -c "exec $<TARGET_FILE:23_tree_balance> ${PROJECT_SOURCE_DIR}/data/d3_5.dat")
set_tests_properties(Treebalance5
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME Treebalance6
    COMMAND sh -c "exec $<TARGET_FILE:23_tree_balance> ${PROJECT_SOURCE_DIR}/data/d3_6.dat")
set_tests_properties(Treebalance6
    PROPERTIES PASS_REGULAR_EXPRESSION "Checksum is correct")

add_test(NAME ValgrindTreebalance
    COMMAND sh -c "exec valgrind --leak-check=full --show-leak-kinds=all $<TARGET_FILE:23_tree_balance> ${PROJECT_SOURCE_DIR}/data/d3_1.dat")
set_tests_properties(ValgrindTreebalance
    PROPERTIES PASS_REGULAR_EXPRESSION "ERROR SUMMARY: 0 errors from 0 contexts")

################################################################################################