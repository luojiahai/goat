    call proc_main
    halt
proc_main:
    push_stack_frame 35
    int_const r0, 0
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
    store 4, r0
    store 5, r0
    store 6, r0
    store 7, r0
    store 8, r0
    store 9, r0
    store 10, r0
    store 11, r0
    store 12, r0
    store 13, r0
    store 14, r0
    store 15, r0
    store 16, r0
    store 17, r0
    store 18, r0
    store 19, r0
    store 20, r0
    store 21, r0
    store 22, r0
    store 23, r0
    store 24, r0
    store 25, r0
    store 26, r0
    store 27, r0
    store 28, r0
    store 29, r0
    store 30, r0
    store 31, r0
    store 32, r0
    store 33, r0
    store 34, r0
    int_const r0, 3
    int_const r2, 2
    int_const r3, 3
    int_const r4, 5
    mul_int r2, r2, r4
    add_int r2, r2, r3
    load_address r1, 0
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r1, 2
    int_const r2, 3
    int_const r3, 5
    mul_int r1, r1, r3
    add_int r1, r1, r2
    load_address r0, 0
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 35
    return
