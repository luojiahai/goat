    call proc_main
    halt
proc_p:
    push_stack_frame 3
    store 0, r0
    store 1, r1
    int_const r0, 0
    store 2, r0
    real_const r0, 100.123
    load r1, 1
    store_indirect r1, r0
    load r0, 1
    load_indirect r0, r0
    call_builtin print_real
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 3
    return
proc_main:
    push_stack_frame 19
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
    int_const r0, 0
    store 16, r0
    int_const r0, 0
    store 17, r0
    real_const r0, 0.0
    store 18, r0
    int_const r0, 19
    int_const r2, 3
    int_const r3, 2
    int_const r4, 4
    mul_int r2, r2, r4
    add_int r2, r2, r3
    load_address r1, 0
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 0
    store 17, r0
    real_const r0, 2.3
    store 18, r0
    int_const r0, 1
    load_address r1, 18
    call proc_p
    pop_stack_frame 19
    return
