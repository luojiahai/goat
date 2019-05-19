    call proc_main
    halt
proc_p:
    push_stack_frame 3
    store 0, r0
    store 1, r1
    int_const r0, 0
    store 2, r0
    load r0, 2
    call_builtin print_int
    pop_stack_frame 3
    return
proc_main:
    push_stack_frame 18
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
label_0:
    load r1, 17
    int_const r2, 5
    cmp_lt_int r1, r1, r2
    branch_on_true r1, label_1
    branch_uncond label_2
label_1:
    load r0, 17
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    load r0, 17
    int_const r1, 1
    add_int r0, r0, r1
    store 17, r0
    branch_uncond label_0
label_2:
    pop_stack_frame 18
    return
