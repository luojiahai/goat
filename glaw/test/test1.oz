    call proc_main
    halt
proc_main:
    push_stack_frame 2
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
label_0:
    load r1, 0
    int_const r2, 5
    cmp_lt_int r1, r1, r2
    branch_on_true r1, label_1
    branch_uncond label_2
label_1:
    load r0, 0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
label_3:
    load r1, 1
    int_const r2, 5
    cmp_lt_int r1, r1, r2
    branch_on_true r1, label_4
    branch_uncond label_5
label_4:
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond label_3
label_5:
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond label_0
label_2:
    pop_stack_frame 2
    return
