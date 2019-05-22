    call proc_main
    halt
proc_factorial_recurse:
    push_stack_frame 2
    store 0, r0
    store 1, r1
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_0
    branch_on_false r0, label_1
label_0:
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_2
label_1:
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load_address r1, 1
    load_indirect r1, r1
    call proc_factorial_recurse
    load r0, 0
    load r1, 1
    load_indirect r1, r1
    mul_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
label_2:
    pop_stack_frame 2
    return
proc_factorial:
    push_stack_frame 2
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
    int_const r0, 5
    store 0, r0
    int_const r0, 1
    store 1, r0
    load r0, 0
    load_address r1, 1
    call proc_factorial_recurse
    string_const r0, "factorial of "
    call_builtin print_string
    load r0, 0
    call_builtin print_int
    string_const r0, " is "
    call_builtin print_string
    load r0, 1
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 2
    return
proc_main:
    push_stack_frame 0
    string_const r0, "Hello, this is a factorial.\n"
    call_builtin print_string
    call proc_factorial
    pop_stack_frame 0
    return
