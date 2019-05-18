    call proc_main
    halt
proc_p:
    push_stack_frame 5
    int_const r0, 0
    store 0, r0
    real_const r0, 0.0
    store 1, r0
    int_const r0, 0
    store 2, r0
    int_const r0, 0
    store 3, r0
    int_const r0, 0
    store 4, r0
    string_const r0, "KK"
    call_builtin print_string
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 5
    return
proc_main:
    push_stack_frame 2
    real_const r0, 0.0
    store 0, r0
    int_const r0, 0
    store 1, r0
    int_const r0, 1
    load r1, 0
    call proc_p
    string_const r0, "Hello, World!\n"
    call_builtin print_string
    pop_stack_frame 2
    return
