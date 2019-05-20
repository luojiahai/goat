    call proc_main
    halt
proc_move:
    push_stack_frame 4
    store 0, r0
    store 1, r1
    store 2, r2
    store 3, r3
    load r0, 0
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 1
    load r2, 3
    load r3, 2
    call proc_move
    string_const r0, "Move disk "
    call_builtin print_string
    load r0, 0
    call_builtin print_int
    string_const r0, " from "
    call_builtin print_string
    load r0, 1
    call_builtin print_int
    string_const r0, "->"
    call_builtin print_string
    load r0, 3
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 2
    load r2, 1
    load r3, 3
    call proc_move
label_1:
    pop_stack_frame 4
    return
proc_tower_of_hanoi:
    push_stack_frame 1
    int_const r0, 0
    store 0, r0
    int_const r0, 3
    store 0, r0
    load r0, 0
    int_const r1, 1
    int_const r2, 2
    int_const r3, 3
    call proc_move
    pop_stack_frame 1
    return
proc_main:
    push_stack_frame 0
    string_const r0, "Hello, this is a Tower of Hanoi.\n"
    call_builtin print_string
    call proc_tower_of_hanoi
    pop_stack_frame 0
    return
