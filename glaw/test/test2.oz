    call proc_main
    halt
proc_main:
    push_stack_frame 2
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
    string_const r0, "Primality checking by looping.\n"
    call_builtin print_string
    string_const r0, "Type a non-negative integer: "
    call_builtin print_string
    call_builtin read_int
    store 0, r0
    load r0, 0
    call_builtin print_int
    load r0, 0
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_on_false r0, label_1
label_0:
    string_const r0, " is a negative number.\n"
    call_builtin print_string
    branch_uncond label_2
label_1:
    load r0, 0
    int_const r1, 1000
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_3
    branch_on_false r0, label_4
label_3:
    string_const r0, " is too large to check.\n"
    call_builtin print_string
    branch_uncond label_5
label_4:
    load r0, 0
    load_address r1, 1
    call proc_check_prime
    load r0, 1
    branch_on_true r0, label_6
    branch_on_false r0, label_7
label_6:
    string_const r0, " is a prime number!\n"
    call_builtin print_string
    branch_uncond label_8
label_7:
    string_const r0, " is not a prime number.\n"
    call_builtin print_string
label_8:
label_5:
label_2:
    pop_stack_frame 2
    return
proc_check_prime:
    push_stack_frame 4
    store 0, r0
    store 1, r1
    int_const r0, 0
    store 2, r0
    int_const r0, 0
    store 3, r0
    load r0, 0
    int_const r1, 2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_9
    branch_on_false r0, label_10
label_9:
    int_const r0, 0
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_11
label_10:
    load r0, 0
    int_const r1, 2
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_12
    branch_on_false r0, label_13
label_12:
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_14
label_13:
    int_const r0, 2
    store 2, r0
    int_const r0, 2
    store 3, r0
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
label_15:
    load r