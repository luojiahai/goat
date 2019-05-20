    call proc_main
    halt
proc_binary_search:
    push_stack_frame 11
    int_const r0, 0
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
    store 4, r0
    int_const r0, 0
    store 5, r0
    int_const r0, 0
    store 6, r0
    int_const r0, 0
    store 7, r0
    int_const r0, 0
    store 8, r0
    int_const r0, 0
    store 9, r0
    int_const r0, 0
    store 10, r0
    int_const r0, 1
    int_const r2, 0
    load_address r1, 0
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 2
    int_const r2, 1
    load_address r1, 0
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 3
    int_const r2, 2
    load_address r1, 0
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 4
    int_const r2, 3
    load_address r1, 0
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 5
    int_const r2, 4
    load_address r1, 0
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 5
    store 5, r0
    int_const r0, 3
    store 6, r0
    int_const r0, 0
    store 7, r0
    load r0, 5
    int_const r1, 1
    sub_int r0, r0, r1
    store 8, r0
    int_const r0, 0
    store 10, r0
    string_const r0, "collection: "
    call_builtin print_string
    int_const r1, 0
    load_address r0, 0
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 1
    load_address r0, 0
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 2
    load_address r0, 0
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 3
    load_address r0, 0
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 4
    load_address r0, 0
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    string_const r0, "x: "
    call_builtin print_string
    load r0, 6
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
label_0:
    load r1, 7
    load r2, 8
    cmp_le_int r1, r1, r2
    branch_on_true r1, label_1
    branch_uncond label_2
label_1:
    load r0, 7
    load r1, 8
    add_int r0, r0, r1
    int_const r1, 2
    div_int r0, r0, r1
    store 9, r0
    load r0, 6
    load r2, 9
    load_address r1, 0
    sub_offset r1, r1, r2
    load_indirect r1, r1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_3
    branch_on_false r0, label_4
label_3:
    int_const r0, 1
    store 10, r0
    load r0, 8
    store 7, r0
    branch_uncond label_5
label_4:
    load r0, 6
    load r2, 9
    load_address r1, 0
    sub_offset r1, r1, r2
    load_indirect r1, r1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_6
    branch_on_false r0, label_7
label_6:
    load r0, 9
    int_const r1, 1
    sub_int r0, r0, r1
    store 8, r0
    branch_uncond label_8
label_7:
    load r0, 9
    int_const r1, 1
    add_int r0, r0, r1
    store 7, r0
label_8:
label_5:
    branch_uncond label_0
label_2:
    load r0, 10
    branch_on_true r0, label_9
    branch_on_false r0, label_10
label_9:
    string_const r0, "x is found\n"
    call_builtin print_string
    branch_uncond label_11
label_10:
    string_const r0, "x is not found\n"
    call_builtin print_string
label_11:
    pop_stack_frame 11
    return
proc_factorial_recurse:
    push_stack_frame 2
    store 0, r0
    store 1, r1
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_12
    branch_on_false r0, label_13
label_12:
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_14
label_13:
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
label_14:
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
proc_fibonacci_recurse:
    push_stack_frame 4
    store 0, r0
    store 1, r1
    int_const r0, 0
    store 2, r0
    int_const r0, 0
    store 3, r0
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_15
    branch_on_false r0, label_16
label_15:
    int_const r0, 0
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_17
label_16:
    load r0, 0
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_18
    branch_on_false r0, label_19
label_18:
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_20
label_19:
    int_const r0, 0
    store 2, r0
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load_address r1, 2
    call proc_fibonacci_recurse
    int_const r0, 0
    store 3, r0
    load r0, 0
    int_const r1, 2
    sub_int r0, r0, r1
    load_address r1, 3
    call proc_fibonacci_recurse
    load r0, 2
    load r1, 3
    add_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
label_20:
label_17:
    pop_stack_frame 4
    return
proc_fibonacci:
    push_stack_frame 2
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
    int_const r0, 8
    store 0, r0
    int_const r0, 0
    store 1, r0
    load r0, 0
    load_address r1, 1
    call proc_fibonacci_recurse
    string_const r0, "fibonacci of "
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
proc_selection_sort:
    push_stack_frame 8
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
    store 2, r0
    store 3, r0
    int_const r0, 0
    store 4, r0
    int_const r0, 0
    store 5, r0
    int_const r0, 0
    store 6, r0
    int_const r0, 0
    store 7, r0
    int_const r0, 3
    store 0, r0
    int_const r0, 3
    int_const r2, 0
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 6
    int_const r2, 1
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 2
    int_const r2, 2
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    string_const r0, "unsorted: "
    call_builtin print_string
    int_const r1, 0
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 1
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 2
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    int_const r0, 0
    store 4, r0
label_21:
    load r1, 4
    load r2, 0
    int_const r3, 1
    sub_int r2, r2, r3
    cmp_lt_int r1, r1, r2
    branch_on_true r1, label_22
    branch_uncond label_23
label_22:
    load r0, 4
    store 7, r0
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
label_24:
    load r1, 5
    load r2, 0
    cmp_lt_int r1, r1, r2
    branch_on_true r1, label_25
    branch_uncond label_26
label_25:
    load r1, 5
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    load r2, 7
    load_address r1, 1
    sub_offset r1, r1, r2
    load_indirect r1, r1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_27
    branch_uncond label_28
label_27:
    load r0, 5
    store 7, r0
label_28:
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
    branch_uncond label_24
label_26:
    load r1, 7
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    store 6, r0
    load r1, 4
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    load r2, 7
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    load r0, 6
    load r2, 4
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
    branch_uncond label_21
label_23:
    string_const r0, "sorted: "
    call_builtin print_string
    int_const r1, 0
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 1
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 2
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 8
    return
proc_move:
    push_stack_frame 4
    store 0, r0
    store 1, r1
    store 2, r2
    store 3, r3
    load r0, 0
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_29
    branch_uncond label_30
label_29:
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
label_30:
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
    string_const r0, "binary_search:\n"
    call_builtin print_string
    call proc_binary_search
    string_const r0, "\nfactorial:\n"
    call_builtin print_string
    call proc_factorial
    string_const r0, "\nfibonacci:\n"
    call_builtin print_string
    call proc_fibonacci
    string_const r0, "\nselection_sort:\n"
    call_builtin print_string
    call proc_selection_sort
    string_const r0, "\ntower_of_hanoi:\n"
    call_builtin print_string
    call proc_tower_of_hanoi
    pop_stack_frame 0
    return
