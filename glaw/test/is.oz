    call proc_main
    halt
proc_insertion_sort:
    push_stack_frame 10
    int_const r0, 0
    store 0, r0
    int_const r0, 0
    store 1, r0
    store 2, r0
    store 3, r0
    store 4, r0
    store 5, r0
    store 6, r0
    int_const r0, 0
    store 7, r0
    int_const r0, 0
    store 8, r0
    int_const r0, 0
    store 9, r0
    int_const r0, 6
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
    int_const r0, 1
    int_const r2, 3
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 5
    int_const r2, 4
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    int_const r0, 4
    int_const r2, 5
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
    int_const r1, 3
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 4
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 5
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    int_const r0, 1
    store 7, r0
label_0:
    load r1, 7
    load r2, 0
    cmp_lt_int r1, r1, r2
    branch_on_true r1, label_1
    branch_uncond label_2
label_1:
    load r0, 7
    store 8, r0
label_3:
    load r1, 8
    int_const r2, 0
    cmp_gt_int r1, r1, r2
    load r3, 8
    int_const r4, 1
    sub_int r3, r3, r4
    load_address r2, 1
    sub_offset r2, r2, r3
    load_indirect r2, r2
    load r4, 8
    load_address r3, 1
    sub_offset r3, r3, r4
    load_indirect r3, r3
    cmp_gt_int r2, r2, r3
    and r1, r1, r2
    branch_on_true r1, label_4
    branch_uncond label_5
label_4:
    load r1, 8
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    store 9, r0
    load r1, 8
    int_const r2, 1
    sub_int r1, r1, r2
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    load r2, 8
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    load r0, 9
    load r2, 8
    int_const r3, 1
    sub_int r2, r2, r3
    load_address r1, 1
    sub_offset r1, r1, r2
    store_indirect r1, r0
    load r0, 8
    int_const r1, 1
    sub_int r0, r0, r1
    store 8, r0
    branch_uncond label_3
label_5:
    load r0, 7
    int_const r1, 1
    add_int r0, r0, r1
    store 7, r0
    branch_uncond label_0
label_2:
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
    int_const r1, 3
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 4
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    int_const r1, 5
    load_address r0, 1
    sub_offset r0, r0, r1
    load_indirect r0, r0
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 10
    return
proc_main:
    push_stack_frame 0
    string_const r0, "Hello, this is an insertion sort.\n"
    call_builtin print_string
    call proc_insertion_sort
    pop_stack_frame 0
    return
