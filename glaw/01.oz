    call proc_main
    halt
proc_main:
    # prologue
    push_stack_frame 5
    int_const r0, 0
    # initialise int val quotient
    store 0, r0
    # initialise int val remainder
    store 1, r0
    # initialise int val temp
    store 2, r0
    # initialise int val x
    store 3, r0
    # initialise int val y
    store 4, r0
    # write string
    string_const r0, "Input two positive integers: "
    call_builtin print_string
    # read x
    call_builtin read_int
    store 3, r0
    # read y
    call_builtin read_int
    store 4, r0
    # write string
    string_const r0, "\n"
    call_builtin print_string
    # if x < y
    load r0, 3
    load r1, 4
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
    # temp := x
    load r0, 3
    store 2, r0
    # x := y
    load r0, 4
    store 3, r0
    # y := temp
    load r0, 2
    store 4, r0
label_1:
    # write string
    string_const r0, "The gcd of "
    call_builtin print_string
    # write x
    load r0, 3
    call_builtin print_int
    # write string
    string_const r0, " and "
    call_builtin print_string
    # write y
    load r0, 4
    call_builtin print_int
    # write string
    string_const r0, " is "
    call_builtin print_string
    # quotient := x / y
    load r0, 3
    load r1, 4
    div_int r0, r0, r1
    store 0, r0
    # remainder := x - (quotient * y)
    load r0, 3
    load r1, 0
    load r2, 4
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    store 1, r0
    # while remainder > 0
label_2:
    load r0, 1
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
    # x := y
    load r0, 4
    store 3, r0
    # y := remainder
    load r0, 1
    store 4, r0
    # quotient := x / y
    load r0, 3
    load r1, 4
    div_int r0, r0, r1
    store 0, r0
    # remainder := x - (quotient * y)
    load r0, 3
    load r1, 0
    load r2, 4
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    store 1, r0
    branch_uncond label_2
label_4:
    # write y
    load r0, 4
    call_builtin print_int
    # write string
    string_const r0, "\n"
    call_builtin print_string
    # epilogue
    pop_stack_frame 5
    return
