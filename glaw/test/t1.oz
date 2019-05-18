    call main
    halt
main:
    # printf("%d\n",g(3,3));
    # Call g(3,3), and assign return value to r0
    int_const r0, 3
    int_const r1, 3
    call g
    call_builtin print_int
    string_const r0, "\n"
    call_builtin print_string
    return
g:
    push_stack_frame 3
    # slot 0 x, slot 1 y, slot 2 z = 0
    store 0, r0
    store 1, r1
    int_const r0, 0
    store 2, r0
    #
    #    load r0, 0
    #    call_builtin print_int
    # if (x == 0)
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, g_if_end
    # return y
    load r0, 1
    pop_stack_frame 3
    return
g_if_end:
    # x--
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    store 0, r0
    # y += 2;
    load r0, 1
    int_const r1, 2
    add_int r0, r0, r1
    store 1, r0
    # z = f(x, &y);
    load r0, 0
    load_address r1, 1
    call f
    store 2, r0
    # return y+z;
    load r0, 1
    load r1, 2
    add_int r0, r0, r1
    pop_stack_frame 3
    return
f:
    push_stack_frame 3
    # int x, int *y, int z
    # slot 0 x, slot 1 y, slot 2 z = 0
    store 0, r0
    store 1, r1
    int_const r0, 0
    store 2, r0
    #
    #    load r0, 0
    #    call_builtin print_int
    # if (x == 0)
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_false r0, f_if_end
    # return *y
    load r0, 1
    load_indirect r0, r0
    pop_stack_frame 3
    return
f_if_end:
    # x--
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    store 0, r0
    # *y += 3;
    load r0, 1
    load_indirect r0, r0
    int_const r1, 3
    add_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
    # z = *y;
    store 2, r0
    # return g(x,z);
    load r0, 0
    load r1, 2
    call g
    pop_stack_frame 3
    return