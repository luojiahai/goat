    call main
    halt
main:
    push_stack_frame 1
    int_const r0, 5
    store 0, r0
    string_const r0, "Testing writes \n"
    call_builtin print_string
    pop_stack_frame 1
    return
