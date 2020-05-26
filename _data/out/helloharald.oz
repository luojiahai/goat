    call proc_main
    halt
proc_main:
    push_stack_frame 0
    string_const r0, "Hello, Harald!\n"
    call_builtin print_string
    pop_stack_frame 0
    return
