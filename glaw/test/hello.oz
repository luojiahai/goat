    call main
    halt
main:
    string_const r0, "Hello, kid!\n"
    call_builtin print_string
    return
