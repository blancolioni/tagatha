next_3x_1:
    ld %3, hello, 0
    set %2, %3
    set %4, %2
    pushj %3, calculate
    set %2, %3
    mod %3, %0, 2
    bz %3, 1f
    mul %3, %0, 3
    add %3, %3, 1
    set %2, %3
    br 2f
1
    div %3, %0, 2
    set %2, %3
2
    set %1, %2
    set %0, %1
    pop  1, 0
hello
    byte 72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 0, 0, 0, 0
