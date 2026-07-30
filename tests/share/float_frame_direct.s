;  VM-level check that a DOUBLE RESULT survives a single call level.
;
;  This is the test that actually discriminates.  `pop n` rotates the callee's
;  %0 .. %(n-1) left by one on the way to the caller, so a two-register result
;  arrives reversed unless Exit_Routine pre-rotates it.  Going through two call
;  levels hides the bug: two rotations of a two-element sequence cancel, so
;  float_frame_main.s (main -> call_scale -> scale) passes either way.  Here
;  main calls the generated `scale` directly.
;
;  Build and run:
;
;      tests                                   (writes float_frame.s)
;      aqua_as -m -o direct.o float_frame_direct.s float_frame.s
;      aqua_vm direct.o                        (needs a .aqua-config nearby)
;
;  Expected output is "P".  "F" means the returned pair came back low word
;  first, i.e. the rotation in Exit_Routine is missing or wrong.
;
;  scale (x : double, n : int, y : double) takes x in $0/$1, n in $2 and y in
;  $3/$4, so the outgoing actuals sit at %11/%12, %13 and %14/%15 for a
;  pushj %10.  scale (1.5, 7, 2.0) = 1.5 * 2.0 + 1.5 = 4.5.

        get %9, rJ
        seth %11, 16376            ; x = 1.5   (0x3FF8_0000_0000_0000)
        setl %12, 0
        setl %13, 7                ; n = 7
        seth %14, 16384            ; y = 2.0   (0x4000_0000_0000_0000)
        setl %15, 0
        pushj %10, scale           ; double result expected in %10/%11

        seth %20, 16402            ; 4.5 = 0x4012_0000_0000_0000
        setl %21, 0
        feql %22, %10, %20

        setl %2, 61443             ; tty data port, 0xFFFF_F003
        inch %2, 65535
        bz %22, 1f
        setl %3, 80                ; 'P'
        jmp 2f
1:      setl %3, 70                ; 'F'
2:      stb %3, %2, 0
        setl %3, 10
        stb %3, %2, 0

        put rJ, %9
        pop 0, 0
