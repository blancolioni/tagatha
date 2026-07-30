;  VM-level check for the Phase B float calling convention.
;
;  Pairs with the float_frame.s that tests.adb generates.  Build and run:
;
;      tests                                   (writes float_frame.s)
;      aqua_as -m -o test.o float_frame_main.s float_frame.s
;      aqua_vm test.o                          (needs a .aqua-config nearby)
;
;  Expected output is "P5": P because call_scale returned a double that is
;  bit-exactly 4.5 (scale (1.5, 7, 2.0) = 1.5 * 2.0 + 1.5), and 5 because
;  fix rounds 4.5 to nearest with ties away from zero.
;
;  artl.s ends with a bare `main` label, so this file is the *body* of main.
;  It therefore has to save and restore rJ around its own pushj, in a register
;  below the pushj boundary: pushj %10 hands %11 upwards to the callee, so a
;  save register up there would be clobbered.

        get %9, rJ
        pushj %10, call_scale       ; double result in %10/%11

        seth %12, 16402             ; 4.5 = 0x4012_0000_0000_0000
        setl %13, 0
        feql %14, %10, %12

        setl %2, 61443              ; tty data port, 0xFFFF_F003
        inch %2, 65535

        bz %14, 1f
        setl %3, 80                 ; 'P'
        jmp 2f
1:      setl %3, 70                 ; 'F'
2:      stb %3, %2, 0

        fix %4, %10                 ; and print '0' + fix (result)
        add %4, %4, 48
        stb %4, %2, 0

        setl %3, 10
        stb %3, %2, 0

        put rJ, %9
        pop 0, 0
