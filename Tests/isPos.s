.data
positive_sign: .asciz "+"
negative_sign: .asciz "-"

.text
# Test whether a number is positive or negative and print either a + or a -
# This function has multiple return statements and tests whether the solution handles (and ignores) load addresses correctly.
isPos:
    isPosMain:
        mv s0, a0 # t0 <- the number we want to test
        beqz s0, NumIsZero # if the number is zero
        bltz s0, NumIsNeg # if the number is negative
        j NumIsPos # if the number is positive

    NumIsZero:
        # test the solution with fake load addresses that do not fit criteria
        
        auipc t0, 0x12345
        addi t0, t1, 4  # not a load address since it uses different registers here

        auipc s0, 0     # not a load address since the immediate is 0
        addi s0, s0, 19 

        auipc s4, 0x12345
        srli s4, s4, 1  # not a load address since it uses a shift

        ret # return
    NumIsPos:
        la a0, positive_sign # load the address of the positive sign
        li a7, 4 # print the positive sign
        ret # return
    NumIsNeg:
        la a0, negative_sign # load the address of the negative sign
        li a7, 4 # print the negative sign
        ret # return