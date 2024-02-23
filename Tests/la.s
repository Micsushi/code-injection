.data
counter: .word 1

.text
# Let's test with a simple function that increments the value of a counter.
# This function has some quirks though because the return isn't the last line in the code.
Function:
    Function_Start:
        j Function_Main

    Function_End:
        ret

    Function_Main:
        la s0, counter
        lw s1, 0(s0)
        addi s1, s1, 1
        sw s1, 0(s0)
        j Function_End
