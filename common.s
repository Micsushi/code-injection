#
# CMPUT 229 Public Materials License
# Version 1.0
#
# Copyright 2023 University of Alberta
# Copyright 2023 Liam Houston
#
# This software is distributed to students in the course
# CMPUT 229 - Computer Organization and Architecture I at the University of
# Alberta, Canada.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice,
#    this list of conditions and the disclaimer below in the documentation
#    and/or other materials provided with the distribution.
#
# 2. Neither the name of the copyright holder nor the names of its
#    contributors may be used to endorse or promote products derived from this
#    software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
#-------------------------------
# Lab - Stack Manipulation
#
# Author: Liam Houston, Andy Yu, Chase Johnson
# Date: July 26, 2023
#	
# This file is for student.
# This file is an common.s file used for unit testing solution of Stack Manipulation lab.
#-------------------------------
#

.data
# see https://github.com/TheThirdOne/rars/wiki/Assembler-Directives for usage
# of different assembler directive such as ".space" and ".asciz"

# allocate memory for file
fileContents:	.space 2048

# dummy statement to indicate the start and end of the function
dummy_statement:.word 0x0e500013 # = addi zero, zero, 229

# print messages
newline:	.asciz "\n"
inputStr:	.asciz "Input Instructions are:\n"
outputStr:	.asciz "Outputted Instructions are:\n"

# error messages
noFileStr:	.asciz "ERROR: Couldn't open file.\n"
outputErrorStr:	.asciz "ERROR: Found a 0x00000000 instruction in the output instructions. The function returned in a0 from stackManipulate should end with a return statement (ret = jalr x0, ra, 0)."
functErrorStr:	.asciz "ERROR: The file provided by the program arguments only has one dummy statement (addi x0, x0, 229). In order to properly execute a function test there should be one at the start of the function and one at the end. Please refer to the instructions and sample tests."
noArguments:    .asciz "ERROR: No arguments given."
notEnoughArguments: .asciz "ERROR: Only one argument given but two are required."
incorrectBitMap:    .asciz "ERROR: Argument is not a valid register bit map. Bit maps should be 8 characters and all capitals (1234FFFF)."

# strings for displaying test output
test_pass: .asciz " [X] Great job!    \n"
test_fail: .asciz " [ ] Almost there! \n"
test_default_running: .asciz "\n\n-- Running tests for functions --\n\n"
test_1_running: .asciz "1: findWrites --"
test_2_running: .asciz "2: storeStackInstructions --"
test_3_running: .asciz "3: fixAccesses --"
test_4_running: .asciz "4: redirectReturns --"
test_5_running: .asciz "5: stackManipulation --"
.align 2

# input for stackManipulation, findWrites, and storeStackInstructions
mock_function_1:
	.word 0x01100513 # addi a0, zero, 17
	.word 0x00100593 # addi a1, zero, 1
	.word 0x00b502b3 # add t0, a0, a1
	.word 0x00008067 # ret
	.word 0xffffffff

# expected output for stackManipulation
mock_function_1_expected_stackManipulation:
	.word 0xff010113 # addi sp, sp, -16
	.word 0x00012023 # sw zero, 0(sp)
	.word 0x00512223 # sw t0, 4(sp)
	.word 0x00a12423 # sw a0, 8(sp)
	.word 0x00b12623 # sw a1, 12(sp)
	.word 0x01100513 # addi a0, zero, 17
	.word 0x00100593 # addi a1, zero, 1
	.word 0x00b502b3 # add t0, a0, a1
	.word 0x0040006f # j 4
	.word 0x00012003 # lw zero, 0(sp)
	.word 0x00412283 # lw t0, 4(sp)
	.word 0x00812503 # lw a0, 8(sp)
	.word 0x00c12583 # lw a1, 12(sp)
	.word 0x01010113 # addi sp, sp, 16
	.word 0x00008067 # ret
	.word 0xffffffff

# input 1 for fixAccesses
mock_function_2:
	.word 0x0fc10297 # auipc t0, 0xfc10
	.word 0x00028293 # addi t0, t0, 0
	.word 0x0002a303 # lw t1, 0(t0)
	.word 0x00008067 # ret
	.word 0xffffffff

# expected output 1 for fixAccesses
mock_function_2_expected:
	.word 0x0fc10297 # auipc t0, 0xfc10
	.word 0xff828293 # addi t0, t0, -8
	.word 0x0002a303 # lw t1, 0(t0)
	.word 0x00008067 # ret
	.word 0xffffffff

# input 2 for fixAccesses
mock_function_2_1:
    .word 0x00052283 # lw t0, 0(a0)
    .word 0x00530313 # addi t1, t1, 5
    .word 0x02630333 # mul t1, t1, t1
    .word 0xff9ff0ef # jal ra, -8
    .word 0x00a30313 # addi t1, t1, 10
    .word 0xfe1ff0ef # jal ra, -32
    .word 0x00c000ef # jal ra, 12
    .word 0x00008067 # ret
	.word 0xffffffff

# expected output 2 for fixAccesses
mock_function_2_1_expected:
    .word 0x00052283 # lw t0, 0(a0)
    .word 0x00530313 # addi t1, t1, 5
    .word 0x02630333 # mul t1, t1, t1
    .word 0xff9ff0ef # jal ra, -8
    .word 0x00a30313 # addi t1, t1, 10
    .word 0xfcdff0ef # jal ra, -52
    .word 0x024000ef # jal ra, 36
    .word 0x00008067 # ret
	.word 0xffffffff

# input for redirectReturns
mock_function_3:
	.word 0x00400513 # addi a0, zero, 4
	.word 0x00050663 # beqz a0, 12
	.word 0x00450513 # addi a0, a0, 4
	.word 0x0080006f # j 8
	.word 0x00008067 # ret
	.word 0x00450513 # addi a0, a0, 4
	.word 0x00008067 # ret
	.word 0xffffffff
	.space 512
# expected output for redirectReturns
mock_function_3_expected:
	.word 0x00400513 # addi a0, zero, 4
	.word 0x00050663 # beqz a0, 12
	.word 0x00450513 # addi a0, a0, 4
	.word 0x0080006f # j 8
	.word 0x00c0006f # j 12
	.word 0x00450513 # addi a0, a0, 4
	.word 0x0040006f # j 4
	.word 0xffffffff

stack_instr_usr:
	.space 512

# expected output for storeStackInstructions register-saving (input in mock_function_1)
stack_instr_reg_saving_expected:
	.word 0xff010113 # addi sp, sp, -16
	.word 0x00012023 # sw zero, 0(sp)
	.word 0x00512223 # sw t0, 4(sp)
	.word 0x00a12423 # sw a0, 8(sp)
	.word 0x00b12623 # sw a1, 12(sp)
	.word 0xffffffff

# expected output for storeStackInstructions register-restore (input in mock_function_1)
stack_instr_reg_restore_expected:
    .word 0x00012003 # lw zero, 0(sp)
    .word 0x00412283 # lw t0, 4(sp)
    .word 0x00812503 # lw a0, 8(sp)
    .word 0x00c12583 # lw a1, 12(sp)
    .word 0x01010113 # addi sp, sp, 16
    .word 0xffffffff

.text
#------------------------------------------------------------------------------
# main
# Calls the functions to run the program and print outputs.
# 
# Register Usage:
#	s0: start of the function
#	s1: end of the function
#------------------------------------------------------------------------------
main:
	mv s0, a1		# s0 <- pointer of pointer to the file path string
    
    # check that argument was given
    beq s0, zero, main_noArg
    # make sure arg is not 0
    lw  t0, 0(s0)
    beq t0, zero, main_noArg
    # arg was given and is not 0
    j   main_noArgContinue
    main_noArg:
    # no argument was given so print error
    la  a0, noArguments
    li  a7, 4
    ecall
    # end program
    j   end
    main_noArgContinue:

    # check that arg 2 is not 0
	addi a1, a1, 4
    lw  t0, 0(a1)
    beq t0, zero, main_notEnoughArgs
    # arg is not 0
    j   main_notEnoughArgsContinue
    main_notEnoughArgs:
    # no argument was given so print error
    la  a0, notEnoughArguments
    li  a7, 4
    ecall
    # end program
    j   end
    main_notEnoughArgsContinue:

	lw a1, 0(a1)	# a1 <- pointer to the register bit map string
	li t0, 0		# t0 <- loop counter
	li s2, 0		# s2 <- register bit map
readBitMap_Loop:
	li t1, 8
	beq t0, t1, readBitMap_End
	slli s2, s2, 4	# shift left by 4
	lbu t1, 0(a1)	# t1 <- current character
	li t2, 65
	bge t1, t2, readBitMap_isChar	# if t1 >= 'A'
	# convert the digit from ASCII to binary
	li t2, 0x30		# t2 <- '0'
	sub t1, t1, t2	# t1 <- t1 - t2
	j readBitMap_Continue
readBitMap_isChar:
	li t2, 0x41		# t2 <- 'A'
	sub t1, t1, t2	# t1 <- t1 - t2
	addi t1, t1, 10	# t1 <- t1 + 10
    # make sure char is valid
    li  t2, 10
    blt t1, t2, readBitMap_notValid
    li  t2, 15
    bgt t1, t2, readBitMap_notValid
    # character is valid so continue
    j   readBitMap_Continue
    readBitMap_notValid:
    # character is not valid so print error
    la  a0, incorrectBitMap
    li  a7, 4
    ecall
    # exit program
    j   end
readBitMap_Continue:
	or s2, s2, t1
	addi t0, t0, 1	# increment loop counter
	addi a1, a1, 1	# increment pointer to register bit map string
	j readBitMap_Loop
readBitMap_End:
# ---------unit testing---------
	la a0, test_default_running
	jal ra, print
# ---------test 1: findWrites---------
test1:
	la a0, test_1_running
	jal ra, print
	la a0, mock_function_1		# a0 <- pointer to the start of the function
	jal ra, findWrites
	li t0, 0x00000c21				# t0 <- expected value
	beq t0, a0, test1_pass
	la a0, test_fail
	jal ra, print
	j test2
	test1_pass:
		la a0, test_pass
		jal ra, print
# ---------test 2: storeStackInstructions---------
test2:
	la a0, test_2_running
	jal ra, print

    # test register-saving
	li a0, 0		# a0 <- store stack register saving instructions
	la a1, stack_instr_usr		# a1 <- pointer to user's stack instructions
	li a2, 0x00000c21			# should save zero, t0, a0, a1 (4 registers)
	jal ra, storeStackInstructions
	la a0, stack_instr_usr
	la a1, stack_instr_reg_saving_expected
	li a2, 6	# 6 instructions
	li a3, 1	# start at 1
	jal ra, equals
	beq a0, zero, test2_fail

    # test register-restoring
	li a0, 1		# a0 <- store register-restoring instructions
	la a1, stack_instr_usr		# a1 <- pointer to user's stack instructions
	li a2, 0x00000c21			# should save zero, t0, a0, a1 (4 registers)
	jal ra, storeStackInstructions
	la a0, stack_instr_usr
	la a1, stack_instr_reg_restore_expected
	li a2, 6	# 6 instructions
	li a3, 1	# start at 1
	jal ra, equals
	beq a0, zero, test2_fail

    j test2_pass
    test2_fail:
	la a0, test_fail
	jal ra, print
	j test3
	test2_pass:
    la a0, test_pass
    jal ra, print
# ---------test 3: fixAccesses---------
test3:
	la a0, test_3_running
	jal ra, print

    # first test
	la a0, mock_function_2		# a0 <- pointer to the start of the function
	li a1, 8	# insert 8 before
	li a2, 4	# insert 4 after
	jal ra, fixAccesses
	la a0, mock_function_2
	la a1, mock_function_2_expected
	li a2, 5	# 5 instructions
	li a3, 1	# start at 1
	jal ra, equals
	beq a0, zero, test3_fail

    # second test
	la a0, mock_function_2_1		# a0 <- pointer to the start of the function
	li a1, 20	# insert 20 before
	li a2, 24	# insert 20 after
	jal ra, fixAccesses
	la a0, mock_function_2_1
	la a1, mock_function_2_1_expected
	li a2, 9	# 9 instructions
	li a3, 1	# start at 1
	jal ra, equals
	beq a0, zero, test3_fail

    j test3_pass
    test3_fail:
	la a0, test_fail
	jal ra, print
	j test4
	test3_pass:
    la a0, test_pass
    jal ra, print
# ---------test 4: redirectReturns---------
test4:
	la a0, test_4_running
	jal ra, print
	la a0, mock_function_3		# a0 <- pointer to the start of the function
	la a1, mock_function_3
	addi a1, a1, 28				# a1 <- pointer to the sentiel value
	jal ra, redirectReturns
	la a0, mock_function_3
	la a1, mock_function_3_expected
	li a2, 8	# 8 instructions
	li a3, 1	# start at 1
	jal ra, equals
	bne a0, zero, test4_pass
	la a0, test_fail
	jal ra, print
	j test5
	test4_pass:
		la a0, test_pass
		jal ra, print
# ---------test 5: stackManipulation---------
test5:
	la a0, test_5_running
	jal ra, print
	la a0, mock_function_1		# a0 <- pointer to the start of the function
	li a1, 0xffffffff	# a1 <- register calling conventions (save all registers)
	jal ra, stackManipulation
	la a1, mock_function_1_expected_stackManipulation
	li a2, 16	# 17 instructions
	li a3, 1	# start at 1
	jal ra, equals
	bne a0, zero, test5_pass
	la a0, test_fail
	jal ra, print
	j endTest
	test5_pass:
		la a0, test_pass
		jal ra, print
endTest:
# ---------end unit testing---------

	# see https://github.com/TheThirdOne/rars/wiki/Environment-Calls for 
	# usage of "ecall", such as the Open environment call used here
	# to open the .s file
	
	# save file decriptor in a0
	lw	a0, 0(s0)
	li	a1, 0
	li	a7, 1024
	ecall
	
	# end with error if file not found
	bltz	a0, fileError
	
	# copy file contents to fileContents
	la	a1, fileContents
	li	a2, 2048
	li	a7, 63
	ecall

	# print the input string
	la a0, inputStr
	li a7, 4
	ecall
	
	la t0, fileContents
findFunctionStart:
	lw t1, 0(t0)
	beqz t1, simpleTest # if we find the end of the text, this is a simple test
	la t2, dummy_statement
	lw t2, 0(t2) # t2 <- dummy statement
	beq t1, t2, functionTest # otherwise if we find a dummy statement, this is a function test (the difference is a function test will not use the entire .binary file but a specific function)
	j findFunctionStart_increment
	
findFunctionStart_increment:
	addi t0, t0, 4
	j findFunctionStart

# we can simply use the entire .text section as the input
simpleTest:
	la s0, fileContents # s0 <- function start
	# t0 points to the 0x00000000 after the last instruction in the function
	li t1, 0xFFFFFFFF
	sw t1, 0(t0) # store the sentinel
	mv s1, t0 # s1 <- function end = sentinel
	j printInputInstructions

# if we found the dummy statement in the binary then this is a functionTest (we should provide just the function code from the .text section)	
functionTest:
	# t0 holds the address of the start dummy statement for the function
	addi t0, t0, 4
	mv s0, t0 # set s0 to the first instruction in the sequence
functionTest_findEnd:
	lw t1, 0(t0) # t1 <- current instruction 
	la t2, dummy_statement
	lw t2, 0(t2) # t2 <- dummy statement
	beq t1, t2, functionTest_end # found ending dummy statement
	beqz t1, functionError
	addi t0, t0, 4
	j functionTest_findEnd
	
functionTest_end:
	li t1, 0xFFFFFFFF
	sw t1, 0(t0) # store the sentinel to replacee the end dummy statement
	mv s1, t0 # s1 <- pointer to the sentinel
	j printInputInstructions

# whether this is a simple test or a function test, s0 points to the start of the instruction sequence and s1 points to the sentinel for the sequence	
printInputInstructions:
	mv t0, s0 # t0 <- pointer to the current instruction
printInputInstructions_loop:
	lw t1, 0(t0) # t1 <- current instruction
	
	# print the hex of the current instruction
	mv a0, t1
 	li a7, 34
	ecall
	
	# print newline
	li a0, 10  # a0 <- '\n'
	li a7, 11
	ecall
	
	beq t0, s1, printInputInstructions_end # if we're at the end of the instructions
	addi t0, t0, 4 
 	j printInputInstructions_loop
	
printInputInstructions_end:
	# call stackmanipulate
	mv a0, s0 # a0 <- start of the instruction sequence

	mv a1, s2	# a1 <- register calling conventions
	jal stackManipulation
	
	mv s0, a0 # save the returned instruction pointer to s0
	
	# print the output string
	la a0, outputStr
	li a7, 4
	ecall
	
	mv t0, s0
printOutputInstructions_loop:
	lw t1, 0(t0) # t1 <- current instruction

	# print the hex of the current instruction
	mv a0, t1
 	li a7, 34
	ecall
	
	# print newline
	li a0, 10  # a0 <- '\n'
	li a7, 11
	ecall
	 
	li t2, 0xFFFFFFFF
	beq t1, t2, printOutputInstructions_end # if we're at the end of the instructions
	beqz t1, outputError # if the student does not have a 0xFFFFFFFF
	addi t0, t0, 4 
 	j printOutputInstructions_loop
	
printOutputInstructions_end:
	j end
  

outputError:
	# print error if file is not found
	la	a0, outputErrorStr
	li	a7, 4
	ecall
	j end
functionError:
	# print error if we only find one sentinel value in the file
	la 	a0, functErrorStr
	li	a7, 4
	ecall
	j end
fileError:
	# print error if file is not found
	la	a0, noFileStr
	li	a7, 4
	ecall
	j end
end: 
	# end program
	li	a7, 10
	ecall

print:
    addi sp, sp, -4
    sw ra, 0(sp)
    
    li a7, 4
    ecall
    
    lw ra, 0(sp)
    addi sp, sp, 4
    jr ra

#------------------------------------------------------------------------------
# equals
# This function checks if 2 arrays are entirely equal.
#
# Args:
#   a0: pointer to string 1
#   a1: pointer to string 2
#   a2: length of string 1
#   a3: counter for recursion (start at 1)
# Returns:
#   a0: 1 if both arrays are entirely equal, 0 if not.
#
# Register Usage:
#   t0: current character to compare from string 1
#   t1: current character to compare from string 2
#-----------------------------------------------------------------------------
equals:

    # load current character
    lw t0, 0(a0)
    lw t1, 0(a1)

    # check if it doesn't equal each other, fail
    bne t0, t1, equal_fail

    # check if we've reached the end of the string
    beq a3, a2, equal_pass

    # increment for next iteration
    addi a0, a0, 4
    addi a1, a1, 4
    addi a3, a3, 1
    # jump to next iteration
    j equals

    equal_fail:

        # return 0 for a fail
        li a0, 0

        ret

    equal_pass:

        # return 1 for a pass
        li a0, 1

        ret
