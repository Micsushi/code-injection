main:
	jal factorial
	
	li a7, 10
	ecall # exit program

addi x0, x0, 229 # THIS TELLS THE COMMON.S FILE THAT THE FUNCTION STARTS ON THE NEXT LINE. -----------------------------------------------------------------------------------

factorial:
	mv s0, ra
	li a0, 9 # number we want to take the factorial of
	li s1, 1 # answer
loop:
	mul s1, a0, s1
	addi a0, a0, -1
	beqz a0, end
	jal x0, loop
end:
	mv a0, s1 # print the answer as an int
	jal printInt
	
	jal printNewline
	
	mv a0, s1 # print the answer as a hex
	jal printHex
	
	mv ra, s0
	ret
	
addi x0, x0, 229 # THIS TELLS THE COMMON.S FILE THAT THE FUNCTION ENDED ON THE PREVIOUS LINE. ----------------------------------------------------------------------------------

printInt:
	li a7, 1
	ecall
	ret
	
printNewline:
	li a0, 10
	li a7, 11
	ecall
	ret
	
printHex:
	li a7, 34
	ecall
	ret
