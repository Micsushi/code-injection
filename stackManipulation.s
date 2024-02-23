
#------------------------------------------------------------------------------
# CCID: michael shi		                
# Lecture Section: A1      
# Instructor: nelson amaral           
# Lab Section: D02          
# Teaching Assistant: Christian
#-----------------------------------------------------------------------------

.data 
# THIS IS APART OF THE HELPER FUNCTIONS. DO NOT REMOVE
registerErrorString: .asciz "ERROR: One of the provided arguments to the helper functions is incorrect. \nRegister Arguments (rs1, rs2, rd) must be greater or equal to 0 and less than or equal to 31. \nOne of your registers does not fit this requirement."
manipulatedInstructions: .space 516   
.include "common.s"

.text
# -----------------------------------------------------------------------------
# stackManipulation:
#
# Description:
# 	This is the main function called from common.s.
# 	Convert a RISC-V function into it's equivalent stack-manipulated variation.
# 
# Args:
# 	a0: address of the first element of an array of RISC-V instructions ending with a sentinel value (0xFFFFFFFF).
#	a1: register calling conventions for the RISC-V function.
#
# Returns:
#	a0: address of the first element of a stack-manipulated variation of the array of RISC-V instructions, ending with a sentinel value (0xFFFFFFFF).
#
# Register Usage:
# s0: address of first element of array
# s1: register calling conventions
# s2: register to save to stack
# s3: number of bytes to insert
# s4: address of output space
# t0: temporary register
# t1: temporary register
# t2: temporary register
# -----------------------------------------------------------------------------
stackManipulation:
	# Program Flow for stackManipulation 
	# 1.Call findWrites to get the register bitmap of registers written to in the function. 
	# 2.Call fixAccesses to correct any jal or la instructions.
	# 3.Call storeStackInstructions to insert the register-saving instructions. 
	# 4.Copy the body of the function to space where the outputted instruction sequence will live. 
	# 5.Call storeStackInstructions to insert the register-restoring instructions. 
	# Remember to keep a pointer to the first instruction in the register-restoring instructions 
	# as this will be the start of the exit node. 
	# 6.Call redirectReturns to redirect all return statements to the exit node. 
	# 7.Insert the return statement at the end of the exit node, followed by the sentinel value (0xFFFFFFFF).

	#save registers
	addi sp, sp, -24 # Adjust stack
	sw s0, 0(sp) # Save s0 into stack
	sw s1, 4(sp) # Save s1 into stack
	sw s2, 8(sp) # Save s2 into stack
	sw s3, 12(sp) # Save s3 into stack
	sw s4, 16(sp) # Save s4 into stack
	sw ra, 20(sp) # Save ra into stack

	#initialize registers
	mv s0, a0 # s0 = address of first element of array
	mv s1, a1 # s1 = register calling conventions

	# 1. get register bitmap ###########################################
	jal ra, findWrites # a0 = bitmap of registers written to by the RISC-V function
	mv s2, a0 # s2 = register bitmap
	and s2, s2, s1 # s2 = registers to save to stack

	# 2. Fix jal and la instructions #################################
	li s3, 0 # s3 = instruction counter
	mv t0, s2 # t0 = temporary bitmap
	# loop to count number of instructions
	loopSumBitmap:
		andi t1, t0, 1 # isolate the right most bit
		add s3, s3, t1 # add to the sum
		srli t0, t0, 1 # shift right
		bnez t0, loopSumBitmap # if t0 != 0, loop again
	li t0, 4 # t0 = 4
	mul s3, s3, t0 # s3 = start and end bytes = 4 * sum of bitmap

	# call fixAccesses
	mv a0, s0 # a0 = address of first element of array
	mv a1, s3 # a1 = start bytes
	addi a1, a1, 4 # add 4 to start bytes to account for addi
	mv a2, s3 # a2 = end bytes
	addi a2, a2, 8 # add 8 to end bytes to account for addi and return statement
    jal ra, fixAccesses

    # 3 Insert register-saving instructions ############################
	li a0, 0 # a0 = 0 (saving instructions)
	la s4, manipulatedInstructions # s4 = address to add instructions
	mv a1, s4 # a1 = address to add instructions
	mv a2, s2 # a2 = register bitmap
    jal ra, storeStackInstructions
	add s4, s4, s3 # s4 increments by start bytes
	beqz s3, copyFunctionBody # if start bytes = 0, skip to copyFunctionBody
	addi s4, s4, 4 # increment address of output space by 4

    # 4 Copy function body to output space #############################
	copyFunctionBody:
		#loop through array at s0 and copy to s4
		li t0, -1 # t0 = -1
		mv t1, s0 # t1 = temporary address of first element of array
		loopCopy:
			lw t2, 0(t1) # t2 = instruction
			beq t2, t0, insertRestoring # if instruction = -1, exit loop
			sw t2, 0(s4) # store instruction
			addi t1, t1, 4 # increment address of array by 4
			addi s4, s4, 4 # increment address of output space by 4
			j loopCopy # jump to loopCopy

	# 5 Insert register-restoring instructions #########################
	insertRestoring:
		li a0, 1 # a0 = 1 (restoring instructions)
		mv a1, s4 # a1 = address to add instructions
		mv a2, s2 # a2 = register bitmap
		jal ra, storeStackInstructions
		# s4 is now the address of the exit node	

	# 6 Redirect return statements #####################################
	la a0, manipulatedInstructions # a0 = address of first element of array
	mv a1, s4 # a1 = address of exit node
	jal ra, redirectReturns

	# 7 Insert return statement and sentinel value #####################
	add s4, s4, s3 # s4 = where to insert return statement
	beqz s3, afterIncrement # if start bytes = 0, skip to afterIncrement
	addi s4, s4, 4 # increment address of output space by 4
	afterIncrement:
		li t0, 0x00008067 # t0 = return statement
		sw t0, 0(s4) # store return statement
		addi s4, s4, 4 # increment address of output space by 4
		li t0, -1 # t0 = -1
		sw t0, 0(s4) # store 0xFFFFFFFF

		la a0, manipulatedInstructions # a0 = address of first element of array

	exitStackManipulation:
		
		# Restore registers
		lw s0, 0(sp) # Restore s0 from stack
		lw s1, 4(sp) # Restore s1 from stack
		lw s2, 8(sp) # Restore s2 from stack
		lw s3, 12(sp) # Restore s3 from stack
		lw s4, 16(sp) # Restore s4 from stack
		lw ra, 20(sp) # Restore ra from stack
		addi sp, sp, 24 # Adjust stack
	    ret
# -----------------------------------------------------------------------------
# findWrites:
#
# Description:
# 	Find all register writes in a RISC-V function.
# 
# Args:
# 	a0: address of the first element of an array of RISC-V instructions ending with a sentinel value (0xFFFFFFFF).
#
# Returns:
#	a0: bitmap of registers written to by the RISC-V function.
#
# Register Usage:
#	a0: bitmap of registers written to by the RISC-V function.
#	t0: address of the first element of an array
# 	t1: opcode for load instruction
#	t2: opcode for store instruction
#	t3: sentinel value
#	t4: instruction
#	t5: opcode
#	t6: register destination.
# -----------------------------------------------------------------------------
findWrites:
    mv t0, a0 # t0 = address of first element of array
    li a0, 0 # a0 = 0
    li t1, 35 # t1 = 35
    li t2, 99 # t2 = 99
    li t3, -1 # t3 = -1
    
    loopFindWrites:
        lw t4, 0(t0) # t4 = instruction
        beq t4, t3, exitFindWrites # if instruction = -1, exit loop
            
        slli t5, t4, 25 # t5 = opcode
        srli t5, t5, 24 # t5 = opcode

		## if opcode = 35 or opcode = 99, next instruction
        beq t5, t1, nextFindWrites # if opcode = 35, next instruction
        beq t5, t2, nextFindWrites # if opcode = 99, next instruction

		## else add rd to bitmap
        slli t5, t4, 20 # get rid of left most 12 bits
        srli t6, t5, 27 # get rid of right most 7 bits, t6 = rd
        li t5, 1 # t5 = 1
        sll t5, t5, t6 # t5 = 2^rs1
        or a0, a0, t5 # a0 = a0 | t5 to get bitmap of registers written to

			nextFindWrites:
				addi t0, t0, 4 # t0 = t0 + 4
				j loopFindWrites # jump to loopFindWrites

    exitFindWrites:
        ret # return
# -----------------------------------------------------------------------------
# storeStackInstructions:
#
# Description:
#   Insert register-saving/restoring instructions to a specified memory location.
#   Store the sentinel value (0xFFFFFFFF) at the end of the register-saving/restoring instructions.
# 
# Args:
# 	a0: boolean value. If 0, store register-saving instructions. If 1, store register-restoring instructions.
#	a1: address of the location to store register-saving/restoring instructions.
#	a2: bitmap indicating which registers to save to the stack.
#
# Returns:
#	None.
#
# Register Usage:
#	s0: boolean value
#	s1: address to store instructions
#	s2: bitmap 
#	s3: counter for bitmap
#	s4: total offset
#	s5: counter for offset
#	t0: temporary register
# -----------------------------------------------------------------------------
storeStackInstructions:
	#save registers
	addi sp, sp, -28 # Adjust stack
	sw ra, 0(sp) # Save ra into stack
	sw s0, 4(sp) # Save s0 into stack
	sw s1, 8(sp) # Save s1 into stack
	sw s2, 12(sp) # Save s2 into stack
	sw s3, 16(sp) # Save s3 into stack
	sw s4, 20(sp) # Save s4 into stack
	sw s5, 24(sp) # Save s5 into stack

	#initialize registers
    mv s0, a0 # s0 = boolean value
    mv s1, a1 # s1 = address of location for instructions
    mv s2, a2 # s2 = bitmap indicating which registers to save to the stack
    li s3, 0 # s3 = counter for bitmap
	li s4, 0 # s4 = sum of bitmap
	li s5, 0 # s5 = counter for offset

	# calculate how much we have to move sp (sum of bitmap)
	mv t0, s2 # t0 = temporary bitmap
	loopSumOfBitmap:
		andi t1, t0, 1 # isolate the right most bit
		add s4, s4, t1 # add to the sum
		srli t0, t0, 1 # shift right
		bnez t0, loopSumOfBitmap # if t0 != 0, loop again
	li t0, 4
	mul s4, s4, t0 # s4 = 4 * sum of bitmap

	beqz s0, storeSaving # if boolean value = 0, store saving instructions

	loopRestoring:
		# check loop exit
		li t0, 32 # t0 = 32
		beq s3, t0, exitRestoring # if counter = 32, exit loop

		# check loop continue (if the bit is 0, skip the instruction)
		srl t0, s2, s3 # t0 = bitmap >> counter
		andi t0, t0, 1 # t0 = bitmap & 1
		beqz t0, nextRestoring # if bitmap & 1 = 0, next instruction

		# make instruction for lw: e.g. lw s0, 0(sp), lw s1, 4(sp)
		# lw rd, immediate(rs1)
		li a0, 2 # a0 = rs1 (sp)
		mv a1, s3 # a1 = rd (s3 = register number) 
		mv a2, s5 # a2 = immediate (s5 = offset)

		jal ra, createLoad # a0 = lw instruction
		sw a0, 0(s1) # store lw instruction 
		addi s1, s1, 4 # increment address for instructions by 4
		addi s5, s5, 4 # increment offset by 4

		nextRestoring:
			addi s3, s3, 1 # counter = counter + 1
			j loopRestoring # jump to loopRestoring

		exitRestoring:
			beqz s4, exitStoreStackInstructions # if sum of bitmap = 0, exit loop
			# make instructions for addi: addi sp, sp, immediate
			# addi rd, rs1, immediate 
			mv a0, s4 # a0 = immediate (4 * sum of bitmap)
			li a1, 2 # a1 = rs1 (sp)
			li a2, 2 # a2 = rd (sp)

			jal ra, createAddi # a0 = addi instruction
			sw a0, 0(s1) # store addi instruction 
			addi s1, s1, 4 # increment address of location for instructions by 4
			j exitStoreStackInstructions # jump to exitStoreStackInstructions
		

	storeSaving:
		beqz s4, exitStoreStackInstructions # if sum of bitmap = 0, exit loop
		# make instructions for addi: addi sp, sp, -1*s4
		li t0, -1
		mul t0, s4, t0 # t0 = -4 * sum of bitmap
		# addi rd, rs1, immediate 
		mv a0, t0 # a0 = immediate (-4 * sum of bitmap)
		li a1, 2 # a1 = rs1 (sp)
		li a2, 2 # a2 = rd (sp)

		jal ra, createAddi # a0 = addi instruction
		sw a0, 0(s1) # store addi instruction 
		addi s1, s1, 4 # increment address of location for instructions by 4
		
		loopSaving:
			# check loop exit
			li t0, 32 # t0 = 32
			beq s3, t0, exitStoreStackInstructions # if counter = 32, exit loop

			# check loop continue (if the bit is 0, skip the instruction)
			srl t0, s2, s3 # t0 = bitmap >> counter
			andi t0, t0, 1 # t0 = bitmap & 1
			beqz t0, nextSaving # if bitmap & 1 = 0, next instruction

			# make instruction for sw: e.g. sw s0, 0(sp), sw s1, 4(sp)
			# sw rd, immediate(rs1)
			li a0, 2 # a0 = rs1 (sp)
			mv a1, s3 # a1 = rd (s3 = register number) 
			mv a2, s5 # a2 = immediate (s5 = offset)
			jal ra, createStore # a0 = sw instruction
			sw a0, 0(s1) # store sw instruction 
			addi s1, s1, 4 # increment address for instructions by 4
			addi s5, s5, 4 # increment offset by 4

			nextSaving:
				addi s3, s3, 1 # counter = counter + 1
				j loopSaving # jump to loopSaving

	exitStoreStackInstructions:
		#add 0xFFFFFFFF to end of instructions
		li t0, -1 # t0 = -1
		sw t0, 0(s1) # store 0xFFFFFFFF 
		
		#restore registers
		lw ra, 0(sp) # Restore ra from stack
		lw s0, 4(sp) # Restore s0 from stack
		lw s1, 8(sp) # Restore s1 from stack
		lw s2, 12(sp) # Restore s2 from stack
		lw s3, 16(sp) # Restore s3 from stack
		lw s4, 20(sp) # Restore s4 from stack
		lw s5, 24(sp) # Restore s5 from stack
		addi sp, sp, 28 # Adjust stack
		ret

# -----------------------------------------------------------------------------
# fixAccesses:
#
# Description:
# 	Correct the accesses in a RISC-V function that has bytes inserted at the start and end.
# 	To correct accesses, adjust the immediates of jal and la instructions.
#
# Args:
# 	a0: address of the first element of an array of RISC-V instructions ending with a sentinel value (0xFFFFFFFF).
#	a1: number of bytes inserted at the start of the function.
#	a2: number of bytes inserted at the end of the function.
#
# Returns:
#	None.
#
# Register Usage:
# s0: address of first element of array
# s1: bytes at the start of the function
# s2: bytes at the end of the function
# s3: instruction counter
# s4: total number of instructions * 4
# t0: instruction
# t1: sentinel value
# t2: -1
# -----------------------------------------------------------------------------
fixAccesses: 
	#save registers
	addi sp, sp, -24 # Adjust stack
	sw s0, 0(sp) # Save s0 into stack
	sw s1, 4(sp) # Save s1 into stack
	sw s2, 8(sp) # Save s2 into stack
	sw s3, 12(sp) # Save s3 into stack
	sw s4, 16(sp) # Save s4 into stack
	sw ra, 20(sp) # Save ra into stack

	#initialize registers
    mv s0, a0 # s0 = address of first element of array
    mv s1, a1 # s1 = bytes at the start of the function
    mv s2, a2 # s2 = bytes at the end of the function
	li s3, 0 # s3 = instruction counter

	# Calculate total number of instructions * 4
	mv t1, a0 # Copy the start address to t1
	countInstructions:
		lw t0, 0(t1) # Load the instruction
		li t2, -1 # Load the sentinel value
		beq t0, t2, doneCounting # If the instruction is the sentinel, we're done
		addi t1, t1, 4 # Otherwise, move to the next instruction
		j countInstructions # And continue counting
	doneCounting:
		sub s4, t1, a0 #s4 = total number of instructions * 4

    loopFixAccesses:
		# check loop exit
		lw t0, 0(s0) # t0 = instruction
		li t1, -1 # t1 = -1
		beq t0, t1, exitFixAccesses # if instruction = -1, exit loop

		# check if jal or auipc, they have unique opcodes
		andi t0, t0, 0x7F # isolate opcode
		li t1, 0x6F # load 0x6F into t2
		beq t0, t1, fixJal # if opcode is 0x6F, it's a jal instruction
		li t1, 0x17 # load 0x17 into t2
		beq t0, t1, fixLa # if opcode is 0x17, it's a auipc instruction
        j nextFixAccesses # else go to next instruction

		fixJal:
			# get the immediate from the jal instruction
			lw a0, 0(s0) # t0 = jal instruction
			jal ra, getJalImm # a0 = sign extended immediate

			li t0, -1 # t0 = -1
			mul t0, t0, s3 # t0 = -1 * instruction counter
			blt a0, t0, jalBeforeFirst # if immediate < -1 * instruction counter, jump to jalBeforeFirst

			##if jumping to after last instruction, add s2
			sub t0, s4, s3 # t0 = total number of instructions * 4 - instruction counter
			bge a0, t0, jalAfterLast # if immediate > total number of instructions * 4 - instruction counter, jump to jalAfterLast

			##else do nothing
			j nextFixAccesses # jump to nextFixAccesses

			jalBeforeFirst:
				sub a0, a0, s1 # a0 = immediate - bytes at the start of the function
				lw t0, 0(s0) # t0 = jal instruction
				srli a1, t0, 7 # shift right to isolate rd
				andi a1, a1, 0x1F # a1 = rd
				jal ra, createJal # a0 = jal instruction
				sw a0, 0(s0) # store the adjusted instruction back to memory
				j nextFixAccesses # jump to nextFixAccesses

			jalAfterLast:
				add a0, a0, s2 # a0 = immediate + bytes at the end of the function
				lw t0, 0(s0) # t0 = jal instruction
				srli a1, t0, 7 # shift right to isolate rd
				andi a1, a1, 0x1F # a1 = rd
				jal ra, createJal # a0 = jal instruction ###############################
				sw a0, 0(s0) # store the adjusted instruction back to memory
				j nextFixAccesses # jump to nextFixAccesses

		fixLa:
			#check if next instruction is addi
			lw t0, 4(s0) # t1 = next instruction
			andi t0, t0, 0x7F # isolate opcode
			li t1, 0x13 # load 0x13 into t1
			bne t0, t1, nextFixAccesses # if opcode is not 0x13, jump to nextFixAccesses
			srli t0, t0, 12 # shift right to isolate funct3
			andi t0, t0, 0x7 # t0 = funct3
			bnez t0, nextFixAccesses # if funct3 is not 0, jump to nextFixAccesses

			# get the immediate from the addi instruction
			addi s0, s0, 4 # we do not change the auipc instruction, so skip it
			lw a0, 0(s0) # t0 = addi instruction
			jal ra, getAddiImm # a0 = sign extended immediate

			# check if immediate is positive
			bgtz a0, nextFixAccesses # if immediate > 0, jump to nextFixAccesses

			# create the new addi instruction
			sub a0, a0, s1 # a0 = immediate + bytes at the start of the function 
			lw t0, 0(s0) # t0 = addi instruction
			srli a1, t0, 15 # shift right to isolate rs1
			andi a1, a1, 0x1F # a1 = rs1
			srli a2, t0, 7 # shift right to isolate rd
			andi a2, a2, 0x1F # a2 = rd
			jal ra, createAddi # a0 = addi instruction
			
			sw a0, 0(s0) # store the adjusted instruction back to memory

        nextFixAccesses:
            addi s0, s0, 4 # s0 = s0 + 4
			addi s3, s3, 4 # s3 = s3 + 4
            j loopFixAccesses # jump to loopFixAccesses

    exitFixAccesses:
		#restore registers
		lw s0, 0(sp) # Restore s0 from stack
		lw s1, 4(sp) # Restore s1 from stack
		lw s2, 8(sp) # Restore s2 from stack
		lw s3, 12(sp) # Restore s3 from stack
		lw s4, 16(sp) # Restore s4 from stack
		lw ra, 20(sp) # Restore ra from stack
		addi sp, sp, 24 # Adjust stack
        ret
# -----------------------------------------------------------------------------
# redirectReturns:
#
# Description:
# 	Redirect all return statements in a RISC-V function to jump to an exit node.
#
# Args:
#    a0: address of the first element of an array of RISC-V instructiosn with one or more return statements, ending with a sentinel value (0xFFFFFFFF).
#    a1: pointer to exit node.
#
# Returns:
#	None.
#
# Register Usage:
# s0: pointer to first element of array
# s1: pointer to exit node
# t0: instruction
# t1: -1
#
# -----------------------------------------------------------------------------
redirectReturns:
	#save registers
	addi sp, sp, -12 # Adjust stack
	sw s0, 0(sp) # Save s0 into stack
	sw s1, 4(sp) # Save s1 into stack
	sw ra, 8(sp) # Save ra into stack

    mv s0, a0 # s0 = pointer to first element of array
    mv s1, a1 # s1 = pointer to exit node

	loopRedictReturns:
		# check loop exit
		lw t0, 0(s0) # t0 = instruction
		li t1, -1 # t1 = -1
		beq t0, t1, exitRedirectReturns # if instruction = -1, exit loop

		# check if jalr (ret is just pseudo instruction for jalr)
		andi t0, t0, 0x7F # isolate opcode
		li t1, 0x67 # load 0x67 into t1
		beq t0, t1, fixJalr # if opcode is 0x67, it's a jalr instruction
		j nextRedirectReturns # else go to next instruction

		fixJalr:
			#j is just jal with rd = x0
			#create jal instruction 
			sub a0, s1, s0 # a0 = exit node - first instruction
			li a1, 0 # a1 = rd (x0)
			jal ra, createJal # a0 = jal instruction
			sw a0, 0(s0) # store the adjusted instruction back to memory

		nextRedirectReturns:
			addi s0, s0, 4 # s0 = s0 + 4
			j loopRedictReturns # jump to loopRedictReturns

	exitRedirectReturns:
		#restore registers
		lw s0, 0(sp) # Restore s0 from stack
		lw s1, 4(sp) # Restore s1 from stack
		lw ra, 8(sp) # Restore ra from stack
		addi sp, sp, 12 # Adjust stack
		ret

# createLoad:
# 
# Description:
# 	Creates a load instruction from a specified rd, rs1, immediate
# 	rd = Mem[rs1 + immediate]	
#
# Args:
# 	a0: rs1
#	a1: rd
#	a2: immediate
#
# Returns: 
#	t0: load instruction
#   t1: temp register
#
# -----------------------------------------------------------------------------
createLoad:
    # verify that 0 <= a0 <= 31 and 0 <= a1 <= 31
    bltz a0, registerError 
    bltz a1, registerError
    li t0, 32
    bge a0, t0, registerError
    bge a1, t0, registerError

    li t0, 0 # t0 = instruction

    # opcode is 000 0011
    li t1, 0x03 # t1 = 0000 0011
    or t0, t0, t1 # add opcode to instruction

    # funct3 is 010
    li t1, 2 # t1 = 010
    slli t1, t1, 12 # shift left 12 bits
    or t0, t0, t1 # add funct3 to instruction

    # add rs1 to instruction
    slli t1, a0, 15 # shift left 15 bits
    or t0, t0, t1 # add rs1 to instruction

    # add rd to instruction
    slli t1, a1, 7 # shift left 7 bits
    or t0, t0, t1 # add rd to instruction

    # add immediate to instruction
    slli t1, a2, 20 # shift left 20 bits
    or t0, t0, t1 # add immediate to instruction

    mv a0, t0 # return the instruction

    ret

# -----------------------------------------------------------------------------
# createStore:
# 
# Description:
# 	Creates a store instruction from a specified rd, rs1, immediate
# 	Mem[rs1 + immediate] = rd	
#
# Args:
# 	a0: rs1
#	a1: rd
#	a2: immediate
#
# Returns: 
#	t0: store instruction
#   t1: temp register
# -----------------------------------------------------------------------------
createStore:
    # verify that 0 <= a0 <= 31 and 0 <= a1 <= 31
    bltz a0, registerError
    bltz a1, registerError
    li t0, 32
    bge a0, t0, registerError
    bge a1, t0, registerError

    li t0, 0 # t0 = instruction

    # opcode is 010 0011
    li t1, 0x23 # t1 = 0100 0011
    or t0, t0, t1 # add opcode to instruction

    # funct3 is 010
    li t1, 2 # t1 = 010
    slli t1, t1, 12 # shift left 12 bits
    or t0, t0, t1 # add funct3 to instruction

    # add rs1 to instruction
    slli t1, a0, 15 # shift left 15 bits
    or t0, t0, t1 # add rs1 to instruction

    # add rd to instruction
    slli t1, a1, 20 # shift left 20 bits
    or t0, t0, t1 # add rd to instruction

    # add immediate to instruction
    slli t1, a2, 7 # shift left 7 bits
    or t0, t0, t1 # add immediate to instruction

    mv a0, t0 # return the instruction

    ret

# -----------------------------------------------------------------------------
# getAddiImm:
# 
# Description:
# 	Extract the immediate from an addi (I-type) instruction and sign extend it.
#
# Args:
# 	a0: addi (I-type) instruction
#
# Returns: 
#	a0: sign extended immediate
#
# Register Usage:
#	s0: addi (I-type) instruction
# 	s1: sign extended immediate
# -----------------------------------------------------------------------------
getAddiImm:
	addi sp, sp, -12
	sw s0, 0(sp)
	sw s1, 4(sp)
	sw ra, 8(sp)

	mv s0, a0 # s0 <- addi instruction
	li s1, 0  # s1 <- sign extended immediate

	li t0, 0x80000000
	and t0, s0, t0 # get bit 11 of immediate
	srli t0, t0, 31

	li t1, 0xFFFFF000
	mul t0, t0, t1 # t0 <- 0x0000 0000 if bit 11 = 0, 0xFFFF F000 if bit 11 = 1

	or s1, s1, t0 # add the sign extension

	li t0, 0xFFF00000
	and t0, s0, t0 # get bits 11-0 of immediate
	srli t0, t0, 20
	or s1, s1, t0 # add bits 11-0 to immediate

	mv a0, s1 # return the sign extended immediate

	lw s0, 0(sp)
	lw s1, 4(sp)
	lw ra, 8(sp)
	addi sp, sp, 12
	ret

# -----------------------------------------------------------------------------
# createAddi:
#
# Description:
# 	Create an addi (I-type) instruction with a specified rs1, rd, and immediate.
#
# Args:
# 	a0: immediate
#	a1: rs1
#	a2: rd
#
# Returns: 
#	a0: addi instruction
#
# Register Usage:
#	s0: immediate
# 	s1: rs1
#	s2: rd
#	s3: instruction
# -----------------------------------------------------------------------------
createAddi:
	# store to stack
	addi sp, sp, -20
	sw s0, 0(sp)
	sw s1, 4(sp)
	sw s2, 8(sp)
	sw s3, 12(sp)
	sw ra, 16(sp)
	
	# verify that 0 <= a1 <= 31 and 0 <= a2 <= 31
	bltz a1, registerError
	bltz a2, registerError
	li t0, 32
	bge a1, t0, registerError
	bge a2, t0, registerError

	mv s0, a0 # s0 <- immediate
	mv s1, a1 # s1 <- rs1
	mv s2, a2 # s2 <- rd
	li s3, 0  # s3 <- instruction
	
	li t0, 0x13 # t0 <- opcode
	or s3, s3, t0 # add opcode to instruction

	# funct3 is 000 so don't need to add

	slli t0, s1, 15
	or s3, s3, t0 # add rs1 to instruction

	slli t0, s2, 7
	or s3, s3, t0 # add rd to instruction

	slli t0, s0, 20
	or s3, s3, t0 # add immediate to instruction

	mv a0, s3 # return the instruction

	lw s0, 0(sp)
	lw s1, 4(sp)
	lw s2, 8(sp)
	lw s3, 12(sp)
	lw ra, 16(sp)
	addi sp, sp, 20
	ret

# -----------------------------------------------------------------------------
# getJalImm:
#
# Args:
# 	a0: UJ (jal) instruction
#
# Returns: 
#	a0: sign extended immediate
#
# Register Usage:
#	s0: UJ (jal) instruction
# 	s1: sign extended immediate
# -----------------------------------------------------------------------------
getJalImm:
	# store to stack
	addi sp, sp, -12
	sw s0, 0(sp)
	sw s1, 4(sp)
	sw ra, 8(sp)
	
	mv s0, a0 # s0 <- UJ instruction 
	li s1, 0  # s1 <- sign extended immediate
	
	li t0, 1
	slli t0, t0, 31
	and t0, s0, t0 # t0 <- 20th bit of imm
	srli t0, t0, 31 # t0 = 1 if 20th bit = 1, 0 if 20th bit = 0.
	li t1, 0x0FFF
	mul t1, t1, t0 # t1 <- 0x0FFF if t0=1, 0 if t0 = 0
	slli t1, t1, 20
	or s1, s1, t1 # add the sign extended 20th bit

	li t0, 0x7FE00000
	and t0, s0, t0 # get bits 10-1 of immediate
	srli t0, t0, 21
	slli t0, t0, 1 
	or s1, s1, t0 # add bits 10-1 to immediate

	li t0, 1
	slli t0, t0, 20
	and t0, s0, t0 # get bit 11
	srli t0, t0, 20
	slli t0, t0, 11
	or s1, s1, t0 # add bit 11 to the instruction

	li t0, 0x0FF000 # t0 <- 0000 0000 0000 1111 1111 0000 0000 0000
	and t0, s0, t0 # get bits 19-12 of immediate
	or s1, s1, t0 # add bits 19-12 to immediate
	
	mv a0, s1 # return the sign extended immediate
	
	lw s0, 0(sp)
	lw s1, 4(sp)
	lw ra, 8(sp)
	addi sp, sp, 12
	ret
	
# -----------------------------------------------------------------------------
# createJal:
#
# Args:
# 	a0: immediate
#	a1: rd
#
# Returns: 
#	a0: UJ (jal) instruction
#
# Register Usage:
#	s0: immediate
# 	s1: rd
#	s2: jal instruction
# -----------------------------------------------------------------------------
createJal:
	# store to stack
	addi sp, sp, -16
	sw s0, 0(sp)
	sw s1, 4(sp)
	sw s2, 8(sp)
	sw ra, 12(sp)
	
	# verify that 0 <= a1 <= 31
	bltz a1, registerError
	li t0, 32
	bge a1, t0, registerError
	
	mv s0, a0 # s0 <- immediate
	mv s1, a1 # s1 <- rd
	li s2, 0  # s2 <- jal instruction
	
	li t0, 0x6F
	or s2, s2, t0 # add opcode to instruction
	
	slli t0, s1, 7
	or s2, s2, t0 # add rd to instruction
	
	li t0, 1
	slli t0, t0, 20
	and t0, s0, t0
	srli t0, t0, 20 
	slli t0, t0, 31
	or s2, s2, t0 # add the 20th bit to the instruction

	li t0, 0x7FE
	and t0, s0, t0
	srli t0, t0, 1
	slli t0, t0, 21
	or s2, s2, t0 # add bits 10 - 1 to the instruction

	li t0, 0x0800
	and t0, s0, t0
	srli t0, t0, 11
	slli t0, t0, 20
	or s2, s2, t0 # add bit 11 to the instruction

	li t0, 0x000FF000
	and t0, s0, t0
	or s2, s2, t0 # add bits 19 - 12 to the instruction
	
	mv a0, s2 # return the instruction
	
	lw s0, 0(sp)
	lw s1, 4(sp)
	lw s2, 8(sp)
	lw ra, 12(sp)
	addi sp, sp, 16
	ret


# this code is called if the arguments to the above functions are incorrect
registerError:
	la a0, registerErrorString
	li a7, 4
	ecall
	ret
