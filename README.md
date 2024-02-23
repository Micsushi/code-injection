# code injection
 
## Functionality
A RISCV program that will save and restore all the used s-registers in the case that the user forgets to do so themselves. The instructions will be generated based on which registers need to be saved and which registers the user used. These instructions will then be injected into the beginning and end of the function. Also, it will change the code associated with jump instructions after the code injection.
