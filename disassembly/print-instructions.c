#include "riscv-disas.c"

void print_inst(uint64_t pc, uint32_t inst)
{
    char buf[80] = { 0 };
    disasm_inst(buf, sizeof(buf), rv64, pc, inst);
    printf("%016" PRIx64 ":  %s\n", pc, buf);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
	   printf("ERROR: This program takes a 1 filename as a program argument.");
	   return 1;
    }
  	
    FILE *file;
    char line[20]; // Assuming each line is at most 19 characters long.

    // Open the file in read mode.
    file = fopen(argv[1], "r");
    if (file == NULL) {
        printf("Error opening the file.\n");
        return 1;
    }
	
    int default_pc = 0x10000;
   // Read and convert each line to hexadecimal until the end of the file.
    while (fgets(line, sizeof(line), file)) {
        unsigned int hex_value;
        if (sscanf(line, "%x", &hex_value) == 1) {
            print_inst(default_pc,hex_value);
	    default_pc+=4;
	}   else {
            printf("Invalid line: %s", line);
        }
    }

    // Close the file after reading.
    fclose(file);

    return 0; 
    }
