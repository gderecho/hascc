### Project hascc
The haskell c compiler.

##### Status: Almost completely non-functional.
##### Targets: Currently, only x86-64 linux.

### Example usage

- Clone this repository, and change directories to `asm/`. 
- Create the following as ./main.c:

        int main()
        {
            return 123;
        }
    
- Execute `stack build`
- Execute `cat main.c | bash ./compile.sh`
- Execute `./a.out; echo "Exited with return code $?"`

The output is

    Hello, World!
    Exited with return code 123





### Goals
The current aim is to compile C files to nasm x86-64 linux assembly output.

The project hopes to later target LLVM or .net or JVM or other assembly syntax.


### Dependencies 

 - [nasm](https://www.nasm.us/) (the netwide assembler)

### Credits: Resources Used
 - Nora Sandler's c compiler blog https://norasandler.com/2017/11/29/Write-a-Compiler.html
 - stephendiehl's haskell llvm compiler tutorial http://www.stephendiehl.com/llvm/
