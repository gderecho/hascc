stack run > main.asm
nasm -felf64 main.asm && 
    ld -dynamic-link /lib64/ld-linux-x86-64.so.2 \
        /usr/lib/x86_64-linux-gnu/crt1.o \
        /usr/lib/x86_64-linux-gnu/crti.o \
        -lc /usr/lib/x86_64-linux-gnu/crtn.o \
        main.o
