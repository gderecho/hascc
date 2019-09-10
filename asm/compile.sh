../.stack-work/install/x86_64-linux-tinfo6/c6ac1a2937538a037cbcd56f52a340227d1422a5b901dcbbe972924efd76f553/8.6.5/bin/hascc-exe > main.asm
nasm -felf64 main.asm && 
    ld -dynamic-link /lib64/ld-linux-x86-64.so.2 \
        /usr/lib/x86_64-linux-gnu/crt1.o \
        /usr/lib/x86_64-linux-gnu/crti.o \
        -lc /usr/lib/x86_64-linux-gnu/crtn.o \
        main.o
