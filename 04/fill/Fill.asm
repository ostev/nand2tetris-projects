// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

(BEGINNING)

@filled
M=-1

@16384
D=A

@address
M=D

@8192
D=A

@count
M=D

@KBD
D=M

@LOOP
D;JGT

@filled
M=0

(LOOP)

@filled
D=M

@address
A=M

M=D

@address
M=M+1

@count
M=M-1
D=M

@LOOP
D;JGT

(END)

@BEGINNING
0;JMP