# Simple Language Compiler to Stack-Based Machine Code
...for fun and education.

## Overview

This project is a **compiler** that translates a simple programming language into **stack-based machine code**. This compiler was developed as a practical application of concepts learned in my undergraduate Compiler course, drawing inspiration from the specifications outlined in there. It takes source code as input and generates machine instructions that can be executed on a stack-based virtual machine.

## Features

- **Supports integer arithmetic** (`+`, `-`, `*`, `/`, `%`)
- **Logical and relational operations** (`and`, `or`, `not`, `<`, `>`, `<=`, `>=`, `=`, `<>`)
- **Conditional statements** (`if-then`, `if-then-else`)
- **Looping via labels and `goto` statements**
- **Basic I/O operations** (`read`, `print`)
- **Procedure calls with `gosub` and `return`**
- **Stack-based execution model**
- **Memory management for variables and labels**

## Input Language Specification

The compiler processes a high-level structured language with:
- **Variable and label declarations** (`var <IDENT>`, `label <IDENT>`)
- **Arithmetic and logical expressions**
- **Control flow statements** (`if`, `goto`, `gosub`, `return`, `begin ... end`)
- **Program termination via `exit`**

Example **input program** (Factorial Calculation):

```plaintext
label loop;
var k;
var j;
var i;
read k;
if k > 0 then begin
  j:=1;
  i:=k;
  loop:
    j:=j*i;
    i:=i-1;
    if i > 1 then goto loop;
  print j;
end;
```

Outputs:
```plaintext
DATA	0,0,0,0,0,0
#	k,j,i,ret,boolSetRet,gp
0	READ
1	POP $0
2	PUSH $0
3	PUSH 0
4	PUSH 9
5	POP $4
6	SUB
7	JGZ $34
8	JMP $38
9	JZ $33
10	PUSH 1
11	POP $1
12	PUSH $0
13	POP $2
14	PUSH $1
15	PUSH $2
16	MUL
17	POP $1
18	PUSH $2
19	PUSH 1
20	SUB
21	POP $2
22	PUSH $2
23	PUSH 1
24	PUSH 29
25	POP $4
26	SUB
27	JGZ $34
28	JMP $38
29	JZ $31
30	JMP $14
31	PUSH $1
32	PRINT
33	STOP
#BoolLabels - setTrue, setFalse
34	POP
35	PUSH 1
36	PUSH $4
37	JMP
38	POP
39	PUSH 0
40	PUSH $4
41	JMP
```