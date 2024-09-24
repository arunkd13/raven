# rdi - stack pointer (&mut [u8; 256])
# rsi - stack index (u8)
# rdx - nexturn stack pointer (&mut [u8; 256])
# rcx - nexturn stack index (u8)
# r8 - RAM pointer (&mut [u8; 65536])
# r9 - program counter (u16), offset of the next value in RAM
# r10 - VM pointer (&mut Uxn) (loaded from stack in native_entry)
# r11 - Device handle pointer (&DeviceHandle) (loaded from stack in native_entry)
# r12 - Jump table pointer (loaded in native_entry)
# r13-15, rbx, rax - scratch registers
.macro next
    # load instruction from program counter
    movzx rax, byte ptr [r8 + r9]
    # increment program counter
    inc r9w 
    # jump to instruction handler from the jump table
    mov rbx, [r12 + rax * 8]
    jmp rbx
.endm

.macro pushd, reg
    inc sil
    mov byte ptr [rdi + rsi], \reg
.endm

.global native_entry
native_entry:
    # Setup frame pointer for debugging
    push rbp
    mov rbp, rsp

    # Load additional arguments from stack
    mov r10, [rsp + 8]
    mov r11, [rsp + 16]

    # Save registers that need to be restored when returning
    push r12
    push r13
    push r14
    push r15
    push rbx

    # Load address of JUMP_TABLE
    lea r12, [rip + JUMP_TABLE]

    # Load the index values for the data and return stacks, from the respective pointer arguments
    movzx rsi, byte ptr [rsi]
    movzx rcx, byte ptr [rcx]

    # Jump into the instruction list
    next

_BRK:
    # Restore registers that were saved in stack
    pop rbx
    pop r15
    pop r14
    pop r13
    pop r12

    # return PC from function
    mov rax, r9 

    # Restore frame pointer
    pop rbp

    ret

_INC:
    next

_POP:
    next

_NIP:
    next

_SWP:
    next

_ROT:
    next

_DUP:
    next

_OVR:
    next

_EQU:
    next

_NEQ:
    next

_GTH:
    next

_LTH:
    next

_JMP:
    next

_JCN:
    next

_JSR:
    next

_STH:
    next

_LDZ:
    next

_STZ:
    next

_LDR:
    next

_STR:
    next

_LDA:
    next

_STA:
    next

_DEI:
    next

_DEO:
    next

_ADD:
    next

_SUB:
    next

_MUL:
    next

_DIV:
    next

_AND:
    next

_ORA:
    next

_EOR:
    next

_SFT:
    next

_JCI:
    next

_INC2:
    next

_POP2:
    next

_NIP2:
    next

_SWP2:
    next

_ROT2:
    next

_DUP2:
    next

_OVR2:
    next

_EQU2:
    next

_NEQ2:
    next

_GTH2:
    next

_LTH2:
    next

_JMP2:
    next

_JCN2:
    next

_JSR2:
    next

_STH2:
    next

_LDZ2:
    next

_STZ2:
    next

_LDR2:
    next

_STR2:
    next

_LDA2:
    next

_STA2:
    next

_DEI2:
    next

_DEO2:
    next

_ADD2:
    next

_SUB2:
    next

_MUL2:
    next

_DIV2:
    next

_AND2:
    next

_ORA2:
    next

_EOR2:
    next

_SFT2:
    next

_JMI:
    next

_INCr:
    next

_POPr:
    next

_NIPr:
    next

_SWPr:
    next

_ROTr:
    next

_DUPr:
    next

_OVRr:
    next

_EQUr:
    next

_NEQr:
    next

_GTHr:
    next

_LTHr:
    next

_JMPr:
    next

_JCNr:
    next

_JSRr:
    next

_STHr:
    next

_LDZr:
    next

_STZr:
    next

_LDRr:
    next

_STRr:
    next

_LDAr:
    next

_STAr:
    next

_DEIr:
    next

_DEOr:
    next

_ADDr:
    next

_SUBr:
    next

_MULr:
    next

_DIVr:
    next

_ANDr:
    next

_ORAr:
    next

_EORr:
    next

_SFTr:
    next

_JSI:
    next

_INC2r:
    next

_POP2r:
    next

_NIP2r:
    next

_SWP2r:
    next

_ROT2r:
    next

_DUP2r:
    next

_OVR2r:
    next

_EQU2r:
    next

_NEQ2r:
    next

_GTH2r:
    next

_LTH2r:
    next

_JMP2r:
    next

_JCN2r:
    next

_JSR2r:
    next

_STH2r:
    next

_LDZ2r:
    next

_STZ2r:
    next

_LDR2r:
    next

_STR2r:
    next

_LDA2r:
    next

_STA2r:
    next

_DEI2r:
    next

_DEO2r:
    next

_ADD2r:
    next

_SUB2r:
    next

_MUL2r:
    next

_DIV2r:
    next

_AND2r:
    next

_ORA2r:
    next

_EOR2r:
    next

_SFT2r:
    next

_LIT:
    movzx rax, byte ptr [r8 + r9]
    inc r9w
    pushd al
    next

_INCk:
    next

_POPk:
    next

_NIPk:
    next

_SWPk:
    next

_ROTk:
    next

_DUPk:
    next

_OVRk:
    next

_EQUk:
    next

_NEQk:
    next

_GTHk:
    next

_LTHk:
    next

_JMPk:
    next

_JCNk:
    next

_JSRk:
    next

_STHk:
    next

_LDZk:
    next

_STZk:
    next

_LDRk:
    next

_STRk:
    next

_LDAk:
    next

_STAk:
    next

_DEIk:
    next

_DEOk:
    next

.macro binary_opk op
    next
.endm

_ADDk:
    next

_SUBk:
    next

_MULk:
    next

_DIVk:
    next

_ANDk:
    next

_ORAk:
    next

_EORk:
    next

_SFTk:
    next

_LIT2:
    # higher byte
    movzx rax, byte ptr [r8 + r9]
    inc r9w
    pushd al
    # lower byte
    movzx rax, byte ptr [r8 + r9]
    inc r9w
    pushd al
    next

_INC2k:
    next

_POP2k:
    next

_NIP2k:
    next

_SWP2k:
    next

_ROT2k:
    next

_DUP2k:
    next

_OVR2k:
    next

_EQU2k:
    next

_NEQ2k:
    next

_GTH2k:
    next

_LTH2k:
    next

_JMP2k:
    next

_JCN2k:
    next

_JSR2k:
    next

_STH2k:
    next

_LDZ2k:
    next

_STZ2k:
    next

_LDR2k:
    next

_STR2k:
    next

_LDA2k:
    next

_STA2k:
    next

_DEI2k:
    next

_DEO2k:
    next

_ADD2k:
    next

_SUB2k:
    next

_MUL2k:
    next

_DIV2k:
    next

_AND2k:
    next

_ORA2k:
    next

_EOR2k:
    next

_SFT2k:
    next

_LITr:
    next

_INCkr:
    next

_POPkr:
    next

_NIPkr:
    next

_SWPkr:
    next

_ROTkr:
    next

_DUPkr:
    next

_OVRkr:
    next

_EQUkr:
    next

_NEQkr:
    next

_GTHkr:
    next

_LTHkr:
    next

_JMPkr:
    next

_JCNkr:
    next

_JSRkr:
    next

_STHkr:
    next

_LDZkr:
    next

_STZkr:
    next

_LDRkr:
    next

_STRkr:
    next

_LDAkr:
    next

_STAkr:
    next

_DEIkr:
    next

_DEOkr:
    next

.macro binary_opkr op
    next
.endm

_ADDkr:
    next

_SUBkr:
    next

_MULkr:
    next

_DIVkr:
    next

_ANDkr:
    next

_ORAkr:
    next

_EORkr:
    next

_SFTkr:
    next

_LIT2r:
    next

_INC2kr:
    next

_POP2kr:
    next

_NIP2kr:
    next

_SWP2kr:
    next

_ROT2kr:
    next

_DUP2kr:
    next

_OVR2kr:
    next

_EQU2kr:
    next

_NEQ2kr:
    next

_GTH2kr:
    next

_LTH2kr:
    next

_JMP2kr:
    next

_JCN2kr:
    next

_JSR2kr:
    next

_STH2kr:
    next

_LDZ2kr:
    next

_STZ2kr:
    next

_LDR2kr:
    next

_STR2kr:
    next

_LDA2kr:
    next

_STA2kr:
    next

_DEI2kr:
    next

_DEO2kr:
    next

_ADD2kr:
    next

_SUB2kr:
    next

_MUL2kr:
    next

_DIV2kr:
    next

_AND2kr:
    next

_ORA2kr:
    next

_EOR2kr:
    next

_SFT2kr:
    next

.data
.balign 4096
.global JUMP_TABLE
JUMP_TABLE:
    .quad _BRK
    .quad _INC
    .quad _POP
    .quad _NIP
    .quad _SWP
    .quad _ROT
    .quad _DUP
    .quad _OVR
    .quad _EQU
    .quad _NEQ
    .quad _GTH
    .quad _LTH
    .quad _JMP
    .quad _JCN
    .quad _JSR
    .quad _STH
    .quad _LDZ
    .quad _STZ
    .quad _LDR
    .quad _STR
    .quad _LDA
    .quad _STA
    .quad _DEI
    .quad _DEO
    .quad _ADD
    .quad _SUB
    .quad _MUL
    .quad _DIV
    .quad _AND
    .quad _ORA
    .quad _EOR
    .quad _SFT
    .quad _JCI
    .quad _INC2
    .quad _POP2
    .quad _NIP2
    .quad _SWP2
    .quad _ROT2
    .quad _DUP2
    .quad _OVR2
    .quad _EQU2
    .quad _NEQ2
    .quad _GTH2
    .quad _LTH2
    .quad _JMP2
    .quad _JCN2
    .quad _JSR2
    .quad _STH2
    .quad _LDZ2
    .quad _STZ2
    .quad _LDR2
    .quad _STR2
    .quad _LDA2
    .quad _STA2
    .quad _DEI2
    .quad _DEO2
    .quad _ADD2
    .quad _SUB2
    .quad _MUL2
    .quad _DIV2
    .quad _AND2
    .quad _ORA2
    .quad _EOR2
    .quad _SFT2
    .quad _JMI
    .quad _INCr
    .quad _POPr
    .quad _NIPr
    .quad _SWPr
    .quad _ROTr
    .quad _DUPr
    .quad _OVRr
    .quad _EQUr
    .quad _NEQr
    .quad _GTHr
    .quad _LTHr
    .quad _JMPr
    .quad _JCNr
    .quad _JSRr
    .quad _STHr
    .quad _LDZr
    .quad _STZr
    .quad _LDRr
    .quad _STRr
    .quad _LDAr
    .quad _STAr
    .quad _DEIr
    .quad _DEOr
    .quad _ADDr
    .quad _SUBr
    .quad _MULr
    .quad _DIVr
    .quad _ANDr
    .quad _ORAr
    .quad _EORr
    .quad _SFTr
    .quad _JSI
    .quad _INC2r
    .quad _POP2r
    .quad _NIP2r
    .quad _SWP2r
    .quad _ROT2r
    .quad _DUP2r
    .quad _OVR2r
    .quad _EQU2r
    .quad _NEQ2r
    .quad _GTH2r
    .quad _LTH2r
    .quad _JMP2r
    .quad _JCN2r
    .quad _JSR2r
    .quad _STH2r
    .quad _LDZ2r
    .quad _STZ2r
    .quad _LDR2r
    .quad _STR2r
    .quad _LDA2r
    .quad _STA2r
    .quad _DEI2r
    .quad _DEO2r
    .quad _ADD2r
    .quad _SUB2r
    .quad _MUL2r
    .quad _DIV2r
    .quad _AND2r
    .quad _ORA2r
    .quad _EOR2r
    .quad _SFT2r
    .quad _LIT
    .quad _INCk
    .quad _POPk
    .quad _NIPk
    .quad _SWPk
    .quad _ROTk
    .quad _DUPk
    .quad _OVRk
    .quad _EQUk
    .quad _NEQk
    .quad _GTHk
    .quad _LTHk
    .quad _JMPk
    .quad _JCNk
    .quad _JSRk
    .quad _STHk
    .quad _LDZk
    .quad _STZk
    .quad _LDRk
    .quad _STRk
    .quad _LDAk
    .quad _STAk
    .quad _DEIk
    .quad _DEOk
    .quad _ADDk
    .quad _SUBk
    .quad _MULk
    .quad _DIVk
    .quad _ANDk
    .quad _ORAk
    .quad _EORk
    .quad _SFTk
    .quad _LIT2
    .quad _INC2k
    .quad _POP2k
    .quad _NIP2k
    .quad _SWP2k
    .quad _ROT2k
    .quad _DUP2k
    .quad _OVR2k
    .quad _EQU2k
    .quad _NEQ2k
    .quad _GTH2k
    .quad _LTH2k
    .quad _JMP2k
    .quad _JCN2k
    .quad _JSR2k
    .quad _STH2k
    .quad _LDZ2k
    .quad _STZ2k
    .quad _LDR2k
    .quad _STR2k
    .quad _LDA2k
    .quad _STA2k
    .quad _DEI2k
    .quad _DEO2k
    .quad _ADD2k
    .quad _SUB2k
    .quad _MUL2k
    .quad _DIV2k
    .quad _AND2k
    .quad _ORA2k
    .quad _EOR2k
    .quad _SFT2k
    .quad _LITr
    .quad _INCkr
    .quad _POPkr
    .quad _NIPkr
    .quad _SWPkr
    .quad _ROTkr
    .quad _DUPkr
    .quad _OVRkr
    .quad _EQUkr
    .quad _NEQkr
    .quad _GTHkr
    .quad _LTHkr
    .quad _JMPkr
    .quad _JCNkr
    .quad _JSRkr
    .quad _STHkr
    .quad _LDZkr
    .quad _STZkr
    .quad _LDRkr
    .quad _STRkr
    .quad _LDAkr
    .quad _STAkr
    .quad _DEIkr
    .quad _DEOkr
    .quad _ADDkr
    .quad _SUBkr
    .quad _MULkr
    .quad _DIVkr
    .quad _ANDkr
    .quad _ORAkr
    .quad _EORkr
    .quad _SFTkr
    .quad _LIT2r
    .quad _INC2kr
    .quad _POP2kr
    .quad _NIP2kr
    .quad _SWP2kr
    .quad _ROT2kr
    .quad _DUP2kr
    .quad _OVR2kr
    .quad _EQU2kr
    .quad _NEQ2kr
    .quad _GTH2kr
    .quad _LTH2kr
    .quad _JMP2kr
    .quad _JCN2kr
    .quad _JSR2kr
    .quad _STH2kr
    .quad _LDZ2kr
    .quad _STZ2kr
    .quad _LDR2kr
    .quad _STR2kr
    .quad _LDA2kr
    .quad _STA2kr
    .quad _DEI2kr
    .quad _DEO2kr
    .quad _ADD2kr
    .quad _SUB2kr
    .quad _MUL2kr
    .quad _DIV2kr
    .quad _AND2kr
    .quad _ORA2kr
    .quad _EOR2kr
    .quad _SFT2kr
