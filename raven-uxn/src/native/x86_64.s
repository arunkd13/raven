# rdi - stack pointer (&mut [u8; 256])
# rsi - stack index (u8)
# rdx - return stack pointer (&mut [u8; 256])
# rcx - return stack index (u8)
# r8 - RAM pointer (&mut [u8; 65536])
# r9 - program counter (u16), offset of the next value in RAM
# r10 - VM pointer (&mut Uxn) (loaded from stack in native_entry)
# r11 - Device handle pointer (&DeviceHandle) (loaded from stack in native_entry)
# r12 - Jump table pointer (loaded in native_entry)
# r13-15, rbx, rax - scratch registers
.macro next
    # load instruction from program counter and increment
    movzx rax, byte ptr [r8 + r9]
    inc r9w 
    # jump to corresponding instruction handler from the jump table
    mov rbx, [r12 + rax * 8]
    jmp rbx
.endm

.macro unimplemented
    int3
    next
.endm

.macro pushd, reg
    inc sil
    mov byte ptr [rdi + rsi], \reg
.endm

.macro precall
    # We have to write our stack index pointers back into the &mut Uxn
    mov rbx, [rsp]          # read ret index pointer
    mov rax, [rsp + 8]      # read stack index pointer
    mov byte ptr [rax], sil # save stack index
    mov byte ptr [rbx], cl  # save ret index

    # Save rest of the register states that we need
    push rdi
    push rdx
    push r8
    push r9

    # Setup the arguments to be passed
    mov rdi, r10 # VM pointer (&mut Uxn)
    mov rsi, r11 # Device handle pointer
.endm

.macro postcall
    # Restore the register states
    pop r9
    pop r8
    pop rdx
    pop rdi

    # The DEO operation may have changed stack pointers, so reload them here
    mov rbx, [rsp]
    mov rax, [rsp + 8]
    movzx rsi, byte ptr [rax]
    movzx rcx, byte ptr [rbx]

    # Load the additional arguments from stack
    mov r10, [rbp + 16]
    mov r11, [rbp + 24]
.endm

.global native_entry
native_entry:
    # Setup frame pointer for debugging
    push rbp
    mov rbp, rsp

    # Load additional arguments from stack
    mov r10, [rbp + 16]
    mov r11, [rbp + 24]

    # Save registers that need to be restored when returning
    push r12
    push r13
    push r14
    push r15
    push rbx

    # Load address of JUMP_TABLE
    lea r12, [rip + JUMP_TABLE]

    # Convert from index pointers to index values in rsi / rcx
    push rsi # save stack index pointer
    push rcx # save ret index pointer
    movzx rsi, byte ptr [rsi] # load stack index
    movzx rcx, byte ptr [rcx] # load ret index

    # Jump into the instruction list
    next

_BRK:
    # Write index values back through index pointers
    pop rbx                 # read ret index pointer
    pop rax                 # read stack index pointer
    mov byte ptr [rax], sil # save stack index
    mov byte ptr [rbx], cl  # save ret index

    # Restore registers that were saved in stack
    pop rbx
    pop r15
    pop r14
    pop r13
    pop r12

    pop rbp # Restore frame pointer

    mov rax, r9 # return PC from function
    ret

_INC:
    unimplemented

_POP:
    unimplemented

_NIP:
    unimplemented

_SWP:
    unimplemented

_ROT:
    unimplemented

_DUP:
    unimplemented

_OVR:
    unimplemented

_EQU:
    unimplemented

_NEQ:
    unimplemented

_GTH:
    unimplemented

_LTH:
    unimplemented

_JMP:
    unimplemented

_JCN:
    unimplemented

_JSR:
    unimplemented

_STH:
    unimplemented

_LDZ:
    unimplemented

_STZ:
    unimplemented

_LDR:
    unimplemented

_STR:
    unimplemented

_LDA:
    unimplemented

_STA:
    unimplemented

_DEI:
    unimplemented

_DEO:
    unimplemented

_ADD:
    unimplemented

_SUB:
    unimplemented

_MUL:
    unimplemented

_DIV:
    unimplemented

_AND:
    unimplemented

_ORA:
    unimplemented

_EOR:
    unimplemented

_SFT:
    unimplemented

_JCI:
    unimplemented

_INC2:
    unimplemented

_POP2:
    unimplemented

_NIP2:
    unimplemented

_SWP2:
    unimplemented

_ROT2:
    unimplemented

_DUP2:
    unimplemented

_OVR2:
    unimplemented

_EQU2:
    unimplemented

_NEQ2:
    unimplemented

_GTH2:
    unimplemented

_LTH2:
    unimplemented

_JMP2:
    unimplemented

_JCN2:
    unimplemented

_JSR2:
    unimplemented

_STH2:
    unimplemented

_LDZ2:
    unimplemented

_STZ2:
    unimplemented

_LDR2:
    unimplemented

_STR2:
    unimplemented

_LDA2:
    unimplemented

_STA2:
    unimplemented

_DEI2:
    unimplemented

_DEO2:
    precall
    call deo_2_entry # todo check return value for early exit?
    postcall
    next

_ADD2:
    unimplemented

_SUB2:
    unimplemented

_MUL2:
    unimplemented

_DIV2:
    unimplemented

_AND2:
    unimplemented

_ORA2:
    unimplemented

_EOR2:
    unimplemented

_SFT2:
    unimplemented

_JMI:
    # read the jump offset from code
    mov al, byte ptr [r8 + r9]
    inc r9w
    shl ax
    mov al, byte ptr [r8 + r9]
    inc r9w

    # move the program counter by the offset
    add r9w, ax

    next

_INCr:
    unimplemented

_POPr:
    unimplemented

_NIPr:
    unimplemented

_SWPr:
    unimplemented

_ROTr:
    unimplemented

_DUPr:
    unimplemented

_OVRr:
    unimplemented

_EQUr:
    unimplemented

_NEQr:
    unimplemented

_GTHr:
    unimplemented

_LTHr:
    unimplemented

_JMPr:
    unimplemented

_JCNr:
    unimplemented

_JSRr:
    unimplemented

_STHr:
    unimplemented

_LDZr:
    unimplemented

_STZr:
    unimplemented

_LDRr:
    unimplemented

_STRr:
    unimplemented

_LDAr:
    unimplemented

_STAr:
    unimplemented

_DEIr:
    unimplemented

_DEOr:
    unimplemented

_ADDr:
    unimplemented

_SUBr:
    unimplemented

_MULr:
    unimplemented

_DIVr:
    unimplemented

_ANDr:
    unimplemented

_ORAr:
    unimplemented

_EORr:
    unimplemented

_SFTr:
    unimplemented

_JSI:
    unimplemented

_INC2r:
    unimplemented

_POP2r:
    unimplemented

_NIP2r:
    unimplemented

_SWP2r:
    unimplemented

_ROT2r:
    unimplemented

_DUP2r:
    unimplemented

_OVR2r:
    unimplemented

_EQU2r:
    unimplemented

_NEQ2r:
    unimplemented

_GTH2r:
    unimplemented

_LTH2r:
    unimplemented

_JMP2r:
    unimplemented

_JCN2r:
    unimplemented

_JSR2r:
    unimplemented

_STH2r:
    unimplemented

_LDZ2r:
    unimplemented

_STZ2r:
    unimplemented

_LDR2r:
    unimplemented

_STR2r:
    unimplemented

_LDA2r:
    unimplemented

_STA2r:
    unimplemented

_DEI2r:
    unimplemented

_DEO2r:
    unimplemented

_ADD2r:
    unimplemented

_SUB2r:
    unimplemented

_MUL2r:
    unimplemented

_DIV2r:
    unimplemented

_AND2r:
    unimplemented

_ORA2r:
    unimplemented

_EOR2r:
    unimplemented

_SFT2r:
    unimplemented

_LIT:
    movzx rax, byte ptr [r8 + r9]
    inc r9w
    pushd al
    next

_INCk:
    unimplemented

_POPk:
    unimplemented

_NIPk:
    unimplemented

_SWPk:
    unimplemented

_ROTk:
    unimplemented

_DUPk:
    unimplemented

_OVRk:
    unimplemented

_EQUk:
    unimplemented

_NEQk:
    unimplemented

_GTHk:
    unimplemented

_LTHk:
    unimplemented

_JMPk:
    unimplemented

_JCNk:
    unimplemented

_JSRk:
    unimplemented

_STHk:
    unimplemented

_LDZk:
    unimplemented

_STZk:
    unimplemented

_LDRk:
    unimplemented

_STRk:
    unimplemented

_LDAk:
    unimplemented

_STAk:
    unimplemented

_DEIk:
    unimplemented

_DEOk:
    unimplemented

.macro binary_opk op
    unimplemented
.endm

_ADDk:
    unimplemented

_SUBk:
    unimplemented

_MULk:
    unimplemented

_DIVk:
    unimplemented

_ANDk:
    unimplemented

_ORAk:
    unimplemented

_EORk:
    unimplemented

_SFTk:
    unimplemented

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
    unimplemented

_POP2k:
    unimplemented

_NIP2k:
    unimplemented

_SWP2k:
    unimplemented

_ROT2k:
    unimplemented

_DUP2k:
    unimplemented

_OVR2k:
    unimplemented

_EQU2k:
    unimplemented

_NEQ2k:
    unimplemented

_GTH2k:
    unimplemented

_LTH2k:
    unimplemented

_JMP2k:
    unimplemented

_JCN2k:
    unimplemented

_JSR2k:
    unimplemented

_STH2k:
    unimplemented

_LDZ2k:
    unimplemented

_STZ2k:
    unimplemented

_LDR2k:
    unimplemented

_STR2k:
    unimplemented

_LDA2k:
    unimplemented

_STA2k:
    unimplemented

_DEI2k:
    unimplemented

_DEO2k:
    unimplemented

_ADD2k:
    unimplemented

_SUB2k:
    unimplemented

_MUL2k:
    unimplemented

_DIV2k:
    unimplemented

_AND2k:
    unimplemented

_ORA2k:
    unimplemented

_EOR2k:
    unimplemented

_SFT2k:
    unimplemented

_LITr:
    unimplemented

_INCkr:
    unimplemented

_POPkr:
    unimplemented

_NIPkr:
    unimplemented

_SWPkr:
    unimplemented

_ROTkr:
    unimplemented

_DUPkr:
    unimplemented

_OVRkr:
    unimplemented

_EQUkr:
    unimplemented

_NEQkr:
    unimplemented

_GTHkr:
    unimplemented

_LTHkr:
    unimplemented

_JMPkr:
    unimplemented

_JCNkr:
    unimplemented

_JSRkr:
    unimplemented

_STHkr:
    unimplemented

_LDZkr:
    unimplemented

_STZkr:
    unimplemented

_LDRkr:
    unimplemented

_STRkr:
    unimplemented

_LDAkr:
    unimplemented

_STAkr:
    unimplemented

_DEIkr:
    unimplemented

_DEOkr:
    unimplemented

.macro binary_opkr op
    unimplemented
.endm

_ADDkr:
    unimplemented

_SUBkr:
    unimplemented

_MULkr:
    unimplemented

_DIVkr:
    unimplemented

_ANDkr:
    unimplemented

_ORAkr:
    unimplemented

_EORkr:
    unimplemented

_SFTkr:
    unimplemented

_LIT2r:
    unimplemented

_INC2kr:
    unimplemented

_POP2kr:
    unimplemented

_NIP2kr:
    unimplemented

_SWP2kr:
    unimplemented

_ROT2kr:
    unimplemented

_DUP2kr:
    unimplemented

_OVR2kr:
    unimplemented

_EQU2kr:
    unimplemented

_NEQ2kr:
    unimplemented

_GTH2kr:
    unimplemented

_LTH2kr:
    unimplemented

_JMP2kr:
    unimplemented

_JCN2kr:
    unimplemented

_JSR2kr:
    unimplemented

_STH2kr:
    unimplemented

_LDZ2kr:
    unimplemented

_STZ2kr:
    unimplemented

_LDR2kr:
    unimplemented

_STR2kr:
    unimplemented

_LDA2kr:
    unimplemented

_STA2kr:
    unimplemented

_DEI2kr:
    unimplemented

_DEO2kr:
    unimplemented

_ADD2kr:
    unimplemented

_SUB2kr:
    unimplemented

_MUL2kr:
    unimplemented

_DIV2kr:
    unimplemented

_AND2kr:
    unimplemented

_ORA2kr:
    unimplemented

_EOR2kr:
    unimplemented

_SFT2kr:
    unimplemented

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
