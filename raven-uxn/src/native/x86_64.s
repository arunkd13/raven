# TODO Check all instructions starting from _SFT

# rdi - stack pointer (&mut [u8; 256])
# rsi - stack index (u8)
# rdx - return stack pointer (&mut [u8; 256])
# r15 - return stack index (u8)
# r8 - RAM pointer (&mut [u8; 65536])
# r9 - program counter (u16), offset of the next value in RAM
# r10 - VM pointer (&mut Uxn) (loaded from stack in native_entry)
# r11 - Device handle pointer (&DeviceHandle) (loaded from stack in native_entry)
# r12 - Jump table pointer (loaded in native_entry)
# r13-14, rcx, rbx, rax - scratch registers

# 1. Register parameters that are only read are called i and the ones
# that are only written to are called o. Registers that are both read and
# written are named x. Registers that are used as temporary storage are called r.
#
# 2. When there are more than one parameters involved, a number suffix is added.
# e.g. i1, o2
#
# 3. Some macros require the name of the lower byte part of a register which
# may need to be passed in addition to the register. These parameters have the
# suffix b.
# e.g. i1b, o2b
#
# 4. When the word part of the register are needed, the parameter will have the
# suffix w.
# e.g. i1w, o2w
#
# 5. When the lower and upper halfs of the word part are needed, the parameters
# will have the suffix l and h.
# e.g. i1l, i1h
#
# 6. Parameters with constant values are named as c.

# Increment a part of a register
.macro inczx, x, xb 
    inc \xb
    movzx \x, \xb
.endm

# Decrement a part of a register
.macro deczx, x, xb
    dec \xb
    movzx \x, \xb
.endm

# Add iw to a part of x
.macro addzx, x, xw, iw
    add \xw, \iw
    movzx \x, \xw
.endm

# Subtract iw from a part of x
.macro subzx, x, xw, iw
    sub \xw, \iw
    movzx \x, \xw
.endm

# Read byte from RAM at index i onto register ob
.macro mpeek, ob, i
    mov \ob, byte ptr [r8 + \i]
.endm

.macro mpeek_zx, o, i
    movzx \p, byte ptr [r8 + \i]
.endm

# Read byte from RAM at index x onto o1b and from x + 1 onto o2b. x is set with
# the index of the second value.
.macro mpeek2, o1b, o2b, x, xw
    mpeek \o1b, \x
    inczx \x, \xw
    mpeek \o2b, \x
.endm

# Read a short from RAM at index x onto o.
#
# After execution, x is set with the index of the lower byte and r with the
# lower byte.
.macro mpeeks, o, x, xw, r
    mpeek_zx \o, \x
    inczx \x, \xw
    mpeek_zx \r
    shl \o, 8
    or \o, \r
.endm

# Write the value from i2b onto RAM at index present in i1
.macro mpoke, i1, i2b
    mov byte ptr [r8 + \i1], \i2b
.endm

# Write the value from i1b onto RAM at index present in x and value from i2b
# at x + 1. x is set to the index where the second value was written.
.macro mpoke2, x, xw, i1b, i2b
    mpoke \x, \i1b
    inczx \x, \xw
    mpoke \x, \i2b
.endm

# Read current program counter into o, offsetted by ib bytes.
.macro nindex, o, ow, ib
    movsx \ow, ib
    add \ow, r9w
    movzx \o, \ow
.endm

# Move the program counter by the offset iw
.macro nseek, iw
    add r9w, \iw
    movzx r9, r9w
.endm

# Load next instruction and increment program counter
# reg - register to store the read instruction
.macro nread, reg
    mov \reg, byte ptr [r8 + r9]
    inczx r9, r9w
.endm

# Read next byte from PC onto o. The rest of the bits of o are zeroed.
.macro nread_zx, o
    movzx \o byte ptr [r8 + r9]
    inczx r9, r9w
.endm


# Write value in ib to the offset from PC present in iow.
# After execution x contains the index into RAM where the value was written.
.macro npoke, x, xw, ib
    addzx \x, \xw, r9w
    mpoke \x, \ib
.endm

.macro npoke2 x, xw, i1b, i2b
    addzx \x, \xw, r9w
    mpoke \x, \xw, \i1b
    inczx \x, \xw
    mpoke \x, \i2b
.endm

.macro next
    nread_zx rax
    # jump to corresponding instruction handler from the jump table
    mov rbx, [r12 + rax * 8]
    jmp rbx
.endm

# Read index of data stack, ib bytes down from the top onto o.
.macro dindex, o, ob, ib
    mov \ob, sil
    sub \ob, \ib
    movzx \o, \ob
.endm

# Drop the top byte from data stack
.macro ddrop
    deczx rsi, sil
.endm

# Drop the top 2 bytes from the data stack
.macro ddrop2
    subzx rsi, sil, 2
.endm

# Drop the top 3 bytes from the data stack
.macro ddrop3
    subzx rsi, sil, 3
.endm

# Read byte from data stack, at index i, onto ob.
#
# After execution, r contains the index of the value read.
.macro dpeek, ob, i
    mov \ob, byte ptr [rdi + \i]
.endm

# Read byte from data stack, at index i, onto o, with remaining bits zeroed.
.macro dpeek_zx, o, i
    movzx \o, byte ptr [rdi + \i]
.endm

# Read byte from data stack, at index x, onto o1b and from index x - 1, onto o2b.
# x is decremented in the process.
.macro dpeek2 o1b, o2b, x, xb
    dpeek \o1b, \x
    deczx \x, \xb
    dpeek \o2b, \x
.endm 

# Read a short from top of data stack onto o. x is modified to have the index
# of the higher byte.
#
# After execution, r contains the value of the higher byte.
#
# NOTE: We do not read a word directly and use xchg to swap endianness, as we
# may not always have the short to be word aligned.
.macro dpeeks, o, x, xb, r
    dpeek_zx \o, \x
    deczx \x, \xb
    dpeek_zx \r, \x
    shl \r, 8
    or \o, \r
.endm

# Read byte from top of data stack onto ob.
.macro dpeek_top, ob
    mov \ob, byte ptr [rdi + rsi]
.endm

# Read byte from top of data stack onto o, with rest of the bits zeroed.
.macro dpeek_top_zx, o
    movzx \o, byte ptr [rdi + rsi]
.endm

# Read byte from top of data stack onto o and sign extend rest of the bits
.macro dpeek_top_sx, o
    movsx \o, byte ptr [rdi + rsi]
.endm

# Read byte from top of data stack onto o1b and the byte under it onto o2b.
#
# After execution r has the index of the second value read.
.macro dpeek2_top, o1b, o2b, r, rb
    dpeek_top \o1b
    dindex \r, \rb, 1
    dpeek \o2b, \r
.endm

# Read a short from top of data stack onto o.
#
# After execution r2w contains the higher byte and r1 contains its index.
.macro dpeeks_top, o, r1, r1b, r2w
    movzx \r1, sil
    dpeeks \o, \r1, \r1b, \r2w
.endm

# Pop top byte from the data stack onto ob.
.macro dpop ob
    dpeek_top \ob
    ddrop
.endm

# Pop top byte from the data stack onto o and set rest of the higher bits to 0
.macro dpop_zx o
    dpeek_top_zx \o
    ddrop
.endm

# Pop the top byte from data stack onto o1b and the byte under it onto o2b.
.macro dpop2 o1b, o2b
    dpeek_top o1b
    ddrop
    dpeek_top o2b
    ddrop
.endm

# Pop the top short from data stack onto o
.macro dpops o r
    dpop_zx \o
    dpop_zx \r
    shl \r 8
    or \o, \r
.endm


# Replace byte at i1b positions down from the top of the data stack with the
# value from i2b
.macro dpoke i1b, i2b, x, xb
    dindex \x, \xb, i1b
    mov byte ptr [rdi + \x], \i2b
.endm

# Replace top byte of data stack with value from ib
.macro dpoke_top, ib
    mov byte ptr [rdi + r14], \reg
.endm

# Replace top byte of data stack with value from i1b and the byte under the top
# with i2b.
.macro dpoke2_top, i1b, i2b, x, xb
    dpoke_top \i1b
    dpoke 1, \i2b, \x, \xb
.endm

.macro dpokes_top sh, sl, x, xl
    dpoke_top \sl
    xchg \sh, \sl
    dpoke 1, \sl, \x, \xl
.endm

# Push byte from ib onto data stack
.macro dpush, ib
    inczx rsi, sil
    dpoke_top \ib
.endm

# Pushes the value of i1b onto data stack and then i2b
.macro dpush2, i1b, i2b
    dpush i1b
    dpush i2b
.endm

.macro dpushs, s, sl
    dpush \sl
    shr \s, 8
    dpush \sl
.endm

# Read index of return stack ib bytes down from the top onto o.
.macro rindex, o, ob, ib
    mov \ob, r15b
    sub \ob, \ib
    movzx \o, \ob
.endm

# Drop the top byte from the return stack
.macro rdrop
    deczx r15, r15b
.endm

# Drop the top 2 bytes from the return stack
.macro rdrop2
    subzx r15, r15b, 2
.endm

# Drop the top 3 bytes from the return stack
.macro rdrop3
    subzx r15, r15b, 3
.endm

# Read byte from return stack, at index i, onto ob.
#
# After execution, r contains the index of the value read.
.macro rpeek, ob, i
    mov \ob, byte ptr [rdx + \i]
.endm

# Read byte from return stack, at index i, onto o, with remaining bits zeroed.
.macro rpeek_zx, o, i
    movzx \o, byte ptr [rdx + \i]
.endm

# Read byte from return stack, at index x, onto o1b and from index x - 1, onto o2b.
# x is decremented in the process.
.macro rpeek2 o1b, o2b, x, xb
    rpeek \o1b, \x
    deczx \x, \xb
    rpeek \o2b, \x
.endm 

# Read a short from top of return stack onto o. x is modified to have the index
# of the higher byte.
#
# After execution, r contains the value of the higher byte.
#
# NOTE: We do not read a word directly and use xchg to swap endianness, as we
# may not always have the short to be word aligned.
.macro rpeeks, o, x, xb, r
    rpeek_zx \o, \x
    deczx \x, \xb
    rpeek_zx \r, \x
    shl \r, 8
    or \o, \r
.endm

# Read byte from top of return stack onto the ob
.macro rpeek_top, ob.
    mov \ob, byte ptr [rdx + r15]
.endm

# Read byte from top of return stack onto o, with rest of the bits zeroed.
.macro rpeek_top_zx, o
    movzx \o, byte ptr [rdx + r15]
.endm

# Read byte from top of return stack onto o and sign extend rest of the bits
.macro rpeek_top_sx, o
    movsx \o, byte ptr [rdx + r15]
.endm

# Read byte from top of return stack onto  o1b and the byte under it onto o2b.
#
# After execution r has the index of the second value read.
.macro rpeek2_top, o1b, o2b, r, rb
    rpeek_top \o1b
    rindex \r, \rb, 1
    rpeek \o2b, \r
.endm

# Read a short from top of return stack onto ow.
#
# After execution r2w contains the higher byte and r1 contains its index.
.macro rpeeks_top, ow, r1, r1b, r2w
    rpeek_top_zx \ow
    rindex r1, r1b
    rpeek_zx \r2w, \r1
    shl \r2w, 8
    or \ow, \r2w
.endm

# Pop top byte from the return stack onto ob.
.macro rpop ob
    rpeek_top ob
    rdrop
.endm

# Pop the top byte from return stack onto o1b and the byte under it onto o2b.
.macro rpop2 o1b, o2b
    rpeek_top o1b
    rdrop
    rpeek_top o2b
    rdrop
.endm

# Pop the top short from return stack onto ow
.macro rpops ow rw
    rpop_zx \ow
    rpop_zx \rw
    shl \rw 8
    or \ow, \rw
.endm

# Replace byte at n positions down from the top of the return stack with the
# value of the register r.
.macro rpoke n, r, x, xl
    rindex \x, \xl, n
    mov byte ptr [rdx + \x], \r
.endm

# Replace top byte of return stack with reg
# reg - byte sized register
.macro rpoke_top, reg
    mov byte ptr [rdx + r15], \reg
.endm

# Replace top byte of return stack with value from i1b and the byte under the top
# with i2b.
.macro rpoke2_top, i1b, i2b, x, xb
    rpoke_top \i1b
    rpoke 1, \i2b, \x, \xb
.endm

.macro rpokes_top, sh, sl, x, xl
    rpoke_top \sl
    xchg \sh, \sl
    rpoke 1, \sl, \x, \xl
.endm

# Push byte from ib onto return stack
.macro rpush, ib
    inczx r15, r15b
    rpoke_top \ib
.endm

# Pushes the value of i1b onto return stack and then i2b
.macro rpush2, i1b, i2b
    rpush i1b
    rpush i2b
.endm

# Pushes the higher byte of iw and then the lower byte of iw onto return stack.
.macro rpushs, iw, il
    rpush \il
    shr \iw, 8
    dpush \il
.endm

.macro precall
    # We have to write our stack index pointers back into the &mut Uxn
    mov rbx, [rsp]              # read ret index pointer
    mov rax, [rsp + 8]          # read stack index pointer
    mov byte ptr [rax], sil     # save stack index
    mov byte ptr [rbx], r15b    # save ret index

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
    movzx r15, byte ptr [rbx]

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
    movzx r15, byte ptr [rcx] # load ret index

    # Jump into the instruction list
    next

_BRK:
    # Write index values back through index pointers
    pop rbx                  # read ret index pointer
    pop rax                  # read stack index pointer
    mov byte ptr [rax], sil  # save stack index
    mov byte ptr [rbx], r15b # save ret index

    # Restore registers that were saved in stack
    pop rbx
    pop r15
    pop r14
    pop r13
    pop r12

    pop rbp # Restore frame pointer

    mov rax, r9 # return PC from function
    ret

# a -- a+1
_INC:
    inc byte ptr [rdi + rsi]
    next

# a --
_POP:
    ddrop
    next

# a b -- b
_NIP:
    # read b
    dpop al

    # write b
    dpoke_top al

    next

# a b -- b a
_SWP:
    dpeek_top al
    dindex rbx, bl, 1
    xchg al, byte ptr [rdi + rbx]
    dpoke_top al
    next

# a b c -- b c a
_ROT:
    # read c
    dpeek_top al

    # read b, along with writing c
    dindex rbx, bl, 1
    xchg al, byte ptr [rdi + rbx]

    # read a, along with writing b
    deczx rbx, bl
    xchg al, byte ptr [rdi + rbx]

    # write a
    dpoke_top al 

    next

# a -- a a
_DUP:
    dpeek_top al
    dpush al
    next

# a b -- a b a
_OVR:
    index rbx, bl, 1
    dpeek al, rbx       # read a
    dpush al            # write a
    next

# a b -- bool8
_EQU:
    dpop bl         # read b
    dpeek_top al    # read a

    cmp al, bl
    sete al

    dpoke_top al    # write bool8

    next
# a b -- bool8
_NEQ:
    dpop bl         # read b
    dpeek_top al    # read a

    cmp al, bl
    setne al

    dpoke_top al    # write bool8

    next

# a b -- bool8
_GTH:
    dpop bl         # read b
    dpeek_top al    # read a

    cmp al, bl
    setg al

    dpoke_top al    # write bool8

    next

# a b -- bool8
_LTH:
    dpop bl         # read b
    dpeek_top al    # read a

    cmp al, bl
    setl al

    dpoke_top al    # write bool8

    next

# addr8
.macro _jmp
    dpop_zx ax  # read addr8
    nseek ax    # write PC
.endm
_JMP:
    _jmp
    next

# cond8 addr8 --
_JCN:
    dpop_zx bx  # read addr8
    dpop al     # read cond8
    
    test al, al
    jz ____JCN_next

    nseek bx    # write PC

____JCN_next:
    next

.macro _stash_pc
    rpushs r8w, ax, ah, al  # write ret8_1 and ret8_0
.endm
# addr8 -- | ret8_1 ret8_0
_JSR:
    _jmp
    _stash_pc
    next

# a -- | a
_STH:
    dpop al
    rpush al
    next

# addr8 -- value8
_LDZ:
    dpeek_top_zx rax    # read addr8
    mpeek bl, rax       
    dpoke_top bl        # write value8
    next

# val8 addr8 --
_STZ:
    dpop_zx rax     # read addr8
    dpop bl         # read val8

    mpoke rax, bl   # write val8

    next

# addr8 -- value8
_LDR:
    dpeek_top al        # read addr8
    nindex rbx, bx, al
    mpeek al, rbx
    dpoke_top al        # write value8
    next

# val8 addr8 --
_STR:
    dpop al             # read addr8
    nindex rbx, bx, al
    dpop al             # read val8
    mpoke rbx, al       # write val8

    next

# addr8_1 addr8_0 -- val8
_LDA:
    dpops rbx, rax      # read addr8_0 and addr8_1
    mpeek al, rbx
    dpush al            # write val8
    next

# val8 addr8_1 addr8_0 --
_STA:
    dpops rbx, rax  # read addr8_0 and addr8_1
    dpop al         # read val8
    mpoke rbx, al   # write val8
    next

# device8 -- value8
_DEI:
    precall
    call dei_entry
    postcall
    next

# value8 device8 --
_DEO:
    precall
    call deo_entry
    postcall
    next

# a8 b8 -- result8
.macro binary_op, op
    dpop bl         # read b8
    dpeek_top al    # read b8 and a8
    \op al, bl 
    dpoke_top al    # write result8
.endm

# a8 b8 -- a8+b8
_ADD:
    binary_op add
    next

# a8 b8 -- a8-b8
_SUB:
    binary_op sub
    next

# a8 b8 -- a8*b8
_MUL:
    binary_op mul
    next

# a8 b8 -- a8/b8
_DIV:
    binary_op div
    next

# a8 b8 -- a&b
_AND:
    binary_op and
    next

_ORA:
    binary_op or

_EOR:
    binary_op xor

# a8 shift8 -- c8
_SFT:
    dpop al         # read shift8
    dpeek_top bl    # read a8

    # shift right
    mov cl, al
    and cl, 0xf
    shr bl, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bl, cl

    dpoke_top bl    # write c8

    next

# cond8 --
_JCI:
    dpop cl     # read cond8
    test cl, cl 
    jz ____JCI_skip
    _jmi
    jmp ____JCI_next
____JCI_skip:
    nseek 2
____JCI_next:
    next

# a1 a0 -- a1+carry a0+1
_INC2:
    # create a copy of the stack pointer so that we can access the higher byte
    dindex rax, al, 1

    add byte ptr [rdi + rsi], 1 # increment lower byte
    adc byte ptr [rdi + rax], 0 # add the carry to the higher byte

    next

# a1 a0 --
_POP2:
    ddrop2
    next

# a1 a0 b1 b0 -- b1 b0
_NIP2:
    # read b0 and b1
    dpop2 al, bl

    ddrop

    # write b1
    dpoke_top bl

    # write b0
    dpush al

    next

# a1 a0 b1 b0 -- b1 b0 a1 a0
_SWP2:
    # lower byte
    dpeek_top al                    # read b0
    dindex rbx, bl, 2
    xchg al, byte ptr [rdi + rbx]   # xchg with a0
    dpoke_top al                    # write a0

    # higher byte
    deczx rbx, bl
    dpeek al, rbx, bl               # read a1
    addzx rbx, bl, 2
    xchg al, byte ptr [rdi + rbx]   # xchg with b1
    subzx rbx, bl, 2
    mov byte ptr [rdi + rbx], al    # write b1

    next

# a1 a0 b1 b0 c1 c0 -- b1 b0 c1 c0 a1 a0
_ROT2:
    # read c
    dpeek2_top al, bl, rcx, cl

    # read b, along with writing c
    deczx rcx, cl
    xchg al, byte ptr [rdi + rcx]
    deczx rcx, cl
    xchg bl, byte ptr [rdi + rcx]

    # read a, along with writing b
    deczx rcx, cl
    xchg al, byte ptr [rdi + rcx]
    deczx rcx, cl
    xchg bl, byte ptr [rdi + rcx]

    # write a
    dpoke2_top al, bl, rcx, cl

    next

# a1 a0 -- a1 a0 a1 a0
_DUP2:
    dpeek2_top al, bl   # read a0 and a1
    dpush2 bl, al       # push a1 and a0
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 a1 a0
_OVR2:
    dindex rcx, cl, 2
    dpeek2 al, bl, rcx, cl  # read a0 and a1
    dpush2 bl, al           # write a1 and a0
    next

# a1 a0 b1 b0 -- bool8
_EQU2:
    dpops bx    # read b
    dpops ax    # read a

    cmp ax, bx
    sete al

    dpush al    # write bool8

    next

# a1 a0 b1 b0 -- bool8
_NEQ2:
    dpops bx    # read b
    dpops ax    # read a

    cmp ax, bx
    setne al

    dpush al    # write bool8

    next

# a1 a0 b1 b0 -- bool8
_GTH2:
    dpops bx    # read b
    dpops ax    # read a

    cmp ax, bx
    setg al

    dpush al    # write bool8

    next

# a1 a0 b1 b0 -- bool8
_LTH2:
    dpops bx    # read b
    dpops ax    # read a

    cmp ax, bx
    setl al

    dpush al    # write bool8

    next

# addr8_1 addr8_0 --
.macro _jmp2
    dpops r9, rax   # read addr8_1 and addr8_0 and write onto the PC
.endm
_JMP2:
    _jmp2
    next

# cond8 addr8_1 addr8_0 --
_JCN2:
    dpops rax, rbx  # read addr8_0 and addr8_1
    dpop bl         # read cond8

    test bl, bl
    jz ____JCN2_next

    mov r9, rax     # write PC

____JCN2_next:
    next

# addr8_1 addr8_0 -- | ret8_1 ret8_0
_JSR2:
    _jmp2
    _stash_pc
    next

# a1 a0 -- | a1 a0
_STH2:
    dpop2 al, bl    # read a0 and a1
    rpush2 bl, al   # write a1 and a0
    next

# addr8 -- value8_1 value8_0
_LDZ2:
    dpeek_top_zx rax        # read addr8

    mpeek bl, rax, al       
    dpoke_top bl            # write value8_1

    inczx rax, al
    mpeek bl, rax, al       
    dpush bl                # write value8_0

    next

# val8_1 val8_0 addr8 --
_STZ2:
    dpop_zx rax             # read addr8
    dpop2 bl, cl            # read val8_0 and val8_1

    mpoke2 rax, al, cl, bl  # write val8_1 and val8_0

    next

# addr8 -- value8_1 value8_0
_LDR2:
    dpeek_top al        # read addr8
    nindex rbx, bx, al

    mpeek al, rbx
    dpoke_top al        # write value8_1

    inczx rbx, bl
    mpeek al, rbx
    dpush al            # write value8_0
    next

# val8_1 val8_0 addr8 --
_STR2:
    dpop al                 # read addr8
    nindex rcx, cx, al
    dpop2 al, bl            # read val8_0 and val8_1
    mpoke2 rcx, cx, bl, al  # write val8_1 and val8_0

    next

# addr8_1 addr8_0 -- val8_1 val8_0
_LDA2:
    dpops rcx, rax      # read addr8_0 and addr8_1
    mpeek2 al, bl, rcx
    dpush2 al, bl       # write val8_1 and val8_0
    next

# val8_1 val8_0 addr8_1 addr8_0 --
_STA2:
    dpops rcx, rax      # read addr8_0 and addr8_1
    dpop2 al, bl        # read val8
    mpoke2 rcx, bl, al  # write val8
    next

# device8 -- value8_1 value8_0
_DEI2:
    precall
    call dei_2_entry
    postcall
    next

# value8_1 value8_0 device8 --
_DEO2:
    precall
    call deo_2_entry # todo check return value for early exit?
    postcall
    next

# a8_1 a8_0 b8_1 b8_0 -- result8_1 result8_0
.macro binary_op2, op
    dpops bx, cx    # read b8_0 and b8_1
    dpops ax, cl    # read a8_0 and a8_1
    \op ax, bx 
    dpushs ax, al   # write result8_1 and result8_0
.endm

# a8_1 a8_0 b8_1 b8_0 -- sum8_1 sum8_0
_ADD2:
    binary_op2 add
    next

# a8_1 a8_0 b8_1 b8_0 -- diff8_1 diff8_0
_SUB2:
    binary_op2 sub
    next

# a8_1 a8_0 b8_1 b8_0 -- prod8_1 prod8_0
_MUL2:
    binary_op2 mul 
    next

# a8_1 a8_0 b8_1 b8_0 -- quot8_1 quot8_0
_DIV2:
    binary_op2 div
    next

# a8_1 a8_0 b8_1 b8_0 -- and8_1 and8_0
_AND2:
    binary_op2 and
    next

# a8_1 a8_0 b8_1 b8_0 -- or8_1 or8_0
_ORA2:
    binary_op2 or
    next

# a8_1 a8_0 b8_1 b8_0 -- xor8_1 xor8_0
_EOR2:
    binary_op2 xor

# a8_1 a8_0 shift8 -- c8_1 c8_0
_SFT2:
    dpop al          # read shift8
    dpops bx, r13w   # read a8_1 and a8_0

    # shift right
    mov cl, al
    and cl, 0xf
    shr bx, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bx, cl

    dpushs bx, bl   # write c8_1 and c8_0

    next

# --
.macro _jmi
    mov rcx, r9
    mpeeks ax, rcx, cx, bx  # read jump offset
    nseek ax                # update PC
.endm
_JMI:
    _jmi
    next

# a -- a+1
_INCr:
    inc byte ptr [rdx + r15]
    next

# a --
_POPr:
    rdrop
    next

# a b -- b
_NIPr:
    # read b
    rpop al

    # write b
    rpoke_top al

    next

# a b -- b a
_SWPr:
    rpeek_top al
    rindex rbx, bl, 1
    xchg al, byte ptr [rdx + rbx]
    rpoke_top al
    next

# a b c -- b c a
_ROTr:
    # read c
    rpeek_top al

    # read b, along with writing c
    rindex rbx, bl, 1
    xchg al, byte ptr [rdx + rbx]

    # read a, along with writing b
    deczx rbx, bl
    xchg al, byte ptr [rdx + rbx]

    # write a
    rpoke_top al 

    next

# a -- a a
_DUPr:
    rpeek_top al    # read a
    rpush al        # write a
    next

# a b -- a b a
_OVRr:
    rindex rbx, bl, 1
    rpeek al, rbx, bl   # read a
    rpush al            # write a
    next

# a b -- bool8
_EQUr:
    rpop al         # read b
    rpeek_top bl    # read a

    cmp al, bl
    sete al

    rpoke_top al    # write bool8

    next

# a b -- bool8
_NEQr:
    rpop al         # read b
    rpeek_top bl    # read a

    cmp al, bl
    setne al

    rpoke_top al    # write bool8

    next


# a b -- bool8
_GTHr:
    rpop al         # read b
    rpeek_top bl    # read a

    cmp al, bl
    setg al

    rpoke_top al    # write bool8

    next

# a b -- bool8
_LTHr:
    rpop al         # read b
    rpeek_top bl    # read a

    cmp al, bl
    setl al

    rpoke_top al    # write bool8

    next

# addr8
.macro _jmp_r
    rpop_zx ax  # read addr8
    nseek ax    # write PC
.endm
_JMPr:
    _jmp_r
    next

# cond8 addr8 --
_JCNr:
    rpop_zx bx  # read addr8
    rpop al     # read cond8
    
    test al, al
    jz ____JCNr_next

    nseek bx    # write PC

____JCNr_next:
    next

# addr8 -- ret8_1 ret8_0
_JSRr:
    _jmp_r
    _stash_pc
    next

# | a -- a |
_STHr:
    rpop al
    dpush al
    next

# addr8 -- value8
_LDZr:
    rpeek_top_zx rax    # read addr8
    mpeek bl, rax       
    rpoke_top bl        # write value8
    next

# val8 addr8 --
_STZr:
    rpop_zx rax     # read addr8
    rpop bl         # read val8

    mpoke rax, bl   # write val8

    next

# addr8 -- value8
_LDRr:
    rpeek_top al        # read addr8
    nindex rbx, bx, al
    mpeek al, rbx
    rpoke_top al        # write value8
    next

# val8 addr8 --
_STRr:
    rpop al             # read addr8
    nindex rbx, bx, al
    rpop al             # read val8
    mpoke rbx, al       # write val8

    next

# addr8_1 addr8_0 -- val8
_LDAr:
    rpops rbx, rax      # read addr8_0 and addr8_1
    mpeek al, rbx
    rpush al            # write val8
    next

# val8 addr8_1 addr8_0 --
_STAr:
    rpops rbx, rax  # read addr8_0 and addr8_1
    rpop al         # read val8
    mpoke rbx, al   # write val8
    next

# device8 -- value8
_DEIr:
    precall
    call dei_r_entry
    postcall
    next

# value8 device8 --
_DEOr:
    precall
    call deo_r_entry
    postcall
    next

# a8 b8 -- a8+b8
.macro binary_opr, op 
    rpop bl         # read b8
    rpeek_top al    # read b8 and a8
    \op al, bl 
    rpoke_top al    # write a8+b8
.endm

# a8 b8 -- a8+b8
_ADDr:
    binary_opr add
    next

# a8 b8 -- a8-b8
_SUBr:
    binary_opr sub
    next

# a8 b8 -- a8*b8
_MULr:
    binary_opr mul
    next

# a8 b8 -- a8/b8
_DIVr:
    binary_opr div
    next

# a8 b8 -- a8&b8
_ANDr:
    binary_opr and
    next

# a8 b8 -- a8|b8
_ORAr:
    binary_opr or
    next

# a8 b8 -- a^b
_EORr:
    binary_opr xor
    next

# a8 shift8 -- c8
_SFTr:
    rpop al         # read shift8
    rpeek_top bl    # read byte

    # shift right
    mov cl, al
    and cl, 0xf
    shr bl, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bl, cl

    rpoke_top bl    # write c8

    next

# --
_JSI:
    _stash_pc
    _jmi
    next

# a1 a0 -- a1+carry a0+1
_INC2r:
    # create a copy of the stack pointer so that we can access the higher byte
    rindex rax, al, 1

    add byte ptr [rdx + r15], 1 # increment lower byte
    adc byte ptr [rdi + rax], 0 # add the carry to the higher byte

    next

# a1 a0 --
_POP2r:
    rdrop2
    next

# a1 a0 b1 b0 -- b1 b0
_NIP2r:
    # read b0 and b1
    rpop2 al, bl

    rdrop

    # write b1
    rpoke_top bl

    # write b0
    rpush al

    next

# a1 a0 b1 b0 -- b1 b0 a1 a0
_SWP2r:
    # lower byte
    rpeek_top al                    # read b0
    rindex rbx, bl, 2
    xchg al, byte ptr [rdx + rbx]   # xchg with a0
    rpoke_top al                    # write a0

    # higher byte
    deczx rbx, bl
    rpeek al, rbx, bl               # read a1
    addzx rbx, bl, 2
    xchg al, byte ptr [rdx + rbx]   # xchg with b1
    subzx rbx, bl, 2
    mov byte ptr [rdx + rbx], al    # write b1

    next

# a1 a0 b1 b0 c1 c0 -- b1 b0 c1 c0 a1 a0
_ROT2r:
    # read c
    rpeek2_top al, bl, rcx, cl

    # read b, along with writing c
    deczx rcx, cl
    xchg al, byte ptr [rdx + rcx]
    deczx rcx, cl
    xchg bl, byte ptr [rdx + rcx]

    # read a, along with writing b
    deczx rcx, cl
    xchg al, byte ptr [rdx + rcx]
    deczx rcx, cl
    xchg bl, byte ptr [rdx + rcx]

    # write a
    rpoke2_top al, bl, rcx, cl

    next

# a1 a0 -- a1 a0 a1 a0
_DUP2r:
    rpeek2_top al, bl   # read a0 and a1
    rpush2 bl, al       # write a1 and a0
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 a1 a0
_OVR2r:
    rindex rcx, cl, 2
    rpeek2 al, bl, 2, rcx, cl   # read a0 and a1
    rpush2 bl, al               # write a1 and a0
    next

# a1 a0 b1 b0 -- bool8
_EQU2r:
    rpops bx    # read b
    rpops ax    # read a

    cmp ax, bx
    sete al

    rpush al    # write bool8

    next

# a1 a0 b1 b0 -- bool8
_NEQ2r:
    rpops bx    # read b
    rpops ax    # read a

    cmp ax, bx
    setne al

    rpush al    # write bool8

    next

# a1 a0 b1 b0 -- bool8
_GTH2r:
    rpops bx    # read b
    rpops ax    # read a

    cmp ax, bx
    setg al

    rpush al    # write bool8

    next

# a1 a0 b1 b0 -- bool8
_LTH2r:
    rpops bx    # read b
    rpops ax    # read a

    cmp ax, bx
    setl al

    rpush al    # write bool8

    next

# addr8_1 addr8_0 --
.macro _jmp_2r
    rpops r9, rax   # read addr8_1 and addr8_0 and write onto the PC
.endm
_JMP2r:
    _jmp_2r
    next

# cond8 addr8_1 addr8_0 --
_JCN2r:
    rpops rax, rbx  # read addr8_0 and addr8_1
    rpop bl         # read cond8

    test bl, bl
    jz ____JCN2r_next

    mov r9, rax     # write PC

____JCN2r_next:
    next

# addr8_1 addr8_0 -- ret8_1 ret8_0
_JSR2r:
    _jmp_2r
    _stash_pc
    next

# | a1 a0 -- a1 a0 |
_STH2r:
    rpop2 al, bl    # read a0 and a1
    dpush2 bl, al   # write a1 and a0
    next

# addr8 -- value8_1 value8_0
_LDZ2r:
    rpeek_top_zx rax        # read addr8

    mpeek bl, rax, al       
    rpoke_top bl            # write value8_1

    inczx rax, al
    mpeek bl, rax, al       
    rpush bl                # write value8_0

    next

# val8_1 val8_0 addr --
_STZ2r:
    rpop_zx rax             # read addr8
    rpop2 bl, cl            # read val8_0 and val8_1

    mpoke2 rax, al, cl, bl  # write val8_1 and val8_0

    next

# addr8 -- value8_1 value8_0
_LDR2r:
    rpeek_top al        # read addr8
    nindex rbx, bx, al

    mpeek al, rbx
    rpoke_top al        # write value8_1

    inczx rbx, bl
    mpeek al, rbx
    rpush al            # write value8_0
    next

# val8_1 val8_0 addr8
_STR2r:
    dpop al                 # read addr8
    nindex rcx, cx, al
    dpop2 al, bl            # read val8_0 and val8_1
    mpoke2 rcx, cx, bl, al  # write val8_1 and val8_0

    next

# addr8_1 addr8_0 -- val8_1 val8_0
_LDA2r:
    rpops rcx, rax      # read addr8_0 and addr8_1
    mpeek2 al, bl, rcx
    rpush2 al, bl       # write val8
    next

# val8_1 val8_0 addr8_1 addr8_0 --
_STA2r:
    rpops rcx, rax      # read addr8_0 and addr8_1
    rpop2 al, bl        # read val8
    mpoke2 rcx, bl, al  # write val8
    next

# device8 -- value8_1 value8_0
_DEI2r:
    precall
    call dei_2r_entry
    postcall
    next

# value8_1 value8_0 device8 --
_DEO2r:
    precall
    call deo_2r_entry
    postcall
    next

# a8_1 a8_0 b8_1 b8_0 -- result8_1 result8_0
.macro binary_op2r, op 
    rpops bx, cx    # read b8_0 and b8_1
    rpops ax, cx    # read a8_0 and a8_1
    \op ax, bx 
    rpushs ax, al   # write result8_1 and result8_0
.endm

# a8_1 a8_0 b8_1 b8_0 -- sum8_1 sum8_0
_ADD2r:
    binary_op2r add
    next

# a8_1 a8_0 b8_1 b8_0 -- diff8_1 diff8_0
_SUB2r:
    binary_op2r
    next

# a8_1 a8_0 b8_1 b8_0 -- prod8_1 prod8_0
_MUL2r:
    binary_op2r mul
    next

# a8_1 a8_0 b8_1 b8_0 -- quot8_1 quot8_0
_DIV2r:
    binary_op2r div
    next

# a8_1 a8_0 b8_1 b8_0 -- and8_1 and8_0
_AND2r:
    binary_op2r and
    next

# a8_1 a8_0 b8_1 b8_0 -- or8_1 or8_0
_ORA2r:
    binary_op2r or
    next

# a8_1 a8_0 b8_1 b8_0 -- xor8_1 xor8_0
_EOR2r:
    binary_op2r xor 
    next

# a8_1 a8_0 shift8 -- c8_1 c8_0
_SFT2r:
    rpop al          # read shift8
    rpops bx, r13w   # read a8_1 and a8_0

    # shift right
    mov cl, al
    and cl, 0xf
    shr bx, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bx, cl

    rpushs bx, bl   # write c8_1 and c8_0

    next

_LIT:
    nread al
    dpush al
    next

# a -- a a+1
_INCk:
    dpeek_top al
    inc al
    dpush al
    next

# a -- a
_POPk:
    next

# a b -- a b b
_NIPk:
    dpeek_top al
    dpush al
    next

# a b -- a b b a
_SWPk:
    dindex rbx, bl, 1
    dpeek_top al            # read b
    dpush al                # write b

    dpeek al, rbx, bl       # read a
    dpush al                # write a
    next

# a b c -- a b c b c a
_ROTk:
    dindex rbx, bl, 1

    dpeek al, rbx      # read b
    dpush al           # write b

    inczx rbx, bl

    dpeek al, rbx      # read c
    dpush al           # write c

    subzx rbx, bl, 2

    dpeek al, rbx       # read a
    dpush al            # write a

    next

# a -- a a a
_DUPk:
    dpeek_top al    # read a
    dpush2 al       # write a and a

# a b -- a b a b a
_OVRk:
    dpeek2_top al, bl, rcx, cl  # read b and a
    dpush2 bl, al               # write a and b

    dpush bl                    # write a
    
    next

# a b -- a b bool8
_EQUk:
    dpeek2_top bl, al   # read b and a

    cmp al, bl
    sete al

    dpush al            # write bool8

    next

# a b -- a b bool8
_NEQk:
    dpeek2_top bl, al   # read b and a

    cmp al, bl
    setne al

    dpush al            # write bool8

    next

# a b -- a b bool8
_GTHk:
    dpeek2_top bl, al   # read b and a

    cmp al, bl
    setg al

    dpush al            # write bool8

    next

# a b -- a b bool8
_LTHk:
    dpeek2_top bl, al   # read b and a

    cmp al, bl
    setl al

    dpush al            # write bool8

    next

# addr8 -- addr8
.macro _jmp_k
    dpeek_top_zx ax # read addr8
    nseek ax        # write PC
.endm
_JMPk:
    _jmp_k
    next

# cond8 addr8 -- cond8 addr8
_JCNk:
    dpeek_top_zx bx     # read addr8
    dindex rcx, cl, 1
    dpeek al, rcx, cl   # read cond8
    
    test al, al
    jz ____JCNk_next

    nseek bx    # write PC

____JCNk_next:
    next

# addr8 -- addr8 | ret8_1 ret8_0
_JSRk:
    _jmp_k
    _stash_pc
    next

# a -- a | a
_STHk:
    dpeek_top al    # read a
    rpush al        # write a
    next

# addr8 -- addr8 value8
_LDZk:
    dpeek_top_zx rax    # read addr8
    mpeek bl, rax       
    dpush bl            # write value8
    next

# val8 addr8 -- val8 addr8
_STZk:
    dpeek_top_zx rax    # read addr8

    dindex rcx, cl, 1
    dpeek bl, rcx, cl   # read val8

    mpoke rax, bl       # write val8

    next

# addr8 -- addr8 value8
_LDRk:
    dpeek_top al        # read addr8
    nindex rbx, bx, al
    mpeek al, rbx
    dpush al            # write value8
    next

# val8 addr8 -- val8 addr8
_STRk:
    dpeek_top al        # read addr8
    nindex rbx, bx, al

    dindex rcx, cl, 1
    dpeek al, rcx, cl   # read val8
    mpoke rbx, al       # write val8

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8
_LDAk:
    dpeeks_top rbx, rax, al, cx # read addr8_0 and addr8_1
    mpeek al, rbx
    dpush al                    # write val8
    next

# val8 addr8_1 addr8_0 -- val8 addr8_1 addr8_0
_STAk:
    dpeeks_top rbx, rcx, cl, ax     # read addr8_0 and addr8_1
    deczx rcx, cl
    dpeek al, rcx                   # read val8
    mpoke rbx, al                   # write val8
    next

# device8 -- device8 value8
_DEIk:
    precall
    call dei_k_entry
    postcall
    next

# value8 device8 -- value8 device8
_DEOk:
    precall
    call deo_k_entry
    postcall
    next

# a8 b8 -- a8 b8 result8
.macro binary_opk, op 
    dpeek2_top bl, al, rcx, cl  # read b8 and a8
    \op al, bl 
    dpush al                    # write result8
.endm

# a8 b8 -- a8 b8 a8+b8
_ADDk:
    binary_opk add
    next

# a8 b8 -- a8 b8 a8-b8
_SUBk:
    binary_opk sub
    next

# a8 b8 --- a8 b8 a8*b8
_MULk:
    binary_opk mul 
    next

# a8 b8 -- a8 b8 a8*b8
_DIVk:
    binary_opk div 
    next

# a8 b8 -- a8 b8 a8&b8
_ANDk:
    binary_opk and
    next

# a8 b8 -- a8 b8 a8|b8
_ORAk:
    binary_opk or
    next

# a8 b8 -- a8 b8 a8^b8
_EORk:
    binary_opk xor
    next

# a8 shift8 -- a8 shift8 c8
_SFTk:
    dpeek2_top al, bl, rcx, cl  # read shift8 and a8

    # shift right
    mov cl, al
    and cl, 0xf
    shr bl, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bl, cl

    dpoke_top bl    # write c8

    next

_LIT2:
    # higher byte
    nread al
    dpush al

    # lower byte
    nread al
    dpush al

    next

# a1 a0 -- a1 a0 a1+carry a0+1
_INC2k:
    dpeek2_top al, bl, rcx, cl

    add al, 1   # increment lower byte
    addc bl, 0  # add the carry to the higher byte

    dpush2 bl, al

    next

# a1 a0 -- a1 a0
_POP2k:
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 b1 b0
_NIP2k:
    dpeek2_top al, bl   # read b0 and b1
    dpush2 bl, al       # write b1 and b0
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 b1 b0 a1 a0
_SWP2k:
    dpeek2_top al, bl, rcx, cl  # read b0 and b1
    dpush2 bl, al               # write b1 and b0
    deczx rcx, cl
    dpeek2 al, bl, rcx, cl      # read a0 and a1
    dpush2 bl, al               # write a1 and a0
    next

# a1 a0 b1 b0 c1 c0 -- a1 a0 b1 b0 c1 c0 b1 b0 c1 c0 a1 a0
_ROT2k:
    dindex rcx, cl, 2
    dpeek2 al, bl, rcx, cl      # read b0 and b1
    dpush2 bl, al               # write b1 and b0

    addzx rcx, cl, 3
    dpeek2 al, bl, rcx          # read c0 and c1
    dpush2 bl, al               # write c1 and c0

    deczx rcx, cl, 3
    dpeek2 al, bl               # read a0 and a1
    dpush2 bl, al               # write a1 and a0

    next

# a1 a0 -- a1 a0 a1 a0 a1 a0
_DUP2k:
    dpeek2_top al, bl, rcx, cl  # read a0 and a1
    dpush2 bl, al               # write a1 and a0
    dpush2 bl, al               # write a1 and a0

    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 a1 a0 b1 b0 a1 a0
_OVR2k:
    dpeek2_top r13b, r14b, rcx, cl  # read b0 and b1

    deczx rcx, cl
    dpeek2 al, bl, rcx, cl  # read a0 and a1

    dpush2 bl, al           # write a1 and a0
    dpush2 r13b, r14b       # write b1 and b0
    dpush2 bl, al           # write a1 and a0
    
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_EQU2k:
    dpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    dpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    sete al

    dpush al                        # write bool8
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_NEQ2k:
    dpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    dpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    setne al

    dpush al                        # write bool8
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_GTH2k:
    dpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    dpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    setg al

    dpush al                        # write bool8
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_LTH2k:
    dpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    dpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    setl al

    dpush al                        # write bool8
    next

# addr8_1 addr8_0 -- addr8_1 addr8_0
.macro _jmp_2k
    dpeeks r9, rax, al, rbx
.endm
_JMP2k:
    _jmp_2k
    next

# cond8 addr8_1 addr8_0 -- cond8 addr8_1 addr8_0
_JCN2k:
    dpeeks_top rax, rcx, cl, rbxa   # read addr8_0 and addr8_1
    dpeek bl, rcx, cl               # read cond8

    test bl, bl
    jz ____JCN2k_next

    mov r9, rax     # write PC

____JCN2k_next:
    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 | ret8_1 ret8_0
_JSR2k:
    _jmp_2k
    _stash_pc
    next

# a1 a0 -- a1 a0 | a1 a0
_STH2k:
    dpeek2 al, bl, rcx, cl  # read a0 and a1
    rpush2 bl, al           # write a1 and a0
    next

# addr8 -- addr8 value8_1 value8_0
_LDZ2k:
    dpeek_top_zx rax    # read addr8

    mpeek bl, rax
    dpush bl            # write value8_1

    inczx rax, al
    mpeek bl, rax
    dpush bl            # write value8_0

    next

# val8_1 val8_0 addr8 -- val8_1 val8_0 addr8
_STZ2k:
    dpeek_top_zx rax            # read addr8

    dindex rcx, cl, 2
    dpeek bl, rcx, cl           # read val8_1
    mpoke rax, bl               # write val8_1

    inczx rax, al
    inczx rcx, cl
    dpeek bl, rcx, cl           # read val8_0
    mpoke rax bl                # write val8_0

    next

# addr8 -- addr8 value8_1 value8_0
_LDR2k:
    dpeek_top al        # read addr8
    nindex rbx, bx, al

    mpeek al, rbx
    dpush al            # write value8_1

    inczx rbx, bl
    mpeek al, rbx
    dpush al            # write value8_0
    next

# val8_1 val8_0 addr8 -- val8_1 val8_0 addr8
_STR2k:
    dpeek_top al        # read addr8
    nindex rbx, bx, al

    dindex rcx, cl, 2
    dpeek al, rcx, cl   # read val8_1
    mpoke rbx, al       # write val8_1

    inczx rcx, cl
    inczx rbx, bx
    dpeek al, rcx, cl   # read val8_0
    mpoke rbx, al       # write val8_0

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8_1 val8_0
_LDA2k:
    dpeeks_top rcx, rax, al, r13w   # read addr8_0 and addr8_1
    mpeek2 al, bl, rcx
    dpush2 al, bl                   # write val8_1 and val8_0
    next

# val8_1 val8_0 addr8_1 addr8_0 -- va8_1 val8_0 addr8_1 addr8_0
_STA2k:
    dpeeks_top rcx, r13, r13b, ax   # read addr8_0 and addr8_1
    deczx r13, r13b
    dpeek2 al, bl, r13              # read val8
    mpoke2 rcx, bl, al              # write val8
    next

# device8 -- device8 value8_1 value8_0
_DEI2k:
    precall
    call dei_2k_entry
    postcall
    next

# value8_1 value8_0 device8 -- value8_1 value8_0 device8
_DEO2k:
    precall
    call deo_2k_entry
    postcall
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 result8_1 result8_0
.macro binary_op2k, op
    dpeeks_top bx, rcx, cl, r13w    # read b8_0 and b8_1
    deczx rcx, cl
    dpeeks ax, rcx, cl, r13w        # read a8_0
    \op ax, bx 
    dpushs ax, al                   # write sum8_1 and sum8_0
.endm

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 sum8_1 sum8_0
_ADD2k:
    binary_op2k add
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 diff8_1 diff8_0
_SUB2k:
    binary_op2k sub
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 prod8_1 prod8_0
_MUL2k:
    binary_op2k mul
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 quot8_1 quot8_0
_DIV2k:
    binary_op2k div
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 and8_1 and8_0
_AND2k:
    binary_op2k and
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 or8_1 or8_0
_ORA2k:
    binary_op2k or
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 xor8_1 xor8_0
_EOR2k:
    binary_op2k xor
    next

# a8_1 a8_0 shift8 -- a8_1 a8_0 shift8 c8_1 c8_0
_SFT2k:
    dpeek_top al          # read shift8
    dindex r13, r13b, 1
    dpeeks bx, r13, r13b, r14b   # read a8_1 and a8_0

    # shift right
    mov cl, al
    and cl, 0xf
    shr bx, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bx, cl

    dpushs bx, bl   # write c8_1 and c8_0

    next

_LITr:
    nread al
    rpush al
    next

# a -- a a+1
_INCkr:
    rpeek_top al
    inc al
    rpush al
    next

# a -- a
_POPkr:
    next

# a b -- a b b
_NIPkr:
    rpeek al    # read b
    rpush al    # write b
    next

# a b -- a b b a
_SWPkr:
    dindex rbx, bl, 1
    rpeek_top al        # read b
    rpush al            # write b
    rpeek al, rbx, bl   # read a
    rpush al            # write a
    next

# a b c -- a b c b c a
_ROTkr:
    rindex rbx, bl, 1

    rpeek al, rbx      # read b
    rpush al           # write b

    inczx rbx, bl

    rpeek al, rbx      # read c
    rpush al           # write c

    subzx rbx, bl, 2

    rpeek al, rbx       # read a
    rpush al            # write a

    next

# a -- a a a
_DUPkr:
    rpeek_top al    # read a
    rpush2 al        # write a and a

# a b -- a b a b a
_OVRkr:
    rpeek2_top al, bl, rcx, cl  # read b and a
    rpush2 bl, al               # write a and b

    rpush bl                    # write a
    
    next

# a b -- a b bool8
_EQUkr:
    rpeek2_top bl, al   # read b and a

    cmp al, bl
    sete al

    rpush al            # write bool8

    next

# a b -- a b bool8
_NEQkr:
    rpeek2_top bl, al   # read b and a

    cmp al, bl
    setne al

    rpush al            # write bool8

    next

# a b -- a b bool8
_GTHkr:
    rpeek2_top bl, al   # read b and a

    cmp al, bl
    setg al

    rpush al            # write bool8

    next

# a b -- a b bool8
_LTHkr:
    rpeek2_top bl, al   # read b and a

    cmp al, bl
    setl al

    rpush al            # write bool8

    next

# addr8 -- addr8
.macro _jmp_kr
    rpeek_top_zx ax # read addr8
    nseek ax        # write PC
.endm
_JMPkr:
    _jmp_kr
    next

# cond8 addr8 -- cond8 addr8
_JCNkr:
    rpeek_top_zx bx     # read addr8
    rindex rcx, cl, 1
    rpeek al, rcx, cl   # read cond8
    
    test al, al
    jz ____JCNkr_next

    nseek bx    # write PC

____JCNkr_next:
    next

# addr8 -- addr8 ret8_1 ret8_0
_JSRkr:
    _jmp_kr
    _stash_pc
    next

# | a -- a | a
_STHkr:
    rpeek_top al    # read a
    dpush al        # write a
    next

# addr8 -- addr8 value8
_LDZkr:
    rpeek_top_zx rax    # read addr8
    mpeek bl, rax
    rpush bl            # write value8
    next

# val8 addr8 -- val8 addr8
_STZkr:
    rpeek_top_zx rax    # read addr8

    rindex rcx, cl, 1
    rpeek bl, rcx, cl   # read val8

    mpoke rax, bl       # write val8

    next

# addr8 -- addr8 value8
_LDRkr:
    rpeek_top al        # read addr8
    nindex rbx, bx, al
    mpeek al, rbx
    rpush al            # write value8
    next

# val8 addr8 -- val8 addr8
_STRkr:
    rpeek_top al        # read addr8
    nindex rbx, bx, al

    rindex rcx, cl, 1
    rpeek al, rcx, cl   # read val8
    mpoke rbx, al       # write val8

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8
_LDAkr:
    rpeeks_top rbx, rax, al, cx # read addr8_0 and addr8_1
    mpeek al, rbx
    rpush al                    # write val8
    next

# val8 addr8_1 addr8_0 -- val8 addr8_1 addr8_0
_STAkr:
    rpeeks_top rbx, rcx, cl, ax     # read addr8_0 and addr8_1
    deczx rcx, cl
    rpeek al, rcx                   # read val8
    mpoke rbx, al                   # write val8
    next

# device8 -- device8 value8
_DEIkr:
    precall
    call dei_kr_entry
    postcall
    next

# value8 device8 -- value8 device8
_DEOkr:
    precall
    call deo_kr_entry
    postcall
    next

# a8 b8 -- a8 b8 result8
.macro binary_opkr, op
    rpeek2_top bl, al, rcx, cl  # read b8 and a8
    \op al, bl 
    rpush al                    # write result8
.endm

# a8 b8 -- a8 b8 a8+b8
_ADDkr:
    binary_opkr add
    next

# a8 b8 -- a8 b8 a8-b8
_SUBkr:
    binary_opkr sub
    next

# a8 b8 -- a8 b8 a8*b8
_MULkr:
    binary_opkr mul
    next

# a8 b8 -- a8 b8 a8/b8
_DIVkr:
    binary_opkr div
    next

# a8 b8 -- a8 b8 a8&b8
_ANDkr:
    binary_opkr and 
    next

# a8 b8 -- a8 b8 a8|b8
_ORAkr:
    binary_opkr or 
    next

# a8 b8 -- a8 b8 a8^b8
_EORkr:
    binary_opkr xor 
    next

# a8 shift8 -- a8 shift8 c8
_SFTkr:
    rpeek2_top al, bl, rcx, cl  # read shift8 and a8

    # shift right
    mov cl, al
    and cl, 0xf
    shr bl, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bl, cl

    rpoke_top bl    # write c8

    next
_LIT2r:
    # higher byte
    nread al
    rpush al
    
    # lower byte
    nread al
    rpush al
    
    next

# a1 a0 -- a1 a0 a1+carry a0+1
_INC2kr:
    # read short
    rpeek2_top al, bl, rcx, cl

    add al, 1   # increment lower byte
    addc bl, 0  # add the carry to the higher byte

    rpush2 bl, al

    next

# a1 a0 -- a1 a0
_POP2kr:
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 b1 b0
_NIP2kr:
    rpeek2_top al, bl, rcx, cl  # read b0 and b1
    rpush2 bl, al               # push b1 and b0
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 b1 b0 a1 a0
_SWP2kr:
    rpeek2_top al, bl, rcx, cl  # read b0 and b1
    rpush2 bl, al               # write b1 and b0

    deczx rcx, cl
    rpeek2 al, bl, rcx, cl  # read a0 and a1
    rpush2 bl, al           # write a1 and a0
    next

# a1 a0 b1 b0 c1 c0 -- a1 a0 b1 b0 c1 c0 b1 b0 c1 c0 a1 a0
_ROT2kr:
    rindex rcx, cl, 2
    rpeek2 al, bl, rcx, cl      # read b0 and b1
    rpush2 bl, al               # write b1 and b0

    addzx rcx, cl, 3
    rpeek2 al, bl, rcx          # read c0 and c1
    rpush2 bl, al               # write c1 and c0

    deczx rcx, cl, 3
    rpeek2 al, bl               # read a0 and a1
    rpush2 bl, al               # write a1 and a0

    next

# a1 a0 -- a1 a0 a1 a0 a1 a0
_DUP2kr:
    rpeek2_top al, bl, rcx, cl  # read a0 and a1
    rpush2 bl, al               # write a1 and a0
    rpush2 bl, al               # write a1 and a0

    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 a1 a0 b1 b0 a1 a0
_OVR2kr:
    rpeek2_top r13b, r14b, rcx, cl  # read b0 and b1

    deczx rcx, cl
    rpeek2 al, bl, rcx, cl  # read a0 and a1

    rpush2 bl, al           # write a1 and a0
    rpush2 r13b, r14b       # write b1 and b0
    rpush2 bl, al           # write a1 and a0
    
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_EQU2kr:
    rpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    rpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    sete al

    rpush al                        # write bool8
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_NEQ2kr:
    rpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    rpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    setne al

    rpush al                        # write bool8
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_GTH2kr:
    rpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    rpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    setg al

    rpush al                        # write bool8
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_LTH2kr:
    rpeeks_top ax, rcx, cl, r13b    # read b0 and b1
    deczx rcx, cl
    rpeeks bx, rcx, cl, r13b        # read a0 and a1

    cmp ax, bx
    setl al

    rpush al                        # write bool8
    next

# addr8_1 addr8_0 -- addr8_1 addr8_0
.macro _jmp_2kr
    rpeeks r9, rax, al, rbx
.endm
_JMP2kr:
    _jmp_2kr
    next

# cond8 addr8_1 addr8_0 -- cond8 addr8_1 addr8_0
_JCN2kr:
    rpeeks_top rax, rcx, cl, rbxa   # read addr8_0 and addr8_1
    rpeek bl, rcx, cl               # read cond8

    test bl, bl
    jz ____JCN2kr_next

    mov r9, rax     # write PC

____JCN2kr_next:
    next

# addr8_1 addr8_0 | addr8_1 addr8_0 ret8_1 ret8_0
_JSR2kr:
    _jmp_2kr
    _stash_pc
    next

# | a1 a0 -- a1 a0 | a1 a0
_STH2kr:
    rpeek2 al, bl, rcx, cl  # read a0 and a1
    dpush2 bl, al           # write a1 and a0
    next

# addr8 -- addr8 value8_1 value8_0
_LDZ2kr:
    rpeek_top_zx rax    # read addr8

    mpeek bl, rax
    rpush bl            # write value8_1

    inczx rax, al
    mpeek bl, rax
    rpush bl            # write value8_0

    next

# val8_1 val8_0 addr8 -- val8_1 val8_0 addr8
_STZ2kr:
    dpeek_top_zx rax            # read addr8

    dindex rcx, cl, 2
    dpeek bl, rcx, cl           # read val8_1
    mpoke rax, bl               # write val8_1

    inczx rax, al
    inczx rcx, cl
    dpeek bl, rcx, cl           # read val8_0
    mpoke rax bl                # write val8_0

    next

# addr8 -- addr8 value8_1 value8_0
_LDR2kr:
    rpeek_top al        # read addr8
    nindex rbx, bx, al

    mpeek al, rbx
    rpush al            # write value8_1

    inczx rbx, bl
    mpeek al, rbx
    rpush al            # write value8_0
    next

# val8_1 val8_0 addr8 -- val8_1 val8_0 addr8
_STR2kr:
    rpeek_top al        # read addr8
    nindex rbx, bx, al

    rindex rcx, cl, 2
    rpeek al, rcx, cl   # read val8_1
    mpoke rbx, al       # write val8_1

    inczx rcx, cl
    inczx rbx, bx
    rpeek al, rcx, cl   # read val8_0
    mpoke rbx, al       # write val8_0

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8_1 val8_0
_LDA2kr:
    rpeeks_top rcx, rax, al, r13w   # read addr8_0 and addr8_1
    mpeek2 al, bl, rcx
    rpush2 al, bl                   # write val8_1 and val8_0
    next

# val8_1 val8_0 addr8_1 addr8_0 -- va8_1 val8_0 addr8_1 addr8_0
_STA2kr:
    rpeeks_top rcx, r13, r13b, ax   # read addr8_0 and addr8_1
    deczx r13, r13b
    rpeek2 al, bl, r13              # read val8
    mpoke2 rcx, bl, al              # write val8
    next

# device8 -- device8 value8_1 value8_0
_DEI2kr:
    precall
    call dei_2kr_entry
    postcall
    next

# value8_1 value8_0 device8 -- value8_1 value8_0 device8
_DEO2kr:
    precall
    call deo_2kr_entry
    postcall
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 result8_1 result8_0
.macro binary_op2kr, op 
    rpeeks_top bx, rcx, cl, r13w  # read b8_0 and b8_1
    deczx rcx, cl
    rpeeks ax, rcx, cl, r13w      # read a8_0 and a8_1
    \op ax, bx 
    rpushs ax, al                 # write sum8_1 and sum8_0
.endm

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 sum8_1 sum8_0
_ADD2kr:
    binary_op2kr add
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 diff8_1 diff8_0
_SUB2kr:
    binary_op2kr sub
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 prod8_1 prod8_0
_MUL2kr:
    binary_op2kr mul
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 quot8_1 quot8_0
_DIV2kr:
    binary_op2kr div 
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 and8_1 and8_0
_AND2kr:
    binary_op2kr and
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 or8_1 or8_0
_ORA2kr:
    binary_op2kr or
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 xor8_1 xor8_0
_EOR2kr:
    binary_op2kr xor
    next

# a8_1 a8_0 shift8 -- a8_1 a8_0 shift8 c8_1 c8_0
_SFT2kr:
    rpeek_top al          # read shift8
    rindex r13, r13b, 1
    rpeeks bx, r13, r13b, r14b   # read a8_1 and a8_0

    # shift right
    mov cl, al
    and cl, 0xf
    shr bx, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bx, cl

    rpushs bx, bl   # write c8_1 and c8_0

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
