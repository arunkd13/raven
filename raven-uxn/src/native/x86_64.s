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

# 1. Register parameters that are only read, are named i and the ones
# that are only written to are named o. Registers that are both read and
# written are named x. Registers that are used as temporary storage are called r.
# 
# 2. When there are more than one parameters involved, a number suffix is added.
# e.g. i1, o2
#
# 3. Some macros require the name of additional components of a register to be
# passed which are passed in addition to the main register.
# 
# The lower byte part of a register is named with suffix b.
# e.g. i1b, o2b
#
# The lower word part of a register is named with suffix w.
# e.g. i1w, o2w
#
# The lower and upper halfs of the word part are  named with the suffix l and h.
# e.g. i1l, i1h
#
# When the macro works with any part of a register, the parameter is named with
# the suffix p.
# e.g. ip, o1p
#
# When the macro works with either the full register or a part, the parameter is
# named with the suffix _.
# e.g. i1_, x2_
#
# Parameters with constant values are named as c.
#
# 4. The input and output for the macros are documented and the side effects on
# x and r are noted separately.

# Increment a part of a register and zero out rest of the bits
.macro inczx, x, xp 
    inc \xp
    movzx \x, \xp
.endm

# Decrement a part of a register and zero out rest of the bits.
.macro deczx, x, xp
    dec \xp
    movzx \x, \xp
.endm

# Add iw to a part of x.
.macro addzx, x, xp, ip
    add \xp, \ip
    movzx \x, \xp
.endm

# Subtract iw from a part of x
.macro subzx, x, xp, ip
    sub \xp, \ip
    movzx \x, \xp
.endm

# Read byte from RAM at index i onto register ob
.macro mpeek, ob, i
    mov \ob, byte ptr [r8 + \i]
.endm

# Read byte from RAM at index i onto register o with rest of the bits zeroed.
.macro mpeek_zx, o_, i
    movzx \o_, byte ptr [r8 + \i]
.endm

# Read byte from RAM at index x onto o1b and from x + 1 onto o2b.
#, \x
# NOTE: x is set with the index of the second value.
.macro mpeek2, o1b, o2b, x, xw
    mpeek \o1b, \x
    inczx \x, \xw
    mpeek \o2b, \x
.endm

# Read a short from RAM at index x onto o_. Rest of the bits of o_ are zeroed
# out.
#
# NOTE: x is set with the index of the lower byte and r_ with the value of the
# lower byte.
.macro mpeeks, o_, x, xw, r_
    mpeek_zx \o_, \x
    inczx \x, \xw
    mpeek_zx \r_, \x
    shl \o_, 8
    or \o_, \r_
.endm

# Write the value from i2b onto RAM at index present in i1
.macro mpoke, i1, i2b
    mov byte ptr [r8 + \i1], \i2b
.endm

# Write the value from i1b onto RAM at index present in x and value from i2b
# at x + 1.
#
# NOTE: x is set to the index where the second value was written.
.macro mpoke2, x, xw, i1b, i2b
    mpoke \x, \i1b
    inczx \x, \xw
    mpoke \x, \i2b
.endm

# Read current program counter into o, offsetted by ib bytes.
.macro nindex, o, ow, ib
    movsx \ow, \ib
    add \ow, r9w
    movzx \o, \ow
.endm

# Move the program counter by the offset iw.
.macro nseek, iw
    addzx r9, r9w, \iw
.endm

# Move the program counter to the next instruction.
.macro nnext
    inczx r9, r9w
.endm

# Load instruction and increment program counter
.macro nread, ob
    mov \ob, byte ptr [r8 + r9]
    nnext
.endm

# Read next byte from PC onto o_. The rest of the bits of o are zeroed.
.macro nread_zx, o_
    movzx \o_, byte ptr [r8 + r9]
    nnext
.endm

.macro nreads, o_, r_
    nread_zx \o_
    shl \o_, 8
    nread_zx \r_
    or \o_, \r_
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

# Read byte from data stack, at index i, onto ob.
#
# NOTE:, r contains the index of the value read.
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

# Read a short, x bytes from the top of data stack onto o. x is modified to have
# the index of the higher byte.
#
# NOTE: r contains the value of the higher byte.
# We do not read a word directly and use xchg to swap endianness, as we
# may not always have the short to be word aligned.
.macro dpeeks, o_, x, xb, r_
    dpeek_zx \o_, \x
    deczx \x, \xb
    dpeek_zx \r_, \x
    shl \r_, 8
    or \o_, \r_
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
# NOTE: r has the index of the second value read.
.macro dpeek2_top, o1b, o2b, r, rb
    dpeek_top \o1b
    dindex \r, \rb, 1
    dpeek \o2b, \r
.endm

# Read a short from top of data stack onto o.
#
# NOTE: r2w contains the higher byte and r1 contains its index.
.macro dpeeks_top, o_, r1, r1b, r2_
    mov \r1, rsi
    dpeeks \o_, \r1, \r1b, \r2_
.endm

# Pop top byte from the data stack onto ob.
.macro dpop, ob
    dpeek_top \ob
    ddrop
.endm

# Pop top byte from the data stack onto o and set rest of the higher bits to 0
.macro dpop_zx, o
    dpeek_top_zx \o
    ddrop
.endm

# Pop top byte from the data stack onto o and sign extend to the higher bits.
.macro dpop_sx, o
    dpeek_top_sx \o
    ddrop
.endm

# Pop the top byte from data stack onto o1b and the byte under it onto o2b.
.macro dpop2, o1b, o2b
    dpeek_top \o1b
    ddrop
    dpeek_top \o2b
    ddrop
.endm

# Pop the top short from data stack onto o
.macro dpops, o_, r_
    dpop_zx \o_
    dpop_zx \r_
    shl \r_, 8
    or \o_, \r_
.endm

# Replace byte at i1b positions down from the top of the data stack with the
# value from i2b
.macro dpoke, i1b, i2b, x, xb
    dindex \x, \xb, \i1b
    mov byte ptr [rdi + \x], \i2b
.endm

# Replace top byte of data stack with value from ib
.macro dpoke_top, ib
    mov byte ptr [rdi + rsi], \ib
.endm

# Replace top byte of data stack with value from i1b and the byte under the top
# with i2b.
.macro dpoke2_top, i1b, i2b, x, xb
    dpoke_top \i1b
    dpoke 1, \i2b, \x, \xb
.endm

# Push byte from ib onto data stack
.macro dpush, ib
    inczx rsi, sil
    dpoke_top \ib
.endm

# Pushes the value of i1b onto data stack and then i2b
.macro dpush2, i1b, i2b
    dpush \i1b
    dpush \i2b
.endm

# Pushes the higher byte of xw and then the lower byte of xw onto data stack.
.macro dpushs, xw, xb, rb
    mov \rb, \xb
    shr \xw, 8
    dpush \xb
    dpush \rb
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

# Read byte from return stack, at index i, onto ob.
#
# NOTE:, r contains the index of the value read.
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

# Read a short, x bytes down from the top of the return stack, onto o_. x is
# modified to have the index of the higher byte.
#
# NOTE: r_ contains the value of the higher byte. We do not read a word directly
# and use xchg to swap endianness, as we may not always have the short to be
# word aligned.
.macro rpeeks, o_, x, xb, r_
    rpeek_zx \o_, \x
    deczx \x, \xb
    rpeek_zx \r_, \x
    shl \r_, 8
    or \o_, \r_
.endm

# Read byte from top of return stack onto the ob
.macro rpeek_top, ob
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
# NOTE: r has the index of the second value read.
.macro rpeek2_top, o1b, o2b, r, rb
    rpeek_top \o1b
    rindex \r, \rb, 1
    rpeek \o2b, \r
.endm

# Read a short from top of return stack onto o.
#
# NOTE: r2w contains the higher byte and r1 contains its index.
.macro rpeeks_top, o_, r1, r1b, r2_
    mov \r1, r15
    rpeeks \o_, \r1, \r1b, \r2_
.endm

# Pop top byte from the return stack onto ob.
.macro rpop ob
    rpeek_top \ob
    rdrop
.endm

# Pop top byte from the return stack onto o and set rest of the higher bits to 0
.macro rpop_zx, o
    rpeek_top_zx \o
    rdrop
.endm

# Pop top byte from the return stack onto o and sign extend to the higher bits.
.macro rpop_sx, o
    rpeek_top_sx \o
    rdrop
.endm

# Pop the top byte from return stack onto o1b and the byte under it onto o2b.
.macro rpop2 o1b, o2b
    rpeek_top \o1b
    rdrop
    rpeek_top \o2b
    rdrop
.endm

# Pop the top short from return stack onto o
.macro rpops, o_, r_
    rpop_zx \o_
    rpop_zx \r_
    shl \r_, 8
    or \o_, \r_
.endm

# Replace byte at i1b positions down from the top of the return stack with the
# value from i2b
.macro rpoke, i1b, i2b, x, xb
    rindex \x, \xb, \i1b
    mov byte ptr [rdi + \x], \i2b
.endm

# Replace top byte of data stack with value from ib
.macro rpoke_top, ib
    mov byte ptr [rdx + r15], \ib
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
    rpush \i1b
    rpush \i2b
.endm

# Pushes the higher byte of xw and then the lower byte of iw onto return stack.
#
# Note: After execution, xw contains only the lower byte.
.macro rpushs, xw, xb, rb
    mov \rb, \xb
    shr \xw, 8
    rpush \xb
    rpush \rb
.endm

.macro precall
    # We have to write our stack index pointers back into the &mut Uxn
    mov rbx, [rsp]              # read return stack index pointer
    mov rax, [rsp + 8]          # read data stack index pointer
    mov byte ptr [rax], sil     # save data stack index
    mov byte ptr [rbx], r15b    # save return stack index

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

    mov rax, r9 # return PC from function

    pop rbp # Restore frame pointer
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
    dindex rbx, bl, 1
    dpeek al, rbx       # read a
    dpush al            # write a
    next

# a b -- bool8
.macro cmp_op, op
    dpop bl         # read b
    dpeek_top al    # read a

    cmp al, bl
    \op al

    dpoke_top al    # write bool8
.endm
_EQU:
    cmp_op sete
    next
# a b -- bool8
_NEQ:
    cmp_op setne
    next

# a b -- bool8
_GTH:
    cmp_op seta
    next

# a b -- bool8
_LTH:
    cmp_op setb
    next

# addr8
.macro _jmp
    dpop_sx ax  # read addr8
    nseek ax    # write PC
.endm
_JMP:
    _jmp
    next

# cond8 addr8 --
_JCN:
    dpop_sx bx  # read addr8
    dpop al     # read cond8
    
    test al, al
    jz ____JCN_next

    nseek bx    # write PC

____JCN_next:
    next

.macro _stash_pc
    mov ax, r9w
    rpushs ax, al, bl   # write ret8_1 and ret8_0
.endm

# addr8 -- | ret8_1 ret8_0
_JSR:
    _stash_pc
    _jmp
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
    dpeek_top al    # read a8
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
    dpop bl         # read b8
    dpeek_top al    # read a8
    mul bl 
    dpoke_top al    # write result8
    next

# a8 b8 -- a8/b8
_DIV:
    dpop bl         # read b8
    test bl, bl
    jz ____DIV_by_zero
    dpeek_top al    # read a8
    div bl 
    jmp ____DIV_write_result
____DIV_by_zero:
    mov al, bl
____DIV_write_result:
    dpoke_top al    # write result8
    next

# a8 b8 -- a&b
_AND:
    binary_op and
    next

_ORA:
    binary_op or
    next

_EOR:
    binary_op xor
    next

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

.macro _jmi
    nreads ax, bx           # read jump offset
    nseek ax                # update PC
.endm

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
    dpeek al, rbx                   # read a1
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
    dpeek2_top al, bl, rcx, cl  # read a0 and a1
    dpush2 bl, al               # push a1 and a0
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 a1 a0
_OVR2:
    dindex rcx, cl, 2
    dpeek2 al, bl, rcx, cl  # read a0 and a1
    dpush2 bl, al           # write a1 and a0
    next

# a1 a0 b1 b0 -- bool8
.macro cmp_op2, op
    dpops bx, cx    # read b
    dpops ax, cx    # read a

    cmp ax, bx
    \op al

    dpush al    # write bool8
.endm
_EQU2:
    cmp_op2 sete
    next

# a1 a0 b1 b0 -- bool8
_NEQ2:
    cmp_op2 setne
    next

# a1 a0 b1 b0 -- bool8
_GTH2:
    cmp_op2 seta
    next

# a1 a0 b1 b0 -- bool8
_LTH2:
    cmp_op2 setb
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
    _stash_pc
    _jmp2
    next

# a1 a0 -- | a1 a0
_STH2:
    dpop2 al, bl    # read a0 and a1
    rpush2 bl, al   # write a1 and a0
    next

# addr8 -- value8_1 value8_0
_LDZ2:
    dpeek_top_zx rax        # read addr8

    mpeek bl, rax       
    dpoke_top bl            # write value8_1

    inczx rax, al
    mpeek bl, rax       
    dpush bl                # write value8_0

    next

# val8_1 val8_0 addr8 --
_STZ2:
    dpop_zx rax             # read addr8
    dpop2 bl, cl            # read val8_0 and val8_1

    mpoke2 rax, ax, cl, bl  # write val8_1 and val8_0

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
    mpeek2 al, bl, rcx, cx
    dpush2 al, bl       # write val8_1 and val8_0
    next

# val8_1 val8_0 addr8_1 addr8_0 --
_STA2:
    dpops rcx, rax          # read addr8_0 and addr8_1
    dpop2 al, bl            # read val8_0 and val8_1
    mpoke2 rcx, cx, bl, al  # write val8_1 and val8_0
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
    dpops ax, cx    # read a8_0 and a8_1
    \op ax, bx 
    dpushs ax, al, bl   # write result8_1 and result8_0
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
    dpops bx, cx    # read b8_0 and b8_1
    dpops ax, cx    # read a8_0 and a8_1
    mul bx 
    dpushs ax, al, bl   # write result8_1 and result8_0
    next

# a8_1 a8_0 b8_1 b8_0 -- quot8_1 quot8_0
_DIV2:
    dpops bx, cx    # read b8_0 and b8_1
    test bx, bx
    jz ____DIV2_by_zero
    dpops ax, cx    # read a8_0 and a8_1
    mov r14, rdx
    xor dx, dx
    div bx 
    mov rdx, r14
    jmp ____DIV2_write_result
____DIV2_by_zero:
    ddrop2
    mov ax, bx
____DIV2_write_result:
    dpushs ax, al, bl   # write result8_1 and result8_0
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
    next

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

    dpushs bx, bl, al   # write c9_1 and c8_0

    next

# --
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
    rpeek al, rbx       # read a
    rpush al            # write a
    next

# a b -- bool8
.macro cmp_opr, op
    rpop bl         # read b
    rpeek_top al    # read a

    cmp al, bl
    \op al

    rpoke_top al    # write bool8
.endm
_EQUr:
    cmp_opr sete
    next

# a b -- bool8
_NEQr:
    cmp_opr setne
    next


# a b -- bool8
_GTHr:
    cmp_opr seta
    next

# a b -- bool8
_LTHr:
    cmp_opr setb
    next

# addr8
.macro _jmp_r
    rpop_sx ax  # read addr8
    nseek ax    # write PC
.endm
_JMPr:
    _jmp_r
    next

# cond8 addr8 --
_JCNr:
    rpop_sx bx  # read addr8
    rpop al     # read cond8
    
    test al, al
    jz ____JCNr_next

    nseek bx    # write PC

____JCNr_next:
    next

# addr8 -- ret8_1 ret8_0
_JSRr:
    mov r14w, r9w
    _jmp_r
    rpushs r14w, r14b, al   # write ret8_1 and ret8_0
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

# a8 b8 -- result8
.macro binary_opr, op 
    rpop bl         # read b8
    rpeek_top al    # read b8 and a8
    \op al, bl 
    rpoke_top al    # write result8
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
    rpop bl         # read b8
    rpeek_top al    # read b8 and a8
    mul bl 
    rpoke_top al    # write result8
    next

# a8 b8 -- a8/b8
_DIVr:
    rpop bl         # read b8
    test bl, bl
    jz ____DIVr_by_zero
    rpeek_top al    # read b8 and a8
    div bl 
    jmp ____DIVr_write_result
____DIVr_by_zero:
    mov al, bl
____DIVr_write_result:
    rpoke_top al    # write result8
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
    mov ax, r9w
    add ax, 2
    rpushs ax, al, bl
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
    rpeek al, rbx                   # read a1
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
    rpeek2_top al, bl, rcx, cl  # read a0 and a1
    rpush2 bl, al               # write a1 and a0
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 a1 a0
_OVR2r:
    rindex rcx, cl, 2
    rpeek2 al, bl, rcx, cl   # read a0 and a1
    rpush2 bl, al               # write a1 and a0
    next

# a1 a0 b1 b0 -- bool8
.macro cmp_op2r, op
    rpops bx, cx    # read b
    rpops ax, cx    # read a

    cmp ax, bx
    \op al

    rpush al    # write bool8
.endm
_EQU2r:
    cmp_op2r sete
    next

# a1 a0 b1 b0 -- bool8
_NEQ2r:
    cmp_op2r setne
    next

# a1 a0 b1 b0 -- bool8
_GTH2r:
    cmp_op2r seta
    next

# a1 a0 b1 b0 -- bool8
_LTH2r:
    cmp_op2r setb
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
    mov r14w, r9w
    _jmp_2r
    rpushs r14w, r14b, al   # write ret8_1 and ret8_0
    next

# | a1 a0 -- a1 a0 |
_STH2r:
    rpop2 al, bl    # read a0 and a1
    dpush2 bl, al   # write a1 and a0
    next

# addr8 -- value8_1 value8_0
_LDZ2r:
    rpeek_top_zx rax        # read addr8

    mpeek bl, rax       
    rpoke_top bl            # write value8_1

    inczx rax, al
    mpeek bl, rax       
    rpush bl                # write value8_0

    next

# val8_1 val8_0 addr --
_STZ2r:
    rpop_zx rax             # read addr8
    rpop2 bl, cl            # read val8_0 and val8_1

    mpoke2 rax, ax, cl, bl  # write val8_1 and val8_0

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
    mpeek2 al, bl, rcx, cx
    rpush2 al, bl       # write val8
    next

# val8_1 val8_0 addr8_1 addr8_0 --
_STA2r:
    rpops rcx, rax          # read addr8_0 and addr8_1
    rpop2 al, bl            # read val8_0 and val8_1
    mpoke2 rcx, cx, bl, al  # write val8_1 and val8_0
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
    rpushs ax, al, bl   # write result8_1 and result8_0
.endm

# a8_1 a8_0 b8_1 b8_0 -- sum8_1 sum8_0
_ADD2r:
    binary_op2r add
    next

# a8_1 a8_0 b8_1 b8_0 -- diff8_1 diff8_0
_SUB2r:
    binary_op2r sub
    next

# a8_1 a8_0 b8_1 b8_0 -- prod8_1 prod8_0
_MUL2r:
    rpops bx, cx    # read b8_0 and b8_1
    rpops ax, cx    # read a8_0 and a8_1
    div bx 
    rpushs ax, al, bl   # write result8_1 and result8_0
    next

# a8_1 a8_0 b8_1 b8_0 -- quot8_1 quot8_0
_DIV2r:
    rpops bx, cx    # read b8_0 and b8_1
    test bx, bx
    jz ____DIV2r_by_zero
    rpops ax, cx    # read a8_0 and a8_1
    mov r14, rdx
    xor dx, dx
    div bx 
    mov rdx, r14
    jmp ____DIV2r_write_result
____DIV2r_by_zero:
    rdrop2
    mov ax, bx
____DIV2r_write_result:
    rpushs ax, al, bl   # write result8_1 and result8_0
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

    rpushs bx, bl, al   # write c8_1 and c8_0

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

    dpeek al, rbx           # read a
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
    dpush2 al, al       # write a and a
    next

# a b -- a b a b a
_OVRk:
    dpeek2_top al, bl, rcx, cl  # read b and a
    dpush2 bl, al               # write a and b

    dpush bl                    # write a
    
    next

# a b -- a b bool8
.macro cmp_opk, op
    dpeek2_top bl, al, rcx, cl   # read b and a

    cmp al, bl
    \op al

    dpush al            # write bool8
.endm
_EQUk:
    cmp_opk sete
    next

# a b -- a b bool8
_NEQk:
    cmp_opk setne
    next

# a b -- a b bool8
_GTHk:
    cmp_opk seta
    next

# a b -- a b bool8
_LTHk:
    cmp_opk setb
    next

# addr8 -- addr8
.macro _jmp_k
    dpeek_top_sx ax # read addr8
    nseek ax        # write PC
.endm
_JMPk:
    _jmp_k
    next

# cond8 addr8 -- cond8 addr8
_JCNk:
    dpeek_top_sx bx     # read addr8
    dindex rcx, cl, 1
    dpeek al, rcx       # read cond8
    
    test al, al
    jz ____JCNk_next

    nseek bx            # write PC

____JCNk_next:
    next

# addr8 -- addr8 | ret8_1 ret8_0
_JSRk:
    _stash_pc
    _jmp_k
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
    dpeek bl, rcx   # read val8

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
    dpeek al, rcx       # read val8
    mpoke rbx, al       # write val8

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8
_LDAk:
    dpeeks_top rbx, rax, al, rcx    # read addr8_0 and addr8_1
    mpeek al, rbx
    dpush al                        # write val8
    next

# val8 addr8_1 addr8_0 -- val8 addr8_1 addr8_0
_STAk:
    dpeeks_top rbx, rcx, cl, rax    # read addr8_0 and addr8_1
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
    dpeek2_top bl, al, rcx, cl  # read b8 and a8
    mul bl 
    dpush al                    # write result8
    next

# a8 b8 -- a8 b8 a8*b8
_DIVk:
    dpeek_top bl                # read b8
    test bl, bl
    jz ____DIVk_by_zero
    dindex rcx, cl, 1
    dpeek al, rcx               # read a8
    div bl 
    jmp ____DIVk_write_result
____DIVk_by_zero:
    mov al, bl
____DIVk_write_result:
    dpush al                    # write result8
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

    dpush bl    # write c8

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
    adc bl, 0  # add the carry to the higher byte

    dpush2 bl, al

    next

# a1 a0 -- a1 a0
_POP2k:
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 b1 b0
_NIP2k:
    dpeek2_top al, bl, rcx, cl  # read b0 and b1
    dpush2 bl, al               # write b1 and b0
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
    dpeek2 al, bl, rcx, cl      # read c0 and c1
    dpush2 bl, al               # write c1 and c0

    subzx rcx, cl, 3
    dpeek2 al, bl, rcx, cl      # read a0 and a1
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
    dpush2 r14b, r13b       # write b1 and b0
    dpush2 bl, al           # write a1 and a0
    
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
.macro cmp_op2k, op
    dpeeks_top bx, rcx, cl, r13w    # read b0 and b1
    deczx rcx, cl
    dpeeks ax, rcx, cl, r13w        # read a0 and a1

    cmp ax, bx
    \op al

    dpush al                        # write bool8
.endm
_EQU2k:
    cmp_op2k sete
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_NEQ2k:
    cmp_op2k setne
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_GTH2k:
    cmp_op2k seta
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_LTH2k:
    cmp_op2k setb
    next

# addr8_1 addr8_0 -- addr8_1 addr8_0
.macro _jmp_2k
    dpeeks_top r9, rax, al, rbx
.endm
_JMP2k:
    _jmp_2k
    next

# cond8 addr8_1 addr8_0 -- cond8 addr8_1 addr8_0
_JCN2k:
    dpeeks_top rax, rcx, cl, rbx   # read addr8_0 and addr8_1
    deczx rcx, cl
    dpeek bl, rcx                   # read cond8

    test bl, bl
    jz ____JCN2k_next

    mov r9, rax     # write PC

____JCN2k_next:
    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 | ret8_1 ret8_0
_JSR2k:
    _stash_pc
    _jmp_2k
    next

# a1 a0 -- a1 a0 | a1 a0
_STH2k:
    dpeek2_top al, bl, rcx, cl  # read a0 and a1
    rpush2 bl, al               # write a1 and a0
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
    dpeek bl, rcx           # read val8_1
    mpoke rax, bl               # write val8_1

    inczx rax, al
    inczx rcx, cl
    dpeek bl, rcx           # read val8_0
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
    dpeek al, rcx       # read val8_1
    mpoke rbx, al       # write val8_1

    inczx rcx, cl
    inczx rbx, bx
    dpeek al, rcx       # read val8_0
    mpoke rbx, al       # write val8_0

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8_1 val8_0
_LDA2k:
    dpeeks_top rcx, rax, al, r13    # read addr8_0 and addr8_1
    mpeek2 al, bl, rcx, cx
    dpush2 al, bl                   # write val8_1 and val8_0
    next

# val8_1 val8_0 addr8_1 addr8_0 -- va8_1 val8_0 addr8_1 addr8_0
_STA2k:
    dpeeks_top rcx, r13, r13b, rax  # read addr8_0 and addr8_1
    deczx r13, r13b
    dpeek2 al, bl, r13, r13b        # read val8_0 and val8_1
    mpoke2 rcx, cx, bl, al          # write val8_1 and val8_0
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
    dpushs ax, al, bl                   # write sum8_1 and sum8_0
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
    dpeeks_top bx, rcx, cl, r13w    # read b8_0 and b8_1
    deczx rcx, cl
    dpeeks ax, rcx, cl, r13w        # read a8_0
    mul bx 
    dpushs ax, al, bl                   # write sum8_1 and sum8_0
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 quot8_1 quot8_0
_DIV2k:
    dpeeks_top bx, rcx, cl, r13w    # read b8_0 and b8_1
    test bx, bx
    jz ____DIV2k_by_zero
    deczx rcx, cl
    dpeeks ax, rcx, cl, r13w        # read a8_0
    mov r14, rdx
    xor dx, dx
    div bx 
    mov rdx, r14
    jmp ____DIV2k_write_result
____DIV2k_by_zero:
    mov ax, bx
____DIV2k_write_result:
    dpushs ax, al, bl                   # write sum8_1 and sum8_0
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
    dpeeks bx, r13, r13b, r14w   # read a8_1 and a8_0

    # shift right
    mov cl, al
    and cl, 0xf
    shr bx, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bx, cl

    dpushs bx, bl, al   # write c8_1 and c8_0

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
    rpeek_top al    # read b
    rpush al        # write b
    next

# a b -- a b b a
_SWPkr:
    rindex rbx, bl, 1
    rpeek_top al        # read b
    rpush al            # write b
    rpeek al, rbx       # read a
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
    rpush2 al, al   # write a and a
    next

# a b -- a b a b a
_OVRkr:
    rpeek2_top al, bl, rcx, cl  # read b and a
    rpush2 bl, al               # write a and b

    rpush bl                    # write a
    
    next

# a b -- a b bool8
.macro cmp_opkr, op
    rpeek2_top bl, al, rcx, cl   # read b and a

    cmp al, bl
    \op al

    rpush al            # write bool8
.endm
_EQUkr:
    cmp_opkr sete
    next

# a b -- a b bool8
_NEQkr:
    cmp_opkr setne
    next

# a b -- a b bool8
_GTHkr:
    cmp_opkr seta
    next

# a b -- a b bool8
_LTHkr:
    cmp_opkr setb
    next

# addr8 -- addr8
.macro _jmp_kr
    rpeek_top_sx ax # read addr8
    nseek ax        # write PC
.endm
_JMPkr:
    _jmp_kr
    next

# cond8 addr8 -- cond8 addr8
_JCNkr:
    rpeek_top_sx bx     # read addr8
    rindex rcx, cl, 1
    rpeek al, rcx       # read cond8
    
    test al, al
    jz ____JCNkr_next

    nseek bx    # write PC

____JCNkr_next:
    next

# | addr8 -- ret8_1 ret8_0 | addr8
_JSRkr:
    mov r14w, r9w
    _jmp_kr
    dpushs r14w, r14b, al   # write ret8_1 and ret8_0
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
    rpeek bl, rcx   # read val8

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
    rpeek al, rcx       # read val8
    mpoke rbx, al       # write val8

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8
_LDAkr:
    rpeeks_top rbx, rax, al, rcx    # read addr8_0 and addr8_1
    mpeek al, rbx
    rpush al                        # write val8
    next

# val8 addr8_1 addr8_0 -- val8 addr8_1 addr8_0
_STAkr:
    rpeeks_top rbx, rcx, cl, rax    # read addr8_0 and addr8_1
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
    rpeek2_top bl, al, rcx, cl  # read b8 and a8
    mul bl 
    rpush al                    # write result8
    next

# a8 b8 -- a8 b8 a8/b8
_DIVkr:
    rpeek_top bl                # read b8
    test bl, bl
    jz ____DIVkr_by_zero
    rindex rcx, cl, 1
    rpeek al, rcx               # read a8
    div bl 
    jmp ____DIVkr_write_result
____DIVkr_by_zero:
    mov al, bl
____DIVkr_write_result:
    rpush al                    # write result8
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

    rpush bl    # write c8

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
    adc bl, 0  # add the carry to the higher byte

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
    rpeek2 al, bl, rcx, cl      # read c0 and c1
    rpush2 bl, al               # write c1 and c0

    subzx rcx, cl, 3
    rpeek2 al, bl, rcx, cl      # read a0 and a1
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
    rpush2 r14b, r13b       # write b1 and b0
    rpush2 bl, al           # write a1 and a0
    
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
.macro cmp_op2kr, op
    rpeeks_top bx, rcx, cl, r13w    # read b0 and b1
    deczx rcx, cl
    rpeeks ax, rcx, cl, r13w        # read a0 and a1

    cmp ax, bx
    \op al

    rpush al                        # write bool8
.endm
_EQU2kr:
    cmp_op2kr sete
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_NEQ2kr:
    cmp_op2kr setne
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_GTH2kr:
    cmp_op2kr seta
    next

# a1 a0 b1 b0 -- a1 a0 b1 b0 bool8
_LTH2kr:
    cmp_op2kr setb
    next

# addr8_1 addr8_0 -- addr8_1 addr8_0
.macro _jmp_2kr
    rpeeks_top r9, rax, al, rbx
.endm
_JMP2kr:
    _jmp_2kr
    next

# cond8 addr8_1 addr8_0 -- cond8 addr8_1 addr8_0
_JCN2kr:
    rpeeks_top rax, rcx, cl, rbx    # read addr8_0 and addr8_1
    deczx rcx, cl
    rpeek bl, rcx                   # read cond8

    test bl, bl
    jz ____JCN2kr_next

    mov r9, rax     # write PC

____JCN2kr_next:
    next

# | addr8_1 addr8_0 -- ret8_1 ret8_0 | addr8_1 addr8_0 
_JSR2kr:
    mov r14w, r9w
    _jmp_2kr
    dpushs r14w, r14b, al   # write ret8_1 and ret8_0
    next

# | a1 a0 -- a1 a0 | a1 a0
_STH2kr:
    rpeek2_top al, bl, rcx, cl  # read a0 and a1
    dpush2 bl, al               # write a1 and a0
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
    rpeek_top_zx rax            # read addr8

    rindex rcx, cl, 2
    rpeek bl, rcx           # read val8_1
    mpoke rax, bl               # write val8_1

    inczx rax, al
    inczx rcx, cl
    rpeek bl, rcx           # read val8_0
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
    rpeek al, rcx       # read val8_1
    mpoke rbx, al       # write val8_1

    inczx rcx, cl
    inczx rbx, bx
    rpeek al, rcx       # read val8_0
    mpoke rbx, al       # write val8_0

    next

# addr8_1 addr8_0 -- addr8_1 addr8_0 val8_1 val8_0
_LDA2kr:
    rpeeks_top rcx, rax, al, r13    # read addr8_0 and addr8_1
    mpeek2 al, bl, rcx, cx
    rpush2 al, bl                   # write val8_1 and val8_0
    next

# val8_1 val8_0 addr8_1 addr8_0 -- va8_1 val8_0 addr8_1 addr8_0
_STA2kr:
    rpeeks_top rcx, r13, r13b, rax  # read addr8_0 and addr8_1
    deczx r13, r13b
    rpeek2 al, bl, r13, r13b        # read val8
    mpoke2 rcx, cx, bl, al              # write val8
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
    rpushs ax, al, bl                 # write sum8_1 and sum8_0
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
    rpeeks_top bx, rcx, cl, r13w  # read b8_0 and b8_1
    deczx rcx, cl
    rpeeks ax, rcx, cl, r13w      # read a8_0 and a8_1
    mov r14, rdx
    mul bx 
    mov rdx, r14
    rpushs ax, al, bl                 # write sum8_1 and sum8_0
    next

# a8_1 a8_0 b8_1 b8_0 -- a8_1 a8_0 b8_1 b8_0 quot8_1 quot8_0
_DIV2kr:
    rpeeks_top bx, rcx, cl, r13w  # read b8_0 and b8_1
    test bx, bx
    jz ____DIV2kr_by_zero
    deczx rcx, cl
    rpeeks ax, rcx, cl, r13w      # read a8_0 and a8_1
    mov r14, rdx
    xor dx, dx
    div bx 
    mov rdx, r14
    jmp ____DIV2kr_write_result
____DIV2kr_by_zero:
    mov ax, bx
____DIV2kr_write_result:
    rpushs ax, al, bl             # write sum8_1 and sum8_0
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
    rpeeks bx, r13, r13w, r14w   # read a8_1 and a8_0

    # shift right
    mov cl, al
    and cl, 0xf
    shr bx, cl

    # shift left
    mov cl, al
    shr cl, 4
    shl bx, cl

    rpushs bx, bl, al   # write c8_1 and c8_0

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
