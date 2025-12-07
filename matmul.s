	.file	"matmul.c"
	.option nopic
	.attribute arch, "rv32i2p1"
	.attribute unaligned_access, 0
	.attribute stack_align, 16
	.text
	.align	2
	.align	4
	.globl	_start
	.type	_start, @function
_start:
	li	a7,700
	li	a6,400
	li	a5,600
	li	a4,300
	li	a3,100
	li	a2,800
	li	a1,200
	li	a0,1500
	call	distance_product_2x2_naked
	xor	a5,a5,a3
	xor	a5,a5,a4
	xor	a0,a5,a0
	call	ecall
	.size	_start, .-_start
	.align	2
	.align	4
	.globl	distance_product_2x2_naked
	.type	distance_product_2x2_naked, @function
distance_product_2x2_naked:
	add	t1,a1,a6
	add	t3,a0,a4
	bleu	t3,t1,.L2
	mv	t3,t1
.L2:
	add	a1,a1,a7
	add	t1,a0,a5
	bleu	t1,a1,.L3
	mv	t1,a1
.L3:
	add	a6,a6,a3
	add	a4,a4,a2
	bleu	a4,a6,.L4
	mv	a4,a6
.L4:
	add	a7,a7,a3
	add	a5,a5,a2
	bleu	a5,a7,.L5
	mv	a5,a7
.L5:
 #APP
# 48 "hw/matmul.c" 1
	mv a0, t3
	mv a1, t1
	mv a2, a4
	mv a3, a5
	ret
# 0 "" 2
 #NO_APP
	.size	distance_product_2x2_naked, .-distance_product_2x2_naked
	.align	2
	.align	4
	.globl	ecall
	.type	ecall, @function
ecall:
 #APP
# 62 "hw/matmul.c" 1
	li a5, 93
mv a0, a0
ecall

# 0 "" 2
 #NO_APP
	.size	ecall, .-ecall
	.ident	"GCC: (g1b306039ac) 15.1.0"
	.section	.note.GNU-stack,"",@progbits
