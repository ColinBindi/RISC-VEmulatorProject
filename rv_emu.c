#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include<inttypes.h>

#include "rv_emu.h"

void unsupported(char *s, uint32_t val) {
    printf("%s: %d\n", s, val);
    exit(-1);
}

void rv_init(struct rv_state *rsp, uint32_t *func,
             uint64_t a0, uint64_t a1, uint64_t a2, uint64_t a3) {
    int i;

    // Zero out registers
    for (i = 0; i < NREGS; i += 1) {
        rsp->regs[i] = 0;
    }

    // Zero out the stack
    for (i = 0; i < STACK_SIZE; i += 1) {
        rsp->stack[i] = 0;
    }

    // Initialize the Program Counter
    rsp->pc = (uint64_t) func;

    // Initialize the Link Register to a sentinel value
    rsp->regs[RA] = 0;

    // Initialize Stack Pointer to the logical bottom of the stack
    rsp->regs[SP] = (uint64_t) &rsp->stack[STACK_SIZE];

    // Initialize the first 4 arguments in emulated r0-r3
    rsp->regs[A0] = a0;
    rsp->regs[A1] = a1;
    rsp->regs[A2] = a2;
    rsp->regs[A3] = a3;

    memset(&rsp->analysis, 0, sizeof(struct rv_analysis_st));
    cache_init(&rsp->i_cache);  
}

static void print_pct(char *fmt, int numer, int denom) {
    double pct = 0.0;

    if (denom) {
        pct = (double) numer / (double) denom * 100.0;
    }
    printf(fmt, numer, pct);
}

void rv_print(struct rv_analysis_st *a) {
    int b_total = a->b_taken + a->b_not_taken;

    printf("=== Analysis\n");
    print_pct("Instructions Executed  = %d\n", a->i_count, a->i_count);
    print_pct("R-type + I-type        = %d (%.2f%%)\n", a->ir_count, a->i_count);
    print_pct("Loads                  = %d (%.2f%%)\n", a->ld_count, a->i_count);
    print_pct("Stores                 = %d (%.2f%%)\n", a->st_count, a->i_count);
    print_pct("Jumps/JAL/JALR         = %d (%.2f%%)\n", a->j_count, a->i_count);
    print_pct("Conditional branches   = %d (%.2f%%)\n", b_total, a->i_count);
    print_pct("  Branches taken       = %d (%.2f%%)\n", a->b_taken, b_total);
    print_pct("  Branches not taken   = %d (%.2f%%)\n", a->b_not_taken, b_total);
}

void emu_r_type(struct rv_state *rsp, uint32_t iw) {
    // Shift data to the right and 0 out the rest
    uint32_t rd = (iw >> 7) & 0b11111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t rs2 = (iw >> 20) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;
    uint32_t funct7 = (iw >> 25) & 0b1111111;

    if (funct3 == 0b000) {
        if (funct7 == 0b0000000) { // ADD
            rsp->regs[rd] = rsp->regs[rs1] + rsp->regs[rs2];
        } else if (funct7 == 0b0000001) { // MUL
            rsp->regs[rd] = rsp->regs[rs1] * rsp->regs[rs2];
        } else if (funct7 == 0b0100000) { // SUB
            rsp->regs[rd] = rsp->regs[rs1] - rsp->regs[rs2];
        }
    } else if (funct3 == 0b001 && funct7 == 0b0000000) { // SLL
        rsp->regs[rd] = rsp->regs[rs1] << rsp->regs[rs2];
    } else if (funct3 == 0b101 && funct7 == 0b0000000) { // SRL
        rsp->regs[rd] = rsp->regs[rs1] >> rsp->regs[rs2];
    } else if (funct3 == 0b111 && funct7 == 0b0000000) { // AND
        rsp->regs[rd] = rsp->regs[rs1] & rsp->regs[rs2];
    } else if (funct3 == 0b100 && funct7 == 0b0000001) { // DIV
        rsp->regs[rd] = rsp->regs[rs1] / rsp->regs[rs2];
    } else {
        unsupported("R-type funct3", funct3);
    }

    rsp->pc += 4; // Next instruction
    rsp->analysis.ir_count++; // Increment # of I-type and R-type instructions executed
}

void emu_i_type(struct rv_state *rsp, uint32_t iw) {
    // Shift data to the right and 0 out the rest
    uint32_t rd = (iw >> 7) & 0b11111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;
    int64_t imm = (iw >> 20) & 0b111111111111;

    // 64 - 12 = 52
    imm = ((int64_t)(imm << 52) >> 52); // 64 bit sign-extension
    int64_t shamt = (iw >> 20) & 0b111111; // 6 bytes used and not 5

    if (funct3 == 0b000) { // ADDI
        rsp->regs[rd] = rsp->regs[rs1] + imm;
    } else if (funct3 == 0b101) { // SRLI
        rsp->regs[rd] = rsp->regs[rs1] >> shamt;
    } else if (funct3 == 0b001) { // SLLI
        rsp->regs[rd] = rsp->regs[rs1] << shamt;
    } else {
        unsupported("I-type funct3", funct3);
    }

    rsp->pc += 4; // Next instruction
    rsp->analysis.ir_count++; // Increment # of I-type and R-type instructions executed
}

void emu_b_type(struct rv_state *rsp, uint32_t iw) {
    // Shift data to the right and 0 out the rest
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t rs2 = (iw >> 20) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;

    // Shift data to the right and 0 out the rest
    int64_t imm11 = (iw >> 7) & 0b1;
    int64_t imm4_1 = (iw >> 8) & 0b1111;
    int64_t imm10_5 = (iw >> 25) & 0b111111;
    int64_t imm12 = (iw >> 31) & 0b1;

    // Shift data to the left and join it with previous data
    int64_t imm10_1 = (imm10_5 << 5) | (imm4_1 << 1); // Shift imm4_1 by one to make room for 0
    int64_t imm11_1 = (imm11 << 11) | imm10_1;
    int64_t imm12_1 = (imm12 << 12) | imm11_1;

    // 32 - (12 + 1) + 32 = 51
    int64_t offset = ((int64_t)(imm12_1 << 51) >> 51); // 64 bit sign-extension

    bool taken = false;

    if ((funct3 == 0b100) && ((int64_t)rsp->regs[rs1] < (int64_t)rsp->regs[rs2])) { // BLT or BGT
        taken = true;
    } else if ((funct3 == 0b101) && ((int64_t)rsp->regs[rs1] >= (int64_t)rsp->regs[rs2])) { // BGE or BLE
        taken = true;
    } else if ((funct3 == 0b001) && ((int64_t)rsp->regs[rs1] != (int64_t)rsp->regs[rs2])) { // BNE
        taken = true;
    } else if ((funct3 == 0b000) && ((int64_t)rsp->regs[rs1] == (int64_t)rsp->regs[rs2])) { // BEQ
        taken = true;
    }

    if (taken) {
        rsp->pc = rsp->pc + offset;
        rsp->analysis.b_taken++; // Increment # of conditional branches taken
    } else {
        rsp->pc += 4; // Next instruction
        rsp->analysis.b_not_taken++; // Increment # of conditional branches not taken
    }
}

void emu_load_type(struct rv_state *rsp, uint32_t iw) {
    // Shift data to the right and 0 out the rest
    uint32_t rd = (iw >> 7) & 0b11111;
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;
    int64_t imm11_0 = (iw >> 20) & 0b111111111111;

    int64_t imm = ((int64_t)(imm11_0 << 52) >> 52); // 64 bit sign-extension

    uint64_t addr = rsp->regs[rs1] + imm; // Ex. lw t0, 8(sp)   <- the imm is 8

    // LB <- 8 bit
    // LH <- 16 bit
    // LW <- 32 bit
    // LD <- 64 bit

    if (funct3 == 0b000) { // LB
        rsp->regs[rd] = *((uint8_t*) addr);
    } else if (funct3 == 0b001) { // LH
        rsp->regs[rd] = *((uint16_t*) addr);
    } else if (funct3 == 0b010) { // LW
        rsp->regs[rd] = *((uint32_t*) addr);
    } else if (funct3 == 0b011) { // LD
        rsp->regs[rd] = *((uint64_t*) addr);
    } else {
        unsupported("Load-type funct3", funct3);
    }

    rsp->pc += 4; // Next instruction
    rsp->analysis.ld_count++; // Increment # of LD instructions executed
}

void emu_store_type(struct rv_state *rsp, uint32_t iw) {
    // Shift data to the right and 0 out the rest
    uint32_t rs1 = (iw >> 15) & 0b11111;
    uint32_t rs2 = (iw >> 20) & 0b11111;
    uint32_t funct3 = (iw >> 12) & 0b111;

    int64_t imm4_0 = (iw >> 7) & 0b11111;
    int64_t imm11_5 = (iw >> 25) & 0b1111111;

    int64_t imm11_0 = (imm11_5 << 5) | imm4_0;

    int64_t imm = ((int64_t)(imm11_0 << 52) >> 52); // 64 bit sign-extension

    uint64_t addr = rsp->regs[rs1] + imm; // Ex. sw t0, 8(a0)   <- the imm is 8

    // SB <- 8 bit
    // SH <- 16 bit
    // SW <- 32 bit
    // SD <- 64 bit

    if (funct3 == 0b000) { // SB
        *((uint8_t*) addr) = (uint8_t) rsp->regs[rs2];
    } else if (funct3 == 0b001) { // SH
        *((uint16_t*) addr) = (uint16_t) rsp->regs[rs2];
    } else if (funct3 == 0b010) { // SW
        *((uint32_t*) addr) = (uint32_t) rsp->regs[rs2];
    } else if (funct3 == 0b011) { // SD
        *((uint64_t*) addr) = (uint64_t) rsp->regs[rs2];
    } else {
        unsupported("Store-type funct3", funct3);
    }

    rsp->pc += 4; // Next instruction
    rsp->analysis.st_count++; // Increment # of LD instructions executed
}

void emu_jal(struct rv_state *rsp, uint32_t iw) {
    uint32_t rd = (iw >> 7) & 0b11111;
    uint64_t imm_20 = (iw >> 31) & 0b1;
    uint64_t imm_10_1 = (iw >> 21) & 0b1111111111;
    uint64_t imm_11 = (iw >> 20) & 0b1;
    uint64_t imm_19_12 = (iw >> 12) & 0b11111111;

    // Shift data to the left and join it with previous data
    // Shift imm10_1 by one to make room for 0
    int64_t imm = (imm_20 << 20) | (imm_19_12 << 12) | (imm_11 << 11) | (imm_10_1 << 1);

    // 32 - (20 + 1) + 32 = 43
    imm = (imm << 43) >> 43;

    // JAL support
    if (rd != 0) {
        rsp->regs[rd] = rsp->pc + 4;
    }

    rsp->pc = rsp->pc + imm; // Jump to offset location
    rsp->analysis.j_count++; // Increment # of jump instructions executed including j, jal, jalr
}

void emu_jalr(struct rv_state *rsp, uint32_t iw) {
    // RET
    uint32_t rs1 = (iw >> 15) & 0b11111;  // Will be ra (aka x1)
    uint64_t val = rsp->regs[rs1];  // Value of regs[1]

    rsp->pc = val;  // PC = return address

    rsp->analysis.j_count++; // Increment # of jump instructions executed including j, jal, jalr
}

void rv_one(struct rv_state *rsp) {
    // Get an instruction word from the current Program Counter
    uint32_t iw = cache_lookup(&rsp->i_cache, (uint64_t) rsp->pc); // Cache lookup

    uint32_t opcode = iw & 0b1111111;

    switch (opcode) {
        case 0b0110011:
            // R-type instructions have two register operands
            emu_r_type(rsp, iw);
            break;
        case 0b0010011:
            // I-type instructions have one register operand
            emu_i_type(rsp, iw);
            break;
        case 0b0000011:
            // Load-type instructions for loading from memory
            emu_load_type(rsp, iw);
            break;
        case 0b0100011:
            // Store-type instructions for storing to memory
            emu_store_type(rsp, iw);
            break;
        case 0b1100011:
            // B-type instructions for BEQ, BNE, etc.
            emu_b_type(rsp, iw);
            break;
        case 0b1101111:
            // JAL (aka j) is a variant of I-type instructions
            emu_jal(rsp, iw);
            break;
        case 0b1100111:
            // JALR (aka RET) is a variant of I-type instructions
            emu_jalr(rsp, iw);
            break;
        default:
            unsupported("Unknown opcode: ", opcode);
    }

    rsp->analysis.i_count++; // Increment # of instructions executed
}

int rv_emulate(struct rv_state *rsp) {
    while (rsp->pc != 0) {
        rv_one(rsp);
    }

    return (int) rsp->regs[A0];
}
