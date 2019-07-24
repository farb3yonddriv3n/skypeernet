#ifndef CU_H_
#define CU_H_

#include "common.h"
#include "CUnit/Basic.h"

#define FILE1 "file1"
#define FILE2 "file2"

#define A(m_a, m_b) (CU_ASSERT(m_b == m_a))

struct test_s {
    const char *description;
    void (*func)(void);
};

void t1_group_mine_block_append_transactions();
void t2_rsa_encrypt_decrypt();
void t3_packet();
void t4_list();

#endif
