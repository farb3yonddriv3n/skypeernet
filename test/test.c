#include <common.h>
#include <cu.h>

static struct test_s testlist[] = {
    { "group, root, block, transaction", t1_group_mine_block_append_transactions },
    { "rsa encrypt and decrypt",         t2_rsa_encrypt_decrypt },
    { "packet",                          t3_packet },
    { "list",                            t4_list },
    { "aes",                             t5_aes },
};

static int init_suite()
{
    return 0;
}

static int clean_suite()
{
    return 0;
}

int main()
{
    CU_pSuite suite = NULL;

    if (CUE_SUCCESS != CU_initialize_registry())
        return CU_get_error();

    suite = CU_add_suite("skypeernet", init_suite, clean_suite);
    if (NULL == suite) {
        CU_cleanup_registry();
        return CU_get_error();
    }

    int ti;
    for (ti = 0; ti < COUNTOF(testlist); ti++) {
        if (CU_add_test(suite, testlist[ti].description, testlist[ti].func) == NULL) {
            CU_cleanup_registry();
            return CU_get_error();
        }
    }

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CU_cleanup_registry();
    return CU_get_error();
}
