#include <common.h>
#include <cu.h>

static struct test_s list[] = {
    /*
    { "root import and export",             t1_root_import_export },
    { "mine block and append transactions", t2_mine_block_append_transactions },
    { "block append transactions",          t3_block_append_transactions },
    { "rsa encrypt and decrypt",            t4_rsa_encrypt_decrypt },
    */
    { "root _compare",                      t5_root_compare },
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

    suite = CU_add_suite("eioie", init_suite, clean_suite);
    if (NULL == suite) {
        CU_cleanup_registry();
        return CU_get_error();
    }

    int ti;
    for (ti = 0; ti < COUNTOF(list); ti++) {
        if (CU_add_test(suite, list[ti].description, list[ti].func) == NULL) {
            CU_cleanup_registry();
            return CU_get_error();
        }
    }

    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    CU_cleanup_registry();
    return CU_get_error();
}
