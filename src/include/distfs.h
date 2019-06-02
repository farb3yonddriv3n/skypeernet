#ifndef DISTFS_H_
#define DISTFS_H_

struct distfs_s {
    struct root_s  *local_block;
    struct group_s *remote_blocks;
    struct list_s   transactions;
};

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc);
int dfs_transaction_list(struct distfs_s *dfs, char **argv, int argc);

#endif
