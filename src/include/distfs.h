#ifndef DISTFS_H_
#define DISTFS_H_

struct distfs_s {
    struct peer_s *peer;
    struct {
        struct root_s  *local;
        struct group_s *remote;
    } blocks;
    struct list_s transactions;
    struct list_s jobs;
};

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc);
int dfs_transaction_list(struct distfs_s *dfs, char **argv, int argc);

#endif
