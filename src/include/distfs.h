#ifndef DISTFS_H_
#define DISTFS_H_

#define MSG_HELLO      "hello"
#define MSG_HELLO_SIZE (sizeof(MSG_HELLO) - 1)

struct distfs_s {
    struct peer_s *peer;
    struct {
        struct root_s  *local;
        struct group_s *remote;
        unsigned char   file[SHA256HEX];
    } blocks;
    struct list_s transactions;
    struct list_s jobs;
    struct {
        struct ev_timer jobs;
        struct ev_timer update;
    } ev;
    struct {
        pthread_mutex_t mutex;
        bool            state;
        bool            newblock;
    } mining;
};

int dfs_transaction_add(struct distfs_s *dfs, char **argv, int argc,
                        int *dfserr);
int dfs_transaction_list(struct distfs_s *dfs, char **argv, int argc,
                         int *dfserr);
int dfs_transaction_share(struct distfs_s *dfs, char **argv, int argc,
                          int *dfserr);
int dfs_job_add(struct distfs_s *dfs, char **argv, int argc,
                int *dfserr);
int dfs_job_finalize(struct distfs_s *dfs, char **argv, int argc,
                     int *dfserr);
int dfs_block_mine(struct distfs_s *dfs, char **argv, int argc,
                   int *dfserr);
int dfs_hello(struct distfs_s *dfs, int host, unsigned short port);
int dfs_block_xet(struct distfs_s *dfs, char **argv, int argc,
                         int *dfserr);
int dfs_block_send(struct peer_s *p, struct distfs_s *dfs,
                   int host, unsigned short port);
int dfs_block_mining(struct distfs_s *dfs, bool *locked);
int dfs_query(struct peer_s *p, int host, unsigned short port,
              int query_host, unsigned short query_port);
int dfs_query_reply(struct peer_s *p, int host, unsigned short port,
                    int query_host, unsigned short query_port, bool reachable);
int dfs_ping_reply(struct peer_s *p, int host, unsigned short port,
                   double ts);
int dfs_pong_reply(struct peer_s *p, int host, unsigned short port,
                   double ts);

#endif
