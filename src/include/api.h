#ifndef API_H_
#define API_H_

enum api_e {
    API_LISTPEERS              = 0,
    API_MESSAGE,
    API_LISTFILES_LOCAL,
    API_LISTFILES_REMOTE,
    API_PEER_ONLINE,
    API_PEER_OFFLINE,
    API_JOBSDUMP,
    API_JOBDONE,
    API_JOBADD,
    API_JOBFINALIZE,
    API_TSHARE                 = 10,
    API_BMINE,
    API_BADVERTISE,
    API_BMINING,
    API_ROGUEDUMP,
    API_VERSIONDUMP            = 15,
    API_TRAFFICDUMP,
    API_TASKDUMP,
    API_TUNNELOPEN,
    API_TUNNELCLOSE,
    API_TUNNELDUMP             = 20,
    API_ENDPOINTOPEN,
    API_ENDPOINTDUMP,
    API_ENDPOINTCLOSE,
};

struct module_api_s {
    int (*write)(struct peer_s *p, enum api_e cmd, json_object *payload,
                 json_object *request, int dfserr);
    int (*read)();
    //int (*clean)();
    struct {
        void (*read)(EV_P_ ev_io *w, int revents);
        void (*write)(EV_P_ ev_io *w, int revents);
    } ev;
};

int api_message_write(struct peer_s *p, int host, unsigned short port,
                      char *msg, int len);
int api_peer_online(struct peer_s *p, struct world_peer_s *wp);
int api_peer_offline(struct peer_s *p, struct world_peer_s *wp);
int api_job_done(struct peer_s *p, unsigned char *filename,
                 int nfilename);
void api_update(struct ev_loop *loop, struct ev_timer *timer, int revents);
int api_endpoint_change(struct peer_s *p, unsigned short dst, unsigned short src,
                        enum api_e change);
int api_taskdump(struct peer_s *p);
int api_jobsdump(struct peer_s *p);
int api_trafficdump(struct peer_s *p);

extern const struct module_api_s api;

#endif
