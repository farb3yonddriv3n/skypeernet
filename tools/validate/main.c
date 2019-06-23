#include <common.h>

int main(int argc, char **argv)
{
    if (argc != 2) return -1;
    struct root_s *r;
    ifr(root.data.load.file(&r, argv[1]));
    printf("Block valid\n");
    return 0;
}
