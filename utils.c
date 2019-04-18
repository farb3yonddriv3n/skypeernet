#include <common.h>

int eioie_fwrite(char *fname, const char *mode, char *content, int ncontent)
{
    FILE *pfile;
    int result;

    pfile = fopen(fname, mode);
    if (pfile == NULL) {
        return -1;
    }

    result = fwrite(content, sizeof(char), ncontent, pfile);

    fclose(pfile);
    return !(result == ncontent);
}

int eioie_fread(char **dst, const char *fname)
{
    FILE *pfile;
    int lsize;
    char *buffer;
    int result;

    pfile = fopen(fname, "rb");
    if (pfile == NULL) {
        return -1;
    }

    fseek(pfile , 0 , SEEK_END);
    lsize = ftell(pfile);
    rewind(pfile);

    if (lsize > MAX_FILE_SIZE) {
        fclose(pfile);
        return -1;
    }

    buffer = malloc(sizeof(char) * lsize);
    if (buffer == NULL) {
        fclose(pfile);
        return -1;
    }

    result = fread(buffer, sizeof(char), lsize, pfile);
    if (result != lsize) {
        fclose(pfile);
        free(buffer);
        return -1;
    }

    *dst = buffer;

    fclose(pfile);
    return result;
}
