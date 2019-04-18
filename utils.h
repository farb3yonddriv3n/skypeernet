#ifndef UTILS_H_
#define UTILS_H_

#define FLAG(m_n)     (1 << m_n)

#define MAX_FILE_SIZE (1024 * 1024 * 1024) // 1GB

#define CHUNK_SIZE    (512 * 10)

int eioie_fwrite(char *fname, const char *mode, char *content, int ncontent);
int eioie_fread(char **dst, const char *fname);

#endif
