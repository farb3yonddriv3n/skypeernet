/*
 *
 * GrizzlyCloud library - simplified VPN alternative for IoT
 * Copyright (C) 2017 - 2018 Filip Pancik
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
#include <gc.h>

int hm_log_impl(enum loglevel_e level, struct hm_log_s *log, const char *file,
                const int line, const char *func, const char *msg, ...)
{
    size_t          len = 0;
    char            out[8192], buf[128];
    time_t          s;
    struct timespec spec;
    long long       ms;
    struct tm       ts;

    assert(log);

    /** only display messages user asked for */
    if (level > log->level) {
        return -1;
    }

    //if (log->fd == STDERR_FILENO)
    {
        const char *colour;
        switch(level) {
            case GCLOG_EMERG: //< system is unusable
                colour = "\33[1;31;41mGCLOG_EMERG\33[m";
                break;
            case GCLOG_ALERT: //< action must be taken immediately
                colour = "\33[1;33;41mGCLOG_ALERT\33[m";
                break;
            case GCLOG_CRIT:  //< critical conditions
                colour = "\33[1;31;40mGCLOG_CRIT\33[m";
                break;
            case GCLOG_ERR:   //< error conditions
                colour = "\33[1;34;41mGCLOG_ERR\33[m";
                break;
            case GCLOG_WARNING: //< warning conditions
                colour = "\33[1;37;43mGCLOG_WARNING\33[m";
                break;
            case GCLOG_NOTICE:    //< normal, but significant, condition
                colour = "\33[1;34;47mGCLOG_NOTICE\33[m";
                break;
            case GCLOG_INFO:  //< informational message
                colour = "\33[1;37;42mGCLOG_INFO\33[m";
                break;
            case GCLOG_DEBUG: //< debug-level message
                colour = "\33[1;32;40mGCLOG_DEBUG\33[m";
                break;
            case GCLOG_TRACE: //< memory message
                colour = "\33[1;35;34mGCLOG_TRACE\33[m";
                break;
            default:
                colour = NULL;
        }
        if (colour) {
            len += snprintf(out, sizeof(out), colour, NULL);
        }
    }

    clock_gettime(CLOCK_REALTIME, &spec);
    s = spec.tv_sec;
    ms = round(spec.tv_nsec / 1.0e6);

    ts = *localtime(&s);
    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", &ts);

    len += snprintf(out + len, sizeof(out) - len, "[%s.%03lld] ", buf, ms);

    va_list args;
    va_start(args, msg);
    len += vsnprintf(out+len, 3*sizeof(out)/4-len, msg, args);
    va_end(args);
    if (len >= 3*sizeof(out)/4) {
        len += snprintf(out + len, sizeof(out) - len, "...");
    }

    len += snprintf(out+len, sizeof(out)-len, ", %s:%d(%s)\n", file, line, func);

    ssize_t nwritten = write(log->fd, out, len);
    return (nwritten == len) ? GC_OK : GC_ERROR;
}

int hm_log_open(struct hm_log_s *l, const char *filename, enum loglevel_e level)
{
    if (filename != NULL) {
        l->file = fopen(filename, "a");
        if (l->file == NULL) {
            return GC_ERROR;
        }
        l->fd = fileno(l->file);
    } else {
        l->file = stderr;
        l->fd = STDERR_FILENO;
    }

    l->level = level;

    return GC_OK;
}

int hm_log_close(struct hm_log_s *l)
{
    if (l->file && l->file != stderr) {
        fclose(l->file);
        return GC_OK;
    } else {
        return GC_ERROR;
    }
}
