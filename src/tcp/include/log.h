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
#ifndef GC_GCLOG_H_
#define GC_GCLOG_H_

/**
 * @brief Log level.
 *
 */
enum loglevel_e {
    GCLOG_EMERG = 0,
    GCLOG_ALERT,
    GCLOG_CRIT,
    GCLOG_ERR,
    GCLOG_WARNING,
    GCLOG_NOTICE,
    GCLOG_INFO,
    GCLOG_DEBUG,
    GCLOG_TRACE,
};

/**
 * @brief Generic log structure.
 *
 */
struct hm_log_s {
    const char *name;    /**< Filename. */
    int             fd;       /**< File descriptor. */
    FILE            *file;    /**< File stream. */
    void            *data;    /**< User data. */
    enum loglevel_e level;    /**< Log level. */
};

#define hm_log(t, l, fmt...)\
    hm_log_impl(t, l, __FILE__, __LINE__, __FUNCTION__, fmt)

/**
 * @brief Add log message.
 *
 * @param level Log level.
 * @param log Log structure.
 * @param file File which log attempt is called from.
 * @param line File line which log attempt is called from.
 * @param func Function which log attempt is called from.
 * @param fmt Varg formatted message.
 * @return GC_OK on success, GC_ERROR on failure.
 */
int hm_log_impl(enum loglevel_e level, struct hm_log_s *log,
                const char *file, int line, const char *func,
                const char *fmt, ...)
                __attribute__ ((format (printf, 6, 7)));

/**
 * @brief Initialize log.
 *
 * @param l Log structure.
 * @param filename Filename.
 * @param level Log level.
 * @return GC_OK on success, GC_ERROR on failure.
 */
int hm_log_open(struct hm_log_s *l, const char *filename, enum loglevel_e level);

/**
 * @brief Close log.
 *
 * @param l Log structure.
 * @return GC_OK on success, GC_ERROR on failure.
 */
int hm_log_close(struct hm_log_s *l);

#endif
