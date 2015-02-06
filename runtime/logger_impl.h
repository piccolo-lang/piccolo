#ifndef __LOGGER_IMPL_H
#define __LOGGER_IMPL_H

#define LOGFILE "piccolo.log"

void PICC_logger_open();
void PICC_logger_log(const char *fmt, ...);
void PICC_logger_close();

#define PICC_LOG(fmt, ...) PICC_logger_log(fmt, __VA_ARGS__)

#endif /* !__LOGGER_IMPL_H */
