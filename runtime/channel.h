/**
 * \file channel.c
 * \brief Channel functions
 *
 * This modules contains functions to manipulate communication channels.
 */
#ifndef __CHANNEL_H
#define __CHANNEL_H

/** The channel type */
typedef struct _PICC_Channel PICC_Channel;

/**
 * \fn void PICC_unlock_channel(PICC_Channel *chan)
 * \brief Release the lock on a channel.
 *
 * \param chan The channel
 */
void PICC_unlock_channel(PICC_Channel *);

#endif /* !__CHANNEL_H */
