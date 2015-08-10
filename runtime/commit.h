/**
 * \file commit.h
 * \brief Commitment functions
 *
 * This modules contains functions to manipulate commitments.
 */
#ifndef __COMMIT_H
#define __COMMIT_H

#include "value.h"

/** The commitment type */
typedef struct _PICC_Commit PICC_Commit;

/** Type of a pithread */
typedef struct _PICC_PiThread PICC_PiThread;

/** Type of the function evaluating the payload of an output */
typedef PICC_Value (*PICC_EvalFunction)(PICC_PiThread *);

/**
 * \fn PICC_PiThread *PICC_commit_get_pithread(PICC_Commit *commit)
 * \brief Retrieve the pithread who created the commitment.
 *
 * \param commit The commitment
 * \return The pithread
 */
PICC_PiThread *PICC_commit_get_pithread(PICC_Commit *);

/**
 * \fn int PICC_commit_get_refvar(PICC_Commit *commit)
 * \brief Retrieve the reference of the variable of an input commitment.
 *
 * \param commit The commitment
 * \return The variable reference
 */
int PICC_commit_get_refvar(PICC_Commit *);

/**
 * \fn PICC_Value PICC_commit_call_evalfun(PICC_Commit *commit)
 * \brief Call the evaluation function of the payload of an output.
 *
 * \param commit The commitment
 * \return The value result of the evaluation function
 */
PICC_Value PICC_commit_call_evalfun(PICC_Commit *);

/**
 * \fn void PICC_register_output_commitment(PICC_PiThread *pt,
 *                                          PICC_Channel *chan,
 *                                          PICC_EvalFunction eval,
 *                                          int label)
 * \brief Create and register an output commitment.
 *
 * \param pt The pithread that register the commitment
 * \param chan The channel related to the commitment
 * \param eval The evaluation function of the payload
 * \param label The continuation label
 */
void PICC_register_output_commitment(PICC_PiThread *, PICC_Channel *,
                                     PICC_EvalFunction, int);

/**
 * \fn void PICC_register_input_commitment(PICC_PiThread *pt,
 *                                         PICC_Channel *chan,
 *                                         int refvar,
 *                                         int label)
 * \brief Create and register an output commitment
 *
 * \param pt The pithread that register the commitment
 * \param chan The channel related to the commitment
 * \param refvar The variable reference in which to put the payload
 * \param label The continuation label
 */
void PICC_register_input_commitment(PICC_PiThread *, PICC_Channel *, int, int);

#endif /* !__COMMIT_H */
