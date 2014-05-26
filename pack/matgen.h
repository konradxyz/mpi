/*
 * Matrix generation library for the MPI assignment.
 * Concurrent and Distributed Programming Course, spring 2014.
 * Faculty of Mathematics, Informatics and Mechanics.
 * University of Warsaw, Warsaw, Poland.
 *
 * Copyright (C) Konrad Iwanicki, 2014.
 */
#ifndef __MIMUW_MATGEN_H__
#define __MIMUW_MATGEN_H__

#ifdef __cplusplus
extern "C"
{
#endif

struct matgen_s;
typedef struct matgen_s matgen_t;

typedef struct matgen_serialized_s
{
    int               size;
    unsigned char *   bytes;
} matgen_serialized_t;



/**
 * Creates a new matrix generator.
 * @param numRows The number of matrix rows (at least 1).
 * @param numColumns The number of matrix columns (at least 1).
 * @param seed A seed.
 * @return A pointer to the new matrix generator or NULL if
 *   creating a generator failed.
 */
matgen_t * matgenNew(int numRows, int numColumns, int seed);

/**
 * Returns the next matrix element from a given generator.
 * The elements are generated row-wise. First, the elements
 * from the first row are generated, starting from the first
 * column, then from the second row, and so on.
 * If invoked after all matrix elements have been generated
 * by the generator, this function terminates the current
 * process with the <tt>exit</tt> system call.
 * @param matgenPtr A pointer to the matrix generator.
 * @return The value of the next matrix element.
 */
int matgenGenerate(matgen_t * matgenPtr);

/**
 * Destroys a given matrix generator.
 * @param matgenPtr A pointer to the matrix generator.
 */
void matgenDestroy(matgen_t * matgenPtr);

/**
 * Serializes a matrix generator.
 * @param matgenPtr A pointer to the matrix generator.
 * @return A pointer to a serialized matrix generator
 *   or NULL if serialization failed.
 */
matgen_serialized_t * matgenNewSerialized(matgen_t const * matgenPtr);

/**
 * Deserializes a matrix generator.
 * @param serPtr A pointer to a serialized matrix generator.
 * @return A pointer to a deserialized matrix generator
 *   or NULL if deserialization failed.
 */
matgen_t * matgenFromSerialized(matgen_serialized_t const * serPtr);

/**
 * Destroys a serialized matrix generator.
 * @param serPtr A pointer to the serialized matrix
 *   generator that will be destroyed.
 */
void matgenDestroySerialized(matgen_serialized_t * serPtr);


#ifdef __cplusplus
}
#endif


#endif /* __MIMUW_MATGEN_H__ */
