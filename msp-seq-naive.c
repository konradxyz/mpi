/*
 * A naive sequential algorithm for the MPI assignment
 * (for corretness testing on small data).
 * Concurrent and Distributed Programming Course, spring 2014.
 * Faculty of Mathematics, Informatics and Mechanics.
 * University of Warsaw, Warsaw, Poland.
 * 
 * Copyright (C) Konrad Iwanicki, 2014.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include "matgen.h"

static void printUsage(char const * prog)
{
    fprintf(stderr, "Usage:\n");
    fprintf(stderr, "    %s <num_rows> <num_colums> <seed>\n\n", prog);
}

static void printMatrix(long long int const * m, int r, int c)
{
    int i, j;
    for (i = 0; i <= r; ++i)
    {
        for (j = 0; j <= c; ++j)
        {
            printf(" %lld", *m++);
        }
        printf("\n");
    }
    printf("\n");
}

int main(int argc, char * argv[])
{
    struct timeval     startTime;
    struct timeval     endTime;
    double             duration;
    int                numRows, numColumns, seed;
    matgen_t *         matgenPtr;
    long long int *    matrixPtr;
    int                i, j, k, l;
    int                bestI, bestJ, bestK, bestL;
    long long int      maxSum;

    if (argc != 4)
    {
        fprintf(stderr, "ERROR: Invalid arguments!\n");
        printUsage(argv[0]);
        exit(1);
    }
    numRows = atoi(argv[1]);
    numColumns = atoi(argv[2]);
    seed = atoi(argv[3]);
    if (numRows <= 0 || numColumns <= 0 || seed <= 0)
    {
        fprintf(stderr, "ERROR: Invalid arguments: %s %s %s!\n", argv[1],
                argv[2], argv[3]);
        printUsage(argv[0]);
        exit(1);
    }
    matgenPtr = matgenNew(numRows, numColumns, seed);
    if (matgenPtr == NULL)
    {
        fprintf(stderr, "ERROR: Unable to create the matrix generator!\n");
        printUsage(argv[0]);
        exit(1);
    }
    matrixPtr = (long long int *) malloc(
            sizeof(long long int) * (numRows + 1) * (numColumns + 1));
    if (matrixPtr == NULL)
    {
        fprintf(stderr, "ERROR: Unable to create the matrix!\n");
        printUsage(argv[0]);
        exit(1);
    }
    for (j = 1; j <= numRows; ++j)
    {
        for (i = 1; i <= numColumns; ++i)
        {
            matrixPtr[j * (numColumns + 1) + i] = matgenGenerate(matgenPtr);
        }
    }
    matgenDestroy(matgenPtr);

    if (gettimeofday(&startTime, NULL))
    {
        fprintf(stderr, "ERROR: Gettimeofday failed!\n");
        exit(1);
    }

    // *** START OF COMPUTATION ***

    for (i = 0; i <= numColumns; ++i)
    {
        matrixPtr[i] = 0;
    }
    for (j = 1; j <= numRows; ++j)
    {
        matrixPtr[j * (numColumns + 1)] = 0;
    }
    /* printMatrix(matrixPtr, numRows, numColumns); */
    maxSum = matrixPtr[numColumns + 1 + 1];
    bestI = bestJ = bestK = bestL = 1;
    for (j = 1; j <= numRows; ++j)
    {
        for (i = 1; i <= numColumns; ++i)
        {
            matrixPtr[j * (numColumns + 1) + i] +=
                    matrixPtr[j * (numColumns + 1) + i - 1] +
                    matrixPtr[(j - 1) * (numColumns + 1) + i] -
                    matrixPtr[(j - 1) * (numColumns + 1) + i - 1];
        }
    }
    /* printMatrix(matrixPtr, numRows, numColumns); */
    for (j = 0; j < numRows; ++j)
    {
        for (i = 0; i < numColumns; ++i)
        {
            for (k = j + 1; k <= numRows; ++k)
            {
                for (l = i + 1; l <= numColumns; ++l)
                {
                    long long int tmp =
                            matrixPtr[j * (numColumns + 1) + i] +
                            matrixPtr[k * (numColumns + 1) + l] -
                            matrixPtr[j * (numColumns + 1) + l] -
                            matrixPtr[k * (numColumns + 1) + i];
                    if (tmp > maxSum)
                    {
                        maxSum = tmp;
                        bestI = i + 1;
                        bestJ = j + 1;
                        bestK = k;
                        bestL = l;
                    }
                }
            }
        }
    }

    // *** END OF COMPUTATION ***

    if (gettimeofday(&endTime, NULL))
    {
        fprintf(stderr, "ERROR: Gettimeofday failed!\n");
        exit(1);
    }

    duration =
            ((double) endTime.tv_sec + ((double) endTime.tv_usec / 1000000.0)) -
            ((double) startTime.tv_sec + ((double) startTime.tv_usec / 1000000.0));

    fprintf(stderr, "PWIR2014_Jan_Kowalski_123456 Input: (%d,%d,%d) Solution: |(%d,%d),(%d,%d)|=%lld Time: %.10f\n",
            numRows, numColumns, seed,
            bestJ, bestI, bestK, bestL, maxSum, duration);

    free(matrixPtr);
    return 0;
}
