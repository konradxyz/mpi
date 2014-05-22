#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sys/time.h>
#include <math.h>


#include "matgen.h"

#define MSG_SIZE 6
#define MPI_MAX_TAG 1234
//#define DEBUG

#ifdef DEBUG
#define DBG(x) {x;}
#else
#define DBG(x)
#endif


typedef struct {
  long long int value;
  int l, u, r, b;
  int has_value;
} result_t;

static void printUsage(char const * prog)
{
    fprintf(stderr, "Usage:\n");
    fprintf(stderr, "    %s <num_rows> <num_colums> <seed>\n\n", prog);
}

static void printMatrix(long long int const * m, int r, int c)
{
    int i, j;
    for (i = 0; i < r; ++i)
    {
        for (j = 0; j < c; ++j)
        {
            printf(" %lld", *m++);
        }
        printf("\n");
    }
    printf("\n");
}

static void printMatrixI(int const * m, int r, int c)
{
    int i, j;
    for (i = 0; i < r; ++i)
    {
        for (j = 0; j < c; ++j)
        {
            printf(" %d", *m++);
        }
        printf("\n");
    }
    printf("\n");
}

long long int* generate_matrix(int numRows, int numColumns, int seed) {
    matgen_t *         matgenPtr;
    long long int *    matrixPtr; 
    int i, j;
    matgenPtr = matgenNew(numRows, numColumns, seed);
    if (matgenPtr == NULL)
    {
        fprintf(stderr, "ERROR: Unable to create the matrix generator!\n");
        return NULL;
    }
    matrixPtr = (long long int *) calloc(numColumns, numRows * sizeof(*matrixPtr));
    if (matrixPtr == NULL)
    {
        fprintf(stderr, "ERROR: Unable to create the matrix!\n");
        matgenDestroy(matgenPtr);
        return NULL;    
    }
    for (j = 0; j < numRows; ++j)
    {
        for (i = 0; i < numColumns; ++i)
        {
            matrixPtr[j * numColumns + i] = matgenGenerate(matgenPtr);
        }
    }
    matgenDestroy(matgenPtr);
    return matrixPtr;
}

void fail() {
  fprintf(stderr, "Error\n");
  MPI_Finalize();
  exit(1);
}

int* rank_responsible(int procCount, int numRows) {
  long long i, j;
  int* result = calloc(numRows, numRows * sizeof(*result));
  if ( calloc == NULL ) {
    fprintf(stderr, "malloc failed");
    return NULL;
  }
  int current_rank = 0;
  long long pair_count = ((long long) (numRows + 1)) * numRows / 2;
  int pair_per_rank =(pair_count + procCount - 1) / procCount;
  int current_rank_count = 0;
  for ( i = 0; i < numRows; i++ ) {
    for ( j = 0; j < numRows; j++ ) {
      if ( i <= j ) {
        if ( current_rank_count >= pair_per_rank ) {
          current_rank_count = 0;
          current_rank++;
        }
        result[i * numRows + j] = current_rank;
        current_rank_count++;
      } else
        result[i * numRows + j] = -1;
    }
  }
  if ( current_rank >= procCount ) { //ERROR
    fprintf(stderr, "Error counting responsible %d %d\n", 
        procCount, current_rank_count);
    free(result);
    result = NULL;
  }
  return result;
}

long long* partial_columns(int numRows, int numColumns, long long* matrix) {
  long long i, j;
  long long* res = calloc(numRows + 1, (numColumns + 1) * sizeof(*res));
  if ( res == NULL )
    return NULL;
  for ( i = 0; i < numRows; i++ ) {
    for ( j = 0; j < numColumns; j++ ) {
      res[(i + 1) * (numColumns + 1) + j + 1] = matrix[i * numColumns + j] 
        + res[i * (numColumns + 1) + j + 1];
    }
  }
  return res;
}

int find_partial_MSP(int rank, int procCount, int numRows, int numColumns, 
    long long int* matrix, result_t* res) {
  long long* partial_cols = NULL;
  long long i, j, k;
  int* resp = rank_responsible(procCount, numRows);
  if ( resp == NULL )
    return 1;
  DBG(printMatrixI(resp, numRows, numRows));
  partial_cols = partial_columns(numRows, numColumns, matrix);
  DBG(printMatrix(partial_cols, numRows + 1, numColumns + 1));
  if ( partial_cols == NULL )
    return 1;
  res->has_value = 0;
  for ( i = 0; i < numRows; i++ ) {
    for ( j = i; j < numRows; j++ ) {
      if ( resp[i * numRows + j] == rank ) {
        long long sum = 0;
        int start = 0;
        for ( k = 0; k < numColumns; k++ ) {
          DBG(fprintf(stderr, "%d %d %d\n", i, j, k));
          sum += partial_cols[(j + 1) * (numColumns + 1) + k + 1] 
            - partial_cols[i * (numColumns + 1) + k + 1];
          if ( sum <= 0 ) {
            sum = 0;
            start = k + 1;
          } else {
            if ( !res->has_value || res->value < sum ) {
              res->value = sum;
              res->has_value = 1;
              res->l = start;
              res->r = k;
              res->u = i;
              res->b = j;
            }
          }
        }
      }
    }
  }
  DBG(fprintf(stderr, "Rank %d, has value %d Solution: |(%d,%d),(%d,%d)|=%lld\n",
    rank, res->has_value, res->l + 1, res->u + 1, res->r + 1, res->b + 1, 
    res->value));


  free(partial_cols);
  free(resp);
  return 0;
}

void to_array(result_t* res, long long* arr) {
  arr[0] = res->has_value;
  arr[1] = res->value;
  arr[2] = res->l;
  arr[3] = res->u;
  arr[4] = res->r;
  arr[5] = res->b;
}


void from_array(result_t* res, long long* arr) {
  res->has_value = arr[0];
  res->value = arr[1];
  res->l = arr[2];
  res->u = arr[3];
  res->r = arr[4];
  res->b = arr[5];
}

void gather_max(int proc_count, result_t* result) {
  int i;
  MPI_Status status;
  result_t res = *result;

  for ( i = 1; i < proc_count; i++ ) {
    result_t other;
    long long input[MSG_SIZE];
    MPI_Recv(
        input,
        MSG_SIZE,
        MPI_LONG_LONG_INT,
        i,
        MPI_ANY_TAG,
        MPI_COMM_WORLD,
        &status
    );
    from_array(&other, input);
    if ( !res.has_value || (other.has_value && other.value > res.value ) )
      res = other;
  }
  *result = res;
}

void send_max(result_t* result) {
  long long out[MSG_SIZE];
  to_array(result, out);
  MPI_Send(
      out,
      MSG_SIZE,
      MPI_LONG_LONG_INT,
      0,
      MPI_MAX_TAG,
      MPI_COMM_WORLD
  );
}

void find_matrix_max(int numRows, int numColumns, long long* matrix, result_t* res) {
  int mi = 0;
  int mj = 0;
  int i, j;
  for ( i = 0; i < numRows; i++ ) {
    for ( j = 0; j < numColumns; j++ ) {
      if ( matrix[i * numColumns + j] > matrix[mi * numColumns + mj] ) {
        mi = i; mj = j;
      }
    }
  }
  res->has_value = 1;
  res->l = res->r = mj;
  res->u = res->b = mi;
  res->value = matrix[mi * numColumns + mj];
}


int main(int argc, char * argv[])
{
    int numRows, numColumns, seed, myRank, proc_count, i;
    long long int* matrix;
    struct timeval     startTime;
    struct timeval     endTime;
    result_t result;
 

    MPI_Init(&argc, &argv);

    if (argc != 4)
    {
        fprintf(stderr, "ERROR: Invalid arguments!\n");
        printUsage(argv[0]);
        fail();
    }
    numRows = atoi(argv[1]);
    numColumns = atoi(argv[2]);
    seed = atoi(argv[3]);
    if (numRows <= 0 || numColumns <= 0 || seed <= 0)
    {
        fprintf(stderr, "ERROR: Invalid arguments: %s %s %s!\n", argv[1],
                argv[2], argv[3]);
        printUsage(argv[0]);
        fail(); 
    }
    matrix = generate_matrix(numRows, numColumns, seed);
    DBG(printMatrix(matrix, numRows, numColumns));
    if ( matrix == NULL ) {
        fail();
    }

    if (gettimeofday(&startTime, NULL))
    {
        fprintf(stderr, "ERROR: Gettimeofday failed!\n");
        free(matrix);
        fail();
    }
    
    MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
    MPI_Comm_size(MPI_COMM_WORLD, &proc_count);

    if ( find_partial_MSP(myRank, proc_count, numRows, numColumns, matrix, &result) ) {
      free(matrix);
      fail();
    }

    if ( myRank == 0 ) {
      gather_max(proc_count, &result);
      if ( !result.has_value ) { //max is negative
        find_matrix_max(numRows, numColumns, matrix, &result);
      }
      fprintf(stderr, 
          "PWIR2014_Konrad_Paziewski_306410 Input: (%d,%d,%d) Solution: |(%d,%d),(%d,%d)|=%lld Time: %.10f\n",
            numRows, numColumns, seed,
            result.l + 1, result.u + 1, result.r + 1, result.b + 1, result.value, 0.0f);



    } else
      send_max(&result);


    free(matrix);
    MPI_Finalize();
    return 0;
}
