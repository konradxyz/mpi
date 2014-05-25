#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <sys/time.h>
#include <math.h>


#include "matgen.h"

#define MSG_SIZE 6
#define MPI_MAX_TAG 1234

void fail() {
  fprintf(stderr, "Error\n");
  MPI_Finalize();
  exit(1);
}


//For debug/profiling purposes
//#define DEBUG

#ifdef DEBUG
#define DBG(x) {x;}
#else
#define DBG(x)
#endif


static void printDuration(struct timeval* startTime, const char* msg) {
  double duration;
  struct timeval endTime;
  if (gettimeofday(&endTime, NULL))
  {
    fprintf(stderr, "ERROR: Gettimeofday failed!\n");
    fail();
  } 
      
  duration =
            ((double) endTime.tv_sec + ((double) endTime.tv_usec / 1000000.0)) -
            ((double) startTime->tv_sec + ((double) startTime->tv_usec / 1000000.0));

  fprintf(stderr, "%s, Time: %.10f\n", msg, duration); 
}
struct timeval start;


void start_timer() {
  if (gettimeofday(&start, NULL))
  {
    fprintf(stderr, "ERROR: Gettimeofday failed!\n");
    fail();
  } 
}

void stop_timer(const char* msg) {
  printDuration(&start, msg);
}


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


//algorithm
typedef struct {
  long long int value;
  int l, u, r, b;
  int has_value;
} result_t;


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
  if ( current_rank >= procCount ) {
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
  int i, j, k;
  DBG(start_timer());
  int* resp = rank_responsible(procCount, numRows);
  if ( resp == NULL )
    return 1;
  partial_cols = partial_columns(numRows, numColumns, matrix);
  DBG(stop_timer("Preparing exec plan"));
  if ( partial_cols == NULL )
    return 1;
  res->value = -1;
  DBG(start_timer());
  for ( i = 0; i < numRows; ++i ) {
    for ( j = i; j < numRows; ++j ) {
      if ( resp[i * numRows + j] == rank ) {
	long long* mi = partial_cols + (long long) i * (long long) (numColumns + 1) + 1;
	long long* mj = partial_cols + (long long) (j + 1) * (long long) (numColumns + 1) + 1;
        long long sum = 0;
        int start = 0;
        int bstart = 0;
        int bend = 0;
        long long bvalue = -1;
        for ( k = 0; k < numColumns; ++k ) {
       	  sum += mj[k] - mi[k];
	  if ( sum <= 0 ) {
            sum = 0;
            start = k + 1;
          } else {
            if ( bvalue < sum ) {
              bvalue = sum;
              bstart = start;
              bend = k;
            }
          }
        }
        if ( bvalue > res->value ) {
          res->value = bvalue;
          res->l = bstart;
          res->r = bend;
          res->u = i;
          res->b = j;
        }
      }
    }
  }
  DBG(stop_timer("main loop"));
  res->has_value = res->value >= 0 ? 1 : 0;

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

long long* transpose(long long* matrix, int numRows, int numCols) {
  int i, j;
  long long* res = calloc(numCols, numRows * sizeof(*res));
  if ( res == NULL ) {
    fprintf(stderr, "calloc failed");
    return NULL;
  }
  for ( i = 0; i < numRows; i++ ) {
    for ( j = 0; j < numCols; j++ ) {
      res[j * numRows + i] = matrix[i * numCols + j];
    }
  }
  return res;
}

void transpose_result(result_t* res) {
  int u = res->l;
  int b = res->r;
  int l = res->u;
  int r = res->b;
  res->l = l;
  res->r = r;
  res->u = u;
  res->b = b;
}

int main(int argc, char * argv[])
{
    int numRows, numColumns, seed, myRank, proc_count, i;
    long long int* matrix;
    double             duration;
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
    if ( matrix == NULL ) {
        fail();
    }
    MPI_Barrier(MPI_COMM_WORLD);
    if (gettimeofday(&startTime, NULL))
    {
        fprintf(stderr, "ERROR: Gettimeofday failed!\n");
        free(matrix);
        fail();
    }
    int transposed = numRows > numColumns; 
    if ( transposed ) {
      long long* tmp = transpose(matrix, numRows, numColumns);
      if ( tmp == NULL ) {
        free(matrix);
        fail();
      }
      free(matrix);
      matrix = tmp;
      int tmpr = numRows;
      numRows = numColumns;
      numColumns = tmpr;

    }
    MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
    MPI_Comm_size(MPI_COMM_WORLD, &proc_count);

    if ( find_partial_MSP(myRank, proc_count, numRows, numColumns, matrix, &result) ) {
      free(matrix);
      fail();
    }
    DBG(printDuration(&startTime, "Find partial msp")); 
    if ( myRank == 0 ) {
      gather_max(proc_count, &result);
      if ( !result.has_value ) { //max is negative
        find_matrix_max(numRows, numColumns, matrix, &result);
      }

      if ( transposed ){
        transpose_result(&result);
        int tmp = numRows;
        numRows = numColumns;
        numColumns = tmp;
      }
      if (gettimeofday(&endTime, NULL))
      {
        fprintf(stderr, "ERROR: Gettimeofday failed!\n");
        free(matrix);
        fail();
      }
      
      duration =
            ((double) endTime.tv_sec + ((double) endTime.tv_usec / 1000000.0)) -
            ((double) startTime.tv_sec + ((double) startTime.tv_usec / 1000000.0));



      fprintf(stderr, 
          "PWIR2014_Konrad_Paziewski_306410 Input: (%d,%d,%d) Solution: |(%d,%d),(%d,%d)|=%lld Time: %.10f\n",
            numRows, numColumns, seed,
            result.u + 1, result.l + 1, result.b + 1, result.r + 1, result.value, duration);



    } else
      send_max(&result);


    free(matrix);
    MPI_Finalize();
    return 0;
}
