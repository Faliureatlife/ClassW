/*  P4.c  –  Banker's Algorithm (single resource) in C  */
/*  Compile:  gcc -o P4 P4.c -lpthread                     */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdbool.h>

/* ------------------------------------------------------------ */
/*  Global banker state – protected by mutex + cond             */
typedef struct {
    int M;                     /* total units (capacity)           */
    int available;             /* currently free units             */

    int *allocation;           /* allocation[i] = units held by i  */
    int *max_claim;            /* max[i] = sum of all its requests */
    int *need;                 /* need[i] = max[i] - allocation[i] */

    pthread_mutex_t mtx;
    pthread_cond_t cv;         /* notified when a unit is freed    */
} Banker;

/* ------------------------------------------------------------ */
/*  Thread argument – one per thread                            */
typedef struct {
    int tid;
    Banker *bank;
    int *requests;             /* array of request sizes           */
    int nreq;                  /* length of requests[]             */
} ThreadArg;

/* ------------------------------------------------------------ */
/*  Load requests.txt  →  vector of request lists               */
int **load_requests(int N, const char *fname, int *sizes) {
    int **reqs = calloc(N, sizeof(int*));
    int *cap   = calloc(N, sizeof(int));
    for (int i = 0; i < N; ++i) cap[i] = 8;   /* initial guess */

    FILE *fp = fopen(fname, "r");
    if (!fp) { perror("fopen"); exit(1); }

    int tid, units;
    while (fscanf(fp, "%d %d", &tid, &units) == 2) {
        if (tid < 0 || tid >= N) continue;
        if (sizes[tid] >= cap[tid]) {
            cap[tid] *= 2;
            reqs[tid] = realloc(reqs[tid], cap[tid] * sizeof(int));
        }
        reqs[tid][sizes[tid]++] = units;
    }
    fclose(fp);

    /* shrink to exact size */
    for (int i = 0; i < N; ++i) {
        reqs[i] = realloc(reqs[i], sizes[i] * sizeof(int));
    }
    return reqs;
}

/* ------------------------------------------------------------ */
/*  Compute max_claim[] from the request lists                  */
void init_max_claim(Banker *b, int **reqs, int *sizes, int N) {
    b->max_claim = calloc(N, sizeof(int));
    for (int i = 0; i < N; ++i) {
        for (int j = 0; j < sizes[i]; ++j) {
            b->max_claim[i] += reqs[i][j];
        }
    }
}

/* ------------------------------------------------------------ */
/*  Safety algorithm – returns true if safe                     */
bool is_safe(const Banker *b) {
    int work = b->available;
    bool *finish = calloc(b->allocation[0] ? b->allocation[0] : 1, sizeof(bool));
    int nproc = 0;
    while (b->allocation[nproc]) ++nproc;   /* find N */

    int *alloc_copy = malloc(nproc * sizeof(int));
    int *need_copy  = malloc(nproc * sizeof(int));
    memcpy(alloc_copy, b->allocation, nproc * sizeof(int));
    memcpy(need_copy , b->need       , nproc * sizeof(int));

    while (1) {
        int p = -1;
        for (int i = 0; i < nproc; ++i) {
            if (!finish[i] && need_copy[i] <= work) {
                p = i; break;
            }
        }
        if (p == -1) break;

        work += alloc_copy[p];
        finish[p] = true;
    }

    bool safe = true;
    for (int i = 0; i < nproc; ++i) if (!finish[i]) safe = false;

    free(finish); free(alloc_copy); free(need_copy);
    return safe;
}

/* ------------------------------------------------------------ */
/*  Thread function – one process in the Banker model           */
void *thread_func(void *arg) {
    ThreadArg *ta = (ThreadArg *)arg;
    int tid = ta->tid;
    Banker *b = ta->bank;
    int *my_reqs = ta->requests;
    int nreq = ta->nreq;
    int req_idx = 0;

    while (req_idx < nreq) {
        int K = my_reqs[req_idx];      /* size of current request */
        int granted = 0;

        while (granted < K) {
            pthread_mutex_lock(&b->mtx);

            /* 1. request <= need ? */
            if (1 > b->need[tid]) {
                pthread_mutex_unlock(&b->mtx);
                usleep(10000);          /* 10 ms back-off */
                continue;
            }

            /* 2. request <= available ? */
            if (1 > b->available) {
                pthread_cond_wait(&b->cv, &b->mtx);
                pthread_mutex_unlock(&b->mtx);
                continue;
            }

            /* 3. pretend allocation */
            b->available--;
            b->allocation[tid]++;
            b->need[tid]--;

            /* 4. safety check */
            if (is_safe(b)) {
                /* safe → keep it */
                pthread_mutex_unlock(&b->mtx);
                ++granted;
            } else {
                /* unsafe → rollback */
                b->available++;
                b->allocation[tid]--;
                b->need[tid]++;
                pthread_cond_wait(&b->cv, &b->mtx);
                pthread_mutex_unlock(&b->mtx);
            }
        }

        /* Full request satisfied → next request */
        ++req_idx;
    }

    /* Optional: release everything when done */
    /*
    pthread_mutex_lock(&b->mtx);
    b->available += b->allocation[tid];
    b->allocation[tid] = 0;
    b->need[tid] = b->max_claim[tid];
    pthread_cond_broadcast(&b->cv);
    pthread_mutex_unlock(&b->mtx);
    */

    return NULL;
}

/* ------------------------------------------------------------ */
int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s N M\n", argv[0]);
        return 1;
    }
    int N = atoi(argv[1]);
    int M = atoi(argv[2]);

    if (N < 5 || N > 15 || M < 5 || M > 10) {
        fprintf(stderr, "Constraints: 5<=N<=15, 5<=M<=10\n");
        return 1;
    }

    /* ---- load requests ---- */
    int *sizes = calloc(N, sizeof(int));
    int **reqs = load_requests(N, "requests.txt", sizes);

    /* ---- initialise banker ---- */
    Banker bank = {0};
    bank.M = M;
    bank.available = M;

    bank.allocation = calloc(N, sizeof(int));
    bank.need       = calloc(N, sizeof(int));

    pthread_mutex_init(&bank.mtx, NULL);
    pthread_cond_init(&bank.cv, NULL);

    init_max_claim(&bank, reqs, sizes, N);
    for (int i = 0; i < N; ++i) bank.need[i] = bank.max_claim[i];

    /* ---- spawn threads ---- */
    pthread_t *threads = malloc(N * sizeof(pthread_t));
    ThreadArg *args   = malloc(N * sizeof(ThreadArg));

    for (int i = 0; i < N; ++i) {
        args[i].tid      = i;
        args[i].bank     = &bank;
        args[i].requests = reqs[i];
        args[i].nreq     = sizes[i];
        pthread_create(&threads[i], NULL, thread_func, &args[i]);
    }

    /* ---- wait forever (Ctrl-C to stop) ---- */
    for (int i = 0; i < N; ++i) {
        pthread_join(threads[i], NULL);
    }

    /* ---- cleanup ---- */
    pthread_mutex_destroy(&bank.mtx);
    pthread_cond_destroy(&bank.cv);
    free(bank.allocation);
    free(bank.max_claim);
    free(bank.need);
    for (int i = 0; i < N; ++i) free(reqs[i]);
    free(reqs); free(sizes);
    free(threads); free(args);

    return 0;
}
