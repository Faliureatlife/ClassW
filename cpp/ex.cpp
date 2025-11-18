/*  P4.cpp  –  Banker's Algorithm (single resource)  */
/*  C++ with <mutex> & <thread>  –  NO pthreads       */
/*  Compile:  g++ -std=c++20 -o P4 P4.cpp -lpthread   */

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <thread>
#include <condition_variable>
#include <vector>

/* ------------------------------------------------------------ */
/*  Global banker state – protected by std::mutex + cv          */
struct Banker {
    int M;                     /* total units (capacity)           */
    int available;             /* currently free units             */

    int *allocation;           /* allocation[i] = units held by i  */
    int *max_claim;            /* max[i] = sum of all its requests */
    int *need;                 /* need[i] = max[i] - allocation[i] */

    std::mutex mtx;
    std::condition_variable cv;/* notified when a unit is freed    */
};

/* ------------------------------------------------------------ */
/*  Thread argument – one per thread                            */
struct ThreadArg {
    int tid;
    Banker *bank;
    int *requests;             /* array of request sizes           */
    int nreq;                  /* length of requests[]             */
};

/* ------------------------------------------------------------ */
/*  Load requests.txt  →  vector of request lists (C-style)     */
int **load_requests(int N, const char *fname, int *sizes) {
    int **reqs = (int**)calloc(N, sizeof(int*));
    int *cap   = (int*)calloc(N, sizeof(int));
    for (int i = 0; i < N; ++i) cap[i] = 8;

    FILE *fp = fopen(fname, "r");
    if (!fp) { perror("fopen"); exit(1); }

    int tid, units;
    while (fscanf(fp, "%d %d", &tid, &units) == 2) {
        if (tid < 0 || tid >= N) continue;
        if (sizes[tid] >= cap[tid]) {
            cap[tid] *= 2;
            reqs[tid] = (int*)realloc(reqs[tid], cap[tid] * sizeof(int));
        }
        reqs[tid][sizes[tid]++] = units;
    }
    fclose(fp);

    for (int i = 0; i < N; ++i) {
        reqs[i] = (int*)realloc(reqs[i], sizes[i] * sizeof(int));
    }
    return reqs;
}

/* ------------------------------------------------------------ */
/*  Compute max_claim[] from the request lists                  */
void init_max_claim(Banker *b, int **reqs, int *sizes, int N) {
    b->max_claim = (int*)calloc(N, sizeof(int));
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
    int nproc = 0;
    while (b->allocation[nproc]) ++nproc;   /* find N */

    bool *finish = (bool*)calloc(nproc, sizeof(bool));
    int *alloc_copy = (int*)malloc(nproc * sizeof(int));
    int *need_copy  = (int*)malloc(nproc * sizeof(int));
    memcpy(alloc_copy, b->allocation, nproc * sizeof(int));
    memcpy(need_copy , b->need       , nproc * sizeof(int));

    while (true) {
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
void thread_func(ThreadArg *ta) {
    int tid = ta->tid;
    Banker *b = ta->bank;
    int *my_reqs = ta->requests;
    int nreq = ta->nreq;
    int req_idx = 0;

    while (req_idx < nreq) {
        int K = my_reqs[req_idx];
        int granted = 0;

        while (granted < K) {
            std::unique_lock<std::mutex> lk(b->mtx);

            if (1 > b->need[tid]) {
                lk.unlock();
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
                continue;
            }

            if (1 > b->available) {
                b->cv.wait(lk, [&]{ return b->available > 0; });
                continue;
            }

            /* pretend allocation */
            b->available--;
            b->allocation[tid]++;
            b->need[tid]--;

            if (is_safe(b)) {
                lk.unlock();
                ++granted;
            } else {
                /* rollback */
                b->available++;
                b->allocation[tid]--;
                b->need[tid]++;
                b->cv.wait(lk, [&]{ return b->available > 0; });
            }
        }

        ++req_idx;
    }

    /* Optional release */
    /*
    {
        std::lock_guard<std::mutex> lk(b->mtx);
        b->available += b->allocation[tid];
        b->allocation[tid] = 0;
        b->need[tid] = b->max_claim[tid];
        b->cv.notify_all();
    }
    */
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

    int *sizes = (int*)calloc(N, sizeof(int));
    int **reqs = load_requests(N, "requests.txt", sizes);

    Banker bank = {0};
    bank.M = M;
    bank.available = M;
    bank.allocation = (int*)calloc(N, sizeof(int));
    bank.need       = (int*)calloc(N, sizeof(int));

    init_max_claim(&bank, reqs, sizes, N);
    for (int i = 0; i < N; ++i) bank.need[i] = bank.max_claim[i];

    std::vector<std::thread> threads;
    std::vector<ThreadArg> args(N);

    for (int i = 0; i < N; ++i) {
        args[i].tid      = i;
        args[i].bank     = &bank;
        args[i].requests = reqs[i];
        args[i].nreq     = sizes[i];
        threads.emplace_back(thread_func, &args[i]);
    }

    for (auto& t : threads) t.join();

    /* cleanup */
    free(bank.allocation);
    free(bank.max_claim);
    free(bank.need);
    for (int i = 0; i < N; ++i) free(reqs[i]);
    free(reqs); free(sizes);

    return 0;
}
