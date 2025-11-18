#include "stdio.h"
#include <thread>
#include <mutex>
#include <cstring>
#include <cstdlib>
#include <vector>
#include <condition_variable>

#define MAXL 100

struct Bank{
  int M;
  int N;
  int avail;

  int* currS;     // amount held by each thread
  int* maxArr;    // maximum claim for each thread
  int* need;      // current remaining need
  std::mutex mtx;
  std::condition_variable cv;
};

struct twork{
  int id;
  Bank *bank;
  int *req;
  int reqn;
};

int** readreq(int N, const char *fn, int *sizes){
  int **reqs = (int**)calloc(N, sizeof(int*));

  FILE* fp = fopen(fn, "r");
  if (!fp) { perror("No File (requests.txt) found"); exit(1); }

  int tn, num;
  while (fscanf(fp, "%d %d", &tn, &num) == 2){
    if (tn >= N || tn < 0) continue;
    if (reqs[tn] == NULL) {
      reqs[tn] = (int*)calloc(200, sizeof(int));
    }
    reqs[tn][sizes[tn]++] = num;
  }
  fclose(fp);
  return reqs;
}

bool safe(const Bank *b, int np){
  int w = b->avail;

  bool *finish = (bool*)calloc(np, sizeof(bool));
  int *currcp = (int*)malloc(np * sizeof(int));
  int *needcp = (int*)malloc(np * sizeof(int));
  memcpy(currcp, b->currS, np * sizeof(int));
  memcpy(needcp, b->need,  np * sizeof(int));

  while(true) {
    int p = -1;
    for (int i = 0; i < np; ++i){
      if (!finish[i] && needcp[i] <= w) {
        p = i;
        break;
      }
    }
    if (p == -1) break;
    w += currcp[p];
    finish[p] = true;
  }

  bool all_done = true;
  for (int i = 0; i < np; ++i)
    if (!finish[i]) all_done = false;

  free(finish);
  free(currcp);
  free(needcp);
  return all_done;
}

void thread_func(twork* tw){
  int id = tw->id;
  Bank *b = tw->bank;
  int *t_req = tw->req;
  int nreq = tw->reqn;
  int reqi = 0;

  while(reqi < nreq) {
    int k = t_req[reqi];           // ← size of THIS request
    int ndone = 0;

    printf("Thread %d starting request for %d units\n", id, k);

    while (ndone < k) {
      std::unique_lock<std::mutex> lk(b->mtx);

      // Not enough remaining need? (shouldn't happen, but protect)
      if (b->need[id] <= 0) {
        lk.unlock();
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
        continue;
      }

      // Wait for at least one free unit
      if (b->avail < 1) {
        b->cv.wait(lk, [&]{ return b->avail > 0; });
        continue;
      }

      // Pretend allocate one unit
      b->avail--;
      b->currS[id]++;
      b->need[id]--;

      if(safe(b, b->N)){                    // ← FIXED: b->N, not nreq
        lk.unlock();
        ++ndone;

        // PRINT ALLOCATION VECTOR
        {
          std::lock_guard<std::mutex> print_lock(b->mtx);
          printf("Thread %d granted 1 unit → Allocation [");
          for (int i = 0; i < b->N; ++i) {
            printf("%d", b->currS[i]);
            if (i < b->N-1) printf(" ");
          }
          printf("]\n");
          fflush(stdout);
        }
      }
      else {
        // Rollback
        b->avail++;
        b->currS[id]--;
        b->need[id]++;
        b->cv.wait(lk, [&]{ return b->avail > 0; });
      }
    }

    // FULL REQUEST IS DONE → RELEASE ALL k UNITS
    {
      std::lock_guard<std::mutex> lk(b->mtx);
      b->avail += k;
      b->currS[id] -= k;
      b->need[id] += k;
      printf("Thread %d FINISHED request of %d units → RELEASED them (avail = %d)\n",
             id, k, b->avail);
      fflush(stdout);
      b->cv.notify_all();       // Wake everyone up
    }

    ++reqi;
  }

  printf("Thread %d has no more requests — exiting\n", id);
}

int main(int argc, char *argv[]) {
  if (argc < 3) { perror("Not enough Args"); exit(1); }
  int N = atoi(argv[1]);
  int M = atoi(argv[2]);

  if (N < 5 || N > 15 || M < 5 || M > 10) {
    perror("Constraints: 5<=N<=15, 5<=M<=10"); exit(1);
  }

  int *sizes = (int*)calloc(N, sizeof(int));
  int **reqs = readreq(N, "requests.txt", sizes);

  Bank bank = {0};
  bank.M = M;
  bank.N = N;
  bank.avail = M;
  bank.currS = (int*)calloc(N, sizeof(int));
  bank.need   = (int*)calloc(N, sizeof(int));
  bank.maxArr = (int*)calloc(N, sizeof(int));

  // Compute max claim
  for (int i = 0; i < N; ++i){
    for (int j = 0; j < sizes[i]; ++j){
      bank.maxArr[i] += reqs[i][j];
    }
    bank.need[i] = bank.maxArr[i];
  }

  std::vector<std::thread> threads;
  std::vector<twork> args(N);

  for (int i = 0; i < N; ++i){
    args[i].id    = i;
    args[i].bank  = &bank;
    args[i].req   = reqs[i];
    args[i].reqn  = sizes[i];
    threads.emplace_back(thread_func, &args[i]);
  }

  for (auto& t : threads) t.join();

  // Cleanup
  free(bank.currS);
  free(bank.maxArr);
  free(bank.need);
  for (int i = 0; i < N; ++i) free(reqs[i]);
  free(reqs);
  free(sizes);

  return 0;
}
