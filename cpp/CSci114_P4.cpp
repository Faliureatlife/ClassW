#include <cstdio> 
#include <thread>
#include <mutex>
#include <cstring>
#include <cstdlib>
#include <vector>
#include <condition_variable>

//I did use Grok-3-Fast in order to understand the assignment and to figure out an approach to the assignment, however I did not copy code from the chat for use here
//video of the conversation can be found at https://files.catbox.moe/qrax7n.mp4
//I compiled and ran the program with $g++ CSci114_P4.cpp && ./a.out 12 8
//each line prints either the beginning of a request or the result of allocating a single resource or freeing a thread
struct Bank{
  int M;
  int N;
  int avail;

  int* currS; //amt held by each thread
  int* maxArr; //max for each process
  int* need; //current need vec
  std::mutex mtx;
  std::condition_variable cv;
};

struct twork{
  int id;
  Bank *bank;
  int *req;
  int reqn;
};


int** readreq(int N, int M, const char *fn, int *sizes){
  int **reqs = (int**)calloc(N, sizeof(int*));

  FILE* fp = fopen(fn, "r");
  if (!fp) {perror("No File (reqeusts.txt) found");exit(1);}
  int tn, num;
  while (fscanf(fp, "%d %d", &tn, &num) == 2){
    if (tn >= N || num >= M) continue; //might need to iterate?
    if (reqs[tn] == NULL) {
      reqs[tn] = (int*)calloc(25565,sizeof(int));
    }
    reqs[tn][sizes[tn]++] = num;
  }
  fclose(fp);
  return reqs;
}

bool safe(const Bank *b, int np, int requesting_thread, int remaining_in_current_request) {
    int work = b->avail - remaining_in_current_request;
    if (work < 0) return false;  // not enough even for this request

    // Copy current state
    bool *finish = (bool*)calloc(np, sizeof(bool));
    int *alloc_copy = (int*)malloc(np * sizeof(int));
    memcpy(alloc_copy, b->currS, np * sizeof(int));

    if (requesting_thread != -1) {
        alloc_copy[requesting_thread] += remaining_in_current_request;
    }

    while (true) {
        int p = -1;
        for (int i = 0; i < np; ++i) {
            if (finish[i]) continue;

            bool can_run = false;
            if (i == requesting_thread) {
                can_run = true;
            } else {
                can_run = true;
            }

            if (can_run) {
                p = i;
                break;
            }
        }
        if (p == -1) break;
        work += alloc_copy[p];
        finish[p] = true;
    }

    bool all_done = true;
    for (int i = 0; i < np; ++i)
        if (!finish[i]) all_done = false;

    free(finish);
    free(alloc_copy);
    return all_done;
}

void thread_func(twork* tw){
  int id = tw->id;
  Bank *b = tw->bank;
  int *t_req = tw->req;
  int nreq = tw->reqn;
  int reqi = 0;

  while(reqi < nreq) {
    int k = t_req[reqi];
    int ndone = 0;
    printf("Thread %d requesting %d\n",id, k);
    while (ndone < k) {
      std::unique_lock<std::mutex> lk(b->mtx);
      if (b->need[id] <= 0) {
        lk.unlock();
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
        continue;
      }
      if (b->avail < 1) {
        b->cv.wait(lk, [&]{return b->avail > 0;});
        continue;
      }
      b->avail--;
      b->currS[id]++;
      b->need[id]--;
      if(safe(b,b->N,id, k - ndone )){
        lk.unlock();
        ++ndone;
        {
          std::lock_guard<std::mutex> print_lock(b->mtx);
          printf("Allocation [");
          for (int i = 0; i < b->N; ++i){
            printf("%d ",b->currS[i]);
            if (i < b->N -1) printf(" ");
          }
          printf("] (avail = %d)\n", b->avail);
          fflush(stdout);
        }
      } else {
        b->avail++;
        b->currS[id]--;
        b->need[id]++;
        b->cv.wait(lk, [&]{ return b->avail> 0; });
      }
    }
    {
      std::lock_guard<std::mutex> lk(b->mtx);
      b->avail +=k;
      b->currS[id] -= k;
      b->need[id] += k;
      b->cv.notify_all();
      printf("Allocation [");
      for (int i = 0; i < b->N; ++i){
        printf("%d ",b->currS[i]);
        if (i < b->N -1) printf(" ");
      }
      printf("] (avail = %d)\n", b->avail);
      fflush(stdout);
    }
    ++reqi;
  }
  printf("Thread %d completed\n",id);
}

int main (int argc, char *argv[]) {
  if (argc < 3) perror("Not enough Args"); 
  int N = atoi(argv[1]);
  int M = atoi(argv[2]);

  if (N < 5 || N > 15 || M < 5 || M > 10) perror("Constraints: 5<=N<=15, 5<=M<=10");

  int *sizes = (int*)calloc(N, sizeof(int)); //num requests 
  int **reqs = readreq(N, M, "requests.txt", sizes);

  Bank bank = {0};
  bank.M = M;
  bank.N = N;
  bank.avail = M;
  bank.currS = (int*)calloc(N, sizeof(int));
  bank.need = (int*)calloc(N,sizeof(int));

  bank.maxArr = (int*)calloc(N,sizeof(int));
  for (int i = 0; i < N; ++i){ //each thread
    for (int j = 0; j < sizes[i]; ++j) { //amt of times asked for 
      bank.maxArr[i] += reqs[i][j]; //im too tired to remember
    }
    bank.need[i] = bank.maxArr[i];
  }

  std::vector<std::thread> threads;
  std::vector<twork> args(N);
  for (int i = 0; i < N; ++i){ //creating each obj
    args[i].id = i;
    args[i].bank = &bank;
    args[i].req = reqs[i];
    args[i].reqn = sizes[i];
    threads.emplace_back(thread_func, &args[i]);
  }

  for (auto& t: threads) t.join(); //using fancy for loop so we dont have to track the vector

  free(bank.currS);
  free(bank.maxArr);
  free(bank.need);
  for (int i = 0; i < N; ++i) free (reqs[i]);
  free(reqs);
  free(sizes);

  return 0;
}
