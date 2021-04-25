#include <benchmark/benchmark.h>

#include "LinkedList.cpp"

bool is_odd(int n) { return n % 2 == 1; }
int sum(int a, int b) { return a + b; }

LinkedList<int> createList(int size) {
  auto list = LinkedList<int>();

  for (int i = 0; i < size; i++) {
    list.push(i + 1);
  }

  return list;
}

auto list1 = createList(10000);
auto list2 = createList(100000);
auto list3 = createList(1000000);

static void BM_SortList10000(benchmark::State& state) {
  for (auto _ : state) {
    list1.mergeSort();
  }
}
static void BM_SortList100000(benchmark::State& state) {
  for (auto _ : state) {
    list2.mergeSort();
  }
}
static void BM_SortList1000000(benchmark::State& state) {
  for (auto _ : state) {
    list3.mergeSort();
  }
}

static void BM_FilterList10000(benchmark::State& state) {
  for (auto _ : state) {
    list1.filter(is_odd);
  }
}
static void BM_FilterList100000(benchmark::State& state) {
  for (auto _ : state) {
    list2.filter(is_odd);
  }
}
static void BM_FilterList1000000(benchmark::State& state) {
  for (auto _ : state) {
    list3.filter(is_odd);
  }
}

static void BM_ReduceList10000(benchmark::State& state) {
  for (auto _ : state) {
    list1.reduce(sum, 0);
  }
}
static void BM_ReduceList100000(benchmark::State& state) {
  for (auto _ : state) {
    list2.reduce(sum, 0);
  }
}
static void BM_ReduceList1000000(benchmark::State& state) {
  for (auto _ : state) {
    list3.reduce(sum, 0);
  }
}

BENCHMARK(BM_SortList10000);
BENCHMARK(BM_SortList100000);
BENCHMARK(BM_SortList1000000);
BENCHMARK(BM_FilterList10000);
BENCHMARK(BM_FilterList100000);
BENCHMARK(BM_FilterList1000000);
BENCHMARK(BM_ReduceList10000);
BENCHMARK(BM_ReduceList100000);
BENCHMARK(BM_ReduceList1000000);

BENCHMARK_MAIN();
