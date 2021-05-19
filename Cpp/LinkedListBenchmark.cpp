#include <benchmark/benchmark.h>

#include "LinkedList.hpp"

bool is_odd(long long n) { return n % 2 == 1; }
long long sum(long long a, long long b) { return a + b; }

LinkedList<long long> createList(int size) {
  auto list = LinkedList<long long>();

  for (int i = 0; i < size; i++) {
    list.push(i + 1);
  }

  return list;
}

static void BM_SortList(benchmark::State& state) {
  auto size = state.range(0);
  auto list = createList(size);

  for (auto _ : state) list.mergeSort();
}

static void BM_FilterList(benchmark::State& state) {
  auto size = state.range(0);
  auto list = createList(size);
  for (auto _ : state) list.filter(is_odd);
}

static void BM_ReduceList(benchmark::State& state) {
  auto size = state.range(0);
  auto list = createList(size);
  for (auto _ : state) list.reduce(sum, 0);
}

BENCHMARK(BM_SortList)
    ->Arg(10)
    ->Arg(100)
    ->Arg(1000)
    ->Arg(10000)
    ->Arg(100000)
    ->Arg(250000)
    ->Arg(500000)
    ->Arg(750000)
    ->Arg(1000000);

BENCHMARK(BM_FilterList)
    ->Arg(10)
    ->Arg(100)
    ->Arg(1000)
    ->Arg(10000)
    ->Arg(100000)
    ->Arg(250000)
    ->Arg(500000)
    ->Arg(750000)
    ->Arg(1000000)
    ->Iterations(1);

BENCHMARK(BM_ReduceList)
    ->Arg(10)
    ->Arg(100)
    ->Arg(1000)
    ->Arg(10000)
    ->Arg(100000)
    ->Arg(250000)
    ->Arg(500000)
    ->Arg(750000)
    ->Arg(1000000);

BENCHMARK_MAIN();
