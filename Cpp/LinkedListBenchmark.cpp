#include <benchmark/benchmark.h>

#include "LinkedList.hpp"

bool is_odd(int n) { return n % 2 == 1; }
int sum(int a, int b) { return a + b; }

LinkedList<int> createList(int size) {
  auto list = LinkedList<int>();

  for (int i = 0; i < size; i++) {
    list.push(i + 1);
  }

  return list;
}

auto list1 = createList(10);
auto list2 = createList(100);
auto list3 = createList(1000);
auto list4 = createList(10000);
auto list5 = createList(100000);
auto list6 = createList(1000000);

static void BM_SortList1(benchmark::State& state) {
  for (auto _ : state) list1.mergeSort();
}
static void BM_SortList2(benchmark::State& state) {
  for (auto _ : state) list2.mergeSort();
}
static void BM_SortList3(benchmark::State& state) {
  for (auto _ : state) list3.mergeSort();
}
static void BM_SortList4(benchmark::State& state) {
  for (auto _ : state) list4.mergeSort();
}
static void BM_SortList5(benchmark::State& state) {
  for (auto _ : state) list5.mergeSort();
}
static void BM_SortList6(benchmark::State& state) {
  for (auto _ : state) list6.mergeSort();
}
static void BM_SortList7(benchmark::State& state) {
  for (auto _ : state) list7.mergeSort();
}

static void BM_FilterList1(benchmark::State& state) {
  for (auto _ : state) list1.filter(is_odd);
}
static void BM_FilterList2(benchmark::State& state) {
  for (auto _ : state) list2.filter(is_odd);
}
static void BM_FilterList3(benchmark::State& state) {
  for (auto _ : state) list3.filter(is_odd);
}
static void BM_FilterList4(benchmark::State& state) {
  for (auto _ : state) list4.filter(is_odd);
}
static void BM_FilterList5(benchmark::State& state) {
  for (auto _ : state) list5.filter(is_odd);
}
static void BM_FilterList6(benchmark::State& state) {
  for (auto _ : state) list6.filter(is_odd);
}

static void BM_ReduceList1(benchmark::State& state) {
  for (auto _ : state) list1.reduce(sum, 0);
}
static void BM_ReduceList2(benchmark::State& state) {
  for (auto _ : state) list2.reduce(sum, 0);
}
static void BM_ReduceList3(benchmark::State& state) {
  for (auto _ : state) list3.reduce(sum, 0);
}
static void BM_ReduceList4(benchmark::State& state) {
  for (auto _ : state) list4.reduce(sum, 0);
}
static void BM_ReduceList5(benchmark::State& state) {
  for (auto _ : state) list5.reduce(sum, 0);
}
static void BM_ReduceList6(benchmark::State& state) {
  for (auto _ : state) list6.reduce(sum, 0);
}

BENCHMARK(BM_SortList1);
BENCHMARK(BM_SortList2);
BENCHMARK(BM_SortList3);
BENCHMARK(BM_SortList4);
BENCHMARK(BM_SortList5);
BENCHMARK(BM_SortList6);
BENCHMARK(BM_FilterList1);
BENCHMARK(BM_FilterList2);
BENCHMARK(BM_FilterList3);
BENCHMARK(BM_FilterList4);
BENCHMARK(BM_FilterList5);
BENCHMARK(BM_FilterList6);
BENCHMARK(BM_ReduceList1);
BENCHMARK(BM_ReduceList2);
BENCHMARK(BM_ReduceList3);
BENCHMARK(BM_ReduceList4);
BENCHMARK(BM_ReduceList5);
BENCHMARK(BM_ReduceList6);

BENCHMARK_MAIN();
