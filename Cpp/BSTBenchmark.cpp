#include <benchmark/benchmark.h>
#include <math.h>

#include "BinarySearchTree.hpp"

using namespace std;

long long double_n(long long n) { return n * 2; }

vector<long long>* listFromRange(long long begin, long long end) {
  auto l = new vector<long long>();

  for (long long i = begin - 1; i < end; i++) l->push_back(i + 1);

  return l;
}

void treeFromList(vector<long long> list, BinarySearchTree<long long>* tree) {
  if (list.empty()) return;
  if (list.size() == 1) {
    tree->push(list.front());
    return;
  };

  long long middleIdx = floor(list.size() / 2.0);
  tree->push(list[middleIdx]);

  treeFromList(vector<long long>(list.begin(), list.begin() + middleIdx), tree);
  treeFromList(vector<long long>(list.begin() + middleIdx + 1, list.end()),
               tree);
}

BinarySearchTree<long long>* treeFromRange(long long from, long long to) {
  auto tree = new BinarySearchTree<long long>();
  treeFromList(*listFromRange(from, to), tree);

  return tree;
}

static void BM_Contains(benchmark::State& state) {
  auto size = state.range(0);
  auto tree = treeFromRange(1, size);

  for (auto _ : state) tree->contains(size);
}

static void BM_Map(benchmark::State& state) {
  auto size = state.range(0);
  auto tree = treeFromRange(1, size);

  for (auto _ : state) tree->map(double_n);
}

BENCHMARK(BM_Contains)
    ->Arg(10)
    ->Arg(100)
    ->Arg(1000)
    ->Arg(10000)
    ->Arg(100000)
    ->Arg(250000)
    ->Arg(500000)
    ->Arg(750000)
    ->Arg(1000000);

BENCHMARK(BM_Map)
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
