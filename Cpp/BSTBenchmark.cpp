#include <benchmark/benchmark.h>
#include <math.h>

#include "BinarySearchTree.hpp"

using namespace std;

int double_n(int n) { return n * 2; }

vector<int>* listFromRange(int begin, int end) {
  auto l = new vector<int>();

  for (int i = begin - 1; i < end; i++) l->push_back(i + 1);

  return l;
}

void treeFromList(vector<int> list, BinarySearchTree<int>* tree) {
  if (list.empty()) return;
  if (list.size() == 1) {
    tree->push(list.front());
    return;
  };

  int middleIdx = floor(list.size() / 2.0);
  tree->push(list[middleIdx]);

  treeFromList(vector<int>(list.begin(), list.begin() + middleIdx), tree);
  treeFromList(vector<int>(list.begin() + middleIdx + 1, list.end()), tree);
}

BinarySearchTree<int>* treeFromRange(int from, int to) {
  auto tree = new BinarySearchTree<int>();
  treeFromList(*listFromRange(from, to), tree);

  return tree;
}

auto tree1 = treeFromRange(1, 10);
auto tree2 = treeFromRange(1, 100);
auto tree3 = treeFromRange(1, 1000);
auto tree4 = treeFromRange(1, 10000);
auto tree5 = treeFromRange(1, 100000);
auto tree6 = treeFromRange(1, 1000000);

static void BM_ContainsT1(benchmark::State& state) {
  for (auto _ : state) tree1->contains(10);
}

static void BM_ContainsT2(benchmark::State& state) {
  for (auto _ : state) tree2->contains(100);
}

static void BM_ContainsT3(benchmark::State& state) {
  for (auto _ : state) tree3->contains(1000);
}

static void BM_ContainsT4(benchmark::State& state) {
  for (auto _ : state) tree4->contains(10000);
}

static void BM_ContainsT5(benchmark::State& state) {
  for (auto _ : state) tree5->contains(100000);
}

static void BM_ContainsT6(benchmark::State& state) {
  for (auto _ : state) tree6->contains(1000000);
}

static void BM_MapT1(benchmark::State& state) {
  for (auto _ : state) tree1->map(double_n);
}

static void BM_MapT2(benchmark::State& state) {
  for (auto _ : state) tree2->map(double_n);
}

static void BM_MapT3(benchmark::State& state) {
  for (auto _ : state) tree3->map(double_n);
}

static void BM_MapT4(benchmark::State& state) {
  for (auto _ : state) tree4->map(double_n);
}

static void BM_MapT5(benchmark::State& state) {
  for (auto _ : state) tree5->map(double_n);
}

static void BM_MapT6(benchmark::State& state) {
  for (auto _ : state) tree6->map(double_n);
}

// int main(int argc, char const* argv[]) {
  // auto list = listFromRange(1, 10);
  // int middleIdx = floor(list->size() / 2);
  // auto v = vector<int>(list->begin(), list->begin() + middleIdx);

  // for (auto i : v) {
  //   cout << i << " ";
  // }
  // cout << endl;

  // auto v2 = vector<int>(list->begin() + middleIdx, list->end());

  // for (auto i : v2) {
  //   cout << i << " ";
  // }
  // cout << endl;
//   cout << endl << tree1->debug() << endl;
//   return 0;
// }

BENCHMARK(BM_ContainsT1);
BENCHMARK(BM_ContainsT2);
BENCHMARK(BM_ContainsT3);
BENCHMARK(BM_ContainsT4);
BENCHMARK(BM_ContainsT5);
BENCHMARK(BM_ContainsT6);
BENCHMARK(BM_MapT1);
BENCHMARK(BM_MapT2);
BENCHMARK(BM_MapT3);
BENCHMARK(BM_MapT4);
BENCHMARK(BM_MapT5);
BENCHMARK(BM_MapT6);

BENCHMARK_MAIN();
