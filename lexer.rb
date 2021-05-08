# frozen_string_literal: true

require "bundler/inline"

gemfile do
  ruby "2.7.2"
  source "http://rubygems.org"

  gem "rouge"
end

TYPESCRIPT_LEXER = Rouge::Lexers::Typescript.new
HASKELL_LEXER = Rouge::Lexers::Haskell.new
CPP_LEXER = Rouge::Lexers::Cpp.new
ELM_LEXER = Rouge::Lexers::Elm.new

linked_list_ts = File.read("./TypeScript/LinkedList.ts")
linked_list_hs = File.read("./Haskell/LinkedList.hs")
linked_list_cpp = File.read("./Cpp/LinkedList.hpp")
linked_list_elm = File.read("./Elm/src/LinkedList.elm")
bst_ts = File.read("./TypeScript/BinarySearchTree.ts")
bst_hs = File.read("./Haskell/BinarySearchTree.hs")
bst_cpp = File.read("./Cpp/BinarySearchTree.hpp")
bst_elm = File.read("./Elm/src/BinarySearchTree.elm")

ll_ts_count = TYPESCRIPT_LEXER.lex(linked_list_ts).filter {!(/\s/ === _1[1])}.count
ll_hs_count = HASKELL_LEXER.lex(linked_list_hs).filter {!(/\s/ === _1[1])}.count
ll_cpp_count = CPP_LEXER.lex(linked_list_cpp).filter {!(/\s/ === _1[1])}.count
ll_elm_count = ELM_LEXER.lex(linked_list_elm).filter {!(/\s/ === _1[1])}.count

bst_ts_count = TYPESCRIPT_LEXER.lex(bst_ts).filter {!(/\s/ === _1[1])}.count
bst_hs_count = HASKELL_LEXER.lex(bst_hs).filter {!(/\s/ === _1[1])}.count
bst_cpp_count = CPP_LEXER.lex(bst_cpp).filter {!(/\s/ === _1[1])}.count
bst_elm_count = ELM_LEXER.lex(bst_elm).filter {!(/\s/ === _1[1])}.count

puts "Linked List"
puts "  - Cpp: #{ll_cpp_count} tokens"
puts "  - Elm: #{ll_elm_count} tokens"
puts "  - Haskell: #{ll_hs_count} tokens"
puts "  - Typescript: #{ll_ts_count} tokens"
puts
puts "Binary Search Tree"
puts "  - Cpp: #{bst_cpp_count} tokens"
puts "  - Elm: #{bst_elm_count} tokens"
puts "  - Haskell: #{bst_hs_count} tokens"
puts "  - Typescript: #{bst_ts_count} tokens"
