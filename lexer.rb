# frozen_string_literal: true

require 'bundler/inline'

gemfile do
  source 'http://rubygems.org'

  gem 'rouge'
end

ts_source = File.read('./TypeScript/LinkedList.ts')
hs_source = File.read('./Haskell/LinkedList.hs')
cpp_source = File.read('./Cpp/LinkedList.hpp')
elm_source = File.read('./Elm/src/LinkedList.elm')
ts_count = Rouge::Lexers::Typescript.new.lex(ts_source).filter {!(/\s/ === _1[1])}.count
hs_count = Rouge::Lexers::Haskell.new.lex(hs_source).filter {!(/\s/ === _1[1])}.count
cpp_count = Rouge::Lexers::Cpp.new.lex(cpp_source).filter {!(/\s/ === _1[1])}.count
elm_count = Rouge::Lexers::Elm.new.lex(elm_source).filter {!(/\s/ === _1[1])}.count

puts "LinkedList.cpp: #{cpp_count} tokens"
puts "LinkedList.elm: #{elm_count} tokens"
puts "LinkedList.hs: #{hs_count} tokens"
puts "LinkedList.ts: #{ts_count} tokens"

ts_source = File.read('./TypeScript/BinarySearchTree.ts')
hs_source = File.read('./Haskell/BinarySearchTree.hs')
cpp_source = File.read('./Cpp/BinarySearchTree.cpp')
elm_source = File.read('./Elm/src/BinarySearchTree.elm')
ts_count = Rouge::Lexers::Typescript.new.lex(ts_source).filter {!(/\s/ === _1[1])}.count
hs_count = Rouge::Lexers::Haskell.new.lex(hs_source).filter {!(/\s/ === _1[1])}.count
cpp_count = Rouge::Lexers::Cpp.new.lex(cpp_source).filter {!(/\s/ === _1[1])}.count
elm_count = Rouge::Lexers::Elm.new.lex(elm_source).filter {!(/\s/ === _1[1])}.count

puts "BST.cpp: #{cpp_count} tokens"
puts "BST.elm: #{elm_count} tokens"
puts "BST.hs: #{hs_count} tokens"
puts "BST.ts: #{ts_count} tokens"
