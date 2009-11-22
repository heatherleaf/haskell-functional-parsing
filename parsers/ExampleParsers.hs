

--------------------------------------------------
-- a list of all parsers that can be created
-- from the given parser modules


module ExampleParsers (module ExampleParsers,
		       module Standard,
		       module StandardEndo,
		       module ContTrans,
		       module StackTrans,
		       module Stream,
		       module Trie,
		       module AmbTrie,
		       module ExTrie,
		       module AmbExTrie,
		       module PairTrie) where

import Parser

import Standard
import StandardEndo
import ContTrans
import StackTrans
import Stream

import Trie
import AmbTrie
import ExTrie
import AmbExTrie
import PairTrie


type StdCont      s  =  ContTrans  (Standard s)
type StdEndoCont  s  =  ContTrans  (StandardEndo s)
type StdStack     s  =  StackTrans (Standard s)
type StdEndoStack s  =  StackTrans (StandardEndo s) 
type StreamCont   s  =  ContTrans  (Stream s)
type StreamStack  s  =  StackTrans (Stream s)

type PairTrieStd          s  =  PairTrie (Standard s)     s 
type PairTrieStdCont      s  =  PairTrie (StdCont s)      s 
type PairTrieStdEndoCont  s  =  PairTrie (StdEndoCont s)  s 
type PairTrieStdStack     s  =  PairTrie (StdStack s)     s 
type PairTrieStdEndoStack s  =  PairTrie (StdEndoStack s) s 


