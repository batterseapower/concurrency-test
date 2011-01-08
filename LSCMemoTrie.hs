{-# LANGUAGE TypeOperators, CPP #-}
import Data.Int
import Data.Word

import Data.MemoTrie        -- Needed to modify this so we export (:->:) constructors
import Test.LazySmallCheck  -- Needed to modify this so we can generate infinite data structures


class HasTrie a => HasSerialTrie a where
    seriesTrie :: Series b -> Series (a :->: b)

instance HasSerialTrie () where
    seriesTrie series = cons UnitTrie >< series

instance HasSerialTrie Bool where
    seriesTrie series = cons BoolTrie >< series >< series

instance (HasSerialTrie a, HasSerialTrie b) => HasSerialTrie (Either a b) where
    seriesTrie series = cons EitherTrie >< seriesTrie series >< seriesTrie series

instance (HasSerialTrie a, HasSerialTrie b) => HasSerialTrie (a, b) where
    seriesTrie series = cons PairTrie >< seriesTrie (seriesTrie series)

instance (HasSerialTrie a, HasSerialTrie b, HasSerialTrie c) => HasSerialTrie (a, b, c) where
    seriesTrie series = cons TripleTrie >< seriesTrie series

instance HasSerialTrie a => HasSerialTrie [a] where
    seriesTrie series = cons ListTrie >< seriesTrie series

#define SimpleInstance(IntType,TrieType) \
instance HasSerialTrie IntType where \
    seriesTrie series = cons TrieType >< seriesTrie series

SimpleInstance(Char,CharTrie)
SimpleInstance(Word,WordTrie)
SimpleInstance(Word8,Word8Trie)
SimpleInstance(Word16,Word16Trie)
SimpleInstance(Word32,Word32Trie)
SimpleInstance(Word64,Word64Trie)
SimpleInstance(Int,IntTrie)
SimpleInstance(Int8,Int8Trie)
SimpleInstance(Int16,Int16Trie)
SimpleInstance(Int32,Int32Trie)
SimpleInstance(Int64,Int64Trie)
SimpleInstance(Integer,IntegerTrie)


instance (HasSerialTrie a, Serial b) => Serial (a -> b) where
    series = cons untrie >< seriesTrie series . (+1)


instance Show (a -> b) where
    show _ = "<function>" -- FIXME: could print a finite representation of function in the form of the MemoTrie


prop_fun :: (Bool -> Bool) -> Bool
prop_fun f = f True == True && f False == False


main :: IO ()
main = test prop_fun
