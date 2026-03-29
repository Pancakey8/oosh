{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Bridge where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Control.Monad
import Foreign.Marshal.Utils (with)
import Numeric (showHex)

import Syntax

#include "wrapper.h"

instance Storable StrContent where
    sizeOf _    = #size struct StrContent
    alignment _ = #alignment struct StrContent
    
    poke ptr (Exact s) = do
        (#poke struct StrContent, kind) ptr ((#const Exact) :: Word8)
        cstr <- newCString s
        (#poke struct StrContent, data.exact) ptr cstr
        
    poke ptr (Subst lit) = do
        (#poke struct StrContent, kind) ptr ((#const Subst) :: Word8)
        litPtr <- malloc
        poke litPtr lit
        (#poke struct StrContent, data.subst) ptr litPtr

    peek _ = error "Peek not implemented for StrContent"

data NumLitData = NumLitData (Ptr CChar) (Ptr CChar)

instance Storable NumLitData where
  sizeOf _ =
    #size struct NumLitData
  alignment _ =
    #alignment struct NumLitData

  poke ptr (NumLitData whole frac) = do
    (#poke struct NumLitData, whole) ptr whole
    (#poke struct NumLitData, frac) ptr frac

data StrLitData = StrLitData (Ptr StrContent) CSize

instance Storable StrLitData where
  sizeOf _ =
    #size struct StrLitData
  alignment _ =
    #alignment struct StrLitData

  poke ptr (StrLitData contents len) = do
    (#poke struct StrLitData, contents) ptr contents
    (#poke struct StrLitData, contents_len) ptr len

instance Storable Literal where
    sizeOf _    = #size struct Literal
    alignment _ = #alignment struct Literal

    poke ptr (NumLit w f) = do
        (#poke struct Literal, kind) ptr ((#const NumLit) :: Word8)
        cWhole <- newCString w
        cFrac  <- newCString f
        (#poke struct Literal, data.num) ptr (NumLitData cWhole cFrac)

    poke ptr (StrLit contents) = do
        (#poke struct Literal, kind) ptr ((#const StrLit) :: Word8)
        let len = length contents
        arrayPtr <- mallocArray len :: IO (Ptr StrContent)
        forM_ (zip [0..] contents) $ \(i, c) -> do
          pokeElemOff arrayPtr i c
        (#poke struct Literal, data.str) ptr (StrLitData arrayPtr (fromIntegral len))

    poke ptr (VarLit name) = do
        (#poke struct Literal, kind) ptr ((#const VarLit) :: Word8)
        cstr <- newCString name
        (#poke struct Literal, data.var) ptr cstr

    poke _ (PathLit _) = error "TODO: PathContent not implemented"

    peek _ = error "Peek not implemented for Literal"

foreign import ccall "stringifyLiteral" c_stringifyLiteral :: Ptr () -> IO ()

dumpBytes :: Ptr a -> Int -> IO ()
dumpBytes p n = do
  bs <- peekArray n (castPtr p :: Ptr Word8)
  putStrLn $ unwords (map (\b -> let h = showHex b "" in if length h == 1 then '0':h else h) bs)

stringifyLiteral :: Literal -> IO ()
stringifyLiteral lit = do
  ptr <- malloc :: IO (Ptr Literal) -- TODO: This leaks
  poke ptr lit
  c_stringifyLiteral (castPtr ptr)
