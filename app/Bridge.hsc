{-# OPTIONS_GHC -fno-warn-orphans #-} -- Orphans are intentional
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Bridge where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Control.Monad

import IR
import Syntax (Operator (..))

#include "wrapper.h"

instance Storable TemplatePart where
  sizeOf _ = #size struct TemplatePart
  alignment _ = #alignment struct TemplatePart

  poke ptr (TempExact s) = do
    (#poke struct TemplatePart, kind) ptr ((#const TempExact) :: CInt)
    cstr <- newCString s
    (#poke struct TemplatePart, data.exact) ptr cstr

  poke ptr (TempVar s) = do
    (#poke struct TemplatePart, kind) ptr ((#const TempVar) :: CInt)
    cstr <- newCString s
    (#poke struct TemplatePart, data.var) ptr cstr

  peek _ = error "C->HS interaction not implemented"

instance Storable PathPart where
  sizeOf _ = #size struct PathPart
  alignment _ = #alignment struct TemplatePart

  poke ptr (PathExact s) = do
    (#poke struct PathPart, kind) ptr ((#const PathExact) :: CInt)
    cstr <- newCString s
    (#poke struct PathPart, data.exact) ptr cstr

  poke ptr PathGlob = (#poke struct PathPart, kind) ptr ((#const PathGlob) :: CInt)
  poke ptr PathRecGlob = (#poke struct PathPart, kind) ptr ((#const PathRecGlob) :: CInt)

  poke ptr (PathTmp ts) = do
    (#poke struct PathPart, kind) ptr ((#const PathTmp) :: CInt)
    let len = length ts
    array <- mallocArray len :: IO (Ptr TemplatePart)
    forM_ (zip [0..] ts) $ \(i, t) -> pokeElemOff array i t
    (#poke struct PathPart, data.tmp.parts) ptr array
    (#poke struct PathPart, data.tmp.parts_length) ptr len

  peek _ = error "C->HS interaction not implemented"

instance Storable IRInstr where
  sizeOf _ = #size struct IRInstr
  alignment _ = #alignment struct IRInstr

  poke ptr (PushNum n) = do
    (#poke struct IRInstr, kind) ptr ((#const PushNum) :: CInt)
    (#poke struct IRInstr, data.num) ptr n

  poke ptr (PushTemplate ts) = do
    (#poke struct IRInstr, kind) ptr ((#const PushTemplate) :: CInt)
    let len = length ts
    array <- mallocArray len :: IO (Ptr TemplatePart)
    forM_ (zip [0..] ts) $ \(i, t) -> pokeElemOff array i t
    (#poke struct IRInstr, data.tmp.parts) ptr array
    (#poke struct IRInstr, data.tmp.parts_length) ptr len

  poke ptr (PushPath ts) = do
    (#poke struct IRInstr, kind) ptr ((#const PushPath) :: CInt)
    let len = length ts
    array <- mallocArray len :: IO (Ptr PathPart)
    forM_ (zip [0..] ts) $ \(i, t) -> pokeElemOff array i t
    (#poke struct IRInstr, data.path.parts) ptr array
    (#poke struct IRInstr, data.path.parts_length) ptr len

  poke ptr (PushFn ts) = do
    (#poke struct IRInstr, kind) ptr ((#const PushFn) :: CInt)
    let len = length ts
    array <- mallocArray len :: IO (Ptr IRInstr)
    forM_ (zip [0..] ts) $ \(i, t) -> pokeElemOff array i t
    (#poke struct IRInstr, data.fn.instrs) ptr array
    (#poke struct IRInstr, data.fn.instrs_length) ptr len

  poke ptr (PushArray n) = do
    (#poke struct IRInstr, kind) ptr ((#const PushArray) :: CInt)
    (#poke struct IRInstr, data.push_array) ptr (fromIntegral n :: CSize)

  poke ptr (LoadVar s) = do
    (#poke struct IRInstr, kind) ptr ((#const LoadVar) :: CInt)
    cstr <- newCString s
    (#poke struct IRInstr, data.load) ptr cstr

  poke ptr (StoreVar s) = do
    (#poke struct IRInstr, kind) ptr ((#const StoreVar) :: CInt)
    cstr <- newCString s
    (#poke struct IRInstr, data.store) ptr cstr

  poke ptr (DefineVar s) = do
    (#poke struct IRInstr, kind) ptr ((#const DefineVar) :: CInt)
    cstr <- newCString s
    (#poke struct IRInstr, data.define) ptr cstr

  poke ptr (CallCommand n) = do
    (#poke struct IRInstr, kind) ptr ((#const CallCommand) :: CInt)
    (#poke struct IRInstr, data.call_cmd) ptr (fromIntegral n :: CSize)

  poke ptr (CallFunction n) = do
    (#poke struct IRInstr, kind) ptr ((#const CallFunction) :: CInt)
    (#poke struct IRInstr, data.call_fn) ptr (fromIntegral n :: CSize)

  poke ptr PipeTo = do
    (#poke struct IRInstr, kind) ptr ((#const PipeTo) :: CInt)

  poke ptr (ApplyOp op) = do
    (#poke struct IRInstr, kind) ptr ((#const ApplyOp) :: CInt)
    (#poke struct IRInstr, data.apply_op) ptr
      ((case op of
         OpPlus -> (#const OpPlus)
         OpMinus -> (#const OpMinus)
         OpAst -> (#const OpAst)
         OpSlash -> (#const OpSlash)
         OpIndex -> (#const OpIndex)
         OpJuxta -> error "Juxta can't appear here"
         OpPipe -> error "Pipe can't appear here") :: CInt)

  poke ptr Drop = do
    (#poke struct IRInstr, kind) ptr ((#const Drop) :: CInt)

  peek _ = error "C->HS interaction not implemented"

type ProgramStatePtr = Ptr ()

foreign import ccall "eval_program" c_evalProgram :: ProgramStatePtr -> Ptr IRInstr -> CSize -> IO () 
foreign import ccall "init_program" initProgram :: IO ProgramStatePtr
foreign import ccall "program_free" freeProgram :: ProgramStatePtr -> IO ()
foreign import ccall "program_import" c_programImport :: ProgramStatePtr -> CString -> IO ()

programImport :: ProgramStatePtr -> String -> IO ()
programImport state path = withCString path $ \cpath -> c_programImport state cpath

evalProgram :: ProgramStatePtr -> [IRInstr] -> IO ()
evalProgram state prog = do
  let len = length prog
  array <- mallocArray len :: IO (Ptr IRInstr) -- TODO: We're leaking this
  forM_ (zip [0..] prog) $ \(i, t) -> pokeElemOff array i t
  c_evalProgram state array (fromIntegral len :: CSize)
