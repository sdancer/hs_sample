module Ast where

import Data.Word
import Hapstone.Internal.X86 as X86

data Flags = Zero
            | Overflow
            | Carry
            | Parity
            | Adjust
            | Sign
            | Direction
              deriving (Eq, Show)

data AstNode =
    BvaddNode AstNode AstNode
  | BvandNode AstNode AstNode
  | BvashrNode AstNode AstNode
  | BvlshrNode AstNode AstNode
  | BvmulNode AstNode AstNode
  | BvnandNode AstNode AstNode
  | BvnegNode AstNode
  | BvnorNode AstNode AstNode
  | BvnotNode AstNode
  | BvorNode AstNode AstNode
  | BvrolNode AstNode AstNode
  | BvrorNode AstNode AstNode -- can lit
  | BvsdivNode AstNode AstNode
  | BvsgeNode AstNode AstNode
  | BvsgtNode AstNode AstNode
  | BvshlNode AstNode AstNode
  | BvsleNode AstNode AstNode
  | BvsltNode AstNode AstNode
  | BvsmodNode AstNode AstNode
  | BvsremNode AstNode AstNode
  | BvsubNode AstNode AstNode
  | BvudivNode AstNode AstNode
  | BvugeNode AstNode AstNode
  | BvugtNode AstNode AstNode
  | BvuleNode AstNode AstNode
  | BvultNode AstNode AstNode
  | BvuremNode AstNode AstNode
  | BvxnorNode AstNode AstNode
  | BvxorNode AstNode AstNode
  | BvNode Word64 Word8
  | CompoundNode -- ! `[<expr1> <expr2> <expr3> ...]` node
  | ConcatNode [AstNode]
  | DecimalNode Int --float?
  | DeclareNode --wtf?
  | DistinctNode AstNode AstNode
  | EqualNode AstNode AstNode
  | ExtractNode Word8 Word8 AstNode -- ! `((_ extract <high> <low>) <expr>)` node
  | IffNode AstNode AstNode -- ! `(iff <expr1> <expr2>)`
  | IteNode AstNode AstNode AstNode -- ! `(ite <ifExpr> <thenExpr> <elseExpr>)`
  | LandNode AstNode AstNode
  | LetNode String AstNode AstNode
  | LnotNode AstNode
  | LorNode AstNode AstNode
  | ReferenceNode --fix
  | StringNode String
  | SxNode Int AstNode
  | VariableNode
  | ZxNode Int AstNode
  | UndefinedNode -- The undefined value
  | Read AstNode
  | GetReg X86.X86Reg
  | GetFlag Flags
  deriving (Eq, Show)

data Stmt =
    Store AstNode AstNode
  | SetReg X86.X86Reg AstNode
  | SetFlag Flags AstNode
  deriving (Eq, Show)

