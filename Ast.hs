module Ast where

import Data.Word
import           Hapstone.Internal.X86      as X86

--for now hapstone definition
data Register = X86Reg X86.X86Reg -- Name String
              deriving (Eq, Show)

data Flags = Zero
            | Overflow
            | Carry
            | Parity
            | Adjust
            | Sign
            | Direction
              deriving (Eq, Show)

data AstNodeType = BvaddNode AstNodeType AstNodeType
             | BvandNode  AstNodeType AstNodeType
             | BvashrNode  AstNodeType AstNodeType
             | BvlshrNode  AstNodeType AstNodeType
             | BvmulNode  AstNodeType AstNodeType
             | BvnandNode  AstNodeType AstNodeType
             | BvnegNode  AstNodeType
             | BvnorNode  AstNodeType AstNodeType
             | BvnotNode  AstNodeType
             | BvorNode  AstNodeType AstNodeType
             | BvrolNode  AstNodeType AstNodeType
             | BvrorNode  AstNodeType AstNodeType -- can lit
             | BvsdivNode  AstNodeType AstNodeType
             | BvsgeNode  AstNodeType AstNodeType
             | BvsgtNode  AstNodeType AstNodeType
             | BvshlNode  AstNodeType AstNodeType
             | BvsleNode  AstNodeType AstNodeType
             | BvsltNode AstNodeType AstNodeType
             | BvsmodNode AstNodeType AstNodeType
             | BvsremNode AstNodeType AstNodeType
             | BvsubNode AstNodeType AstNodeType
             | BvudivNode AstNodeType AstNodeType
             | BvugeNode AstNodeType AstNodeType
             | BvugtNode AstNodeType AstNodeType
             | BvuleNode AstNodeType AstNodeType
             | BvultNode AstNodeType AstNodeType
             | BvuremNode AstNodeType AstNodeType
             | BvxnorNode AstNodeType AstNodeType
             | BvxorNode AstNodeType AstNodeType
             | BvNode Word64 Int
             | CompoundNode -- ! `[<expr1> <expr2> <expr3> ...]` node
             | ConcatNode [AstNodeType]
             | DecimalNode Int --float?
             | DeclareNode --wtf?
             | DistinctNode AstNodeType AstNodeType
             | EqualNode AstNodeType AstNodeType
             | ExtractNode Int Int  AstNodeType -- ! `((_ extract <high> <low>) <expr>)` node
             | IffNode AstNodeType AstNodeType -- ! `(iff <expr1> <expr2>)`
             | IteNode AstNodeType AstNodeType AstNodeType -- ! `(ite <ifExpr> <thenExpr> <elseExpr>)`
             | LandNode AstNodeType AstNodeType
             | LetNode String AstNodeType AstNodeType
             | LnotNode AstNodeType
             | LorNode AstNodeType AstNodeType
             | ReferenceNode --fix
             | StringNode String
             | SxNode Int AstNodeType
             | VariableNode
             | ZxNode Int AstNodeType
             -- nodes fow low level
             | Store AstNodeType AstNodeType
             | Read AstNodeType
             | SetReg Register AstNodeType
             | GetReg Register
             | SetFlag Flags AstNodeType
             | GetFlag Flags

             | AssertNode String -- errormsg
             deriving (Eq, Show)


    -- //! `(let ((<alias> <expr2>)) <expr3>)`
    -- class LetNode : public AbstractNode {
    --   public:
    --     TRITON_EXPORT LetNode(std::string alias, const SharedAbstractNode& expr2, const SharedAbstractNode& expr3);
    --     TRITON_EXPORT void init(void);
    --     TRITON_EXPORT triton::uint512 hash(triton::uint32 deep) const;
    -- };
    --
    --
    --
    -- //! Reference node
    -- class ReferenceNode : public AbstractNode {
    --   protected:
    --     triton::engines::symbolic::SharedSymbolicExpression expr;
    --
    --   public:
    --     TRITON_EXPORT ReferenceNode(const triton::engines::symbolic::SharedSymbolicExpression& expr);
    --     TRITON_EXPORT void init(void);
    --     TRITON_EXPORT triton::uint512 hash(triton::uint32 deep) const;
    --     TRITON_EXPORT const triton::engines::symbolic::SharedSymbolicExpression& getSymbolicExpression(void) const;
    -- };
    --
    --
    -- //! Variable node
    -- class VariableNode : public AbstractNode {
    --   protected:
    --     triton::engines::symbolic::SharedSymbolicVariable symVar;
    --
    --   public:
    --     TRITON_EXPORT VariableNode(const triton::engines::symbolic::SharedSymbolicVariable& symVar, AstContext& ctxt);
    --     TRITON_EXPORT void init(void);
    --     TRITON_EXPORT triton::uint512 hash(triton::uint32 deep) const;
    --     TRITON_EXPORT const triton::engines::symbolic::SharedSymbolicVariable& getVar(void);
    -- };
