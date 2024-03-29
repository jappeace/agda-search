module All where

import Algebra.Bundles
import Algebra.Consequences.Base
import Algebra.Consequences.Propositional
import Algebra.Consequences.Setoid
import Algebra.Construct.DirectProduct
import Algebra.Construct.LexProduct.Base
import Algebra.Construct.LexProduct.Inner
import Algebra.Construct.LexProduct
import Algebra.Construct.LiftedChoice
import Algebra.Construct.NaturalChoice.Base
import Algebra.Construct.NaturalChoice.Max
import Algebra.Construct.NaturalChoice.MaxOp
import Algebra.Construct.NaturalChoice.Min
import Algebra.Construct.NaturalChoice.MinMaxOp
import Algebra.Construct.NaturalChoice.MinOp
import Algebra.Construct.Subst.Equality
import Algebra.Construct.Zero
import Algebra.Core
import Algebra.Definitions.RawMagma
import Algebra.Definitions.RawMonoid
import Algebra.Definitions.RawSemiring
import Algebra.Definitions
import Algebra.FunctionProperties.Consequences.Core
import Algebra.FunctionProperties.Consequences.Propositional
import Algebra.FunctionProperties.Consequences
import Algebra.FunctionProperties.Core
import Algebra.FunctionProperties
import Algebra.Module.Bundles
import Algebra.Module.Consequences
import Algebra.Module.Construct.DirectProduct
import Algebra.Module.Construct.TensorUnit
import Algebra.Module.Construct.Zero
import Algebra.Module.Definitions.Bi
import Algebra.Module.Definitions.Left
import Algebra.Module.Definitions.Right
import Algebra.Module.Definitions
import Algebra.Module.Structures.Biased
import Algebra.Module.Structures
import Algebra.Morphism.Consequences
import Algebra.Morphism.Definitions
import Algebra.Morphism.GroupMonomorphism
import Algebra.Morphism.LatticeMonomorphism
import Algebra.Morphism.MagmaMonomorphism
import Algebra.Morphism.MonoidMonomorphism
import Algebra.Morphism.RingMonomorphism
import Algebra.Morphism.Structures
import Algebra.Morphism
import Algebra.Operations.CommutativeMonoid
import Algebra.Operations.Ring
import Algebra.Properties.AbelianGroup
import Algebra.Properties.BooleanAlgebra.Expression
import Algebra.Properties.BooleanAlgebra
import Algebra.Properties.CancellativeCommutativeSemiring
import Algebra.Properties.CommutativeMagma.Divisibility
import Algebra.Properties.CommutativeMonoid.Mult.TCOptimised
import Algebra.Properties.CommutativeMonoid.Mult
import Algebra.Properties.CommutativeMonoid.Sum
import Algebra.Properties.CommutativeMonoid
import Algebra.Properties.CommutativeSemigroup.Divisibility
import Algebra.Properties.CommutativeSemigroup
import Algebra.Properties.CommutativeSemiring.Exp.TCOptimised
import Algebra.Properties.CommutativeSemiring.Exp
import Algebra.Properties.DistributiveLattice
import Algebra.Properties.Group
import Algebra.Properties.Lattice
import Algebra.Properties.Magma.Divisibility
import Algebra.Properties.Monoid.Divisibility
import Algebra.Properties.Monoid.Mult.TCOptimised
import Algebra.Properties.Monoid.Mult
import Algebra.Properties.Monoid.Sum
import Algebra.Properties.Ring
import Algebra.Properties.Semigroup.Divisibility
import Algebra.Properties.Semigroup
import Algebra.Properties.Semilattice
import Algebra.Properties.Semiring.Divisibility
import Algebra.Properties.Semiring.Exp.TCOptimised
import Algebra.Properties.Semiring.Exp
import Algebra.Properties.Semiring.Mult.TCOptimised
import Algebra.Properties.Semiring.Mult
import Algebra.Properties.Semiring.Primality
import Algebra.Properties.Semiring.Sum
import Algebra.Solver.CommutativeMonoid.Example
import Algebra.Solver.CommutativeMonoid
import Algebra.Solver.IdempotentCommutativeMonoid.Example
import Algebra.Solver.IdempotentCommutativeMonoid
import Algebra.Solver.Monoid
import Algebra.Solver.Ring.AlmostCommutativeRing
import Algebra.Solver.Ring.Lemmas
import Algebra.Solver.Ring.NaturalCoefficients.Default
import Algebra.Solver.Ring.NaturalCoefficients
import Algebra.Solver.Ring.Simple
import Algebra.Solver.Ring
import Algebra.Structures.Biased
import Algebra.Structures
import Algebra
import Axiom.DoubleNegationElimination
import Axiom.ExcludedMiddle
import Axiom.Extensionality.Heterogeneous
import Axiom.Extensionality.Propositional
import Axiom.UniquenessOfIdentityProofs.WithK
import Axiom.UniquenessOfIdentityProofs
import Category.Applicative.Indexed
import Category.Applicative.Predicate
import Category.Applicative
import Category.Comonad
import Category.Functor.Predicate
import Category.Functor
import Category.Monad.Continuation
import Category.Monad.Indexed
import Category.Monad.Partiality.All
import Category.Monad.Partiality.Instances
import Category.Monad.Partiality
import Category.Monad.Predicate
import Category.Monad.Reader
import Category.Monad.State
import Category.Monad
import Codata.Colist.Bisimilarity
import Codata.Colist.Categorical
import Codata.Colist.Properties
import Codata.Colist
import Codata.Conat.Bisimilarity
import Codata.Conat.Literals
import Codata.Conat.Properties
import Codata.Conat
import Codata.Covec.Bisimilarity
import Codata.Covec.Categorical
import Codata.Covec.Instances
import Codata.Covec.Properties
import Codata.Covec
import Codata.Cowriter.Bisimilarity
import Codata.Cowriter
import Codata.Delay.Bisimilarity
import Codata.Delay.Categorical
import Codata.Delay.Properties
import Codata.Delay
import Codata.M.Bisimilarity
import Codata.M.Properties
import Codata.M
import Codata.Musical.Cofin
import Codata.Musical.Colist.Base
import Codata.Musical.Colist.Bisimilarity
import Codata.Musical.Colist.Infinite-merge
import Codata.Musical.Colist.Properties
import Codata.Musical.Colist.Relation.Unary.All.Properties
import Codata.Musical.Colist.Relation.Unary.All
import Codata.Musical.Colist.Relation.Unary.Any.Properties
import Codata.Musical.Colist.Relation.Unary.Any
import Codata.Musical.Colist
import Codata.Musical.Conat.Base
import Codata.Musical.Conat
import Codata.Musical.Conversion
import Codata.Musical.Costring
import Codata.Musical.Covec
import Codata.Musical.M.Indexed
import Codata.Musical.M
import Codata.Musical.Notation
import Codata.Musical.Stream
import Codata.Stream.Bisimilarity
import Codata.Stream.Categorical
import Codata.Stream.Instances
import Codata.Stream.Properties
import Codata.Stream
import Codata.Thunk
import Data.AVL.Height
import Data.AVL.Indexed.WithK
import Data.AVL.Indexed
import Data.AVL.IndexedMap
import Data.AVL.Key
import Data.AVL.Map
import Data.AVL.NonEmpty.Propositional
import Data.AVL.NonEmpty
import Data.AVL.Sets
import Data.AVL.Value
import Data.AVL
import Data.Bin
import Data.Bool.Base
import Data.Bool.Instances
import Data.Bool.Properties
import Data.Bool.Show
import Data.Bool.Solver
import Data.Bool
import Data.BoundedVec.Inefficient
import Data.BoundedVec
import Data.Char.Base
import Data.Char.Instances
import Data.Char.Properties
import Data.Char
import Data.Container.Any
import Data.Container.Combinator.Properties
import Data.Container.Combinator
import Data.Container.Core
import Data.Container.Fixpoints.Guarded
import Data.Container.Fixpoints.Sized
import Data.Container.FreeMonad
import Data.Container.Indexed.Combinator
import Data.Container.Indexed.Core
import Data.Container.Indexed.Fixpoints.Guarded
import Data.Container.Indexed.FreeMonad
import Data.Container.Indexed.WithK
import Data.Container.Indexed
import Data.Container.Membership
import Data.Container.Morphism.Properties
import Data.Container.Morphism
import Data.Container.Properties
import Data.Container.Related
import Data.Container.Relation.Binary.Equality.Setoid
import Data.Container.Relation.Binary.Pointwise.Properties
import Data.Container.Relation.Binary.Pointwise
import Data.Container.Relation.Unary.All
import Data.Container.Relation.Unary.Any.Properties
import Data.Container.Relation.Unary.Any
import Data.Container
import Data.DifferenceList
import Data.DifferenceNat
import Data.DifferenceVec
import Data.Digit.Properties
import Data.Digit
import Data.Empty.Irrelevant
import Data.Empty.Polymorphic
import Data.Empty
import Data.Erased
import Data.Fin.Base
import Data.Fin.Dec
import Data.Fin.Induction
import Data.Fin.Instances
import Data.Fin.Literals
import Data.Fin.Patterns
import Data.Fin.Permutation.Components
import Data.Fin.Permutation.Transposition.List
import Data.Fin.Permutation
import Data.Fin.Properties
import Data.Fin.Reflection
import Data.Fin.Show
import Data.Fin.Subset.Induction
import Data.Fin.Subset.Properties
import Data.Fin.Subset
import Data.Fin.Substitution.Example
import Data.Fin.Substitution.Lemmas
import Data.Fin.Substitution.List
import Data.Fin.Substitution
import Data.Fin
import Data.Float.Base
import Data.Float.Instances
import Data.Float.Properties
import Data.Float
import Data.Graph.Acyclic
import Data.Integer.Base
import Data.Integer.Coprimality
import Data.Integer.DivMod
import Data.Integer.Divisibility.Signed
import Data.Integer.Divisibility
import Data.Integer.GCD
import Data.Integer.Instances
import Data.Integer.LCM
import Data.Integer.Literals
import Data.Integer.Properties
import Data.Integer.Show
import Data.Integer.Solver
import Data.Integer.Tactic.RingSolver
import Data.Integer
import Data.List.All.Properties
import Data.List.All
import Data.List.Any.Properties
import Data.List.Any
import Data.List.Base
import Data.List.Categorical
import Data.List.Countdown
import Data.List.Extrema.Core
import Data.List.Extrema.Nat
import Data.List.Extrema
import Data.List.Fresh.Membership.Setoid.Properties
import Data.List.Fresh.Membership.Setoid
import Data.List.Fresh.Properties
import Data.List.Fresh.Relation.Unary.All.Properties
import Data.List.Fresh.Relation.Unary.All
import Data.List.Fresh.Relation.Unary.Any.Properties
import Data.List.Fresh.Relation.Unary.Any
import Data.List.Fresh
import Data.List.Instances
import Data.List.Kleene.AsList
import Data.List.Kleene.Base
import Data.List.Kleene
import Data.List.Literals
import Data.List.Membership.DecPropositional
import Data.List.Membership.DecSetoid
import Data.List.Membership.Propositional.Properties.Core
import Data.List.Membership.Propositional.Properties.WithK
import Data.List.Membership.Propositional.Properties
import Data.List.Membership.Propositional
import Data.List.Membership.Setoid.Properties
import Data.List.Membership.Setoid
import Data.List.NonEmpty.Base
import Data.List.NonEmpty.Categorical
import Data.List.NonEmpty.Instances
import Data.List.NonEmpty.Properties
import Data.List.NonEmpty
import Data.List.Properties
import Data.List.Relation.BagAndSetEquality
import Data.List.Relation.Binary.BagAndSetEquality
import Data.List.Relation.Binary.Disjoint.Propositional
import Data.List.Relation.Binary.Disjoint.Setoid.Properties
import Data.List.Relation.Binary.Disjoint.Setoid
import Data.List.Relation.Binary.Equality.DecPropositional
import Data.List.Relation.Binary.Equality.DecSetoid
import Data.List.Relation.Binary.Equality.Propositional
import Data.List.Relation.Binary.Equality.Setoid
import Data.List.Relation.Binary.Infix.Heterogeneous.Properties
import Data.List.Relation.Binary.Infix.Heterogeneous
import Data.List.Relation.Binary.Infix.Homogeneous.Properties
import Data.List.Relation.Binary.Lex.Core
import Data.List.Relation.Binary.Lex.NonStrict
import Data.List.Relation.Binary.Lex.Strict
import Data.List.Relation.Binary.Lex
import Data.List.Relation.Binary.Permutation.Homogeneous
import Data.List.Relation.Binary.Permutation.Inductive.Properties
import Data.List.Relation.Binary.Permutation.Inductive
import Data.List.Relation.Binary.Permutation.Propositional.Properties
import Data.List.Relation.Binary.Permutation.Propositional
import Data.List.Relation.Binary.Permutation.Setoid.Properties
import Data.List.Relation.Binary.Permutation.Setoid
import Data.List.Relation.Binary.Pointwise.Base
import Data.List.Relation.Binary.Pointwise.Properties
import Data.List.Relation.Binary.Pointwise
import Data.List.Relation.Binary.Prefix.Heterogeneous.Properties
import Data.List.Relation.Binary.Prefix.Heterogeneous
import Data.List.Relation.Binary.Prefix.Homogeneous.Properties
import Data.List.Relation.Binary.Sublist.DecPropositional.Solver
import Data.List.Relation.Binary.Sublist.DecPropositional
import Data.List.Relation.Binary.Sublist.DecSetoid.Solver
import Data.List.Relation.Binary.Sublist.DecSetoid
import Data.List.Relation.Binary.Sublist.Heterogeneous.Core
import Data.List.Relation.Binary.Sublist.Heterogeneous.Properties
import Data.List.Relation.Binary.Sublist.Heterogeneous.Solver
import Data.List.Relation.Binary.Sublist.Heterogeneous
import Data.List.Relation.Binary.Sublist.Propositional.Disjoint
import Data.List.Relation.Binary.Sublist.Propositional.Example.UniqueBoundVariables
import Data.List.Relation.Binary.Sublist.Propositional.Properties
import Data.List.Relation.Binary.Sublist.Propositional
import Data.List.Relation.Binary.Sublist.Setoid.Properties
import Data.List.Relation.Binary.Sublist.Setoid
import Data.List.Relation.Binary.Subset.Propositional.Properties
import Data.List.Relation.Binary.Subset.Propositional
import Data.List.Relation.Binary.Subset.Setoid.Properties
import Data.List.Relation.Binary.Subset.Setoid
import Data.List.Relation.Binary.Suffix.Heterogeneous.Properties
import Data.List.Relation.Binary.Suffix.Heterogeneous
import Data.List.Relation.Binary.Suffix.Homogeneous.Properties
import Data.List.Relation.Equality.DecPropositional
import Data.List.Relation.Equality.DecSetoid
import Data.List.Relation.Equality.Propositional
import Data.List.Relation.Equality.Setoid
import Data.List.Relation.Lex.Core
import Data.List.Relation.Lex.NonStrict
import Data.List.Relation.Lex.Strict
import Data.List.Relation.Permutation.Inductive.Properties
import Data.List.Relation.Permutation.Inductive
import Data.List.Relation.Pointwise
import Data.List.Relation.Sublist.Propositional.Properties
import Data.List.Relation.Sublist.Propositional
import Data.List.Relation.Subset.Propositional.Properties
import Data.List.Relation.Subset.Propositional
import Data.List.Relation.Subset.Setoid.Properties
import Data.List.Relation.Subset.Setoid
import Data.List.Relation.Ternary.Appending.Properties
import Data.List.Relation.Ternary.Appending.Propositional.Properties
import Data.List.Relation.Ternary.Appending.Propositional
import Data.List.Relation.Ternary.Appending.Setoid.Properties
import Data.List.Relation.Ternary.Appending.Setoid
import Data.List.Relation.Ternary.Appending
import Data.List.Relation.Ternary.Interleaving.Properties
import Data.List.Relation.Ternary.Interleaving.Propositional.Properties
import Data.List.Relation.Ternary.Interleaving.Propositional
import Data.List.Relation.Ternary.Interleaving.Setoid.Properties
import Data.List.Relation.Ternary.Interleaving.Setoid
import Data.List.Relation.Ternary.Interleaving
import Data.List.Relation.Unary.All.Properties
import Data.List.Relation.Unary.All
import Data.List.Relation.Unary.AllPairs.Core
import Data.List.Relation.Unary.AllPairs.Properties
import Data.List.Relation.Unary.AllPairs
import Data.List.Relation.Unary.Any.Properties
import Data.List.Relation.Unary.Any
import Data.List.Relation.Unary.Enumerates.Setoid.Properties
import Data.List.Relation.Unary.Enumerates.Setoid
import Data.List.Relation.Unary.First.Properties
import Data.List.Relation.Unary.First
import Data.List.Relation.Unary.Grouped.Properties
import Data.List.Relation.Unary.Grouped
import Data.List.Relation.Unary.Linked.Properties
import Data.List.Relation.Unary.Linked
import Data.List.Relation.Unary.Sorted.TotalOrder.Properties
import Data.List.Relation.Unary.Sorted.TotalOrder
import Data.List.Relation.Unary.Unique.DecPropositional.Properties
import Data.List.Relation.Unary.Unique.DecPropositional
import Data.List.Relation.Unary.Unique.DecSetoid.Properties
import Data.List.Relation.Unary.Unique.DecSetoid
import Data.List.Relation.Unary.Unique.Propositional.Properties
import Data.List.Relation.Unary.Unique.Propositional
import Data.List.Relation.Unary.Unique.Setoid.Properties
import Data.List.Relation.Unary.Unique.Setoid
import Data.List.Reverse
import Data.List.Solver
import Data.List.Sort.Base
import Data.List.Sort.MergeSort
import Data.List.Sort
import Data.List.Zipper.Properties
import Data.List.Zipper
import Data.List
import Data.Maybe.Base
import Data.Maybe.Categorical
import Data.Maybe.Instances
import Data.Maybe.Properties
import Data.Maybe.Relation.Binary.Connected
import Data.Maybe.Relation.Binary.Pointwise
import Data.Maybe.Relation.Unary.All.Properties
import Data.Maybe.Relation.Unary.All
import Data.Maybe.Relation.Unary.Any
import Data.Maybe
import Data.Nat.Base
import Data.Nat.Binary.Base
import Data.Nat.Binary.Induction
import Data.Nat.Binary.Instances
import Data.Nat.Binary.Properties
import Data.Nat.Binary.Subtraction
import Data.Nat.Binary
import Data.Nat.Coprimality
import Data.Nat.DivMod.Core
import Data.Nat.DivMod.WithK
import Data.Nat.DivMod
import Data.Nat.Divisibility.Core
import Data.Nat.Divisibility
import Data.Nat.GCD.Lemmas
import Data.Nat.GCD
import Data.Nat.GeneralisedArithmetic
import Data.Nat.Induction
import Data.Nat.InfinitelyOften
import Data.Nat.Instances
import Data.Nat.LCM
import Data.Nat.Literals
import Data.Nat.Primality
import Data.Nat.Properties.Core
import Data.Nat.Properties
import Data.Nat.PseudoRandom.LCG.Unsafe
import Data.Nat.PseudoRandom.LCG
import Data.Nat.Reflection
import Data.Nat.Show.Properties
import Data.Nat.Show
import Data.Nat.Solver
import Data.Nat.Tactic.RingSolver
import Data.Nat.WithK
import Data.Nat
import Data.Plus
import Data.Product.Algebra
import Data.Product.Categorical.Examples
import Data.Product.Categorical.Left.Base
import Data.Product.Categorical.Left
import Data.Product.Categorical.Right.Base
import Data.Product.Categorical.Right
import Data.Product.Function.Dependent.Propositional.WithK
import Data.Product.Function.Dependent.Propositional
import Data.Product.Function.Dependent.Setoid.WithK
import Data.Product.Function.Dependent.Setoid
import Data.Product.Function.NonDependent.Propositional
import Data.Product.Function.NonDependent.Setoid
import Data.Product.Instances
import Data.Product.N-ary.Categorical
import Data.Product.N-ary.Properties
import Data.Product.N-ary
import Data.Product.Nary.NonDependent
import Data.Product.Properties.WithK
import Data.Product.Properties
import Data.Product.Relation.Binary.Lex.NonStrict
import Data.Product.Relation.Binary.Lex.Strict
import Data.Product.Relation.Binary.Pointwise.Dependent.WithK
import Data.Product.Relation.Binary.Pointwise.Dependent
import Data.Product.Relation.Binary.Pointwise.NonDependent
import Data.Product.Relation.Lex.NonStrict
import Data.Product.Relation.Lex.Strict
import Data.Product.Relation.Pointwise.Dependent
import Data.Product.Relation.Pointwise.NonDependent
import Data.Product.Relation.Unary.All
import Data.Product
import Data.Rational.Base
import Data.Rational.Instances
import Data.Rational.Literals
import Data.Rational.Properties
import Data.Rational.Show
import Data.Rational.Solver
import Data.Rational.Unnormalised.Base
import Data.Rational.Unnormalised.Properties
import Data.Rational.Unnormalised.Solver
import Data.Rational.Unnormalised
import Data.Rational
import Data.Record
import Data.Refinement.Relation.Unary.All
import Data.Refinement
import Data.ReflexiveClosure
import Data.Sign.Base
import Data.Sign.Instances
import Data.Sign.Properties
import Data.Sign
import Data.Star.BoundedVec
import Data.Star.Decoration
import Data.Star.Environment
import Data.Star.Fin
import Data.Star.List
import Data.Star.Nat
import Data.Star.Pointer
import Data.Star.Properties
import Data.Star.Vec
import Data.Star
import Data.String.Base
import Data.String.Instances
import Data.String.Literals
import Data.String.Properties
import Data.String.Unsafe
import Data.String
import Data.Sum.Algebra
import Data.Sum.Base
import Data.Sum.Categorical.Examples
import Data.Sum.Categorical.Left
import Data.Sum.Categorical.Right
import Data.Sum.Function.Propositional
import Data.Sum.Function.Setoid
import Data.Sum.Instances
import Data.Sum.Properties
import Data.Sum.Relation.Binary.LeftOrder
import Data.Sum.Relation.Binary.Pointwise
import Data.Sum.Relation.LeftOrder
import Data.Sum.Relation.Pointwise
import Data.Sum.Relation.Unary.All
import Data.Sum
import Data.Table.Base
import Data.Table.Properties
import Data.Table.Relation.Binary.Equality
import Data.Table.Relation.Equality
import Data.Table
import Data.These.Base
import Data.These.Categorical.Left.Base
import Data.These.Categorical.Left
import Data.These.Categorical.Right.Base
import Data.These.Categorical.Right
import Data.These.Instances
import Data.These.Properties
import Data.These
import Data.Tree.AVL.Height
import Data.Tree.AVL.Indexed.Relation.Unary.All
import Data.Tree.AVL.Indexed.Relation.Unary.Any.Properties
import Data.Tree.AVL.Indexed.Relation.Unary.Any
import Data.Tree.AVL.Indexed.WithK
import Data.Tree.AVL.Indexed
import Data.Tree.AVL.IndexedMap
import Data.Tree.AVL.Key
import Data.Tree.AVL.Map.Relation.Unary.Any
import Data.Tree.AVL.Map
import Data.Tree.AVL.NonEmpty.Propositional
import Data.Tree.AVL.NonEmpty
import Data.Tree.AVL.Relation.Unary.Any
import Data.Tree.AVL.Sets
import Data.Tree.AVL.Value
import Data.Tree.AVL
import Data.Tree.Binary.Properties
import Data.Tree.Binary.Relation.Unary.All.Properties
import Data.Tree.Binary.Relation.Unary.All
import Data.Tree.Binary.Show
import Data.Tree.Binary.Zipper.Properties
import Data.Tree.Binary.Zipper
import Data.Tree.Binary
import Data.Tree.Rose.Properties
import Data.Tree.Rose.Show
import Data.Tree.Rose
import Data.Trie.NonEmpty
import Data.Trie
import Data.Unit.Base
import Data.Unit.Instances
import Data.Unit.NonEta
import Data.Unit.Polymorphic.Base
import Data.Unit.Polymorphic.Instances
import Data.Unit.Polymorphic.Properties
import Data.Unit.Polymorphic
import Data.Unit.Properties
import Data.Unit
import Data.Universe.Indexed
import Data.Universe
import Data.Vec.All.Properties
import Data.Vec.All
import Data.Vec.Any
import Data.Vec.Base
import Data.Vec.Bounded.Base
import Data.Vec.Bounded
import Data.Vec.Categorical
import Data.Vec.Functional.Properties
import Data.Vec.Functional.Relation.Binary.Equality.Setoid
import Data.Vec.Functional.Relation.Binary.Pointwise.Properties
import Data.Vec.Functional.Relation.Binary.Pointwise
import Data.Vec.Functional.Relation.Unary.All.Properties
import Data.Vec.Functional.Relation.Unary.All
import Data.Vec.Functional.Relation.Unary.Any
import Data.Vec.Functional
import Data.Vec.Instances
import Data.Vec.Membership.DecPropositional
import Data.Vec.Membership.DecSetoid
import Data.Vec.Membership.Propositional.Properties
import Data.Vec.Membership.Propositional
import Data.Vec.Membership.Setoid
import Data.Vec.N-ary
import Data.Vec.Properties.WithK
import Data.Vec.Properties
import Data.Vec.Recursive.Categorical
import Data.Vec.Recursive.Properties
import Data.Vec.Recursive
import Data.Vec.Relation.Binary.Equality.DecPropositional
import Data.Vec.Relation.Binary.Equality.DecSetoid
import Data.Vec.Relation.Binary.Equality.Propositional.WithK
import Data.Vec.Relation.Binary.Equality.Propositional
import Data.Vec.Relation.Binary.Equality.Setoid
import Data.Vec.Relation.Binary.Lex.Core
import Data.Vec.Relation.Binary.Lex.NonStrict
import Data.Vec.Relation.Binary.Lex.Strict
import Data.Vec.Relation.Binary.Pointwise.Extensional
import Data.Vec.Relation.Binary.Pointwise.Inductive
import Data.Vec.Relation.Equality.DecPropositional
import Data.Vec.Relation.Equality.DecSetoid
import Data.Vec.Relation.Equality.Propositional
import Data.Vec.Relation.Equality.Setoid
import Data.Vec.Relation.Pointwise.Extensional
import Data.Vec.Relation.Pointwise.Inductive
import Data.Vec.Relation.Unary.All.Properties
import Data.Vec.Relation.Unary.All
import Data.Vec.Relation.Unary.AllPairs.Core
import Data.Vec.Relation.Unary.AllPairs.Properties
import Data.Vec.Relation.Unary.AllPairs
import Data.Vec.Relation.Unary.Any.Properties
import Data.Vec.Relation.Unary.Any
import Data.Vec.Relation.Unary.Unique.Propositional.Properties
import Data.Vec.Relation.Unary.Unique.Propositional
import Data.Vec.Relation.Unary.Unique.Setoid.Properties
import Data.Vec.Relation.Unary.Unique.Setoid
import Data.Vec
import Data.W.Indexed
import Data.W.Sized
import Data.W.WithK
import Data.W
import Data.Word.Base
import Data.Word.Instances
import Data.Word.Properties
import Data.Word
import Data.Wrap
import Debug.Trace
import Foreign.Haskell
import Function.Base
import Function.Bijection
import Function.Bundles
import Function.Construct.Composition
import Function.Construct.Identity
import Function.Construct.Symmetry
import Function.Core
import Function.Definitions.Core1
import Function.Definitions.Core2
import Function.Definitions
import Function.Endomorphism.Propositional
import Function.Endomorphism.Setoid
import Function.Equality
import Function.Equivalence
import Function.HalfAdjointEquivalence
import Function.Identity.Categorical
import Function.Identity.Instances
import Function.Injection
import Function.Inverse
import Function.LeftInverse
import Function.Metric.Bundles
import Function.Metric.Core
import Function.Metric.Definitions
import Function.Metric.Nat.Bundles
import Function.Metric.Nat.Core
import Function.Metric.Nat.Definitions
import Function.Metric.Nat.Structures
import Function.Metric.Nat
import Function.Metric.Rational.Bundles
import Function.Metric.Rational.Core
import Function.Metric.Rational.Definitions
import Function.Metric.Rational.Structures
import Function.Metric.Rational
import Function.Metric.Structures
import Function.Metric
import Function.Nary.NonDependent.Base
import Function.Nary.NonDependent
import Function.Properties.Equivalence
import Function.Properties.Inverse
import Function.Properties
import Function.Reasoning
import Function.Related.TypeIsomorphisms.Solver
import Function.Related.TypeIsomorphisms
import Function.Related
import Function.Structures
import Function.Surjection
import Function
import IO.Base
import IO.Finite
import IO.Infinite
import IO.Primitive.Finite
import IO.Primitive.Infinite
import IO.Primitive
import IO
import Induction.Lexicographic
import Induction.Nat
import Induction.WellFounded
import Induction
import Level.Literals
import Level
import Record
import Reflection.Abstraction
import Reflection.Annotated.Free
import Reflection.Annotated
import Reflection.Argument.Information
import Reflection.Argument.Modality
import Reflection.Argument.Quantity
import Reflection.Argument.Relevance
import Reflection.Argument.Visibility
import Reflection.Argument
import Reflection.DeBruijn
import Reflection.Definition
import Reflection.External
import Reflection.Instances
import Reflection.Literal
import Reflection.Meta
import Reflection.Name
import Reflection.Pattern
import Reflection.Show
import Reflection.Term
import Reflection.Traversal
import Reflection.TypeChecking.Format
import Reflection.TypeChecking.Monad.Categorical
import Reflection.TypeChecking.Monad.Instances
import Reflection.TypeChecking.Monad.Syntax
import Reflection.TypeChecking.Monad
import Reflection.TypeChecking.MonadSyntax
import Reflection.Universe
import Reflection
import Relation.Binary.Bundles
import Relation.Binary.Consequences
import Relation.Binary.Construct.Add.Extrema.Equality
import Relation.Binary.Construct.Add.Extrema.NonStrict
import Relation.Binary.Construct.Add.Extrema.Strict
import Relation.Binary.Construct.Add.Infimum.Equality
import Relation.Binary.Construct.Add.Infimum.NonStrict
import Relation.Binary.Construct.Add.Infimum.Strict
import Relation.Binary.Construct.Add.Point.Equality
import Relation.Binary.Construct.Add.Supremum.Equality
import Relation.Binary.Construct.Add.Supremum.NonStrict
import Relation.Binary.Construct.Add.Supremum.Strict
import Relation.Binary.Construct.Always
import Relation.Binary.Construct.Closure.Equivalence.Properties
import Relation.Binary.Construct.Closure.Equivalence
import Relation.Binary.Construct.Closure.Reflexive.Properties.WithK
import Relation.Binary.Construct.Closure.Reflexive.Properties
import Relation.Binary.Construct.Closure.Reflexive
import Relation.Binary.Construct.Closure.ReflexiveTransitive.Properties.WithK
import Relation.Binary.Construct.Closure.ReflexiveTransitive.Properties
import Relation.Binary.Construct.Closure.ReflexiveTransitive
import Relation.Binary.Construct.Closure.Symmetric
import Relation.Binary.Construct.Closure.SymmetricTransitive
import Relation.Binary.Construct.Closure.Transitive.WithK
import Relation.Binary.Construct.Closure.Transitive
import Relation.Binary.Construct.Composition
import Relation.Binary.Construct.Constant.Core
import Relation.Binary.Construct.Constant
import Relation.Binary.Construct.Converse
import Relation.Binary.Construct.Flip
import Relation.Binary.Construct.FromPred
import Relation.Binary.Construct.FromRel
import Relation.Binary.Construct.Intersection
import Relation.Binary.Construct.NaturalOrder.Left
import Relation.Binary.Construct.NaturalOrder.Right
import Relation.Binary.Construct.Never
import Relation.Binary.Construct.NonStrictToStrict
import Relation.Binary.Construct.On
import Relation.Binary.Construct.StrictToNonStrict
import Relation.Binary.Construct.Subst.Equality
import Relation.Binary.Construct.Union
import Relation.Binary.Core
import Relation.Binary.Definitions
import Relation.Binary.EqReasoning
import Relation.Binary.EquivalenceClosure
import Relation.Binary.HeterogeneousEquality.Core
import Relation.Binary.HeterogeneousEquality.Quotients.Examples
import Relation.Binary.HeterogeneousEquality.Quotients
import Relation.Binary.HeterogeneousEquality
import Relation.Binary.Indexed.Heterogeneous.Bundles
import Relation.Binary.Indexed.Heterogeneous.Construct.At
import Relation.Binary.Indexed.Heterogeneous.Construct.Trivial
import Relation.Binary.Indexed.Heterogeneous.Core
import Relation.Binary.Indexed.Heterogeneous.Definitions
import Relation.Binary.Indexed.Heterogeneous.Structures
import Relation.Binary.Indexed.Heterogeneous
import Relation.Binary.Indexed.Homogeneous.Bundles
import Relation.Binary.Indexed.Homogeneous.Construct.At
import Relation.Binary.Indexed.Homogeneous.Core
import Relation.Binary.Indexed.Homogeneous.Definitions
import Relation.Binary.Indexed.Homogeneous.Structures
import Relation.Binary.Indexed.Homogeneous
import Relation.Binary.Lattice
import Relation.Binary.Morphism.Bundles
import Relation.Binary.Morphism.Construct.Composition
import Relation.Binary.Morphism.Construct.Constant
import Relation.Binary.Morphism.Construct.Identity
import Relation.Binary.Morphism.Definitions
import Relation.Binary.Morphism.OrderMonomorphism
import Relation.Binary.Morphism.RelMonomorphism
import Relation.Binary.Morphism.Structures
import Relation.Binary.Morphism
import Relation.Binary.OrderMorphism
import Relation.Binary.PartialOrderReasoning
import Relation.Binary.PreorderReasoning
import Relation.Binary.Properties.BoundedJoinSemilattice
import Relation.Binary.Properties.BoundedLattice
import Relation.Binary.Properties.BoundedMeetSemilattice
import Relation.Binary.Properties.DecTotalOrder
import Relation.Binary.Properties.DistributiveLattice
import Relation.Binary.Properties.HeytingAlgebra
import Relation.Binary.Properties.JoinSemilattice
import Relation.Binary.Properties.Lattice
import Relation.Binary.Properties.MeetSemilattice
import Relation.Binary.Properties.Poset
import Relation.Binary.Properties.Preorder
import Relation.Binary.Properties.Setoid
import Relation.Binary.Properties.StrictPartialOrder
import Relation.Binary.Properties.StrictTotalOrder
import Relation.Binary.Properties.TotalOrder
import Relation.Binary.PropositionalEquality.Algebra
import Relation.Binary.PropositionalEquality.Core
import Relation.Binary.PropositionalEquality.Properties
import Relation.Binary.PropositionalEquality.TrustMe
import Relation.Binary.PropositionalEquality.WithK
import Relation.Binary.PropositionalEquality
import Relation.Binary.Reasoning.Base.Double
import Relation.Binary.Reasoning.Base.Partial
import Relation.Binary.Reasoning.Base.Single
import Relation.Binary.Reasoning.Base.Triple
import Relation.Binary.Reasoning.MultiSetoid
import Relation.Binary.Reasoning.PartialOrder
import Relation.Binary.Reasoning.PartialSetoid
import Relation.Binary.Reasoning.Preorder
import Relation.Binary.Reasoning.Setoid
import Relation.Binary.Reasoning.StrictPartialOrder
import Relation.Binary.Reflection
import Relation.Binary.Rewriting
import Relation.Binary.SetoidReasoning
import Relation.Binary.StrictPartialOrderReasoning
import Relation.Binary.Structures
import Relation.Binary.SymmetricClosure
import Relation.Binary.TypeClasses
import Relation.Binary
import Relation.Nary
import Relation.Nullary.Construct.Add.Extrema
import Relation.Nullary.Construct.Add.Infimum
import Relation.Nullary.Construct.Add.Point
import Relation.Nullary.Construct.Add.Supremum
import Relation.Nullary.Decidable.Core
import Relation.Nullary.Decidable
import Relation.Nullary.Implication
import Relation.Nullary.Indexed.Negation
import Relation.Nullary.Indexed
import Relation.Nullary.Negation.Core
import Relation.Nullary.Negation
import Relation.Nullary.Product
import Relation.Nullary.Reflects
import Relation.Nullary.Sum
import Relation.Nullary.Universe
import Relation.Nullary
import Relation.Unary.Closure.Base
import Relation.Unary.Closure.Preorder
import Relation.Unary.Closure.StrictPartialOrder
import Relation.Unary.Consequences
import Relation.Unary.Indexed
import Relation.Unary.PredicateTransformer
import Relation.Unary.Properties
import Relation.Unary.Sized
import Relation.Unary
import Size
import Strict
import System.Environment.Primitive
import System.Environment
import System.Exit.Primitive
import System.Exit
import Tactic.MonoidSolver
import Tactic.RingSolver.Core.AlmostCommutativeRing
import Tactic.RingSolver.Core.Expression
import Tactic.RingSolver.Core.NatSet
import Tactic.RingSolver.Core.Polynomial.Base
import Tactic.RingSolver.Core.Polynomial.Homomorphism.Addition
import Tactic.RingSolver.Core.Polynomial.Homomorphism.Constants
import Tactic.RingSolver.Core.Polynomial.Homomorphism.Exponentiation
import Tactic.RingSolver.Core.Polynomial.Homomorphism.Lemmas
import Tactic.RingSolver.Core.Polynomial.Homomorphism.Multiplication
import Tactic.RingSolver.Core.Polynomial.Homomorphism.Negation
import Tactic.RingSolver.Core.Polynomial.Homomorphism.Variables
import Tactic.RingSolver.Core.Polynomial.Homomorphism
import Tactic.RingSolver.Core.Polynomial.Parameters
import Tactic.RingSolver.Core.Polynomial.Reasoning
import Tactic.RingSolver.Core.Polynomial.Semantics
import Tactic.RingSolver.Core.ReflectionHelp
import Tactic.RingSolver.NonReflective
import Tactic.RingSolver
import Text.Format.Generic
import Text.Format
import Text.Pretty.Core
import Text.Pretty
import Text.Printf.Generic
import Text.Printf
import Text.Regex.Base
import Text.Regex.Derivative.Brzozowski
import Text.Regex.Properties.Core
import Text.Regex.Properties
import Text.Regex.Search
import Text.Regex.SmartConstructors
import Text.Regex.String.Unsafe
import Text.Regex.String
import Text.Regex
import Text.Tabular.Base
import Text.Tabular.List
import Text.Tabular.Vec
import Text.Tree.Linear
import Universe
