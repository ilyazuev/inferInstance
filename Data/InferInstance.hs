{-# LANGUAGE GADTs, ConstraintKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.InferInstance (InferInstanceBase, WrappedInstance(..), instanceOf, wrappedInstanceOf, asInstanceOf, asWrappedInstanceOf) where

import Language.Haskell.TH (reify, Name, Q, Exp(ListE), Type(ConT, AppT), ExpQ, Dec(InstanceD), Info(ClassI))
import GHC.Exts (Constraint)
import Unsafe.Coerce (unsafeCoerce)
import Data.Typeable (Typeable, typeOf, TypeRep)
import Data.Maybe (maybe)
import Data.Set (Set, fromList, member, lookupIndex, elemAt)

class InferInstanceBase a

data WrappedInstance (constraint :: * -> Constraint) where
    WrappedInstance :: (Show a, Typeable a, constraint a) => a -> WrappedInstance constraint
    WrappedEmpty :: WrappedInstance constraint

instance Show (WrappedInstance a) where
    show (WrappedInstance a) = show a
    show WrappedEmpty = ""

data WrappedInstanceTypeRep (constraint :: * -> Constraint) = WTR { tr::TypeRep, wr::WrappedInstance constraint }

instance Eq (WrappedInstanceTypeRep a) where (==) (WTR tr1 _) (WTR tr2 _) = tr1 == tr2

instance Ord (WrappedInstanceTypeRep a) where (<=) (WTR tr1 _) (WTR tr2 _) = tr1 <= tr2

internalInstanceOf::Typeable a => a -> Set(WrappedInstanceTypeRep (constraint :: * -> Constraint)) -> Bool
internalInstanceOf a = member$ WTR (typeOf a) WrappedEmpty

internalWrappedInstanceOf:: WrappedInstance (constraint2 :: * -> Constraint) -> Set(WrappedInstanceTypeRep (constraint :: * -> Constraint)) -> Bool
internalWrappedInstanceOf (WrappedInstance a) = internalInstanceOf a

internalAsWrappedInstanceOf:: WrappedInstance (constraint2 :: * -> Constraint) -> Set(WrappedInstanceTypeRep (constraint1 :: * -> Constraint)) -> WrappedInstance (constraint1 :: * -> Constraint)
internalAsWrappedInstanceOf (WrappedInstance a) = internalAsInstanceOf a

internalAsInstanceOf:: Typeable a => a -> Set(WrappedInstanceTypeRep (constraint :: * -> Constraint)) -> WrappedInstance (constraint :: * -> Constraint)
internalAsInstanceOf a set =
    maybe WrappedEmpty (unwrap.wr)$ do
       i<-lookupIndex (WTR (typeOf a) WrappedEmpty) set
       return$ elemAt i set
    where
        unwrap (WrappedInstance b) = substitute a b
        substitute::forall t1 t2 constraint. (Typeable t2, Show t2, (constraint :: * -> Constraint) t2) => t1 -> t2 -> WrappedInstance (constraint :: * -> Constraint)
        substitute x y = WrappedInstance (unsafeCoerce x::t2)

------------------------------------------------------------------------------------------------------------------------------------------------------------

wrapToLambda::ExpQ->Name->ExpQ
wrapToLambda fn cls = [| \a-> let set = fromList$ $(typeReps cls)::Set(WrappedInstanceTypeRep $(return$ ConT cls)) in $fn a set |]

instanceOf::Name->ExpQ
instanceOf = wrapToLambda [| internalInstanceOf |]

wrappedInstanceOf::Name->ExpQ
wrappedInstanceOf = wrapToLambda [| internalWrappedInstanceOf |]

asInstanceOf::Name->ExpQ
asInstanceOf = wrapToLambda [| internalAsInstanceOf |]

asWrappedInstanceOf::Name->ExpQ
asWrappedInstanceOf = wrapToLambda [| internalAsWrappedInstanceOf |]

reifyTypes::Name->Q [Name]
reifyTypes cls = do
    ClassI _ instances <- reify cls
    return [typ | InstanceD _ _ (AppT _ (ConT typ)) _ <- instances]

typeReps::Name->ExpQ
typeReps cls = do
    types<-reifyTypes cls
    typesExp<-mapM mkTypeItem types
    return$ ListE$ typesExp
    where
        mkTypeItem typ = [| WTR (typeOf $(undefTyp typ)) (WrappedInstance $(undefTyp typ)) |]
        undefTyp typ = [| (undefined:: $(return$ ConT typ)) |]

