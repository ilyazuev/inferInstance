{-# LANGUAGE GADTs, ConstraintKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.InferInstance (InferInstanceBase, WrappedInstance(..), instanceOf, wrappedInstanceOf, asInstanceOf, asWrappedInstanceOf) where

import Language.Haskell.TH (reify, Name, Q, Exp(ListE), Type(ConT, AppT), ExpQ, Dec(InstanceD), Info(ClassI))
import GHC.Exts (Constraint)
import Unsafe.Coerce (unsafeCoerce)
import Data.Typeable (Typeable, typeOf)
import Data.List (find)
import Data.Maybe (maybe, isJust)

class InferInstanceBase a

data WrappedInstance (constraint :: * -> Constraint) where
    WrappedInstance :: (Show a, Typeable a, constraint a) => a -> WrappedInstance constraint
    WrappedEmpty :: WrappedInstance constraint

instance Show (WrappedInstance a) where
    show (WrappedInstance a) = show a
    show WrappedEmpty = ""

internalInstanceOf::(Foldable t, Typeable a)=> t(WrappedInstance (constraint :: * -> Constraint)) -> a -> Bool
internalInstanceOf list = isJust . findType list

internalWrappedInstanceOf::[WrappedInstance (constraint :: * -> Constraint)] -> WrappedInstance (constraint2 :: * -> Constraint) -> Bool
internalWrappedInstanceOf list (WrappedInstance a) = internalInstanceOf list a

findType::(Foldable t, Typeable a) => t(WrappedInstance (constraint :: * -> Constraint)) -> a -> Maybe( WrappedInstance (constraint :: * -> Constraint) )
findType list a = find inList list
    where
        inList (WrappedInstance b) = typeOf a == typeOf b
        inList _ = False

internalAsWrappedInstanceOf::Foldable t => t(WrappedInstance (constraint1 :: * -> Constraint)) -> WrappedInstance (constraint2 :: * -> Constraint) -> WrappedInstance (constraint1 :: * -> Constraint)
internalAsWrappedInstanceOf list (WrappedInstance a) = internalAsInstanceOf list a

internalAsInstanceOf::(Foldable t, Typeable a) => t(WrappedInstance (constraint :: * -> Constraint)) -> a -> WrappedInstance (constraint :: * -> Constraint)
internalAsInstanceOf list a =
    maybe WrappedEmpty unwrap$ findType list a
    where
        unwrap (WrappedInstance b) = substitute a b
        substitute::forall t1 t2 constraint. (Typeable t2, Show t2, (constraint :: * -> Constraint) t2) => t1 -> t2 -> WrappedInstance (constraint :: * -> Constraint)
        substitute x y = WrappedInstance (unsafeCoerce x::t2)

------------------------------------------------------------------------------------------------------------------------------------------------------------

wrapToLambda::ExpQ->Name->ExpQ
wrapToLambda fn cls = [| \a-> let list = $(types cls)::[WrappedInstance $(return$ ConT cls)] in $fn list a |]

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

types::Name->ExpQ
types cls = do
    types<-reifyTypes cls
    typesExp<-mapM mkTypeItem types
    return$ ListE$ typesExp
    where
        mkTypeItem typ = [| WrappedInstance (undefined:: $(return$ ConT typ)) |]

