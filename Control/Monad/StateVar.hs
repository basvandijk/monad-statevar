{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
	   , FlexibleInstances
  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.StateVar
-- Copyright   :  (c) 2011 Bas van Dijk, Sven Panne
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- State variables are references in the IO monad, like 'IORef's or parts of
-- the OpenGL state. Note that state variables are not neccessarily writable or
-- readable, they may come in read-only or write-only flavours, too. As a very
-- simple example for a state variable, consider an explicitly allocated memory
-- buffer. This buffer can easily be converted into a 'StateVar':
--
-- @
-- makeStateVarFromPtr :: Storable a => Ptr a -> StateVar a
-- makeStateVarFromPtr p = makeStateVar (peek p) (poke p)
-- @
--
-- The example below puts 11 into a state variable (i.e. into the buffer),
-- increments the contents of the state variable by 22, and finally prints the
-- resulting content:
--
-- @
--   do p <- malloc :: IO (Ptr Int)
--      let v = makeStateVarFromPtr p
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--
-- 'IORef's are state variables, too, so an example with them looks extremely
-- similiar:
--
-- @
--   do v <- newIORef (0 :: Int)
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--------------------------------------------------------------------------------

module Control.Monad.StateVar (
   -- * Readable State Variables
   Getter(..),
   GettableStateVar, makeGettableStateVar,
   -- * Writable State Variables
   Setter(..),
   SettableStateVar, makeSettableStateVar,
   -- * General State Variables
   StateVar, makeStateVar,
   -- * Utility Functions
   ($~), ($=!), ($~!)
) where

import Data.IORef ( IORef, readIORef, writeIORef )
import Data.STRef ( STRef, readSTRef, writeSTRef )

import GHC.Conc ( STM, TVar, readTVar, writeTVar )

import Control.Monad.ST ( ST )

--------------------------------------------------------------------------------

infixr 2 $=

--------------------------------------------------------------------------------

-- | The class of all readable state variables.
class Getter g m | g -> m where
   -- | Read the value of a state variable.
   get :: g a -> m a

instance Getter IORef     IO     where get = readIORef
instance Getter (STRef s) (ST s) where get = readSTRef
instance Getter TVar      STM    where get = readTVar

-- | A concrete implementation of a read-only state variable, carrying an @m@
-- action to read the value.
newtype GettableStateVar m a = GettableStateVar (m a)

instance Getter (GettableStateVar m) m where
   get (GettableStateVar g) = g

-- | Construct a 'GettableStateVar' from an @m@ action.
makeGettableStateVar :: m a -> GettableStateVar m a
makeGettableStateVar = GettableStateVar

--------------------------------------------------------------------------------

-- | The class of all writable state variables.
class Setter s m | s -> m where
   -- | Write a new value into a state variable.
   ($=) :: s a -> a -> m ()

instance Setter IORef     IO     where ($=) = writeIORef
instance Setter (STRef s) (ST s) where ($=) = writeSTRef
instance Setter TVar      STM    where ($=) = writeTVar

-- | A concrete implementation of a write-only state variable, carrying an @m@
-- action to write the new value.
newtype SettableStateVar m a = SettableStateVar (a -> m ())

instance Setter (SettableStateVar m) m where
   ($=) (SettableStateVar s) a = s a

-- | Construct a 'SettableStateVar' from an @m@ action.
makeSettableStateVar :: (a -> m ()) -> SettableStateVar m a
makeSettableStateVar = SettableStateVar

--------------------------------------------------------------------------------

-- | A concrete implementation of a readable and writable state variable,
-- carrying one @m@ action to read the value and another @m@ action to write the
-- new value.
data StateVar m a =
   StateVar (GettableStateVar m a) (SettableStateVar m a)

instance Getter (StateVar m) m where
   get (StateVar g _) = get g

instance Setter (StateVar m) m where
   ($=) (StateVar _ s) a = s $= a

-- | Construct a 'StateVar' from two @m@ actions, one for reading and one for
-- writing.
makeStateVar :: m a -> (a -> m ()) -> StateVar m a
makeStateVar g s = StateVar (makeGettableStateVar g) (makeSettableStateVar s)

--------------------------------------------------------------------------------

-- | A modificator convenience function, transforming the contents of a state
-- variable with a given funtion.

($~) :: (Getter v m, Setter v m, Monad m) => v a -> (a -> a) -> m ()
v $~ f = do
   x <- get v
   v $= f x

-- | A variant of '$=' which is strict in the value to be set.
($=!) :: Setter s m => s a -> a -> m ()
v $=! x = x `seq` v $= x

-- | A variant of '$~' which is strict in the transformed value.
($~!) :: (Getter v m, Setter v m, Monad m) => v a -> (a -> a) -> m ()
v $~! f = do
   x <- get v
   v $=! f x
