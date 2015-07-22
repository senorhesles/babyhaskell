module OrphanGuestList where

import Data.Monoid
import Employee

instance Monoid GuestList where
  mempty = (GL [] 0)
  mappend = trueGlCons

trueGlCons :: GuestList -> GuestList -> GuestList
trueGlCons (GL x y) (GL z u) = (GL (x ++ z) (y + u))
