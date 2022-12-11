--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Interfaces;
with Interfaces.C.Strings;
with Interfaces.C;
with Notcurses_Thin;

package Notcurses is
   type Notcurses_Context is private;
   type Notcurses_Plane is private;

   type Coordinate is record
      Y, X : Integer;
   end record;

   function "+" (Left, Right : Coordinate) return Coordinate;
   function "-" (Left, Right : Coordinate) return Coordinate;

   Notcurses_Error : exception;

   function Version
      return String;

private

   package Thin renames Notcurses_Thin;

   type Notcurses_Context is access all Thin.notcurses;
   type Notcurses_Plane is access all Thin.ncplane;

   Default_Options : aliased Thin.notcurses_options :=
      (termtype => Interfaces.C.Strings.Null_Ptr,
       loglevel => Thin.NCLOGLEVEL_ERROR,
       flags    => 0,
       others   => 0);

   Default_Context : Notcurses_Context := null;

end Notcurses;
