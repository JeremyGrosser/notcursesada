--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Unchecked_Conversion;
with Interfaces;
with Interfaces.C.Strings;
with Interfaces.C_Streams;
with Interfaces.C;
with Notcurses_Thin;

package Notcurses is
   type Notcurses_Context is private;
   type Notcurses_Plane is private;

   type Notcurses_Input is record
      Id     : Wide_Wide_Character;
      Y      : Interfaces.C.int;
      X      : Interfaces.C.int;
      Alt    : Boolean;
      Shift  : Boolean;
      Ctrl   : Boolean;
      Seqnum : Interfaces.Unsigned_64;
   end record;

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
       renderfp => Interfaces.C_Streams.NULL_Stream,
       loglevel => Thin.NCLOGLEVEL_ERROR,
       flags    => 0,
       others   => 0);

   Default_Context : Notcurses_Context := null;

   function To_Ada is new Ada.Unchecked_Conversion
      (Source => Thin.ncinput,
       Target => Notcurses_Input);

   function To_C   is new Ada.Unchecked_Conversion
      (Source => Notcurses_Input,
       Target => Thin.ncinput);

end Notcurses;
