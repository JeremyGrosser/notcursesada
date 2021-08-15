--
--  Copyright 2021 (C) Jeremy Grosser <jeremy@synack.me>
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

package body Notcurses is
   function Version
      return String
   is
   begin
      return Interfaces.C.Strings.Value (Thin.notcurses_version);
   end Version;
end Notcurses;
