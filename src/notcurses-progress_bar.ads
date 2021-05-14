with Interfaces.C;

package Notcurses.Progress_Bar is
   type Notcurses_Progress_Bar is private;
   subtype Progress_Value is Interfaces.C.double range 0.0 .. 1.0;

   function Create
      (Plane        : Notcurses_Plane;
       Retrograde   : Boolean := False)
      return Notcurses_Progress_Bar;

   procedure Set_Progress
      (This     : Notcurses_Progress_Bar;
       Progress : Progress_Value);

   function Progress
      (This : Notcurses_Progress_Bar)
      return Progress_Value;

   procedure Destroy
      (This : in out Notcurses_Progress_Bar);

private

   type Notcurses_Progress_Bar is access all Thin.ncprogbar;

end Notcurses.Progress_Bar;
