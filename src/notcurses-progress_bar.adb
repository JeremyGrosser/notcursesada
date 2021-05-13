package body Notcurses.Progress_Bar is

   function Create
      (Plane        : Notcurses_Plane;
       Retrograde   : Boolean := False)
      return Notcurses_Progress_Bar
   is
      Options : aliased Thin.ncprogbar_options :=
         (flags  => 0,
          others => 0);
   begin
      if Retrograde then
         Options.flags := Thin.NCPROGBAR_OPTION_RETROGRADE;
      end if;
      return Thin.ncprogbar_create (Plane, Options'Access);
   end Create;

   procedure Set_Progress
      (This     : Notcurses_Progress_Bar;
       Progress : Progress_Value)
   is
      use Interfaces.C;
   begin
      if Thin.ncprogbar_set_progress (This, double (Progress)) /= 0 then
         raise Notcurses_Error with "Failed to set progress";
      end if;
   end Set_Progress;

   function Progress
      (This : Notcurses_Progress_Bar)
      return Progress_Value
   is (Progress_Value (Thin.ncprogbar_progress (This)));

   procedure Destroy
      (This : in out Notcurses_Progress_Bar)
   is
   begin
      Thin.ncprogbar_destroy (This);
      This := null;
   end Destroy;

end Notcurses.Progress_Bar;
