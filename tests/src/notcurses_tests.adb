with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces;
with NC.Media;
with NC;

procedure Notcurses_Tests is
   Status  : NC.Status_Code;
   Options : aliased NC.Options :=
      (Log_Level => NC.Silent,
       Flags =>
         (Suppress_Banners => True,
          others           => False),
       others => <>);
   Context : access NC.Context;
begin
   Context := NC.Core_Init (Options'Access);
   if Context = null then
      return;
   end if;

   declare
      Root  : access NC.Plane := NC.Top (Context);
      Opts  : aliased NC.Plane_Options :=
         (Y       => 1,
          X       => 1,
          Rows    => 10,
          Columns => 10,
          others  => <>);
      Child : access NC.Plane := NC.Plane_Create (Root, Opts'Access);
   begin
      null;
   end;

   Status := NC.Render (Context);
   if NC.Is_Error (Status) then
      return;
   end if;

   declare
      I : aliased NC.Input;
      C : Interfaces.Unsigned_32;
   begin
      C := NC.Get (Context, null, I'Access);
   end;

   Status := NC.Leave_Alternate_Screen (Context);
   if NC.Is_Error (Status) then
      return;
   end if;

   Status := NC.Stop (Context);
end Notcurses_Tests;
