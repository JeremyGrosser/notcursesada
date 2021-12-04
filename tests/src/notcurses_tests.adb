with Interfaces.C.Strings; use Interfaces.C.Strings;
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

   Status := NC.Enter_Alternate_Screen (Context);
   if NC.Is_Error (Status) then
      return;
   end if;

   declare
      Plane : access NC.Plane := NC.Top (Context);
   begin
      null;
   end;

   Status := NC.Render (Context);
   if NC.Is_Error (Status) then
      return;
   end if;

   Status := NC.Leave_Alternate_Screen (Context);
   if NC.Is_Error (Status) then
      return;
   end if;

   Status := NC.Stop (Context);
end Notcurses_Tests;
