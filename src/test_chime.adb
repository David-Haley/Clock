-- Author    : David Haley
-- Created   : 29/03/2019
-- Last Edit : 25/05/2026
-- Test program for the Chime package

--  20260525 : Compiler warnings removed
-- 20220125 : Get_Chiming removed.
-- 20190707 : Spelling of Chiming corrected

with Ada.Text_IO; use Ada.Text_IO;
with Chime; use Chime;

procedure Test_Chime is

   Test_Requested : Character := ' ';

begin -- Test_Chime
   loop -- one test
      Put_Line ("0: End Tests");
      Put_Line ("1: Chiming Enabled");
      Put_Line ("2: Chiming_Disabled");
      Put ("Test? ");
      Get (Test_Requested);
      case Test_Requested is
      when '0' =>
         exit;
      when '1' =>
         Chiming;
      when '2' =>
         Silent;
      when others =>
         Put_Line ("Invalid Command");
      end case; -- Test_Requested
   end loop; -- one test
   End_Chiming;
end Test_Chime;
