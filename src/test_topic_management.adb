--  This program is a simple test to vefify Topic_Management.json files.

--  Author    : David Haley
--  Created   : 08/05/2026
--  Last Edit : 04/06/2026

--  20260604: name Changed from Test_Topic_Manager to Test_Topic_Management.
--  20260603: Merged Topic Manager

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Topic_Manager; use Topic_Manager;

procedure Test_Topic_Management is

   Item_Id : Unbounded_String;
   
begin -- Test_Topic_Management
   Put_Line ("Test_Topic_Management version 20260604");
   Put_Line ("Reading topics");
   Read_Topics;
   loop -- Read one Item_Id
      Put ("Item_Id: ");
      Item_Id := Get_Line;
      exit when Length (Item_Id) = 0;
      Put_Line (Item_Id & ": """ &
        Get_For_Display (To_String (Item_Id)) & """");
   end loop; -- Read one Item_Id
end Test_Topic_Management;