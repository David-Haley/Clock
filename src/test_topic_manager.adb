--  This program is a simple test for Topic_Manager, to test the ability to
--  retrieve MQTT items.

--  Author    : David Haley
--  Created   : 08/05/2026
--  Last Edit : 20/05/2026

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with MQTT_Subscription; use MQTT_Subscription;
with Topic_Manager; use Topic_Manager;

procedure Test_Topic_Manager is

   Item_Id : Unbounded_String;
   
begin -- Test_Topic_Manager
   Put_Line ("Test_Topic_Manager 20260520");
   Read_Subscription;
   Read_Topics;
   loop -- Read one Item_Id
      Put ("Item_Id: ");
      Item_Id := Get_Line;
      exit when Length (Item_Id) = 0;
      Put_Line (Item_Id & ": """ &
        Get_For_Display (To_String (Item_Id)) & """");
   end loop; -- Read one Item_Id
end  Test_Topic_Manager;