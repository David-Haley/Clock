-- Package to provide secondary display functionality.
-- It is assumed that Update_Secondary is called once per second with
-- Step_Display True to update the secondary display contents.
-- Author    : David Haley
-- Created   : 17/07/2019
-- Last Edit : 09/06/2022

-- 20220609 : Port to 64 bit native compiler, Driver_Types renamed to
-- TLC5940_Driver_Types.
-- 20220126 : Removed Diagnostic_Strings and Current_Item, no longer required
-- replaced by reporting to User_Interface_Server. Next_Time changed to
-- Current_Time.
-- 20220115 : Step_Display added to allow brightness update without also,
-- updating Time_Remaining. Initialise_Secondary_Display added.
-- 20190726 : Resync_Secondary added
-- 20190725 : Diagnostic_Strings added as return type for Current_Item
-- 20190722 : Secondary_Configuration exception added
-- 20190718 : Current Item provided for diagnostic purposes.

with Ada.Calendar; use Ada.Calendar;
with TLC5940_Driver_Types; use TLC5940_Driver_Types;

package Secondary_Display is

   procedure Initialise_Secondary_Display;

   procedure Update_Secondary (Current_Time : in Time;
                               Display_Brightness : in Greyscales;
                               Step_Display : Boolean := False);

   procedure Resync_Secondary;
   -- causes secondary display to be cleared and restartes at 00 seconds.

   Secondary_Configuration : Exception;

end Secondary_Display;
